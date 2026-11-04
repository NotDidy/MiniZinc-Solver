module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , SolverResult (..)
    , SolverStats (..)
    , miniZincParser
    , solveProblem
    , solveProblemWithStats
    , getTime
    , saveSolution
    ) where

import Control.Applicative ((<|>), many, some, Alternative)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, diffUTCTime)
import Data.Char (isAlpha, isDigit)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

import Instances (ParseError (UnexpectedString), Parser)
import Parser (failed, stringTok, charTok, eof, tok, satisfy, int, commaTok, sepBy)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    | ArrayDeclaration Identifier Domain Domain  -- array[indexDomain] of var valueDomain: name
    deriving (Eq, Show)

data Domain
    = Range Int Int  -- low..high (evaluated at parse time)
    deriving (Eq, Show)

data Expr
    = IntLit Int
    | Var Identifier
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | ArrayIndex Identifier Expr  -- arr[idx]
    deriving (Eq, Show)

data CompareOperator
    = OpEq
    | OpNeq
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    deriving (Eq, Show)

-- Macro names for constraints
data MacroName
    = MacroSum
    | MacroMin
    | MacroMax
    | MacroAll
    | MacroAny
    | MacroAllDifferent
    deriving (Eq, Show)

-- Forms that a macro can take
data MacroForm
    = MacroLiteral [Expr]           -- macro([e1, e2, ...])
    | MacroArray Identifier         -- macro(arr)
    | MacroGen Identifier Domain Expr  -- macro(i in domain)(expr)
    deriving (Eq, Show)

data Constraint
    = Compare CompareOperator Expr Expr
    | MacroConstraint MacroName (Maybe CompareOperator) MacroForm (Maybe Expr)  -- numeric macros: Just op, Just rhsExpr
    deriving (Eq, Show)

data SolverGoal
    = Satisfy
    | Maximize Expr
    | Minimize Expr
    deriving (Eq, Show)

-- | Statistics collected during solving
data SolverStats = SolverStats
    { nodesExplored :: Int
    , solveTimeMs   :: Double
    , objectiveValue :: Maybe Int
    } deriving (Eq, Show)

-- | Complete solver result with all solutions and statistics
data SolverResult = SolverResult
    { allSolutions  :: [[(String, Int)]]
    , bestSolution  :: Maybe [(String, Int)]
    , solverStats   :: SolverStats
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Intermediate AST (with Expr bounds for parsing)
--------------------------------------------------------------------------------

data IDomain
    = IRange Expr Expr
    deriving (Eq, Show)

data IDeclaration
    = IConstantDeclaration Identifier Expr
    | IVariableDeclaration Identifier IDomain
    | IArrayDeclaration Identifier IDomain IDomain
    deriving (Eq, Show)

data IConstraint
    = ICompare CompareOperator Expr Expr
    | IMacroConstraint MacroName (Maybe CompareOperator) IMacroForm (Maybe Expr)
    deriving (Eq, Show)

data IMacroForm
    = IMacroLiteral [Expr]
    | IMacroArray Identifier
    | IMacroGen Identifier IDomain Expr
    deriving (Eq, Show)

data IADT = IADT
    { ideclarations :: [IDeclaration]
    , iconstraints :: [IConstraint]
    , isolverGoal :: SolverGoal
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser (produces IADT with Expr bounds)
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = do
    iadt <- miniZincParserI
    adt <- evaluateIADT iadt
    eof
    pure adt

miniZincParserI :: Parser IADT
miniZincParserI = do
    decls <- many declarationI
    constrs <- many constraintI
    goal <- solveGoal
    pure IADT { ideclarations = decls, iconstraints = constrs, isolverGoal = goal }

declarationI :: Parser IDeclaration
declarationI = constantDeclI <|> variableDeclI <|> arrayDeclI

constantDeclI :: Parser IDeclaration
constantDeclI = do
    stringTok "int"
    charTok ':'
    name <- identifier
    charTok '='
    e <- expr
    charTok ';'
    pure $ IConstantDeclaration name e

variableDeclI :: Parser IDeclaration
variableDeclI = do
    stringTok "var"
    dom <- domainI
    charTok ':'
    name <- identifier
    charTok ';'
    pure $ IVariableDeclaration name dom

arrayDeclI :: Parser IDeclaration
arrayDeclI = do
    stringTok "array"
    charTok '['
    indexDom <- domainI
    charTok ']'
    stringTok "of"
    stringTok "var"
    valueDom <- domainI
    charTok ':'
    name <- identifier
    charTok ';'
    pure $ IArrayDeclaration name indexDom valueDom

domainI :: Parser IDomain
domainI = do
    lo <- expr
    stringTok ".."
    hi <- expr
    pure $ IRange lo hi

constraintI :: Parser IConstraint
constraintI = do
    stringTok "constraint"
    c <- constraintBodyI
    charTok ';'
    pure c

compareOp :: Parser CompareOperator
compareOp = (stringTok "=" *> pure OpEq)
        <|> (stringTok "!=" *> pure OpNeq)
        <|> (stringTok "<=" *> pure OpLe)
        <|> (stringTok "<" *> pure OpLt)
        <|> (stringTok ">=" *> pure OpGe)
        <|> (stringTok ">" *> pure OpGt)

constraintBodyI :: Parser IConstraint
constraintBodyI = macroConstraintI <|> compareConstraintI

compareConstraintI :: Parser IConstraint
compareConstraintI = do
    a <- expr
    op <- compareOp
    b <- expr
    pure $ ICompare op a b

-- Parse macro constraints: sum, min, max, all, any, allDifferent
-- Each can have literal, array, or generator form
-- Numeric macros (sum, min, max) need comparison operator
-- Boolean macros (all, any, allDifferent) don't
macroConstraintI :: Parser IConstraint
macroConstraintI = do
    macroName <- macroNameP
    charTok '('
    form <- macroFormP
    case macroName of
        MacroAllDifferent -> pure $ IMacroConstraint macroName Nothing form Nothing
        MacroAll -> pure $ IMacroConstraint macroName Nothing form Nothing
        MacroAny -> pure $ IMacroConstraint macroName Nothing form Nothing
        -- Numeric macros need comparison and RHS expression
        _ -> do
            op <- compareOp
            rhs <- expr
            pure $ IMacroConstraint macroName (Just op) form (Just rhs)

macroNameP :: Parser MacroName
macroNameP = (stringTok "sum" *> pure MacroSum)
        <|> (stringTok "min" *> pure MacroMin)
        <|> (stringTok "max" *> pure MacroMax)
        <|> (stringTok "allDifferent" *> pure MacroAllDifferent)
        <|> (stringTok "all" *> pure MacroAll)
        <|> (stringTok "any" *> pure MacroAny)

macroFormP :: Parser IMacroForm
macroFormP = genForm <|> arrayForm <|> listForm
  where
    genForm = do
        var <- identifier
        stringTok "in"
        dom <- domainI
        charTok ')'
        charTok '('
        e <- expr
        charTok ')'
        pure $ IMacroGen var dom e
    arrayForm = do
        ident <- identifier
        charTok ')'
        pure $ IMacroArray ident
    listForm = do
        charTok '['
        es <- expr `sepBy` commaTok
        charTok ']'
        charTok ')'
        pure $ IMacroLiteral es

-- Solver goal parser (same for both ASTs)
solveGoal :: Parser SolverGoal
solveGoal = do
    stringTok "solve"
    goal <- (stringTok "satisfy" *> pure Satisfy)
        <|> (stringTok "maximize" *> (Maximize <$> expr))
        <|> (stringTok "minimize" *> (Minimize <$> expr))
    charTok ';'
    pure goal

expr :: Parser Expr
expr = parseAddSub

parseAddSub :: Parser Expr
parseAddSub = parseMulDiv `chainl1` ( (stringTok "+" *> pure Add) <|> (stringTok "-" *> pure Sub) )

parseMulDiv :: Parser Expr
parseMulDiv = parseAtom `chainl1` ( (stringTok "*" *> pure Mul) <|> (stringTok "/" *> pure Div) <|> (stringTok "mod" *> pure Mod) )

parseAtom :: Parser Expr
parseAtom = parseArrayIndex <|> parseParen <|> parseIntLit <|> parseVar

parseArrayIndex :: Parser Expr
parseArrayIndex = do
    name <- identifier
    charTok '['
    idx <- expr
    charTok ']'
    pure $ ArrayIndex name idx

parseParen :: Parser Expr
parseParen = do
    charTok '('
    e <- expr
    charTok ')'
    pure e

parseIntLit :: Parser Expr
parseIntLit = IntLit <$> int

parseVar :: Parser Expr
parseVar = Var <$> identifier

identifier :: Parser Identifier
identifier = tok $ some (satisfy (\c -> isAlpha c || isDigit c || c == '_'))

-- Helper: chainl1 for left-associative operators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    x <- p
    rest x
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> pure x

--------------------------------------------------------------------------------
-- Evaluate IADT to ADT (resolve constant expressions in domains)
--------------------------------------------------------------------------------

type ConstEnv = [(Identifier, Int)]

evaluateIADT :: IADT -> Parser ADT
evaluateIADT iadt = do
    -- First pass: evaluate all constant declarations
    let constEnv = evalConstants [] (ideclarations iadt)
    -- Second pass: evaluate domains using constants
    decls <- mapM (evalDecl constEnv) (ideclarations iadt)
    constrs <- mapM (evalConstraintI constEnv) (iconstraints iadt)
    pure ADT { declarations = decls, constraints = constrs, solverGoal = isolverGoal iadt }

evalConstants :: ConstEnv -> [IDeclaration] -> ConstEnv
evalConstants env [] = env
evalConstants env (IConstantDeclaration name e : rest) =
    case evalExprPure env e of
        Just v -> evalConstants ((name, v) : env) rest
        Nothing -> evalConstants env rest  -- Should not happen for valid programs
evalConstants env (_ : rest) = evalConstants env rest

evalExprPure :: ConstEnv -> Expr -> Maybe Int
evalExprPure env (IntLit n) = Just n
evalExprPure env (Var x) = lookup x env
evalExprPure env (Add a b) = (+) <$> evalExprPure env a <*> evalExprPure env b
evalExprPure env (Sub a b) = (-) <$> evalExprPure env a <*> evalExprPure env b
evalExprPure env (Mul a b) = (*) <$> evalExprPure env a <*> evalExprPure env b
evalExprPure env (Div a b) = do
    a' <- evalExprPure env a
    b' <- evalExprPure env b
    if b' == 0 then Nothing else Just (a' `div` b')
evalExprPure env (Mod a b) = do
    a' <- evalExprPure env a
    b' <- evalExprPure env b
    if b' == 0 then Nothing else Just (a' `mod` b')
evalExprPure _ (ArrayIndex _ _) = Nothing  -- Not allowed in constant expressions

evalDecl :: ConstEnv -> IDeclaration -> Parser Declaration
evalDecl env (IConstantDeclaration name e) = pure $ ConstantDeclaration name e
evalDecl env (IVariableDeclaration name (IRange lo hi)) = do
    lo' <- liftMaybe (evalExprPure env lo) "lower bound"
    hi' <- liftMaybe (evalExprPure env hi) "upper bound"
    pure $ VariableDeclaration name (Range lo' hi')
evalDecl env (IArrayDeclaration name (IRange lo hi) (IRange lo2 hi2)) = do
    lo' <- liftMaybe (evalExprPure env lo) "index lower bound"
    hi' <- liftMaybe (evalExprPure env hi) "index upper bound"
    lo2' <- liftMaybe (evalExprPure env lo2) "value lower bound"
    hi2' <- liftMaybe (evalExprPure env hi2) "value upper bound"
    pure $ ArrayDeclaration name (Range lo' hi') (Range lo2' hi2')

evalConstraintI :: ConstEnv -> IConstraint -> Parser Constraint
evalConstraintI env (ICompare op a b) = pure $ Compare op a b
evalConstraintI env (IMacroConstraint macroName mOp form mRhs) = do
    form' <- evalMacroForm env form
    pure $ MacroConstraint macroName mOp form' mRhs

evalMacroForm :: ConstEnv -> IMacroForm -> Parser MacroForm
evalMacroForm env (IMacroLiteral es) = pure $ MacroLiteral es
evalMacroForm env (IMacroArray ident) = pure $ MacroArray ident
evalMacroForm env (IMacroGen var (IRange lo hi) body) = do
    lo' <- liftMaybe (evalExprPure env lo) "generator lower bound"
    hi' <- liftMaybe (evalExprPure env hi) "generator upper bound"
    pure $ MacroGen var (Range lo' hi') body

liftMaybe :: Maybe a -> String -> Parser a
liftMaybe (Just x) _ = pure x
liftMaybe Nothing msg = failed (UnexpectedString ("Failed to evaluate " ++ msg ++ " (referenced constant not defined?)"))

--------------------------------------------------------------------------------
-- Solver
--------------------------------------------------------------------------------

type Binding = (Identifier, Int)

data ConstraintProblem = ConstraintProblem
    { searchConstants :: [Binding]
    , solvedVars :: [Binding]
    , remainingDeclarations :: [Declaration]
    , searchConstraints :: [Constraint]
    }

solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt =
    case solverGoal adt of
        Satisfy -> listToMaybe (search problem)
        Maximize obj -> maximize obj (search problem)
        Minimize obj -> minimize obj (search problem)
  where
    problem =
        ConstraintProblem
            { searchConstants = []
            , solvedVars = []
            , remainingDeclarations = declarations adt
            , searchConstraints = constraints adt
            }

maximize :: Expr -> [[Binding]] -> Maybe [(String, Int)]
maximize obj solutions =
    case filter (not . null) solutions of
        [] -> Nothing
        sols -> Just $ maximumBy (comparing (evalObjective obj)) sols

minimize :: Expr -> [[Binding]] -> Maybe [(String, Int)]
minimize obj solutions =
    case filter (not . null) solutions of
        [] -> Nothing
        sols -> Just $ minimumBy (comparing (evalObjective obj)) sols

evalObjective :: Expr -> [Binding] -> Int
evalObjective obj bindings =
    case evalExpr (makeProblem bindings) obj of
        Just v -> v
        Nothing -> 0

makeProblem :: [Binding] -> ConstraintProblem
makeProblem bindings =
    ConstraintProblem
        { searchConstants = []
        , solvedVars = bindings
        , remainingDeclarations = []
        , searchConstraints = []
        }

search :: ConstraintProblem -> [[Binding]]
search p = case remainingDeclarations p of
    [] -> if allSatisfied p then [reverse (solvedVars p)] else []
    ConstantDeclaration name e : rest ->
        case evalExpr p e of
            Nothing -> []
            Just v ->
                search
                    p
                        { remainingDeclarations = rest
                        , searchConstants = (name, v) : searchConstants p
                        }
    VariableDeclaration name dom : rest -> do
        v <- domainValues p dom
        let p' = p { remainingDeclarations = rest, solvedVars = (name, v) : solvedVars p }
        if constraintsSatisfiedSoFar p'
            then search p'
            else []
    ArrayDeclaration name indexDom valueDom : rest -> do
        -- For arrays, we need to generate all combinations of values for each index
        let indices = domainValues p indexDom
        values <- mapM (\_ -> domainValues p valueDom) indices
        let bindings = zipWith (\idx val -> (name ++ "[" ++ show idx ++ "]", val)) indices values
        let p' = p { remainingDeclarations = rest, solvedVars = bindings ++ solvedVars p }
        if constraintsSatisfiedSoFar p'
            then search p'
            else []

-- | Search with node counting (each variable assignment = 1 node)
-- Returns (all solutions, total nodes explored)
searchWithCount :: ConstraintProblem -> ([[Binding]], Int)
searchWithCount p = go p
  where
    go prob = case remainingDeclarations prob of
        [] -> if allSatisfied prob
                 then ([reverse (solvedVars prob)], 1)
                 else ([], 1)
        ConstantDeclaration name e : rest ->
            case evalExpr prob e of
                Nothing -> ([], 1)
                Just v ->
                    let prob' = prob { remainingDeclarations = rest
                                     , searchConstants = (name, v) : searchConstants prob }
                    in go prob'
        VariableDeclaration name dom : rest ->
            let results = map (\v ->
                    let prob' = prob { remainingDeclarations = rest
                                     , solvedVars = (name, v) : solvedVars prob }
                    in if constraintsSatisfiedSoFar prob'
                          then go prob'
                          else ([], 1))
                    (domainValues prob dom)
            in combineResults results
        ArrayDeclaration name indexDom valueDom : rest ->
            let indices = domainValues prob indexDom
                allValues = sequence (replicate (length indices) (domainValues prob valueDom))
                results = map (\values ->
                    let bindings = zipWith (\idx val -> (name ++ "[" ++ show idx ++ "]", val)) indices values
                        prob' = prob { remainingDeclarations = rest
                                     , solvedVars = bindings ++ solvedVars prob }
                    in if constraintsSatisfiedSoFar prob'
                          then go prob'
                          else ([], 1))
                    allValues
            in combineResults results

    -- Combine multiple (solutions, nodes) results by concatenating solutions and summing nodes
    combineResults :: [([[Binding]], Int)] -> ([[Binding]], Int)
    combineResults = foldr (\(sols1, n1) (sols2, n2) -> (sols1 ++ sols2, n1 + n2)) ([], 0)

-- | Solve with statistics (nodes explored, time, objective value)
solveProblemWithStats :: ADT -> IO SolverResult
solveProblemWithStats adt = do
    startTime <- getCurrentTime
    let (solutions, nodes) = searchWithCount problem
    endTime <- getCurrentTime
    let solveTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000
        bestSol = case solverGoal adt of
            Satisfy -> listToMaybe solutions
            Maximize obj -> maximize obj solutions
            Minimize obj -> minimize obj solutions
        objVal = case (solverGoal adt, bestSol) of
            (Maximize obj, Just sol) -> Just (evalObjective obj sol)
            (Minimize obj, Just sol) -> Just (evalObjective obj sol)
            _ -> Nothing
    pure SolverResult
        { allSolutions = solutions
        , bestSolution = bestSol
        , solverStats = SolverStats
            { nodesExplored = nodes
            , solveTimeMs = solveTimeMs
            , objectiveValue = objVal
            }
        }
  where
    problem =
        ConstraintProblem
            { searchConstants = []
            , solvedVars = []
            , remainingDeclarations = declarations adt
            , searchConstraints = constraints adt
            }

-- Check all constraints that can be fully evaluated with current assignments.
-- Returns False immediately if any evaluable constraint is violated (enabling incremental backtracking).
constraintsSatisfiedSoFar :: ConstraintProblem -> Bool
constraintsSatisfiedSoFar p =
    all (\c -> case evalConstraint p c of
                    Just False -> False  -- constraint violated, prune this branch
                    _          -> True)  -- True or Nothing (not yet evaluable) -> continue
        (searchConstraints p)

domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues _ (Range lo hi) = [lo..hi]

evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr p (IntLit n) = Just n
evalExpr p (Var x) = lookup x (searchConstants p) <|> lookup x (solvedVars p)
evalExpr p (Add a b) = (+) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Sub a b) = (-) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Mul a b) = (*) <$> evalExpr p a <*> evalExpr p b
evalExpr p (Div a b) = do
    a' <- evalExpr p a
    b' <- evalExpr p b
    if b' == 0 then Nothing else Just (a' `div` b')
evalExpr p (Mod a b) = do
    a' <- evalExpr p a
    b' <- evalExpr p b
    if b' == 0 then Nothing else Just (a' `mod` b')
evalExpr p (ArrayIndex arr idx) = do
    idx' <- evalExpr p idx
    lookup (arr ++ "[" ++ show idx' ++ "]") (solvedVars p)

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompareOp op <$> evalExpr p a <*> evalExpr p b
evalConstraint p (MacroConstraint macroName mOp form mRhs) = evalMacro p macroName mOp form mRhs

evalMacro :: ConstraintProblem -> MacroName -> Maybe CompareOperator -> MacroForm -> Maybe Expr -> Maybe Bool
evalMacro p macroName mOp form mRhs = do
    vals <- evalMacroFormVals p form
    case macroName of
        MacroAllDifferent -> pure $ allDifferent vals
        MacroAll -> pure $ all (toBool) vals
        MacroAny -> pure $ any (toBool) vals
        -- Numeric macros: compare macro result with RHS expression
        MacroSum -> do
            rhsVal <- evalExpr p =<< mRhs
            applyCompare mOp (sum vals) rhsVal
        MacroMin -> do
            rhsVal <- evalExpr p =<< mRhs
            applyCompare mOp (minimum vals) rhsVal
        MacroMax -> do
            rhsVal <- evalExpr p =<< mRhs
            applyCompare mOp (maximum vals) rhsVal
  where
    toBool 0 = False
    toBool _ = True

applyCompare :: Maybe CompareOperator -> Int -> Int -> Maybe Bool
applyCompare (Just op) lhs rhs = Just (applyCompareOp op lhs rhs)
applyCompare Nothing _ _ = Nothing

applyCompareOp :: CompareOperator -> Int -> Int -> Bool
applyCompareOp OpEq = (==)
applyCompareOp OpNeq = (/=)
applyCompareOp OpLt = (<)
applyCompareOp OpLe = (<=)
applyCompareOp OpGt = (>)
applyCompareOp OpGe = (>=)

allDifferent :: [Int] -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

evalMacroFormVals :: ConstraintProblem -> MacroForm -> Maybe [Int]
evalMacroFormVals p (MacroLiteral es) = mapM (evalExpr p) es
evalMacroFormVals p (MacroArray arrName) = do
    let prefix = arrName ++ "["
        vars = [v | (k, v) <- solvedVars p, prefix `startsWith` k]
    pure vars
evalMacroFormVals p (MacroGen var dom body) = do
    let vals = domainValues p dom
    mapM (\v -> evalExpr (p { solvedVars = (var, v) : solvedVars p }) body) vals

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- | Save a solution to a file with the format:
-- -- Generated: <currentTime>
-- x = 1;
-- y = 2;
saveSolution :: [(String, Int)] -> IO ()
saveSolution solution = do
    currentTime <- getTime
    let filename = "solution_" ++ currentTime ++ ".out"
        header = "-- Generated: " ++ currentTime
        body = unlines [ name ++ " = " ++ show value ++ ";" | (name, value) <- solution ]
        content = header ++ "\n" ++ body ++ "\n"
    writeFile filename content
