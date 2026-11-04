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

import Control.Applicative ((<|>), many, some)
import Data.Maybe (listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, diffUTCTime)
import Data.Char (isAlpha, isDigit)
import Data.List (maximumBy, minimumBy, isPrefixOf)
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

-- | Parse a MiniZinc model and evaluate constant expressions to produce an ADT.
miniZincParser :: Parser ADT
miniZincParser = do
    iadt <- miniZincParserI
    adt <- evaluateIADT iadt
    eof
    pure adt

-- | Parse an intermediate AST (IADT) with unevaluated domain bounds.
miniZincParserI :: Parser IADT
miniZincParserI = do
    decls <- many declarationI
    constrs <- many constraintI
    goal <- solveGoal
    pure IADT { ideclarations = decls, iconstraints = constrs, isolverGoal = goal }

-- | Parse a single declaration (constant, variable, or array).
declarationI :: Parser IDeclaration
declarationI = constantDeclI <|> variableDeclI <|> arrayDeclI

-- | Parse a constant declaration: int: name = expr;
constantDeclI :: Parser IDeclaration
constantDeclI = do
    stringTok "int"
    charTok ':'
    name <- identifier
    charTok '='
    e <- expr
    charTok ';'
    pure $ IConstantDeclaration name e

-- | Parse a variable declaration: var domain: name;
variableDeclI :: Parser IDeclaration
variableDeclI = do
    stringTok "var"
    dom <- domainI
    charTok ':'
    name <- identifier
    charTok ';'
    pure $ IVariableDeclaration name dom

-- | Parse an array declaration: array[indexDom] of var valueDom: name;
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

-- | Parse a domain: lo..hi
domainI :: Parser IDomain
domainI = do
    lo <- expr
    stringTok ".."
    hi <- expr
    pure $ IRange lo hi

-- | Parse a constraint: constraint <body>;
constraintI :: Parser IConstraint
constraintI = do
    stringTok "constraint"
    c <- constraintBodyI
    charTok ';'
    pure c

-- | Parse a comparison operator (=, !=, <=, <, >=, >)
compareOp :: Parser CompareOperator
compareOp = (stringTok "=" *> pure OpEq)
        <|> (stringTok "!=" *> pure OpNeq)
        <|> (stringTok "<=" *> pure OpLe)
        <|> (stringTok "<" *> pure OpLt)
        <|> (stringTok ">=" *> pure OpGe)
        <|> (stringTok ">" *> pure OpGt)

-- | Parse a constraint body: either a macro or a comparison
constraintBodyI :: Parser IConstraint
constraintBodyI = macroConstraintI <|> compareConstraintI

-- | Parse a comparison constraint: expr op expr
compareConstraintI :: Parser IConstraint
compareConstraintI = do
    a <- expr
    op <- compareOp
    b <- expr
    pure $ ICompare op a b

-- | Parse a macro constraint (sum, min, max, all, any, allDifferent)
-- Each can have literal, array, or generator form.
-- Numeric macros (sum, min, max) require a comparison operator and RHS expression.
-- Boolean macros (all, any, allDifferent) do not.
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

-- | Parse a macro name (sum, min, max, allDifferent, all, any)
macroNameP :: Parser MacroName
macroNameP = (stringTok "sum" *> pure MacroSum)
        <|> (stringTok "min" *> pure MacroMin)
        <|> (stringTok "max" *> pure MacroMax)
        <|> (stringTok "allDifferent" *> pure MacroAllDifferent)
        <|> (stringTok "all" *> pure MacroAll)
        <|> (stringTok "any" *> pure MacroAny)

-- | Parse a macro form: generator (i in dom)(expr), array (arr), or literal [expr,...]
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

-- | Parse the solve goal: solve satisfy; | solve maximize expr; | solve minimize expr;
solveGoal :: Parser SolverGoal
solveGoal = do
    stringTok "solve"
    goal <- (stringTok "satisfy" *> pure Satisfy)
        <|> (stringTok "maximize" *> (Maximize <$> expr))
        <|> (stringTok "minimize" *> (Minimize <$> expr))
    charTok ';'
    pure goal

-- | Parse an expression (entry point)
expr :: Parser Expr
expr = parseAddSub

-- | Parse addition and subtraction (lowest precedence)
parseAddSub :: Parser Expr
parseAddSub = parseMulDiv `chainl1` ((stringTok "+" *> pure Add) <|> (stringTok "-" *> pure Sub))

-- | Parse multiplication, division, and modulo (medium precedence)
parseMulDiv :: Parser Expr
parseMulDiv = parseAtom `chainl1`
    ((stringTok "*" *> pure Mul) <|> (stringTok "/" *> pure Div) <|> (stringTok "mod" *> pure Mod))

-- | Parse an atomic expression: array index, parenthesized, integer literal, or variable
parseAtom :: Parser Expr
parseAtom = parseArrayIndex <|> parseParen <|> parseIntLit <|> parseVar

-- | Parse an array index expression: arr[expr]
parseArrayIndex :: Parser Expr
parseArrayIndex = do
    name <- identifier
    charTok '['
    idx <- expr
    charTok ']'
    pure $ ArrayIndex name idx

-- | Parse a parenthesized expression: (expr)
parseParen :: Parser Expr
parseParen = do
    charTok '('
    e <- expr
    charTok ')'
    pure e

-- | Parse an integer literal
parseIntLit :: Parser Expr
parseIntLit = IntLit <$> int

-- | Parse a variable reference
parseVar :: Parser Expr
parseVar = Var <$> identifier

-- | Parse an identifier (alphanumeric + underscore)
identifier :: Parser Identifier
identifier = tok $ some (satisfy (\c -> isAlpha c || isDigit c || c == '_'))

-- | Left-associative parser combinator for binary operators
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

-- | Evaluate IADT to ADT by resolving constant expressions in domains.
-- First pass: collect constant declarations.
-- Second pass: evaluate variable/array domains using constant environment.
evaluateIADT :: IADT -> Parser ADT
evaluateIADT iadt = do
    let constEnv = evalConstants [] (ideclarations iadt)
    decls <- mapM (evalDecl constEnv) (ideclarations iadt)
    constrs <- mapM (evalConstraintI constEnv) (iconstraints iadt)
    pure ADT { declarations = decls, constraints = constrs, solverGoal = isolverGoal iadt }

-- | Collect and evaluate all constant declarations into an environment.
evalConstants :: ConstEnv -> [IDeclaration] -> ConstEnv
evalConstants env [] = env
evalConstants env (IConstantDeclaration name e : rest) =
    case evalExprPure env e of
        Just v -> evalConstants ((name, v) : env) rest
        Nothing -> evalConstants env rest  -- Should not happen for valid programs
evalConstants env (_ : rest) = evalConstants env rest

-- | Evaluate an IDeclaration to a Declaration using the constant environment.
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

-- | Evaluate an IConstraint to a Constraint using the constant environment.
evalConstraintI :: ConstEnv -> IConstraint -> Parser Constraint
evalConstraintI env (ICompare op a b) = pure $ Compare op a b
evalConstraintI env (IMacroConstraint macroName mOp form mRhs) = do
    form' <- evalMacroForm env form
    pure $ MacroConstraint macroName mOp form' mRhs

-- | Evaluate a macro form using the constant environment.
evalMacroForm :: ConstEnv -> IMacroForm -> Parser MacroForm
evalMacroForm env (IMacroLiteral es) = pure $ MacroLiteral es
evalMacroForm env (IMacroArray ident) = pure $ MacroArray ident
evalMacroForm env (IMacroGen var (IRange lo hi) body) = do
    lo' <- liftMaybe (evalExprPure env lo) "generator lower bound"
    hi' <- liftMaybe (evalExprPure env hi) "generator upper bound"
    pure $ MacroGen var (Range lo' hi') body

-- | Lift a Maybe value to a Parser, failing with a custom message on Nothing.
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

-- | Solve a MiniZinc problem, returning the first/best solution.
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

-- | Select the solution with the maximum objective value.
maximize :: Expr -> [[Binding]] -> Maybe [(String, Int)]
maximize obj solutions =
    case filter (not . null) solutions of
        [] -> Nothing
        sols -> Just $ maximumBy (comparing (evalObjective obj)) sols

-- | Select the solution with the minimum objective value.
minimize :: Expr -> [[Binding]] -> Maybe [(String, Int)]
minimize obj solutions =
    case filter (not . null) solutions of
        [] -> Nothing
        sols -> Just $ minimumBy (comparing (evalObjective obj)) sols

-- | Evaluate the objective expression under a given set of bindings.
evalObjective :: Expr -> [Binding] -> Int
evalObjective obj bindings =
    case evalExpr (makeProblem bindings) obj of
        Just v -> v
        Nothing -> 0

-- | Create a minimal ConstraintProblem for objective evaluation.
makeProblem :: [Binding] -> ConstraintProblem
makeProblem bindings =
    ConstraintProblem
        { searchConstants = []
        , solvedVars = bindings
        , remainingDeclarations = []
        , searchConstraints = []
        }

-- | Depth-first search for all valid variable assignments.
-- Returns a list of all solutions (each solution is a list of bindings).
search :: ConstraintProblem -> [[Binding]]
search p = fst (searchWithCount p)

-- | Search with node counting.
-- Each variable assignment and array element increments the node counter.
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
                    go prob { remainingDeclarations = rest
                            , searchConstants = (name, v) : searchConstants prob }
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

    -- | Combine multiple (solutions, nodes) results by concatenating solutions and summing nodes.
    combineResults :: [([[Binding]], Int)] -> ([[Binding]], Int)
    combineResults = foldr (\(sols1, n1) (sols2, n2) -> (sols1 ++ sols2, n1 + n2)) ([], 0)

-- | Solve a problem with full statistics (nodes, time, objective value).
-- Forces strict evaluation of the search to measure actual solve time.
solveProblemWithStats :: ADT -> IO SolverResult
solveProblemWithStats adt = do
    startTime <- getCurrentTime
    -- Force evaluation of the search before measuring end time
    let (solutions, nodes) = searchWithCount problem
    -- Use seq to force the tuple (and thus the search) to be evaluated now
    -- Evaluate the length of solutions to force the entire list
    length solutions `seq` nodes `seq` pure ()
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

-- | Check all constraints that can be fully evaluated with current assignments.
-- Returns False immediately if any evaluable constraint is violated (enabling incremental backtracking).
constraintsSatisfiedSoFar :: ConstraintProblem -> Bool
constraintsSatisfiedSoFar p =
    all (\c -> case evalConstraint p c of
                    Just False -> False  -- constraint violated, prune this branch
                    _          -> True)  -- True or Nothing (not yet evaluable) -> continue
        (searchConstraints p)

-- | Get all integer values in a domain range.
domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues _ (Range lo hi) = [lo..hi]

-- | Generic expression evaluator parameterized by a variable lookup function.
-- Returns Nothing for division by zero or unresolved variables.
evalExprGeneric :: (Identifier -> Maybe Int) -> Expr -> Maybe Int
evalExprGeneric lookupVar (IntLit n) = Just n
evalExprGeneric lookupVar (Var x) = lookupVar x
evalExprGeneric lookupVar (Add a b) = (+) <$> evalExprGeneric lookupVar a <*> evalExprGeneric lookupVar b
evalExprGeneric lookupVar (Sub a b) = (-) <$> evalExprGeneric lookupVar a <*> evalExprGeneric lookupVar b
evalExprGeneric lookupVar (Mul a b) = (*) <$> evalExprGeneric lookupVar a <*> evalExprGeneric lookupVar b
evalExprGeneric lookupVar (Div a b) = do
    a' <- evalExprGeneric lookupVar a
    b' <- evalExprGeneric lookupVar b
    if b' == 0 then Nothing else Just (a' `div` b')
evalExprGeneric lookupVar (Mod a b) = do
    a' <- evalExprGeneric lookupVar a
    b' <- evalExprGeneric lookupVar b
    if b' == 0 then Nothing else Just (a' `mod` b')
evalExprGeneric lookupVar (ArrayIndex arr idx) = do
    idx' <- evalExprGeneric lookupVar idx
    lookupVar (arr ++ "[" ++ show idx' ++ "]")

-- | Evaluate an expression in the context of a ConstraintProblem.
-- Looks up variables in searchConstants and solvedVars.
evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr p = evalExprGeneric lookupVar
  where
    lookupVar x = lookup x (searchConstants p) <|> lookup x (solvedVars p)

-- | Pure evaluation of expressions using only the constant environment.
-- Returns Nothing for division by zero or array indices (not allowed in constants).
evalExprPure :: ConstEnv -> Expr -> Maybe Int
evalExprPure env = evalExprGeneric (\x -> lookup x env)

-- | Evaluate a constraint, returning Just True/False if fully evaluable, Nothing otherwise.
evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompareOp op <$> evalExpr p a <*> evalExpr p b
evalConstraint p (MacroConstraint macroName mOp form mRhs) = evalMacro p macroName mOp form mRhs

-- | Evaluate a macro constraint (sum, min, max, all, any, allDifferent).
-- Numeric macros compare their result with a RHS expression using the given operator.
-- Boolean macros (all, any, allDifferent) check properties of the value list.
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

-- | Apply a comparison operator, returning Just Bool if operator is provided.
applyCompare :: Maybe CompareOperator -> Int -> Int -> Maybe Bool
applyCompare (Just op) lhs rhs = Just (applyCompareOp op lhs rhs)
applyCompare Nothing _ _ = Nothing

-- | Apply a concrete comparison operator.
applyCompareOp :: CompareOperator -> Int -> Int -> Bool
applyCompareOp OpEq = (==)
applyCompareOp OpNeq = (/=)
applyCompareOp OpLt = (<)
applyCompareOp OpLe = (<=)
applyCompareOp OpGt = (>)
applyCompareOp OpGe = (>=)

-- | Check if all elements in a list are distinct.
allDifferent :: [Int] -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- | Evaluate a macro form to a list of integer values.
evalMacroFormVals :: ConstraintProblem -> MacroForm -> Maybe [Int]
evalMacroFormVals p (MacroLiteral es) = mapM (evalExpr p) es
evalMacroFormVals p (MacroArray arrName) = do
    let prefix = arrName ++ "["
        vars = [v | (k, v) <- solvedVars p, isPrefixOf prefix k]
    pure vars
evalMacroFormVals p (MacroGen var dom body) = do
    let vals = domainValues p dom
    mapM (\v -> evalExpr (p { solvedVars = (var, v) : solvedVars p }) body) vals

-- | Check if all constraints are fully satisfied (no Nothing results).
allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

-- | Get current time as a formatted string for filenames.
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
