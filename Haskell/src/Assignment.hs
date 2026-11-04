module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Control.Applicative ((<|>), many, some, Alternative)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
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

data Constraint
    = Compare CompareOperator Expr Expr
    | AllDifferent [Expr]  -- allDifferent([...])
    | AllDifferentArray Identifier  -- allDifferent(arr) where arr is an array
    | AllDifferentGen Identifier Domain Expr  -- allDifferent(i in domain)(expr)
    deriving (Eq, Show)

data SolverGoal
    = Satisfy
    | Maximize Expr
    | Minimize Expr
    deriving (Eq, Show)

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
    | IAllDifferent [Expr]
    | IAllDifferentArray Identifier
    | IAllDifferentGen Identifier IDomain Expr
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
constraintBodyI = compareConstraintI <|> allDifferentConstraintI

compareConstraintI :: Parser IConstraint
compareConstraintI = do
    a <- expr
    op <- compareOp
    b <- expr
    pure $ ICompare op a b

allDifferentConstraintI :: Parser IConstraint
allDifferentConstraintI = do
    stringTok "allDifferent"
    charTok '('
    genForm <|> arrayForm <|> listForm
  where
    genForm = do
        var <- identifier
        stringTok "in"
        dom <- domainI
        charTok ')'
        charTok '('
        e <- expr
        charTok ')'
        pure $ IAllDifferentGen var dom e
    arrayForm = do
        ident <- identifier
        charTok ')'
        pure $ IAllDifferentArray ident
    listForm = do
        es <- expr `sepBy` commaTok
        charTok ')'
        pure $ IAllDifferent es

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
evalConstraintI env (IAllDifferent es) = pure $ AllDifferent es
evalConstraintI env (IAllDifferentArray ident) = pure $ AllDifferentArray ident
evalConstraintI env (IAllDifferentGen var (IRange lo hi) body) = do
    lo' <- liftMaybe (evalExprPure env lo) "generator lower bound"
    hi' <- liftMaybe (evalExprPure env hi) "generator upper bound"
    pure $ AllDifferentGen var (Range lo' hi') body

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
solveProblem adt = listToMaybe (search problem)
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
        search
            p
                { remainingDeclarations = rest
                , solvedVars = (name, v) : solvedVars p
                }
    ArrayDeclaration name indexDom valueDom : rest -> do
        -- For arrays, we need to generate all combinations of values for each index
        let indices = domainValues p indexDom
        values <- mapM (\_ -> domainValues p valueDom) indices
        let bindings = zipWith (\idx val -> (name ++ "[" ++ show idx ++ "]", val)) indices values
        search
            p
                { remainingDeclarations = rest
                , solvedVars = bindings ++ solvedVars p
                }

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
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b
evalConstraint p (AllDifferent es) = do
    vals <- mapM (evalExpr p) es
    pure $ allDifferent vals
  where
    allDifferent [] = True
    allDifferent (x:xs) = x `notElem` xs && allDifferent xs
evalConstraint p (AllDifferentArray arrName) = do
    -- Find all variables with prefix "arrName["
    let prefix = arrName ++ "["
        vars = [v | (k, v) <- solvedVars p, prefix `startsWith` k]
    pure $ allDifferent vars
  where
    allDifferent [] = True
    allDifferent (x:xs) = x `notElem` xs && allDifferent xs
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (x:xs) (y:ys) = x == y && startsWith xs ys
evalConstraint p (AllDifferentGen var dom body) = do
    let vals = domainValues p dom
    results <- mapM (\v -> evalExpr (p { solvedVars = (var, v) : solvedVars p }) body) vals
    pure $ allDifferent results
  where
    allDifferent [] = True
    allDifferent (x:xs) = x `notElem` xs && allDifferent xs

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNeq = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime