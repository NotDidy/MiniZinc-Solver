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
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = failed (UnexpectedString "miniZincParser: not written yet")

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

domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues p (Range lo hi) = [lo..hi]

evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr p (IntLit n) = Just n
evalExpr p (Var x) = lookup x (searchConstants p) <|> lookup x (solvedVars p)

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
