{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing and
-- solving MiniZinc-lite models. This server exposes an API that accepts a
-- model, parses and solves it, and returns the resulting assignment.
module Main (main) where

import Assignment
    ( ADT
    , miniZincParser
    , solveProblem
    , solveProblemWithStats
    , SolverResult
    , allSolutions
    , bestSolution
    , solverStats
    , nodesExplored
    , solveTimeMs
    , objectiveValue
    , saveSolution
    )
import Control.Arrow (second)
import Control.Concurrent.QSemN (newQSemN)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
    ( object
    , (.=)
    , eitherDecode
    , Value (Object, String, Number, Null)
    , toJSON
    )
import Data.Aeson.Key (fromString, toString)
import Data.Aeson.KeyMap (toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Instances (ParseResult (..), parse)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- | Convert a list of string pairs to a JSON response.
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (TL.pack value :: TL.Text)
            | (key, value) <- pairs
            ]

-- | Convert a SolverResult to a JSON Value for the API response.
solverResultToJSON :: SolverResult -> Value
solverResultToJSON result = object
    [ "solutions" .=
        map
            (map (\(k, v) -> object ["var" .= k, "value" .= v]))
            (allSolutions result)
    , "bestSolution" .=
        case bestSolution result of
            Nothing -> Null
            Just sol ->
                toJSON (map (\(k, v) -> object ["var" .= k, "value" .= v]) sol)
    , "stats" .=
        object
            [ "nodesExplored" .= nodesExplored (solverStats result)
            , "solveTimeMs" .= solveTimeMs (solverStats result)
            , "objectiveValue" .= objectiveValue (solverStats result)
            ]
    ]

-- | Parse the model text and run the given solver action.
runSolver :: (ADT -> Maybe [(String, Int)]) -> TL.Text -> ActionM ()
runSolver solver modelText = do
    liftIO $ putStrLn (TL.unpack modelText)
    case parse miniZincParser (TL.unpack modelText) of
        Error err -> jsonResponse [("error", show err)]
        Result _ model ->
            case solver model of
                Nothing -> jsonResponse [("result", "No solution")]
                Just env -> jsonResponse (map (second show) env)

-- | Parse the model text and run the stats solver action.
runSolverWithStats :: TL.Text -> ActionM ()
runSolverWithStats modelText = do
    liftIO $ putStrLn (TL.unpack modelText)
    case parse miniZincParser (TL.unpack modelText) of
        Error err -> jsonResponse [("error", show err)]
        Result _ model -> do
            result <- liftIO $ solveProblemWithStats model
            json (solverResultToJSON result)

-- | Parse a JSON integer value.
parseInt :: Value -> Int
parseInt (String t) = read (T.unpack t) :: Int
parseInt (Number n) = floor n
parseInt _ = 0

-- --------------------------------------------------------------------------------
-- | Entry point – launches Scotty web server and defines API routes
main :: IO ()
main =
    newQSemN 1 >>= \_ -> scotty 3000 $ do
        post "/api/minizinc" $ do
            modelText <- formParam "model" :: ActionM TL.Text
            runSolver solveProblem modelText

        -- New endpoint with full statistics and all solutions
        post "/api/minizinc/solve" $ do
            modelText <- formParam "model" :: ActionM TL.Text
            runSolverWithStats modelText

        post "/api/minizinc/save" $ do
            solutionText <- formParam "solution" :: ActionM TL.Text
            case eitherDecode (TLE.encodeUtf8 solutionText) of
                Left err ->
                    jsonResponse [("error", "Failed to parse solution: " ++ err)]
                Right (Object obj) -> do
                    let solution = [ (toString k, parseInt v) | (k, v) <- toList obj ]
                    liftIO $ saveSolution solution
                    jsonResponse [("result", "Solution saved")]
                Right _ ->
                    jsonResponse [("error", "Solution must be a JSON object")]
