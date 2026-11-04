PART E: All Solutions + Optimization Output
==========================================

OVERVIEW
--------
Part E extends the MiniZinc solver with two major features:
1. **All Solutions** - Returns ALL valid solutions instead of just the first one
2. **Optimization Output** - Provides detailed statistics about the solving process

NEW DATA TYPES
--------------

SolverStats
-----------
data SolverStats = SolverStats
    { nodesExplored :: Int        -- Total search nodes visited
    , solveTimeMs   :: Double     -- Time taken in milliseconds
    , objectiveValue :: Maybe Int -- Objective value for optimize problems
    } deriving (Eq, Show)

SolverResult
------------
data SolverResult = SolverResult
    { allSolutions  :: [[(String, Int)]]     -- All solutions found
    , bestSolution  :: Maybe [(String, Int)] -- Best solution (first for satisfy, optimal for optimize)
    , solverStats   :: SolverStats           -- Statistics
    } deriving (Eq, Show)

NEW API ENDPOINT
----------------
POST /api/minizinc/solve

Accepts: form-urlencoded "model" parameter
Returns: JSON with full SolverResult

Example Response (Satisfy):
{
  "bestSolution": [{"var":"x","value":1},{"var":"y","value":2}],
  "solutions": [
    [{"var":"x","value":1},{"var":"y","value":2}],
    [{"var":"x","value":1},{"var":"y","value":3}],
    ...
  ],
  "stats": {
    "nodesExplored": 9,
    "objectiveValue": null,
    "solveTimeMs": 0.0
  }
}

Example Response (Optimize):
{
  "bestSolution": [{"var":"x","value":5},{"var":"y","value":1}],
  "solutions": [
    [{"var":"x","value":1},{"var":"y","value":5}],
    [{"var":"x","value":2},{"var":"y","value":4}],
    [{"var":"x","value":3},{"var":"y","value":3}],
    [{"var":"x","value":4},{"var":"y","value":2}],
    [{"var":"x","value":5},{"var":"y","value":1}]
  ],
  "stats": {
    "nodesExplored": 25,
    "objectiveValue": 5,
    "solveTimeMs": 0.0
  }
}

HOW IT WORKS
------------

1. **searchWithCount Function**
   - Modified the original `search` function to track nodes explored
   - Each variable assignment = 1 node
   - Uses `combineResults` to aggregate (solutions, nodes) from all branches
   - Returns `([[Binding]], Int)` - all solutions and total node count

2. **solveProblemWithStats Function**
   - Calls `searchWithCount` to get all solutions and node count
   - Measures wall-clock time using `getCurrentTime` and `diffUTCTime`
   - Selects best solution based on solver goal:
     * Satisfy: first solution found
     * Maximize: solution with highest objective value
     * Minimize: solution with lowest objective value
   - Computes objective value for optimization problems
   - Returns complete `SolverResult` with all data

3. **Web API Integration**
   - New endpoint `/api/minizinc/solve` calls `solveProblemWithStats`
   - Converts `SolverResult` to JSON using `solverResultToJSON`
   - Original `/api/minizinc` endpoint unchanged (backward compatible)

TESTING
-------

Test 1: Satisfy (multiple solutions)
  var 1..3: x; var 1..3: y; constraint x != y; solve satisfy;
  -> 6 solutions, 9 nodes, best = (x=1, y=2)

Test 2: Maximize
  var 1..5: x; var 1..5: y; constraint x + y = 6; solve maximize x;
  -> 5 solutions, 25 nodes, best = (x=5, y=1), objective = 5

Test 3: N-Queens (8 queens)
  int: n = 8; array[1..n] of var 1..n: q;
  constraint allDifferent(q);
  constraint allDifferent(i in 1..n)(q[i] + i);
  constraint allDifferent(i in 1..n)(q[i] - i);
  solve minimize q[1];
  -> 92 solutions, 16,777,216 nodes, best q[1]=1

FILES MODIFIED
--------------
- Haskell/src/Assignment.hs: Added SolverStats, SolverResult, searchWithCount, solveProblemWithStats
- Haskell/app/Main.hs: Added /api/minizinc/solve endpoint with JSON output

BACKWARD COMPATIBILITY
----------------------
- Original `solveProblem` function unchanged
- Original `/api/minizinc` endpoint unchanged
- New functionality additive only

USAGE IN WEB UI
---------------
1. Start server: `stack run` or `.stack-work/dist/.../main.exe`
2. Open http://localhost:5173
3. Enter model and click Run (uses original endpoint)
4. For full stats, call `/api/minizinc/solve` directly via curl or modify frontend
