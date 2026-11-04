PART E: All Solutions + Optimization Output
==========================================

OVERVIEW
--------
Part E extends the MiniZinc solver with two new features:
1. All Solutions - Returns ALL valid solutions instead of just the first one
2. Optimization Output - Provides detailed statistics about the solving process

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


HOW IT WORKS
------------

1. **searchWithCount Function**
   - Modified the original search function to track nodes explored
   - Each variable assignment = 1 node, array elements also counted
   - Uses combineResults to aggregate (solutions, nodes) from all branches
   - Returns `([[Binding]], Int)` - all solutions and total node count

2. **search Function (Unified)**
   - Now calls `searchWithCount` and returns `fst` (solutions only)
   - Eliminates code duplication between simple and stats search

3. **solveProblemWithStats Function**
   - Calls searchWithCount to get all solutions and node count
   - Measures wall-clock time using getCurrentTime and diffUTCTime
   - Forces strict evaluation of solutions list before timing (`length solutions \`seq\``)
   - Selects best solution based on solver goal:
     * Satisfy: first solution found
     * Maximize: solution with highest objective value
     * Minimize: solution with lowest objective value
   - Computes objective value for optimization problems
   - Returns complete SolverResult with all data


4. **Web API Integration**
   - New endpoint /api/minizinc/solve calls solveProblemWithStats
   - Converts SolverResult to JSON using solverResultToJSON
   - Original /api/minizinc endpoint unchanged 

TESTING
-------

Test 1: Satisfy (multiple solutions)
  var 1..3: x; var 1..3: y; constraint x != y; solve satisfy;
  -> 6 solutions, 9 nodes, best = (x=1, y=2), time ~0-1ms

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
- Haskell/src/Assignment.hs: SolverStats, SolverResult, searchWithCount, solveProblemWithStats, unified search/evalExpr, isPrefixOf
- Haskell/app/Main.hs: /api/minizinc/solve endpoint with JSON output, helper functions
- JS/index.html: "Show all solutions & statistics" toggle, stats panel, solutions list with best highlighting
- JS/src/main.ts: Dual endpoint logic, renderSolutions with best row highlighting
- JS/src/types.ts: Solution, SolverResultResponse types



