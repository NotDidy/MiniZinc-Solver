export type State = Readonly<{
    model: string;
    run: boolean;
    useAllSolutions: boolean;
}>;

export type Output = Readonly<
    { result: string; error: "" } | { result: ""; error: string }
> & {
    solverResult?: SolverResultResponse;
};

export type Action<T> = (_: T) => T;

export interface Solution {
    var: string;
    value: number;
}

export interface SolverResultResponse {
    solutions: Solution[][];
    bestSolution: Solution[] | null;
    stats: {
        nodesExplored: number;
        solveTimeMs: number;
        objectiveValue: number | null;
    };
}
