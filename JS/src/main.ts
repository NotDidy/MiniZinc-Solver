import {
    fromEvent,
    map,
    merge,
    scan,
    startWith,
    switchMap,
    filter,
    pairwise,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import { Action, Output, State, Solution, SolverResultResponse } from "./types";

const modelInput = document.getElementById(
    "model-input",
) as HTMLTextAreaElement;

const runButton = document.getElementById("run-model")!;
const saveButton = document.getElementById("save-solution") as HTMLButtonElement;
const allSolutionsToggle = document.getElementById("all-solutions-toggle") as HTMLInputElement;
const outputEl = document.getElementById("model-output") as HTMLOutputElement;
const solutionsContainer = document.getElementById("solutions-container") as HTMLDivElement;
const statsPanel = document.getElementById("stats-panel") as HTMLDivElement;
const solutionsList = document.getElementById("solutions-list") as HTMLDivElement;
const statNodes = document.getElementById("stat-nodes") as HTMLSpanElement;
const statTime = document.getElementById("stat-time") as HTMLSpanElement;
const statObjective = document.getElementById("stat-objective") as HTMLSpanElement;
const statCount = document.getElementById("stat-count") as HTMLSpanElement;

const initialState: State = {
    model: "",
    run: false,
    useAllSolutions: false,
};

const initialOutput: Output = {
    result: "",
    error: "",
};

const modelInput$: Observable<Action<State>> = fromEvent(
    modelInput,
    "input",
).pipe(
    map(event => (event.target as HTMLTextAreaElement).value),
    map(value => state => ({
        ...state,
        model: value,
        run: false,
    })),
);

const run$: Observable<Action<State>> = fromEvent(runButton, "click").pipe(
    map(() => state => ({
        ...state,
        run: true,
    })),
);

const toggle$: Observable<Action<State>> = fromEvent(allSolutionsToggle, "change").pipe(
    map(event => (event.target as HTMLInputElement).checked),
    map(checked => state => ({
        ...state,
        useAllSolutions: checked,
    })),
);

function getOutput(state: State): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("model", state.model);

    const endpoint = state.useAllSolutions ? "/api/minizinc/solve" : "/api/minizinc";

    return fromFetch<Readonly<SolverResultResponse | { error: string }>>(
        endpoint,
        {
            method: "POST",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            body: body.toString(),
            selector: res => res.json(),
        },
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ result: "", error: res.error });
            }
            if (state.useAllSolutions) {
                return () => ({ result: "", error: "", solverResult: res });
            }
            return () => ({ result: JSON.stringify(res), error: "", solverResult: undefined });
        }),
    );
}

function saveSolution(solution: string): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("solution", solution);

    return fromFetch<Readonly<{ result: string } | { error: string }>>(
        "/api/minizinc/save",
        {
            method: "POST",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            body: body.toString(),
            selector: res => res.json(),
        },
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ result: "", error: res.error });
            }
            return () => ({ result: "Solution saved to file", error: "" });
        }),
    );
}

function formatSolution(solution: Solution[]): string {
    return solution.map(s => `${s.var} = ${s.value}`).join(", ");
}

function renderSolutions(solverResult: SolverResultResponse): void {
    // Update stats panel
    statsPanel.style.display = "block";
    statNodes.textContent = solverResult.stats.nodesExplored.toLocaleString();
    statTime.textContent = solverResult.stats.solveTimeMs.toFixed(2);
    statObjective.textContent = solverResult.stats.objectiveValue !== null
        ? solverResult.stats.objectiveValue.toString()
        : "N/A (satisfy)";
    statCount.textContent = solverResult.solutions.length.toString();

    // Render solutions list
    solutionsList.innerHTML = "";

    if (solverResult.solutions.length === 0) {
        solutionsList.innerHTML = '<div class="solution-item" style="color: #888; text-align: center; padding: 20px;">No solutions found</div>';
        return;
    }

    const bestSolutionStr = solverResult.bestSolution ? formatSolution(solverResult.bestSolution) : "";

    solverResult.solutions.forEach((solution, index) => {
        const solutionStr = formatSolution(solution);
        const isBest = solutionStr === bestSolutionStr && bestSolutionStr !== "";

        const item = document.createElement("div");
        item.className = "solution-item";
        if (isBest) {
            item.style.backgroundColor = "#e8f5e9";  
            item.style.fontWeight = "500";
        }

        const header = document.createElement("div");
        header.className = "solution-item-header";

        const indexSpan = document.createElement("span");
        indexSpan.className = "solution-index";
        indexSpan.textContent = `#${index + 1}`;
        header.appendChild(indexSpan);

        item.appendChild(header);

        const bindings = document.createElement("div");
        bindings.className = "solution-bindings";
        bindings.textContent = solutionStr;
        item.appendChild(bindings);

        solutionsList.appendChild(item);
    });
}

function main() {
    const state$ = merge(modelInput$, run$, toggle$).pipe(
        scan((state, action) => action(state), initialState),
        startWith(initialState),
    );
    state$.subscribe(console.log);

    let lastSolution = "";

    const output$ = state$.pipe(
        pairwise(),
        filter(([prev, curr]) => !prev.run && curr.run),
        map(([, curr]) => curr),
        switchMap(getOutput),
        scan((output, action) => action(output), initialOutput),
        startWith(initialOutput),
    );

    output$.subscribe(output => {
        console.log(output);

        // Toggle UI visibility based on mode
        if (allSolutionsToggle.checked) {
            outputEl.classList.add("hidden");
            solutionsContainer.classList.add("visible");

            if (output.solverResult) {
                renderSolutions(output.solverResult);
                // Uses best solution if available, otherwise the first solution, or empty if no solutions.
                lastSolution = JSON.stringify(output.solverResult.bestSolution || output.solverResult.solutions[0] || []);
            } else if (output.error) {
                statsPanel.style.display = "none";
                solutionsList.innerHTML = `<div class="solution-item error-output" style="padding: 20px; text-align: center;">${output.error}</div>`;
                lastSolution = "";
            }
        } else {
            outputEl.classList.remove("hidden");
            solutionsContainer.classList.remove("visible");
            outputEl.value = output.error || output.result;
            lastSolution = output.error ? "" : output.result;
        }

        // Enable save button if there's a valid solution (not an error)
        if (!output.error && (output.result !== "No solution" || (output.solverResult && output.solverResult.solutions.length > 0))) {
            saveButton.disabled = false;
        } else {
            saveButton.disabled = true;
            lastSolution = "";
        }
    });

    // Handle save button click
    fromEvent(saveButton, "click").pipe(
        filter(() => !saveButton.disabled),
        map(() => lastSolution),
        switchMap(saveSolution),
    ).subscribe(action => {
        const output = action(initialOutput);
        console.log(output);
        if (allSolutionsToggle.checked) {
            solutionsList.innerHTML = `<div class="solution-item" style="color: #2e7d32; text-align: center; padding: 20px;">${output.error || output.result}</div>`;
        } else {
            outputEl.value = output.error || output.result;
        }
    });
}

if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}