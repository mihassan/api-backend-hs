const NEXT_ATTEMPT = {
  Absent: "Correct",
  Correct: "Misplaced",
  Misplaced: "Absent",
};

const COLORS = {
  Correct: "green",
  Misplaced: "yellow",
  Absent: "grey",
};

document.addEventListener("alpine:init", () => {
  Alpine.data("wordleData", () => ({
    attempts: [],
    selectedSolver: "RandomSolver",
    solvers: [
      "RandomSolver",
      "NaiveSolver",
      "FastSolver1",
      "FastSolver2",
      "MixedSolver1",
      "MixedSolver2",
      "SlowSolver1",
      "SlowSolver2",
    ],

    initAttempts() {
      this.attempts = [
        {
          word: "TRACE",
          feedback: Array(5).fill("Absent"),
        },
      ];
    },

    populateGrid(data) {
      this.attempts.push({
        word: data.word,
        feedback: Array(5).fill("Absent"),
      });
    },

    getClass(rowIdx, colIdx) {
      let feedback = this.attempts[rowIdx].feedback[colIdx];
      return {
        cell: true,
        last: rowIdx == this.attempts.length - 1,
        [COLORS[feedback]]: true,
      };
    },

    toggleCell(rowIdx, colIdx) {
      if (rowIdx != this.attempts.length - 1) return;
      let lastAttempt = this.attempts[rowIdx];
      let feedback = lastAttempt["feedback"];
      feedback[colIdx] = NEXT_ATTEMPT[feedback[colIdx]];
    },

    solve() {
      fetch("../api/wordle", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          solver: this.selectedSolver,
          attempts: this.attempts,
        }),
      })
        .then((response) => response.json())
        .then((data) => this.populateGrid(data));
    },

    init() {
      this.initAttempts();
    },
  }));
});
