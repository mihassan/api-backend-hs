const NEXT_ATTEMPT = {
  Absent: "Correct",
  Correct: "Misplaced",
  Misplaced: "Absent",
};

const COLORS = {
  Correct: "green",
  Misplaced: "yellow",
  Absent: "gray",
};

document.addEventListener("alpine:init", () => {
  Alpine.data("wordleData", () => ({
    attempts: [],
    selectedSolver: "LetterCountSolver",
    solvers: [
      "LetterCountSolver",
      "LetterCountByPositionSolver",
      "MinimizeMaxPartitionSolver",
      "EvenPartitionSolver",
    ],
    info: `
      1. Use the word shown below in the wordle game.
      2. Click on the grid below to match the color of the cell with the feedback you get from the game.
      3. Slelect the solver you want to use to solve the word.
      4. Click on the Solve button to get the next word to guess.
    `,

    initAttempts() {
      this.attempts = [
        {
          word: "SALET",
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
      let attempt = this.attempts[rowIdx];
      let feedback = attempt ? attempt["feedback"][colIdx] : "Absent";
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
