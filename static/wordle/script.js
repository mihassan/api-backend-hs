document.addEventListener("alpine:init", () => {
  Alpine.data("wordleData", () => ({
    attempts: [],
    currentAttempt: -1,

    clearAttempts() {
      this.attempts = [
        ["TRACE", ["Absent", "Absent", "Absent", "Absent", "Absent"]],
      ];
      this.currentAttempt = 0;
    },

    populateGrid(data) {
      this.attempts.push([
        data.word,
        ["Absent", "Absent", "Absent", "Absent", "Absent"],
      ]);
      this.currentAttempt++;
    },

    toggleCell(attemptIndex, letterIndex) {
      let attempt = this.attempts[attemptIndex][1][letterIndex];
      let newAttempt;

      if (attempt === "Absent") {
        newAttempt = "Correct";
      } else if (attempt === "Correct") {
        newAttempt = "Misplaced";
      } else {
        newAttempt = "Absent";
      }

      this.attempts[attemptIndex][1][letterIndex] = newAttempt;
    },

    solve() {
      fetch("../api/wordle", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          solver: "MixedSolver2",
          attempts: this.attempts,
        }),
      })
        .then((response) => response.json())
        .then((data) => this.populateGrid(data));
    },

    init() {
      console.log("init");
      this.clearAttempts();
    },
  }));
});
