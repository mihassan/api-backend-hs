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

function isLetter(str) {
  return str.length === 1 && str.match(/[a-z]/i);
}

document.addEventListener("alpine:init", () => {
  Alpine.data("wordleData", () => ({
    attempts: [],
    words: [[]],
    loading: false,
    selectedSolver: "LetterCountHard",
    solvers: [
      "LetterCountHard",
      "LetterCountByPositionHard",
      "LetterEntropyHard",
      "MinMaxPartitionHard",
      "EvenPartitionHard",
      "MinMaxPartitionSlow",
      "EvenPartitionSlow",
    ],
    infoShown: false,

    initAttempts() {
      if (this.loading) return;
      this.words = [[]];
      this.attempts = [
        {
          word: "SALET",
          feedback: Array(5).fill("Absent"),
        },
      ];
    },

    populateGrid(data) {
      this.words.push(data.words);
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
        clickable: !this.loading && rowIdx == this.attempts.length - 1,
        [COLORS[feedback]]: true,
      };
    },

    toggleCell(rowIdx, colIdx) {
      if (this.loading) return;
      if (rowIdx != this.attempts.length - 1) return;
      let lastAttempt = this.attempts[rowIdx];
      let feedback = lastAttempt["feedback"];
      feedback[colIdx] = NEXT_ATTEMPT[feedback[colIdx]];
    },

    canRemoveAttempt(rowIdx) {
      return !this.loading && rowIdx > 0 && rowIdx == this.attempts.length - 1;
    },

    removeAttempt(rowIdx) {
      if (this.canRemoveAttempt(rowIdx)) {
        this.attempts.pop();
        this.words.pop();
      }
    },

    canEditAttempt(rowIdx) {
      return !this.loading && rowIdx == this.attempts.length - 1;
    },

    editAttempt(rowIdx) {
      if (!this.canEditAttempt(rowIdx)) return;

      let attempt = this.attempts[rowIdx];
      let newWord = prompt("Enter a new word:", attempt.word);
      if (newWord.length != 5 || !newWord.split("").every(isLetter)) {
        alert("Invalid word. Please enter a 5-letter word.");
      } else {
        attempt.word = newWord.toUpperCase();
      }
    },

    setLastAttempt(e) {
      let attempt = this.attempts[this.attempts.length - 1];
      let newWord = e.target.textContent;
      attempt.word = newWord;
    },

    solve() {
      if (this.loading) return;
      this.loading = true;
      fetch("../api/wordle", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          solver: this.selectedSolver,
          attempts: this.attempts,
        }),
      })
        .then((response) => response.json())
        .then((data) => {
          this.populateGrid(data);
          this.loading = false;
        })
        .catch((error) => {
          console.error("Error solving:", error);
          this.loading = false;
        });
    },

    showInfo(e) {
      this.infoShown = true;
    },

    hideInfo() {
      this.infoShown = false;
    },

    init() {
      this.initAttempts();
    },
  }));
});
