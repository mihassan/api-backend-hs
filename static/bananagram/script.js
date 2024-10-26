document.addEventListener("alpine:init", () => {
  Alpine.data("data", () => ({
    letters: "",
    errorMessage: "",
    infoShown: false,
    grid: "",
    dictionary: "Tiny",
    sizes: ["Tiny", "Small", "Medium", "Large", "Huge", "Full"],
    loading: false,

    showInfo() {
      this.infoShown = true;
    },

    hideInfo() {
      this.infoShown = false;
    },

    solve() {
      if (this.loading) return;

      this.grid = "";
      this.errorMessage = "";
      this.letters = this.letters.toUpperCase().replace(/[^A-Z]/g, "");
      if (this.letters.length < 2) {
        this.errorMessage = "Please enter at least two letters.";
        return;
      }

      this.loading = true;

      fetch("../api/bananagram", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          dictionary: this.dictionary,
          letters: this.letters,
        }),
      })
        .then((response) => response.json())
        .then((data) => {
          if (data.grid.length > 0) {
            this.grid = data.grid.join("\n");
          } else {
            this.errorMessage = "No solution found. Please try again (maybe) with a different dictionary size.";
          }
          this.loading = false;
        })
        .catch((error) => {
          this.errorMessage = "Error solving. Please try again (maybe) with a different dictionary size.";
          this.loading = false;
        });
    },
  }));
});
