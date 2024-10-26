document.addEventListener("alpine:init", () => {
  Alpine.data("data", () => ({
    letters: "",
    grid: "",
    dictionary: "Tiny",
    sizes: ["Tiny", "Small", "Medium", "Large", "Huge", "Full"],
    loading: false,

    solve() {
      if (this.loading) return;
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
          this.grid = data.grid.join("\n");
          this.loading = false;
        })
        .catch((error) => {
          console.error("Error solving:", error);
          this.loading = false;
        });
    },
  }));
});
