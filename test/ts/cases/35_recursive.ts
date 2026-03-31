function countdown(n: number): void {
  if (n <= 0) {
    console.log("done")
    return
  }
  countdown(n - 1)
}
countdown(3)
