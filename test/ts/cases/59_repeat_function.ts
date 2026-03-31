function repeat(msg: string, n: number): void {
  let i: number = 0
  while (i < n) {
    console.log(msg)
    i = i + 1
  }
}
repeat("hi", 3)
