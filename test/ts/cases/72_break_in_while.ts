function solve(n: number): string {
  let result: string = ""
  let i: number = 1
  while (i <= n) {
    if (i === n) {
      result = "found"
      break
    }
    i = i + 1
  }
  return result
}
console.log(solve(5))
