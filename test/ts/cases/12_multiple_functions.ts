function isPositive(n: number): boolean {
  return n > 0
}

function abs(n: number): number {
  if (isPositive(n)) {
    return n
  }
  return 0 - n
}

function max(a: number, b: number): number {
  if (a > b) {
    return a
  }
  return b
}

console.log("functions work")
