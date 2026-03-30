function classify(x: number): string {
  if (x > 100) {
    return "big"
  } else if (x > 10) {
    return "medium"
  } else {
    return "small"
  }
}
console.log(classify(5))
console.log(classify(50))
console.log(classify(500))
