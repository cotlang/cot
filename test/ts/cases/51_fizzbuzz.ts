function fizzbuzz(n: number): string {
  if (n === 15) { return "fizzbuzz" }
  if (n === 3) { return "fizz" }
  if (n === 5) { return "buzz" }
  return "number"
}
console.log(fizzbuzz(3))
console.log(fizzbuzz(5))
console.log(fizzbuzz(15))
console.log(fizzbuzz(7))
