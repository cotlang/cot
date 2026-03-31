function outer(): void {
  const x: string = "outer"
  if (true) {
    const y: string = "inner"
    console.log(y)
  }
  console.log(x)
}
outer()
