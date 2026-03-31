function outer(): void {
  function inner(): void {
    console.log("nested works")
  }
  inner()
}
outer()
