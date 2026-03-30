class Counter {
  count: number
  constructor(start: number) {
    this.count = start
  }
  increment(): void {
    this.count = this.count + 1.0
  }
}
const c = new Counter(0)
c.increment()
c.increment()
c.increment()
console.log("done")
