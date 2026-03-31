class Greeter {
  prefix: string
  constructor(p: string) {
    this.prefix = p
  }
  greet(): string {
    return this.prefix
  }
}
const g = new Greeter("Hi")
console.log(g.greet())
