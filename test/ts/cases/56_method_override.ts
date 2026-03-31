class Base {
  name: string
  constructor(n: string) { this.name = n }
  greet(): string { return this.name }
}
class Child extends Base {
  constructor(n: string) { this.name = n }
  greet(): string { return "child: " + this.name }
}
const c = new Child("test")
console.log(c.greet())
