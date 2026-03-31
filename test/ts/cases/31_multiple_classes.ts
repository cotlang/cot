class A {
  val: string
  constructor(v: string) { this.val = v }
  get(): string { return this.val }
}
class B {
  val: string
  constructor(v: string) { this.val = v }
  get(): string { return this.val }
}
const a = new A("first")
const b = new B("second")
console.log(a.get())
console.log(b.get())
