class Node {
  value: string
  constructor(v: string) { this.value = v }
  getValue(): string { return this.value }
}
class Container {
  label: string
  constructor() { this.label = "empty" }
  isEmpty(): boolean { return this.label === "empty" }
}
const n = new Node("test")
const c = new Container()
console.log(n.getValue())
if (c.isEmpty()) { console.log("container empty") }
