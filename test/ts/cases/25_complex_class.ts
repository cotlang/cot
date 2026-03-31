class Stack {
  items: string
  size: number
  constructor() {
    this.items = ""
    this.size = 0
  }
  push(item: string): void {
    this.items = item
    this.size = this.size + 1
  }
  isEmpty(): boolean {
    return this.size === 0
  }
}
const s = new Stack()
if (s.isEmpty()) {
  console.log("empty")
}
s.push("hello")
if (!s.isEmpty()) {
  console.log("not empty")
}
console.log("stack works")
