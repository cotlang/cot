class Builder {
  value: string
  constructor() {
    this.value = ""
  }
  add(s: string): string {
    this.value = s
    return this.value
  }
}
const b = new Builder()
console.log(b.add("chained"))
