class Secret {
  private value: string
  constructor(v: string) { this.value = v }
  reveal(): string { return this.value }
}
const s = new Secret("hidden")
console.log(s.reveal())
