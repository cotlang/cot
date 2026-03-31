class User {
  name: string
  email: string
  active: boolean
  constructor(name: string, email: string) {
    this.name = name
    this.email = email
    this.active = true
  }
  deactivate(): void { this.active = false }
  isActive(): boolean { return this.active }
  getName(): string { return this.name }
  getEmail(): string { return this.email }
}
const u = new User("alice", "alice@test.com")
console.log(u.getName())
console.log(u.getEmail())
if (u.isActive()) { console.log("active") }
u.deactivate()
if (!u.isActive()) { console.log("inactive") }
