class Base {
  name: string
  constructor(name: string) {
    this.name = name
  }
}
class Child extends Base {
  age: number
  constructor(name: string, age: number) {
    this.name = name
    this.age = age
  }
}
const c = new Child("alice", 30)
console.log(c.name)
