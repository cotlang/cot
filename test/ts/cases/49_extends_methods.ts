class Animal {
  name: string
  constructor(name: string) { this.name = name }
  speak(): string { return this.name }
}
class Dog extends Animal {
  breed: string
  constructor(name: string, breed: string) {
    this.name = name
    this.breed = breed
  }
  bark(): string { return "woof" }
}
const d = new Dog("Rex", "Lab")
console.log(d.speak())
console.log(d.bark())
