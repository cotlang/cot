class Animal {
  name: string
  constructor(name: string) {
    this.name = name
  }
  describe(): string {
    return this.name
  }
}
const dog = new Animal("dog")
console.log(dog.describe())
