interface Shape {
  area(): number
}
class Circle implements Shape {
  radius: number
  constructor(r: number) { this.radius = r }
  area(): number { return this.radius * this.radius * 3.14 }
}
const c = new Circle(5)
console.log("implements ok")
