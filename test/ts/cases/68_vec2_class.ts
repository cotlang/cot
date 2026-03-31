class Vec2 {
  x: number
  y: number
  constructor(x: number, y: number) { this.x = x; this.y = y }
  lengthSq(): number { return this.x * this.x + this.y * this.y }
}
const v = new Vec2(3, 4)
console.log(v.lengthSq())
