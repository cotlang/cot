class Point {
  x: number
  y: number
  constructor(x: number, y: number) {
    this.x = x
    this.y = y
  }
}

function isOrigin(p: Point): boolean {
  if (p.x === 0 && p.y === 0) {
    return true
  }
  return false
}

const origin = new Point(0, 0)
const other = new Point(1, 2)
if (isOrigin(origin)) {
  console.log("origin detected")
}
if (!isOrigin(other)) {
  console.log("non-origin detected")
}
