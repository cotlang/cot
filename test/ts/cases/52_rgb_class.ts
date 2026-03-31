class RGB {
  r: number
  g: number
  b: number
  constructor(r: number, g: number, b: number) {
    this.r = r
    this.g = g
    this.b = b
  }
  isBlack(): boolean {
    return this.r === 0 && this.g === 0 && this.b === 0
  }
}
const black = new RGB(0, 0, 0)
const white = new RGB(255, 255, 255)
if (black.isBlack()) { console.log("black") }
if (!white.isBlack()) { console.log("not black") }
