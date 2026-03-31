function double(x: number): number { return x + x }
function quadruple(x: number): number { return double(double(x)) }
console.log(quadruple(3))
