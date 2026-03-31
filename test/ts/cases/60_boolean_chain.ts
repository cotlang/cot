function check(a: boolean, b: boolean, c: boolean): boolean {
  return (a && b) || c
}
if (check(true, false, true)) { console.log("chain ok") }
if (!check(false, false, false)) { console.log("chain2 ok") }
