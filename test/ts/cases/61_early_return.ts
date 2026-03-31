function validate(s: string): boolean {
  if (s === "") { return false }
  if (s === "invalid") { return false }
  return true
}
if (validate("hello")) { console.log("valid") }
if (!validate("")) { console.log("empty invalid") }
if (!validate("invalid")) { console.log("keyword invalid") }
