function isHello(s: string): boolean {
  return s === "hello"
}
if (isHello("hello")) { console.log("yes") }
if (!isHello("world")) { console.log("no") }
