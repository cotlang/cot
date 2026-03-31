function greet(name: string, title: string = "Mr"): string {
  return title + " " + name
}
console.log(greet("Smith"))
console.log(greet("Jones", "Dr"))
