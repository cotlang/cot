function config(host: string = "localhost"): string {
  return host
}
console.log(config())
console.log(config("example.com"))
