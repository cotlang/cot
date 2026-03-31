class Config {
  host: string
  port: number
  constructor(host: string, port: number) {
    this.host = host
    this.port = port
  }
}
const config = new Config("localhost", 8080)
console.log(config.host)
