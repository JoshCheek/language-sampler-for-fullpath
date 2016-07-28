fun main(args : Array<String>) {
  val path = System.getProperty("user.dir")

  if (args.size == 0) {
    println("Please provide a name as a command-line argument")
    return
  }

  print("${path}/${args[0]}")
}
