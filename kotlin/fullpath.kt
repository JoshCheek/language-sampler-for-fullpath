import kotlin.io.readLine

fun main(args : Array<String>) {
  val path = System.getProperty("user.dir")

  if (args.size == 0) {
    print("${path}/${readLine()}")
    return
  }

  if (args.size == 1) {
    print("${path}/${args[0]}")
    return
  }

  for (arg in args)
    if (arg != "\n") println("${path}/${arg}")
}
