import kotlin.CharSequence

fun main(args : Array<String>) {
  val dir = System.getProperty("user.dir")

  val paths = getPaths(args.asList())
  if (paths.size == 1) {
    print("${dir}/${paths[0]}")
    return
  }
  for (word in paths) {
    println("${dir}/${word}")
  }
}

fun getPaths(args : List<String>):List<String> {
  val lines = filterBlanks(breakNewlines(args))
  if (lines.isEmpty()) {
    return filterBlanks(readLines())
  }
  return lines
}

fun filterBlanks(strings : List<String>):List<String> {
  return strings.filter { a -> a != "" }
}

fun breakNewlines(rawPaths : List<String>):List<String> {
  return rawPaths.flatMap { str -> str.split("\n") }
}

fun readLines():List<String> {
  var newArgs = mutableListOf<String>()
  while (true) {
    val line = readLine()
    if(line != null) {
      newArgs.add(line)
    } else {
      break
    }
  }
  return newArgs
}

/* Maybe useful:

  val paths = arrayOf<String>()
*/
