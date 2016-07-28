import kotlin.CharSequence
import java.io.BufferedReader
import java.io.InputStreamReader
import java.lang.System

fun main(args : Array<String>) {
  val dir   = System.getProperty("user.dir")
  var stdin = BufferedReader(InputStreamReader(System.`in`));
  val paths = getPaths(stdin, args.asList())

  if (paths.size == 1)
    print("${dir}/${paths[0]}")
  else
    for (path in paths)
      println("${dir}/${path}")
}

fun getPaths(inStream:BufferedReader, args:List<String>):List<String> {
  val lines = filterBlanks(breakNewlines(args))
  if (lines.isEmpty())
    return filterBlanks(readLines(inStream))
  return lines
}

fun filterBlanks(strings:List<String>):List<String> {
  return strings.filter { a -> a != "" }
}

fun breakNewlines(rawPaths:List<String>):List<String> {
  return rawPaths.flatMap { str -> str.split("\n") }
}

fun readLines(inStream:BufferedReader):List<String> {
  var newArgs = mutableListOf<String>()
  while (true) {
    val line = inStream.readLine()
    if(line != null)
      newArgs.add(line)
    else
      break
  }
  return newArgs
}
