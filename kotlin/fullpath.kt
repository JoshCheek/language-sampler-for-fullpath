import kotlin.CharSequence
import java.io.BufferedReader
import java.io.InputStreamReader
import java.lang.System
import java.io.PrintStream

fun main(args : Array<String>) {
  val dir    = System.getProperty("user.dir")
  var stdin  = BufferedReader(InputStreamReader(System.`in`))
  var stdout = System.`out`
  val paths  = getPaths(stdin, args.asList(), dir)
  printPaths(stdout, paths)
}

fun printPaths(outStream:PrintStream, paths:List<String>) {
  if (paths.size == 1)
    outStream.print(paths[0])
  else
    for (path in paths)
      outStream.println(path)
}

fun getPaths(inStream:BufferedReader, args:List<String>, dir:String):List<String> {
  var lines = filterBlanks(breakNewlines(args))
  if (lines.isEmpty())
    lines = filterBlanks(readLines(inStream))
  return lines.map { line -> "${dir}/${line}" }
}

fun filterBlanks(strings:List<String>):List<String> {
  return strings.filter { a -> a != "" }
}

fun breakNewlines(rawPaths:List<String>):List<String> {
  return rawPaths.flatMap { str -> str.split("\n") }
}

fun readLines(inStream:BufferedReader):List<String> {
  var newArgs = mutableListOf<String>()
  eachLine(inStream) { line -> newArgs.add(line) }
  return newArgs
}

fun eachLine(inStream:BufferedReader, block:(String)->Any) {
  while (true) {
    val line = inStream.readLine()
    if(line == null)
      break
    block(line)
  }
}
