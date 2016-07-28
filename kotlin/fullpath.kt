import kotlin.CharSequence
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import java.io.PrintStream
import java.lang.System
import java.lang.Runtime

data class Invocation(
  val args       : Array<String>,
  val paths      : List<String>,
  val dir        : String,
  val reader     : BufferedReader,
  val writer     : PrintStream,
  val printHelp  : Boolean,
  val copyResult : Boolean,
  val helpScreen : String = """
  usage: fullpath *[relative-paths] [-c]

    Prints the fullpath of the paths
    If no paths are given as args, it will read them from stdin

    If there is only one path, the trailing newline is omitted

    The -c flag will copy the results into your pasteboard
  """.trimIndent()
)

fun main(args : Array<String>) {
  invoke(withPaths(Invocation(
    args       = args,
    paths      = emptyList<String>(),
    dir        = System.getProperty("user.dir"),
    reader     = BufferedReader(InputStreamReader(System.`in`)),
    writer     = System.`out`,
    printHelp  = args.contains("-h") || args.contains("--help"),
    copyResult = args.contains("-c") || args.contains("--copy")
  )))
}

fun invoke(invocation:Invocation) {
  if (invocation.printHelp) {
    println(invocation.helpScreen)
    return
  }
  val output = formatPaths(invocation.paths)
  if (invocation.copyResult)
    copyToClipboard(output)
  invocation.writer.print(output)
}

fun copyToClipboard(string:String) {
  val process = Runtime.getRuntime().exec("pbcopy")
  val writer  = PrintWriter(process.getOutputStream())
  writer.print(string)
  writer.close()
}

fun formatPaths(paths:List<String>) =
  if (paths.size == 1)
    paths[0]
  else
    paths.map { "${it}\n" }.joinToString(separator="")

fun withPaths(invocation:Invocation):Invocation {
  var paths:List<String>
  paths = invocation.args.asList()
  paths = breakNewlines(paths)
  paths = filterBlanks(paths)
  paths = filterFlags(paths)
  if (paths.isEmpty()) {
    paths = readLines(invocation.reader)
    paths = filterBlanks(paths)
  }
  val dir = invocation.dir
  return invocation.copy(paths=paths.map { "$dir/$it" })
}

val dash = "-"[0]
fun filterFlags(args:List<String>):List<String> =
  args.filter { it[0] != dash }

fun filterBlanks(strings:List<String>):List<String> =
  strings.filter { it != "" }

fun breakNewlines(rawPaths:List<String>):List<String> =
  rawPaths.flatMap { it.split("\n") }

fun readLines(inStream:BufferedReader):List<String> {
  var newArgs = mutableListOf<String>()
  while (true) {
    inStream.readLine()?.let { newArgs.add(it) } ?: break
  }
  return newArgs
}
