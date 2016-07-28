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
  /* if (lines.length == 0) { */
  /*   return filterBlanks(readLines()) */
  /* } */
  return lines
}

fun filterBlanks(strings : List<String>):List<String> {
  return strings
/*   return strings.filter { a -> a != "" } */
}

fun breakNewlines(rawPaths : List<String>):List<String> {
  return rawPaths.flatMap { str -> str.split("\n") }
  /* val paths = arrayOf<String>() */
  /* for (rawPath in rawPaths) { */
  /*   for (line in rawPath.split("\n")) { */
  /*     paths.push(line) */
  /*   } */
  /* } */
  /* return paths */
}

/* fun readLines() { */
/*   var newArgs = [] */
/*   while (true) { */
/*     val line = readLine() */
/*     if(!line) { */
/*       break */
/*     } */
/*     newArgs.push(line) */
/*   } */
/*   return newArgs */
/* } */
