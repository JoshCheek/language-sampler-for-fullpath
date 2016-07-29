import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.lang.System;
import java.lang.Runtime;

public class Fullpath {
  public static void main(String[] args) {
    Invocation invocation = new Invocation()
      .withArgs(args)
      .withDir(System.getProperty("user.dir"))
      .withReader(new BufferedReader(new InputStreamReader(System.in)))
      .withWriter(System.out)
      .withPrintHelp( contains(args, "-h") || contains(args, "--help"))
      .withCopyResult(contains(args, "-c") || contains(args, "--copy"));
    setPaths(invocation);
    invoke(invocation);
  }

  public static boolean contains(String[] haystack, String needle) {
    for(String hay : haystack)
      if(hay == needle)
        return true;
    return false;
  }

  public static Invocation setPaths(Invocation invocation) {
    ArrayList<String> paths = new ArrayList<String>();
    paths = breakNewlines(paths);
    paths = filterBlanks(paths);
    paths = filterFlags(paths);
    if (paths.isEmpty()) {
      paths = readLines(invocation.reader);
      paths = filterBlanks(paths);
    }
    val dir = invocation.dir;
    invocation.paths = paths;
    return invocation.copy(paths=paths.map { "$dir/$it" })
  }

  public static void invoke(Invocation invocation) {
    // if (invocation.printHelp) {
    //   println(invocation.helpScreen)
    //   return
    // }
    // val output = formatPaths(invocation.paths)
    // if (invocation.copyResult)
    //   copyToClipboard(output)
    // invocation.writer.print(output)
  }

  private static class Invocation {
    String[]       args       = {};
    String[]       paths      = {};
    String         dir        = "/";
    BufferedReader reader     = null;
    PrintStream    writer     = null;
    boolean        printHelp  = false;
    boolean        copyResult = false;
    String         helpScreen =
        "usage: fullpath *[relative-paths] [-c]\n" +
        "\n" +
        "  Prints the fullpath of the paths\n" +
        "  If no paths are given as args, it will read them from stdin\n" +
        "\n" +
        "  If there is only one path, the trailing newline is omitted\n" +
        "\n" +
        "  The -c flag will copy the results into your pasteboard\n";

    public Invocation withArgs(String[] args) {
      this.args = args;
      return this;
    }

    public Invocation withPaths(String[] paths) {
      this.paths = paths;
      return this;
    }

    public Invocation withDir(String dir) {
      this.dir = dir;
      return this;
    }

    public Invocation withDir(BufferedReader reader) {
      this.reader = reader;
      return this;
    }

    public Invocation withReader(BufferedReader reader) {
      this.reader = reader;
      return this;
    }

    public Invocation withWriter(PrintStream writer) {
      this.writer = writer;
      return this;
    }

    public Invocation withPrintHelp(boolean printHelp) {
      this.printHelp = printHelp;
      return this;
    }

    public Invocation withCopyResult(boolean copyResult) {
      this.copyResult = copyResult;
      return this;
    }
  }
}

// fun invoke(invocation:Invocation) {
//   if (invocation.printHelp) {
//     println(invocation.helpScreen)
//     return
//   }
//   val output = formatPaths(invocation.paths)
//   if (invocation.copyResult)
//     copyToClipboard(output)
//   invocation.writer.print(output)
// }

// fun copyToClipboard(string:String) {
//   val process = Runtime.getRuntime().exec("pbcopy")
//   val writer  = PrintWriter(process.getOutputStream())
//   writer.print(string)
//   writer.close()
// }

// fun formatPaths(paths:List<String>) =
//   if (paths.size == 1)
//     paths[0]
//   else
//     paths.map { "${it}\n" }.joinToString(separator="")

// fun withPaths(invocation:Invocation):Invocation {
//   var paths:List<String>
//   paths = invocation.args.asList()
//   paths = breakNewlines(paths)
//   paths = filterBlanks(paths)
//   paths = filterFlags(paths)
//   if (paths.isEmpty()) {
//     paths = readLines(invocation.reader)
//     paths = filterBlanks(paths)
//   }
//   val dir = invocation.dir
//   return invocation.copy(paths=paths.map { "$dir/$it" })
// }

// val dash = "-"[0]
// fun filterFlags(args:List<String>):List<String> =
//   args.filter { it[0] != dash }

// fun filterBlanks(strings:List<String>):List<String> =
//   strings.filter { it != "" }

// fun breakNewlines(rawPaths:List<String>):List<String> =
//   rawPaths.flatMap { it.split("\n") }

// fun readLines(inStream:BufferedReader):List<String> {
//   var newArgs = mutableListOf<String>()
//   while (true) {
//     inStream.readLine()?.let { newArgs.add(it) } ?: break
//   }
//   return newArgs
// }
