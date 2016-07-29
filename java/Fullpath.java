import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.lang.System;
import java.lang.Runtime;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

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
    List<String> paths = new ArrayList<String>();
    for(String path : invocation.args)
      paths.add(path);
    paths = breakNewlines(paths);
    paths = filterBlanks(paths);
    paths = filterFlags(paths);
    if (paths.isEmpty()) {
      paths = readLines(invocation.reader);
      paths = filterBlanks(paths);
    }
    String dir = invocation.dir;
    List<String> absolutePaths = new ArrayList<String>();
    for(String path : paths)
      absolutePaths.add(dir + "/" + path);
    return invocation.withPaths(paths);
  }

  public static void invoke(Invocation invocation) {
    if (invocation.printHelp) {
      System.out.println(invocation.helpScreen);
      return;
    }
    String output = formatPaths(invocation.paths);
    if (invocation.copyResult)
      copyToClipboard(output);
    invocation.writer.print(output);
  }

  public static List<String> breakNewlines(List<String> rawPaths) {
    ArrayList<String> paths = new ArrayList<String>();
    for(String rawPath : rawPaths)
      for(String path : rawPath.split("\n"))
        paths.add(path);
    return paths;
  }

  public static List<String> filterBlanks(List<String> strings) {
    ArrayList<String> newStrings = new ArrayList<String>();
    for(String string : strings)
      if(string.length() != 0)
        newStrings.add(string);
    return newStrings;
  }

  public static List<String> filterFlags(List<String> args) {
    char dash = "-".charAt(0);
    ArrayList<String> newStrings = new ArrayList<String>();
    for(String arg : args)
      if(arg.charAt(0) != dash)
        newStrings.add(arg);
    return newStrings;
  }

  public static List<String> readLines(BufferedReader inStream) {
    List<String> newArgs = new ArrayList<String>();
    String line = "";
    while (true) {
      try { line = inStream.readLine(); }
      catch(java.io.IOException e) { break; }
      if(line != null) newArgs.add(line);
      else break;
    }
    return newArgs;
  }

  public static void copyToClipboard(String string) {
    // val process = Runtime.getRuntime().exec("pbcopy")
    // val writer  = PrintWriter(process.getOutputStream())
    // writer.print(string)
    // writer.close()
  }

  public static String formatPaths(List<String> paths) {
    return "";
    // if (paths.size == 1)
      // paths[0]
    // else
      // paths.map { "${it}\n" }.joinToString(separator="")
  }


  private static class Invocation {
    String[]       args       = {};
    List<String>   paths      = new ArrayList<String>();
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

    public Invocation withPaths(List<String> paths) {
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
