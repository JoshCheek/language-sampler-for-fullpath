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
    Invocation invocation = new Invocation(
      args,
      System.getProperty("user.dir"),
      new BufferedReader(new InputStreamReader(System.in)),
      System.out
    );
    invocation.paths = getPaths(invocation);
    invoke(invocation);
  }

  public static List<String> getPaths(Invocation invocation) {
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
    List<String> absolutePaths = new ArrayList<String>();
    for(String path : paths)
      absolutePaths.add(invocation.dir + "/" + path);
    return absolutePaths;
  }

  public static void invoke(Invocation invocation) {
    if (invocation.printHelp) {
      System.out.print(invocation.helpScreen);
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
    try {
      Process process = Runtime.getRuntime().exec("pbcopy");
      PrintWriter writer  = new PrintWriter(process.getOutputStream());
      writer.print(string);
      writer.close();
    } catch (java.io.IOException e) { /* noop */ }
  }

  public static String formatPaths(List<String> paths) {
    if (paths.size() == 1)
      return paths.get(0);
    String formated = "";
    for(String path : paths)
      formated += path + "\n";
    return formated;
  }


  private static class Invocation {
    String         dir;
    BufferedReader reader;
    PrintStream    writer;
    String[]       args;
    List<String>   paths      = new ArrayList<String>();
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

    public Invocation(String[] args, String dir, BufferedReader reader, PrintStream writer) {
      this.args       = args;
      this.dir        = dir;
      this.reader     = reader;
      this.writer     = writer;
      this.printHelp  = contains(args, "-h") || contains(args, "--help");
      this.copyResult = contains(args, "-c") || contains(args, "--copy");
    }

    private boolean contains(String[] haystack, String needle) {
      for(String hay : haystack)
        if(hay.equals(needle))
          return true;
      return false;
    }
  }
}
