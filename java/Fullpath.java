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
    // realistically, there's probably some abstraction missing here
    // that would need to be introduced as more requirements were added to the program,
    // but since I can't quite feel what it is (I hava a vague sense),
    // I'm not going to try introducing it and making it worse
    Invocation invocation = new Invocation(
      args,
      System.getProperty("user.dir"),
      new BufferedReader(new InputStreamReader(System.in)),
      System.out
    );
    invocation.setPaths();
    invoke(invocation);
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

    public void setPaths() {
      this.paths = getPaths();
    }

    private List<String> getPaths() {
      List<String> paths = filterFlags(filterBlanks(breakNewlines(arrayToList(this.args))));
      if (paths.isEmpty())
        paths = filterBlanks(readLines(this.reader));
      return prependDir(this.dir, paths);
    }

    private List<String> prependDir(String dir, List<String> relativePaths) {
      List<String> absolutePaths = new ArrayList<String>();
      for(String path : relativePaths)
        absolutePaths.add(dir + "/" + path);
      return absolutePaths;
    }

    private List<String> arrayToList(String[] strings) {
      List<String> list = new ArrayList<String>();
      for(String string : strings)
        list.add(string);
      return list;
    }

    private List<String> breakNewlines(List<String> stringsWithNewlines) {
      ArrayList<String> stringsWithoutNewlines = new ArrayList<String>();
      for(String stringWithNewlines : stringsWithNewlines)
        for(String string : stringWithNewlines.split("\n"))
          stringsWithoutNewlines.add(string);
      return stringsWithoutNewlines;
    }

    private List<String> filterBlanks(List<String> strings) {
      ArrayList<String> newStrings = new ArrayList<String>();
      for(String string : strings)
        if(string.length() != 0)
          newStrings.add(string);
      return newStrings;
    }

    private List<String> filterFlags(List<String> args) {
      char dash = "-".charAt(0);
      ArrayList<String> newStrings = new ArrayList<String>();
      for(String arg : args)
        if(arg.charAt(0) != dash)
          newStrings.add(arg);
      return newStrings;
    }

    private List<String> readLines(BufferedReader inStream) {
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

  }
}
