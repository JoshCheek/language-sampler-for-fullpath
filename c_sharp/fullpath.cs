using System;
using System.IO;
using System.Collections.Generic;
using System.Diagnostics;

public class Fullpath {
  static public void Main (String[] args) {
    string       cwd   = Directory.GetCurrentDirectory();
    bool         copy  = false;
    bool         help  = false;
    List<String> paths = new List<String>();

    foreach(string arg in args)
      if(arg == "-c" || arg == "--copy")
        copy = true;
      else if(arg == "-h" || arg == "--help")
        help = true;
      else
        paths.Add(arg);

    if(help) {
      Console.WriteLine("usage: fullpath *[relative-paths] [-c]");
      Console.WriteLine("");
      Console.WriteLine("  Prints the fullpath of the paths");
      Console.WriteLine("  If no paths are given as args, it will read them from stdin");
      Console.WriteLine("");
      Console.WriteLine("  If there is only one path, the trailing newline is omitted");
      Console.WriteLine("");
      Console.WriteLine("  The -c flag will copy the results into your pasteboard");
      return;
    }

    if(paths.Count == 0)
      paths = ReadLines();

    paths = Expand(cwd, paths);

    if(copy) using (Process process = new Process()) {
      ProcessStartInfo info = new ProcessStartInfo("pbcopy");
      info.RedirectStandardInput = true;
      info.UseShellExecute = false;
      process.StartInfo = info;
      process.Start();
      PrintPaths(paths, process.StandardInput);
    }

    Stream       stream = Console.OpenStandardOutput();
    StreamWriter writer = new StreamWriter(stream);
    PrintPaths(paths, writer);
    writer.Dispose();
  }

  static public List<String> Expand(String cwd, List<String> paths) {
    List<String> expanded = new List<String>();
    foreach(string path in paths)
      if(path != "")
        expanded.Add(cwd+"/"+path);
    return expanded;
  }

  static public List<String> ReadLines() {
    List<String> lines = new List<String>();
    while(true) {
      string line = Console.ReadLine();
      if(line == null) break;
      lines.Add(line);
    }
    return lines;
  }

  static public void PrintPaths(List<String> paths, TextWriter writer) {
    if(paths.Count == 1)
      writer.Write(paths[0]);
    else foreach(string fullpath in paths) {
      writer.WriteLine(fullpath);
    }
  }
}
