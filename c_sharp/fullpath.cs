using System;
using System.IO;
using System.Collections.Generic;

public class HelloWorld {
  static public void Main (String[] args) {
    string       cwd   = Directory.GetCurrentDirectory();
    List<String> paths = new List<String>(args);

    if(paths.Count == 0)
      paths = ReadLines();

    paths = Expand(cwd, paths);

    if(paths.Count == 1)
      Console.Write(paths[0]);
    else foreach(string fullpath in paths)
      Console.WriteLine(fullpath);
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
}
