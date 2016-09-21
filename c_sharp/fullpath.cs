using System;
using System.IO;
using System.Collections.Generic;

public class HelloWorld {
  static public void Main (String[] args) {
    string cwd = Directory.GetCurrentDirectory();
    List<String> paths = new List<String>();

    foreach(string arg in args) {
      paths.Add(arg);
    }

    if(paths.Count == 0) {
      while(true) {
        string line = Console.ReadLine();
        if(line == null)
          break;
        paths.Add(line);
      }
    }

    paths = Expand(cwd, paths);
    foreach(string fullpath in paths) {
      Console.Write(fullpath);
    }
  }

  static public List<String> Expand(String cwd, List<String> paths) {
    List<String> expanded = new List<String>();
    if(paths.Count == 1) {
      expanded.Add(cwd+"/"+paths[0]);
    } else {
      foreach(string path in paths) {
        expanded.Add(cwd+"/"+path+"\n");
      }
    }
    return expanded;
  }
}
