using System;
using System.IO;
using System.Collections.Generic;

public class HelloWorld {
  static public void Main (String[] args) {
    string cwd = Directory.GetCurrentDirectory();

    if(args.Length == 1) {
      Console.Write("{0}/{1}", cwd, args[0]);
    } else if (args.Length > 0) {
      for(int i = 0; i < args.Length; i++) {
        Console.WriteLine("{0}/{1}", cwd, args[i]);
      }
    } else {
      List<String> lines = new List<String>();
      while(true) {
        string line = Console.ReadLine();
        if(line == null)
          break;
        lines.Add(line);
      }
      foreach(string line in lines) {
        Console.WriteLine("{0}/{1}", cwd, line);
      }
    }
  }
}
