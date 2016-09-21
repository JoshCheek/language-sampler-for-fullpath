using System;
using System.IO;

public class HelloWorld {
  static public void Main (String[] args) {
    string cwd = Directory.GetCurrentDirectory();

    if(args.Length == 1) {
        Console.Write("{0}/{1}", cwd, args[0]);
    } else {
      for(int i = 0; i < args.Length; i++) {
        Console.WriteLine("{0}/{1}", cwd, args[i]);
      }
    }
  }
}
