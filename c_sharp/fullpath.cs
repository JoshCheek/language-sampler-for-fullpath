using System;
using System.IO;

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
      /* List<int> lines = new List<String>(); */
      /* integers.Add(line); */
      string line = Console.ReadLine();
      Console.WriteLine("("+line+")");
      if(line == null) {
        Console.WriteLine("t("+line+")");
      } else {
        Console.WriteLine("f("+line+")");
      }
    }
  }
}
