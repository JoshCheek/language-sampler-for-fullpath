using System;
using System.IO;

public class HelloWorld {
  static public void Main (String[] args) {
    string path = Directory.GetCurrentDirectory();

    for(int i = 0; i < args.Length; i++) {
      Console.WriteLine("{0}/{1}", path, args[i]);
    }
  }
}
