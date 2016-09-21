using System;

public class HelloWorld {
  static public void Main (String[] args) {
    Console.WriteLine("Number of command line parameters = {0}",
        args.Length);
    for(int i = 0; i < args.Length; i++) {
      Console.WriteLine("Arg[{0}] = [{1}]", i, args[i]);
    }
  }
}
