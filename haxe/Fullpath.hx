class Fullpath {
  static var helpScreen =
"usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard
";

  static public function main() {
    var stdout = Sys.stdout();
    var stdin  = Sys.stdin();
    var args   = Sys.args();
    var cwd    = chomp(Sys.getCwd());
    var dirs   = getDirs(args, stdin);

    if(hasArg(args, "-h") || hasArg(args, "--help"))
      stdout.writeString(helpScreen);
    else {
      // if arg is -c
      // then print the output to pbcopy

      if(dirs.length == 1)
        stdout.writeString(cwd+dirs[0]);
      else
        for(arg in dirs)
          stdout.writeString(cwd+arg+"\n");
    }
  }

  static public function getDirs(args:Array<String>, stdin:haxe.io.Input) {
    var dirs = selectPaths(args);
    if(args.length == 0)
      dirs = selectPaths(readLines(Sys.stdin()));
    return dirs;
  }

  static public function readLines(instream:haxe.io.Input) {
    var lines = [];
    try { while(true) lines.push(chomp(instream.readLine())); }
    catch(error:haxe.io.Eof) { /* noop */ }
    return lines;
  }

  static public function chomp(string:String) {
    if(string.length == 0)
      return string;
    if(string.charAt(string.length-1) != "\n")
      return string;
    return string.substr(0, string.length-1);
  }

  static public function selectPaths(potentials:Array<String>) {
    var actuals = [];
    for(potential in potentials)
      if(isDir(potential))
        actuals.push(potential);
    return actuals;
  }

  static public function isDir(maybeDir:String) {
    if(maybeDir.length == 0)      return false;
    if(maybeDir.charAt(0) == "-") return false;
    return true;
  }

  static public function hasArg(args:Array<String>, maybePresent:String) {
    return -1 != args.indexOf(maybePresent);
  }
}
