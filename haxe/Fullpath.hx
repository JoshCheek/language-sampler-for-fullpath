class Fullpath {
  static var helpScreen =
"usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard
";

  static public function main() {
    var stdout     = Sys.stdout();
    var stdin      = Sys.stdin();
    var args       = Sys.args();
    var cwd        = chomp(Sys.getCwd());
    var dirs       = getDirs(cwd, args, stdin);
    var printHelp  = hasArg(args, "-h") || hasArg(args, "--help");
    var copyOutput = hasArg(args, "-c") || hasArg(args, "--copy");

    if(printHelp)
      stdout.writeString(helpScreen);
    else {
      printDirs(dirs, stdout);
      if(copyOutput) {
        var pbcopy = new sys.io.Process('pbcopy', []);
        printDirs(dirs, pbcopy.stdin);
        pbcopy.close();
      }
    }
  }

  static public function printDirs(dirs:Array<String>, stdout:haxe.io.Output) {
    if(dirs.length == 1)
      stdout.writeString(dirs[0]);
    else
      for(arg in dirs)
        stdout.writeString(arg+"\n");
  }

  static public function getDirs(cwd:String, args:Array<String>, stdin:haxe.io.Input) {
    var dirs = selectPaths(args);
    if(dirs.length == 0)
      dirs = selectPaths(readLines(Sys.stdin()));
    return dirs.map(function(dir) { return cwd + dir; });
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
