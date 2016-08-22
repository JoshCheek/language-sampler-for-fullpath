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
    else if(copyOutput)
      open('pbcopy', function(pbcopy) {
        printDirs(dirs, pbcopy.stdin);
        printDirs(dirs, stdout);
      });
    else
      printDirs(dirs, stdout);
  }

  static public function open(programName:String, callback:sys.io.Process->Void) {
    var pbcopy = new sys.io.Process('pbcopy', []);
    callback(pbcopy);
    pbcopy.close();
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
    return potentials.filter(isDir);
  }

  static public function isDir(maybeDir:String) {
    return maybeDir.length != 0 && maybeDir.charAt(0) != "-";
  }

  static public function hasArg(args:Array<String>, maybePresent:String) {
    return -1 != args.indexOf(maybePresent);
  }
}
