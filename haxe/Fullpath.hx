class Fullpath {
  static public function main() {
    var args = Sys.args();
    var cwd  = chomp(Sys.getCwd());

    var dirs = selectPaths(args);
    if(args.length == 0)
      dirs = selectPaths(readLines(Sys.stdin()));

    if(dirs.length == 1) {
      Sys.stdout().writeString(cwd+dirs[0]);
    } else {
      for(arg in dirs) {
        Sys.stdout().writeString(cwd+arg+"\n");
      }
    }
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
}
