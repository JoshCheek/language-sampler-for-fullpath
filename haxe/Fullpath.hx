class Fullpath {
  static public function main() {
    var args = Sys.args();
    var cwd  = chomp(Sys.getCwd());

    var dirs = args;
    if(args.length == 0)
      dirs = readLines(Sys.stdin());

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
    try { while(true) lines.push(instream.readLine()); }
    catch(error:haxe.io.Eof) { /* noop */ }
    return lines;
  }

  static public function chomp(string:String) {
    if(string.charAt(string.length-1) != "\n")
      return string;
    return string.substr(0, string.length-1);
  }
}
