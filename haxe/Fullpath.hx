class Fullpath {
  static public function main() {
    var args = Sys.args();

    var cwd = Sys.getCwd();
    if(cwd.charAt(cwd.length-1) == "\n") {
      cwd = cwd.substr(0, cwd.length-1);
    }

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
}
