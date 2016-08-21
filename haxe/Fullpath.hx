class Fullpath {
  static public function main() {
    var args = Sys.args();

    var dir  = Sys.getCwd();
    if(dir.charAt(dir.length-1) == "\n") {
      dir = dir.substr(0, dir.length-1);
    }

    if(args.length == 0) {
      args = readLines(Sys.stdin());
    }

    if(args.length == 1) {
      Sys.stdout().writeString(dir+args[0]);
    } else {
      for(arg in args) {
        Sys.stdout().writeString(dir+arg+"\n");
      }
    }
  }

  static public function readLines(instream:haxe.io.Input) {
    return [instream.readLine()];
  }
}
