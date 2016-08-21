class Fullpath {
  static public function main() {
    var dir = Sys.getCwd();
    if(dir.charAt(dir.length-1) == "\n") {
      dir = dir.substr(0, dir.length-1);
    }
    for(arg in Sys.args()) {
      Sys.println(dir+arg);
    }
  }
}
