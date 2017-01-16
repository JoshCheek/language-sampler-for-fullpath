import "dart:io";

class Fullpath {
  List<String>      args;
  Directory         cwd;
  Stream<List<int>> instream;
  IOSink            outstream;
  Fullpath(this.args, this.cwd, this.instream, this.outstream);

  call() {
    for(var filename in args) {
      // this is kind of dumb, IMO, why even have this drectory class if it
      // won't let me do useful things like talk about a child of the dir?
      // This basically just casts it to a string and assumes the file separator
      // is a slash, which it wouldn't be on Windows, for example
      var filepath = cwd + "/" + filename;
      outstream.write(filepath.toString());
      // normalizePath
      outstream.write("\n");
    }
  }
}

main(List<String> args) {
  var cwd = Directory.current.path;
  new Fullpath(args, cwd, stdin, stdout).call();

  // print("*** Robot Stuffs ***");
  // var r = new Robot();
}
