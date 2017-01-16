import "dart:io";

class Fullpath {
  List<String>      args;
  Directory         cwd;
  Stream<List<int>> instream;
  IOSink            outstream;
  Fullpath(this.args, this.cwd, this.instream, this.outstream);

  call() {
    outstream.write(
        cwd.toString()
);
    outstream.write("\n");
    for(var filename in args) {
      outstream.write(filename);
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
