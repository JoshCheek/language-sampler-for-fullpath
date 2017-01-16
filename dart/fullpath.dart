import "dart:io";

class Fullpath {
  List<String>      argv;
  Stream<List<int>> instream;
  IOSink            stdout;
  Fullpath(this.argv, this.instream, this.stdout);

  call() {
    for(var filename in this.argv) {
      this.stdout.write(filename);
      this.stdout.write("\n");
    }
  }
}

main(List<String> args) {
  new Fullpath(args, stdin, stdout).call();

  // print("*** Robot Stuffs ***");
  // var r = new Robot();
}
