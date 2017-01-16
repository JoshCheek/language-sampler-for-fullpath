import "dart:io";

class Fullpath {
  List<String>      argv;
  Stream<List<int>> instream;
  IOSink            stdout;
  Fullpath(this.argv, this.instream, this.stdout);

  call() {
    this.stdout.write(this.argv.toString());
  }
}

main(List<String> args) {
  new Fullpath(args, stdin, stdout).call();

  // print("*** Robot Stuffs ***");
  // var r = new Robot();
}
