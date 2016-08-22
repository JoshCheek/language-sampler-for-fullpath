using Lambda;
using StringTools;

class Fullpath {
  static var helpScreen =
"usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard
";

  static public function main() {
    var args       = Sys.args();
    var dirs       = getDirs(Sys.getCwd(), args, Sys.stdin());
    var printHelp  = args.has("-h") || args.has("--help");
    var copyOutput = args.has("-c") || args.has("--copy");

    if(printHelp)
      Sys.print(helpScreen);
    else if(copyOutput)
      run('pbcopy', function(pbcopy) {
        printDirs(dirs, pbcopy.stdin);
        printDirs(dirs, Sys.stdout());
      });
    else
      printDirs(dirs, Sys.stdout());
  }

  static public function getDirs(cwd:String, args:Array<String>, stdin:haxe.io.Input) {
    var dirs = args.filter(isDir);
    if(dirs.empty())
      dirs = readLines(Sys.stdin()).filter(isDir);
    return dirs.map(function(dir) { return cwd + dir; });
  }

  static public function readLines(instream:haxe.io.Input) {
    var lines = [];
    try { while(true) lines.push(instream.readLine().rtrim()); }
    catch(error:haxe.io.Eof) { /* noop */ }
    return lines;
  }

  static inline public function isDir(maybeDir:String)
    return 0 != maybeDir.length && !maybeDir.startsWith("-");

  static public function printDirs(dirs:Array<String>, stdout:haxe.io.Output)
    if(dirs.length == 1)
      stdout.writeString(dirs[0]);
    else for(arg in dirs)
      stdout.writeString(arg+"\n");

  static public function run(programName:String, callback:sys.io.Process->Void) {
    var pbcopy = new sys.io.Process(programName, []);
    callback(pbcopy);
    pbcopy.close();
    return pbcopy.exitCode();
  }

}
