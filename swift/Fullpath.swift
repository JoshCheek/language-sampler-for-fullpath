import Foundation
import Darwin

// First dir is the program name
var args       = Array(ProcessInfo.processInfo.arguments.dropFirst())
let dir        = FileManager.default.currentDirectoryPath
let printHelp  = args.contains("-h") || args.contains("--help")
let copyOutput = args.contains("-c") || args.contains("--copy")
var paths: [String] = []

if printHelp {
  print("usage: fullpath *[relative-paths] [-c]")
  print("")
  print("  Prints the fullpath of the paths")
  print("  If no paths are given as args, it will read them from stdin")
  print("")
  print("  If there is only one path, the trailing newline is omitted")
  print("")
  print("  The -c flag will copy the results into your pasteboard")
  exit(0)
}

// remove blank lines
args = args.filter({$0 != "" && $0 != "-c" && $0 != "--copy"})

// Paths come from args or stdin
if args.count == 0 {
  while let line = readLine() {
    if line != "" {
      paths.append(line)
    }
  }
} else {
  for arg in args {
    if arg != "" {
      paths.append(arg)
    }
  }
}

let task = Process()
task.launchPath = "/usr/bin/pbcopy"

let pipe = Pipe()
task.standardInput = pipe
if copyOutput {
  task.launch()
}


// print the paths
if paths.count == 1 {
  if let path = paths.first {
    let fullpath = "\(dir)/\(path)"
    print(fullpath, terminator: "")
    pipe.fileHandleForWriting.write(fullpath.data(using: .utf8)!)
  }
} else {
  for path in paths {
    let fullpath = "\(dir)/\(path)"
    print(fullpath)
    pipe.fileHandleForWriting.write((fullpath+"\n").data(using: .utf8)!)
  }
}

pipe.fileHandleForWriting.closeFile()
task.waitUntilExit()
// pipe.fileHandleForReading.closeFile()
