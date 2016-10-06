import Foundation
import Darwin

func isCopy(_ arg:String) -> Bool {
  return arg == "-c" || arg == "--copy"
}

func isHelp(_ arg:String) -> Bool {
  return arg == "-h" || arg == "--help"
}

// First dir is the program name
var args       = Array(ProcessInfo.processInfo.arguments.dropFirst())
let dir        = FileManager.default.currentDirectoryPath
var printHelp  = false
var copyOutput = false
var paths: [String] = []

for arg in args {
  if isCopy(arg) {
    copyOutput = true
  } else if isHelp(arg) {
    printHelp  = true
  } else if arg != "" {
    paths.append(arg)
  }
}

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


// Paths come from args or stdin
if paths.count == 0 {
  while let line = readLine() {
    if line != "" {
      paths.append(line)
    }
  }
}

// the pbcopy task
let task = Process()
let pipe = Pipe()
task.launchPath = "/usr/bin/pbcopy" // <-- uhm, how do I not hard code the path?
task.standardInput = pipe
if copyOutput {
  task.launch()
}

// print the paths (we can write to the pipe regardless of whether we've invoked the copy task)
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
