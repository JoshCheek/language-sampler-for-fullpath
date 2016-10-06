import Foundation
import Darwin

// First dir is the program name
let args = ProcessInfo.processInfo.arguments.dropFirst()
let dir  = FileManager.default.currentDirectoryPath
var paths: ArraySlice<String> = []

if args.contains("-h") || args.contains("--help") {
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

if args.count == 0 {
  while let line = readLine() {
    paths.append(line)
  }
} else {
  for arg in args {
    paths.append(arg)
  }
}

if paths.count == 1 {
  if let path = paths.first {
    print("\(dir)/\(path)", terminator: "")
  }
} else {
  for path in paths {
    print("\(dir)/\(path)")
  }
}
