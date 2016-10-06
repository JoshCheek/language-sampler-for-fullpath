import Foundation

// First dir is the program name
let args = ProcessInfo.processInfo.arguments.dropFirst()
let dir  = FileManager.default.currentDirectoryPath
var paths: ArraySlice<String> = []

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
