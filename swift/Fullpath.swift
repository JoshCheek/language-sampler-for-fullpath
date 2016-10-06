import Foundation

// First dir is the program name
let args = ProcessInfo.processInfo.arguments.dropFirst()
let dir  = FileManager.default.currentDirectoryPath

if args.count == 1 {
  if let arg = args.first {
    print("\(dir)/\(arg)", terminator: "")
  }
} else {
  for arg in args {
    print("\(dir)/\(arg)")
  }
}
