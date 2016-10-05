import Foundation

// First dir is the program name
let args = ProcessInfo.processInfo.arguments.dropFirst()
let dir  = FileManager.default.currentDirectoryPath

for arg in args {
  print("\(dir)/\(arg)")
}
