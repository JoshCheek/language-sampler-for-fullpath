import Foundation

let args = ProcessInfo.processInfo.arguments
let dir  = "idk"

for arg in args {
  print("\(dir)/\(arg)")
}
