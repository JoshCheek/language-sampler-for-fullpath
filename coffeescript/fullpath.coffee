# first arg is "coffee", 2nd arg is the path to this file, 3rd arg are where real args begin
argv   = process.argv.slice 2
cwd    = process.cwd()
stdin  = process.stdin
stdout = process.stdout
paths  = argv.map (path) -> [cwd, path].join("/")
if paths.length == 1
  stdout.write paths[0]
else
  paths.forEach (path) -> stdout.write "#{path}\n"

