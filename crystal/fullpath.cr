help_screen = <<-HELP_SCREEN
usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard
HELP_SCREEN

cwd   = Dir.current
help  = ARGV.delete("-h") || ARGV.delete("--help")
copy  = ARGV.delete("-c") || ARGV.delete("--copy")

help && (puts help_screen; exit)

paths = ARGV.reject &.empty?
paths = STDIN.each_line.to_a if paths.empty?
paths = paths.map(&.chomp).reject(&.empty?).map { |p| File.join cwd, p }

copy && Process.run "pbcopy" do |pbcopy|
  print_paths paths, pbcopy.input
end
print_paths paths, STDOUT


def print_paths(paths, stream)
  if paths.size == 1
    stream.print paths[0]
  else
    paths.each { |p| stream.puts p }
  end
end
