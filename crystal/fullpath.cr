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

if help
  puts help_screen
  exit
end


def print_paths(paths, stream)
  if paths.size == 1
    stream.print paths[0]
  else
    paths.each { |p| stream.puts p }
  end
end

paths = ARGV.reject { |p| p.empty? }
paths = STDIN.each_line.to_a if paths.empty?
paths = paths.map    { |p| p.chomp }
             .reject { |p| p.empty? }
             .map    { |p| File.join cwd, p }

if copy
  # Their tests have been repeatedly helpful!
  # https://github.com/crystal-lang/crystal/blob/9156f797ec18bfdc6c5a8575c8d681d40b5e1a12/spec/std/process_spec.cr#L70
  Process.run("pbcopy") { |pbcopy| print_paths paths, pbcopy.input }
end
print_paths paths, STDOUT
