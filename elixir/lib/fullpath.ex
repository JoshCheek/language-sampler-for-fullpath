defmodule Fullpath do
  def main(args) do
    paths = remove_empty(args)
    paths = expand_args(paths, System.cwd())
    print_paths paths
  end

  def remove_empty(args) do
    Enum.filter args, fn(arg) -> arg != "" end
  end

  def expand_args(args, working_dir) do
    Enum.map args, fn(arg) -> expand(arg, working_dir) end
  end

  def expand(relative_path, working_dir) do
    Path.expand relative_path, working_dir
  end

  def print_paths(paths) do
    toPrint = Enum.join paths, "\n"
    if 1 < length(paths) do
      toPrint = toPrint <> "\n"
    end
    IO.write(toPrint)
    # paths = Enum.map paths, fn(path) -> path + "\n" end
    # Enum.each args, &IO.puts(&1)
  end
end

Fullpath.main System.argv()

# Maybe useful?
# IO.inspect(args)
# List.first(args)
# IO.puts("Hello, world")
