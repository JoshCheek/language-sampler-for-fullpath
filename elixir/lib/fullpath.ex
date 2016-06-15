import Enum # lets me do `map(...)` instead of `Enum.map(...)`

defmodule Fullpath do
  def main(args) do
    args |> get_paths |> expand_args(System.cwd) |> format |> IO.write
  end

  def get_paths(potential_paths) do
    potential_paths |> map(&chomp(&1)) |> filter(&!empty_string?(&1))
  end

  def expand_args(args, working_dir) do
    map args, fn(arg) -> expand(arg, working_dir) end
  end

  def expand(relative_path, working_dir) do
    Path.expand relative_path, working_dir
  end

  def format(paths) do
    toPrint = join paths, "\n"
    if 1 < length paths do
      toPrint = toPrint <> "\n"
    end
    toPrint
  end

  def chomp(string) do
    String.rstrip(string, ?\n)
  end

  def empty_string?(string) do
    0 == String.length(string)
  end
end

Fullpath.main System.argv()

# Maybe useful?
# IO.inspect(args)
# List.first(args)
# IO.puts("Hello, world")
