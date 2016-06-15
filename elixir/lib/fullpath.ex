import Enum # lets me do `map(...)` instead of `Enum.map(...)`

defmodule Fullpath do
  def main(args) do
    # We should do this, but it yells at me for having unused variables, so leaving them commented
    # help? = find args, &"-h"==&1
    # copy? = find args, &"-c"==&1
    paths = get_paths args
    if empty_list? paths do
      paths = get_paths IO.stream(:stdio, :line)
    end
    formatted = (paths |> expand_args(System.cwd) |> format)
    # here, we should copy them to the clipboard by piping through pbcopy,
    # but I can't figure out how :(
    IO.write formatted
  end

  def get_paths(potential_paths) do
    potential_paths |> map(&chomp(&1)) |> filter(&!empty_string?(&1)) |> filter(&!flag?(&1))
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

  def flag?(string) do
    String.starts_with? string, "-"
  end

  def chomp(string) do
    String.rstrip(string, ?\n)
  end

  def empty_string?(string) do
    0 == String.length(string)
  end

  def empty_list?(list) do
    0 == length list
  end
end

Fullpath.main System.argv()

# Maybe useful?
# IO.inspect(args)
# List.first(args)
# IO.puts("Hello, world")
