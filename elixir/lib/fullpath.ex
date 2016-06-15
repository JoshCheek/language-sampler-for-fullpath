import Enum # lets me do `map(...)` instead of `Enum.map(...)`

defmodule Fullpath do
  def main(args) do
    if help? args do
      IO.write help_screen
    else
      paths = get_paths args, IO, System.cwd
      if copy?(args) do
        copy_to_pasteboard(format paths)
      end
      IO.write format paths
    end
  end

  def help?(args) do
    includes?(args, "-h") || includes?(args, "--help")
  end

  def copy?(args) do
    includes?(args, "-c") || includes?(args, "--copy")
  end

  def help_screen do
    "usage: fullpath *[relative-paths] [-c]\n" <>
    "\n" <>
    "  Prints the fullpath of the paths\n" <>
    "  If no paths are given as args, it will read them from stdin\n" <>
    "\n" <>
    "  If there is only one path, the trailing newline is omitted\n" <>
    "\n" <>
    "  The -c flag will copy the results into your pasteboard\n"
  end

  def get_paths(args, io, cwd) do
    paths = to_paths args
    if empty_list? paths do
      paths = to_paths io.stream(:stdio, :line)
    end
    paths |> expand_args(cwd)
  end

  def copy_to_pasteboard(string) do
    port = Port.open({:spawn, "pbcopy"}, [:stream, :binary, :exit_status, :use_stdio])
    Port.command port, string
    Port.close port
  end

  def includes?(haystack, needle) do
    find(haystack, &needle==&1)
  end

  def to_paths(potential_paths) do
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
