import Enum # lets me do `map(...)` instead of `Enum.map(...)`

defmodule Fullpath do
  def main(args) do
    if help? args do
      IO.write help_screen
    else
      paths = get_paths args, IO, System.cwd
      if copy?(args), do: copy_to_pasteboard format paths
      IO.write format paths
    end
  end

  def help?(args), do: includes?(args, "-h") || includes?(args, "--help")
  def copy?(args), do: includes?(args, "-c") || includes?(args, "--copy")
  def help_screen, do: """
    usage: fullpath *[relative-paths] [-c]

      Prints the fullpath of the paths
      If no paths are given as args, it will read them from stdin

      If there is only one path, the trailing newline is omitted

      The -c flag will copy the results into your pasteboard
    """

  def includes?(haystack, needle),      do: find haystack, &needle==&1
  def flag?(string),                    do: String.starts_with? string, "-"
  def chomp(str),                       do: String.trim_trailing str
  def empty_string?(string),            do: 0 == String.length(string)
  def empty_list?(list),                do: 0 == length list
  def expand_paths(paths, working_dir), do: map paths, &Path.expand(&1, working_dir)
  def to_paths(potential_paths),        do: potential_paths
                                            |> map(&chomp &1)
                                            |> filter(&!empty_string? &1)
                                            |> filter(&!flag? &1)

  def get_paths(args, io, cwd) do
    paths = to_paths args
    if empty_list? paths do
      paths = to_paths io.stream(:stdio, :line)
      expand_paths paths, cwd
    else
      expand_paths paths, cwd
    end
  end

  def copy_to_pasteboard(string) do
    port = Port.open {:spawn, "pbcopy"}, [:stream, :binary, :exit_status, :use_stdio]
    Port.command port, string
    Port.close port
  end

  def format(paths) do
    toPrint = join paths, "\n"
    if 1 < length paths do
      toPrint <> "\n"
    else
      toPrint
    end
  end
end
