defmodule Fullpath do
  def main(args) do
    args = remove_empty(args)
    args = expand_args(args, System.cwd())
    Enum.each args, &IO.puts(&1)
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
end

Fullpath.main System.argv()

# Maybe useful?
# IO.inspect(args)
# List.first(args)
# IO.puts("Hello, world")
