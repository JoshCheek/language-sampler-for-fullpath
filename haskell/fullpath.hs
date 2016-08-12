import System.Environment
import Data.List

helpScreen programName =
  "usage: " ++ programName ++ " *[relative-paths] [-c]\n" ++
  "\n" ++
  "  Prints the fullpath of the paths\n" ++
  "  If no paths are given as args, it will read them from stdin\n" ++
  "\n" ++
  "  If there is only one path, the trailing newline is omitted\n" ++
  "\n" ++
  "  The -c flag will copy the results into your pasteboard\n"

argstream args = "arg stream"
doPrintHelp args = True

main = do
  args <- getArgs
  programName <- getProgName
  if doPrintHelp args
    then putStr $ helpScreen programName
    else putStr $ argstream args
