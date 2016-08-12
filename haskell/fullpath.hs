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

doPrintHelp args = Data.List.any (\arg -> arg == "-h" || arg == "--help") args
removeFlags args = Data.List.filter (\arg -> True) args
formatPaths paths = foldl (\formatted toFormat -> formatted ++ toFormat ++ "\n") "" paths

main = do
  args <- getArgs
  programName <- getProgName
  let paths = removeFlags args in
    if doPrintHelp args
      then putStr $ helpScreen programName
      else putStr $ formatPaths paths
