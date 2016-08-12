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

doPrintHelp args  = Data.List.any (\arg -> arg == "-h" || arg == "--help") args
isPath ""         = False
isPath ('-':rest) = False
isPath nonflag    = True
selectPaths args  = Data.List.filter isPath args
formatPaths paths = join paths "\n"
join list delim =
  foldl append "" list
  where
    append joined toJoin = joined ++ toJoin ++ delim

main = do
  args <- getArgs
  programName <- getProgName
  let paths = selectPaths args in
    if doPrintHelp args
      then putStr $ helpScreen programName
      else putStr $ formatPaths paths
