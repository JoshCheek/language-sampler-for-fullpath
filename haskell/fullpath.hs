import System.Environment
import System.Directory
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

exactlyOne :: [a] -> Bool
exactlyOne []        = False
exactlyOne (head:[]) = True
exactlyOne list      = False

join :: [String] -> String -> String
join list delim =
  foldl append "" list
  where
    append joined toJoin = joined ++ toJoin ++ delim

doPrintHelp :: [String] -> Bool
doPrintHelp args =
  Data.List.any (\arg -> arg == "-h" || arg == "--help") args

selectPaths args =
  Data.List.filter isPath args
  where
    isPath ""         = False
    isPath ('-':rest) = False
    isPath nonflag    = True

formatPaths dir relativePaths =
  if exactlyOne relativePaths
    then pathFromDir $ head relativePaths
    else join absolutePaths "\n"
  where
    absolutePaths = map pathFromDir relativePaths
    pathFromDir path = dir ++ "/" ++ path

main = do
  programName <- getProgName
  args        <- getArgs
  cwd         <- getCurrentDirectory
  let argPaths = selectPaths args in
    if doPrintHelp args
      then putStr $ helpScreen programName
      else putStr $ formatPaths cwd argPaths

-- stdinPaths    = do
--   rawStdinLines <- getContents
--   [rawStdinLines]
-- relativePaths =
--   if null argPaths
--     then stdinPaths
--     else argPaths
