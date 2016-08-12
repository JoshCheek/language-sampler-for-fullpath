module Main where

import System.Environment
import System.Directory
import System.Process
import Data.List
import Data.List.Split
import GHC.IO.Handle

helpScreen programName =
  "usage: " ++ programName ++ " *[relative-paths] [-c]\n" ++
  "\n" ++
  "  Prints the fullpath of the paths\n" ++
  "  If no paths are given as args, it will read them from stdin\n" ++
  "\n" ++
  "  If there is only one path, the trailing newline is omitted\n" ++
  "\n" ++
  "  The -c flag will copy the results into your pasteboard\n"

main :: IO ()
main = do
  programName <- getProgName
  args        <- getArgs
  cwd         <- getCurrentDirectory
  let argPaths = selectPaths args in
    if doPrintHelp args
      then putStr $ helpScreen programName
      else
        if null argPaths
          then do
            rawStdinLines <- getContents
            let
              stdinPaths = selectPaths (splitOn "\n" rawStdinLines)
              toPrint = formatPaths cwd stdinPaths
              in
                output toPrint (doCopyOutput args)
          else
            let toPrint = formatPaths cwd argPaths in
              output toPrint (doCopyOutput args)

copyOutput :: String -> IO ()
copyOutput str = do
  (Just hin, _, _, _) <- createProcess (proc "pbcopy" []){ std_in = CreatePipe }
  hPutStr hin str
  hClose hin

output :: String -> Bool -> IO ()
output toPrint doCopy = do
  if doCopy
  then do
    copyOutput toPrint
    putStr toPrint
  else
    putStr toPrint


exactlyOne :: [a] -> Bool
exactlyOne []        = False
exactlyOne (head:[]) = True
exactlyOne list      = False

join :: [String] -> String -> String
join list delim =
  foldl append "" list
  where
    append joined toJoin = joined ++ toJoin ++ delim

doPrintHelp  args = Data.List.any (\arg -> arg == "-h" || arg == "--help") args
doCopyOutput args = Data.List.any (\arg -> arg == "-c" || arg == "--copy") args

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
