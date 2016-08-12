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
  let doPrintHelp  = checkPrintHelp args
      doCopyOutput = checkCopyOutput args
      argPaths     = selectPaths args
    in
      if doPrintHelp
        then putStr $ helpScreen programName
        else
          if null argPaths
            then do
              rawStdinLines <- getContents
              output (formatPaths cwd (selectPaths (splitOn "\n" rawStdinLines))) doCopyOutput
            else do
              output (formatPaths cwd argPaths) doCopyOutput

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
  else
    return ()
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

checkPrintHelp  args = Data.List.any (\arg -> arg == "-h" || arg == "--help") args
checkCopyOutput args = Data.List.any (\arg -> arg == "-c" || arg == "--copy") args

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
