module Main where

import System.Environment (getArgs, getProgName)
import System.Directory (getCurrentDirectory)
import System.Process (std_in, proc, createProcess, StdStream(CreatePipe))
import GHC.IO.Handle (hPutStr, hClose)
import Data.List (any, filter)
import Data.List.Split (splitOn)

helpScreen programName =
  unlines
    [ "usage: " ++ programName ++ " *[relative-paths] [-c]"
    , ""
    , "  Prints the fullpath of the paths"
    , "  If no paths are given as args, it will read them from stdin"
    , ""
    , "  If there is only one path, the trailing newline is omitted"
    , ""
    , "  The -c flag will copy the results into your pasteboard"
    ]

checkPrintHelp  args = Data.List.any (\arg -> arg == "-h" || arg == "--help") args
checkCopyOutput args = Data.List.any (\arg -> arg == "-c" || arg == "--copy") args

main :: IO ()
main = do
  programName <- getProgName
  args        <- getArgs
  cwd         <- getCurrentDirectory
  let doPrintHelp           = checkPrintHelp  args
      doCopyOutput          = checkCopyOutput args
      argPaths              = selectPaths     args
      formatAndOutput paths = do output (formatPaths cwd paths) doCopyOutput
    in if doPrintHelp
      then putStr $ helpScreen programName
      else if null argPaths
        then do rawStdinLines <- getContents
                formatAndOutput $ selectPaths (splitOn "\n" rawStdinLines)
        else do formatAndOutput argPaths

exactlyOne :: [a] -> Bool
exactlyOne []        = False
exactlyOne (head:[]) = True
exactlyOne list      = False

copyOutput :: String -> IO ()
copyOutput str = do
  (Just hin, _, _, _) <- createProcess (proc "pbcopy" []) { std_in = CreatePipe }
  hPutStr hin str
  hClose hin

output :: String -> Bool -> IO ()
output toOutput doCopy = do
  if doCopy then do
    copyOutput toOutput
  else
    return () -- note that this doesn't return, it's just a way to make the else branch have the same type
  putStr toOutput

selectPaths :: [String] -> [String]
selectPaths args =
  Data.List.filter isPath args
  where
    isPath ""         = False
    isPath ('-':rest) = False
    isPath nonflag    = True

formatPaths :: String -> [String] -> String
formatPaths dir relativePaths =
  if exactlyOne relativePaths
    then pathFromDir $ head relativePaths
    else unlines absolutePaths
  where
    absolutePaths = map pathFromDir relativePaths
    pathFromDir path = dir ++ "/" ++ path
