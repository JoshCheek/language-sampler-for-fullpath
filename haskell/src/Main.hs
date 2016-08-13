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

data Options = Options { printHelp :: Bool, shouldCopyOutput :: Bool, argvPaths :: [String] } deriving (Show)

parseOptions :: [String] -> Options
parseOptions argv =
  foldr handleArg defaults argv
  where
    defaults = Options { printHelp = False, shouldCopyOutput = False, argvPaths = [] }
    -- seems I have to duplicate bodies for now,
    -- though there is a proposal for allowing multiple patterns to be matched https://wiki.haskell.org/MultiCase
    handleArg "-h"     options = options { printHelp        = True }
    handleArg "--help" options = options { printHelp        = True }
    handleArg "-c"     options = options { shouldCopyOutput = True }
    handleArg "--copy" options = options { shouldCopyOutput = True }
    handleArg path     options = options { argvPaths        = path : argvPaths options }

main :: IO ()
main = do
  programName <- getProgName
  args        <- getArgs
  cwd         <- getCurrentDirectory
  let options               = parseOptions args
      argPaths              = selectPaths args
      formatAndOutput paths = do output (formatPaths cwd paths) (shouldCopyOutput options)
    in if printHelp options
      then putStr (helpScreen programName)
      else if null (argvPaths options)
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
