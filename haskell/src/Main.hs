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

data Options = Options { printHelp :: Bool, copyOutput :: Bool, argvPaths :: [String] } deriving (Show)

parseOptions :: [String] -> Options
parseOptions argv =
  foldr handleArg defaults argv
  where
    isHelp arg = "-h" == arg || "--help" == arg
    isCopy arg = "-c" == arg || "--copy" == arg
    defaults = Options { printHelp = False, copyOutput = False, argvPaths = [] }
    handleArg arg options
      | isHelp arg = options { printHelp  = True }
      | isCopy arg = options { copyOutput = True }
      | otherwise  = options { argvPaths  = arg : (argvPaths options) }

main :: IO ()
main = do
  programName <- getProgName
  args        <- getArgs
  cwd         <- getCurrentDirectory
  let options               = parseOptions args
      argPaths              = selectPaths args
      formatAndOutput paths = output (formatPaths cwd paths) (copyOutput options)
    in if printHelp options
      then putStr (helpScreen programName)
      else if null (argvPaths options)
        then do rawStdinLines <- getContents
                formatAndOutput $ selectPaths (splitOn "\n" rawStdinLines)
        else do formatAndOutput argPaths

exactlyOne :: [a] -> Bool
exactlyOne (_:[]) = True
exactlyOne _      = False

doCopyOutput :: String -> IO ()
doCopyOutput str = do
  (hin, _, _, _) <- createProcess (proc "pbcopy" []) { std_in = CreatePipe }
  case hin of
    Nothing -> return ()
    Just hin -> do
      hPutStr hin str
      hClose hin

output :: String -> Bool -> IO ()
output toOutput copyOutput = do
  if copyOutput then do
    doCopyOutput toOutput
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
