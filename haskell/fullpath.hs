import System.Environment
import Data.List

helpScreen programName = "help screen for " ++ programName
argstream args = "arg stream"
doPrintHelp args = True

main = do
  args <- getArgs
  programName <- getProgName
  if doPrintHelp args
    then putStrLn $ helpScreen programName
    else putStrLn $ argstream args
