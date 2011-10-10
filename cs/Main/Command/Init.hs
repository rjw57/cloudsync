module Main.Command.Init ( command ) where

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import Text.Printf              ( printf )

import Data.CloudSync.Cabinet as Cab
import Main.Command as Cmd

command :: Cmd.Command
command = Cmd.Command thisAction "init" "Initialise a directory for synchronising." []

commandName = Cmd.name command

-- |This command's action
thisAction :: [String] -> IO Bool
thisAction args = do
    let (opts, nonopts, errs) = getOpt RequireOrder options args

    -- Handle options
    mapM_ handleFlag opts

    -- Exit immediately if 'Help' was in command flags
    if Help `elem` opts
        then return True
        else if null errs
            then do
                targetDir <- if null nonopts then getCurrentDirectory else return (head args)
                Cab.init targetDir
                return True
            else do
                pn <- getProgName
                mapM_ (hPutStr stderr . printf "%s: %s: %s" pn commandName) errs
                return False

-- |A command line flag
data Flag = Help
          deriving (Show, Eq)

-- |Available command line options
options :: [OptDescr Flag]
options = [ Option "h" ["help"] (NoArg Help) "Show a brief usage summary."
          ]

-- |A a string which, when given the prgram name, gives usage information for this command.
usage :: String -> String
usage pn = unlines [
      printf "Usage: %s %s [options] [<cabinet directory>]\n" pn commandName
    , usageInfo "Allowed options:" options
    ]

hPrintUsage :: Handle -> IO ()
hPrintUsage h = getProgName >>= (\pn -> hPutStr h $ usage pn)

handleFlag :: Flag -> IO()
handleFlag Help = hPrintUsage stdout
