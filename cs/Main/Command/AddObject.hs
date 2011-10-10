module Main.Command.AddObject ( command ) where

import Data.Maybe               ( fromMaybe )
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import Text.Printf              ( printf )

import Data.CloudSync.Cabinet as Cab
import Main.Command as Cmd

command :: Cmd.Command
command = Cmd.Command thisAction "add-object" "Add an object to the cabinet from an existing file within it." []

commandName = Cmd.name command

-- |Command options record
data Options = Options
    { optHelp       :: Bool
    , optCabinetDir :: String
    }

-- |Default option values
defaultOptions :: IO Options
defaultOptions = do
    cwd <- getCurrentDirectory
    return Options
        { optHelp       = False
        , optCabinetDir = cwd
        }

-- |Available command line options
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h" ["help"] (NoArg (\o -> o { optHelp = True }))
        "Show a brief usage summary."
    , Option [] ["cabinet-dir"] (ReqArg (\s o -> o { optCabinetDir = s }) "DIR")
        "Use DIR as the cabinet directory."
    ]

-- |Parse the action's options returning either a list of errors or the option values.
actionOpts :: [String] -> IO (Either [String] (Options, [String]))
actionOpts args = do
    defOpts <- defaultOptions
    case getOpt RequireOrder options args of
        (o, n, [])  -> return $ Right $ (foldl (flip id) defOpts o, n)
        (_, _, es)  -> return $ Left es

-- |This command's action
thisAction :: [String] -> IO Bool
thisAction args = do
    parsedOpts <- actionOpts args
    case parsedOpts of
        Left errs           -> do
            pn <- getProgName
            mapM_ (hPutStr stderr . printf "%s: %s: %s" pn commandName) errs
            return False
        Right (o, posArgs)  -> do
            if (optHelp o)
                then hPrintUsage stdout >> return True
                else do
                    print (optCabinetDir o)
                    targetDir <- if null posArgs then getCurrentDirectory else return (head posArgs)
                    return True

-- |A a string which, when given the prgram name, gives usage information for this command.
usage :: String -> String
usage pn = unlines [
      printf "Usage: %s %s [options] [<file>]\n" pn commandName
    , usageInfo "Allowed options:" options
    ]

hPrintUsage :: Handle -> IO ()
hPrintUsage h = getProgName >>= (hPutStr h . usage)
