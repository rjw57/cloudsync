module Main                                 ( main ) where

import Control.Exception                    ( IOException )
import Control.Monad                        ( when, unless )
import System.Console.GetOpt
import System.Environment                   ( getArgs, getProgName )
import System.Exit                          ( exitFailure, exitSuccess )
import System.IO
import Text.Printf                          ( printf )

import Main.Command as Cmd

-- |List of all commands
commands = [ Cmd.Command helpCmd "help" "Show usage for a particular command."
           , Cmd.Command infoCmd "init" "Initialise a directory for synchronising."
           , Cmd.Command infoCmd "info" "Show information on a cabinet directory."
           ]

-- ----- COMMAND LINE OPTIONS -----

data Flag = Help
          deriving (Show)

-- |Available command line options
options :: [OptDescr Flag]
options = [ Option "h" ["help"] (NoArg Help) "Show a brief usage summary."
          ]

-- |Handle any state change implied by a command-line flag
handleFlag :: Flag -> IO ()
handleFlag Help = printUsage >> exitSuccess

-- |Given the program name, generate a string giving top-level usage
-- information.
usage :: String -> String
usage pn = unlines [
      printf "Usage: %s <command> [<arguments>]" pn
    , ""
    , usageInfo "Allowed options:" options
    , "Available commands:"
    , Cmd.descTable commands
    , printf "See `%s help <command>' for more information on a specific command." pn
    ]

-- |Print a brief top-level usage summary to the console.
printUsage :: IO ()
printUsage = hPutUsage stdout

-- |Print a brief top-level usage summary to a filehandle
hPutUsage :: Handle -> IO ()
hPutUsage h = getProgName >>= (hPutStr h . usage)

-- ----- COMMANDS -----

-- HELP

-- |The help command
helpCmd :: [String] -> IO Bool
helpCmd _ = printUsage >> return True

-- INFO

-- |The info command
infoCmd :: [String] -> IO Bool
infoCmd args = print args >> return True

-- ----- MAIN PROGRAM -----

-- |Run the named command with the provided argument list. Return a flag
-- indicating successful completion.
runCmd :: String -> [String] -> IO Bool
runCmd cmd args = do
    rv <- Cmd.run commands cmd args
    case rv of
        Nothing -> do
            pn <- getProgName
            hPutStr stderr (printf "%s: unrecognized command: `%s'\n" pn cmd)
            hPutUsage stderr
            exitFailure
        Just f  -> return f

main :: IO ()
main = do
    -- get arguments
    args <- getArgs

    -- process top-level options
    let (opts, nonopts, errs) = getOpt RequireOrder options args

    -- handle flags
    mapM_ handleFlag opts

    -- if there were any errors, show them
    unless (null errs) $ do
        -- print each error on one line
        pn <- getProgName
        mapM_ (hPutStr stderr . printf "%s: %s" pn) errs
        hPutUsage stderr
        exitFailure

    -- if no arguments given, print a usage summary and fail
    when (null nonopts) $ hPutUsage stderr >> exitFailure

    -- run the specified command
    success <- runCmd (head nonopts) (tail nonopts)
    unless success exitFailure
