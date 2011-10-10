module Main.Command.Help                ( command ) where

import System.Environment                   ( getProgName )
import System.IO
import Text.Printf                          ( printf )

import Main.Command as Cmd

command :: [Cmd.Command] -> Cmd.Command
command cs = Cmd.Command (thisAction cs) "help" "Show usage for a particular command." (helpLines cs)

-- |The help command
thisAction :: [Cmd.Command] -> [String] -> IO Bool
thisAction cs args = case length args of
    0   -> printUsage cs >> return True
    1   -> helpSpecificCmd cs (head args)
    2   -> do
        pn <- getProgName
        hPutStrLn stderr $ printf "%s: help: multiple arguments passed to help command" pn
        return False

helpLines :: [Cmd.Command] -> [String]
helpLines _ = [
    "Provide help on a specific command or list available commands."
    ]

-- |Given the program name and set of available commands, generate a string
-- giving top-level usage information.
usage :: [Cmd.Command] -> String -> String
usage cs pn = unlines $
    printf "Usage: %s %s [<command>]" pn (name $ command cs) : (full $ command cs)
    ++
    [ ""
    , "Available commands:"
    , Cmd.descTable cs
    ]

-- |Print a brief top-level usage summary to the console.
printUsage :: [Cmd.Command] -> IO ()
printUsage = hPutUsage stdout

-- |Print a brief top-level usage summary to a filehandle
hPutUsage :: Handle -> [Cmd.Command] -> IO ()
hPutUsage h cs = getProgName >>= (hPutStr h . usage cs)

helpSpecificCmd :: [Cmd.Command] -> String -> IO Bool
helpSpecificCmd cs n = case Cmd.named cs n of
    Just c  -> mapM_ putStrLn (Cmd.full c) >> return True
    Nothing -> do
        pn <- getProgName
        hPutStrLn stderr $ printf "%s: help: %s: no such command" pn n
        return False

