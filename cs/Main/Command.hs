module Main.Command ( Command(Command)
                    , action
                    , name
                    , brief
                    , full

                    , run
                    , all
                    , descTable
                    , named
                    ) where

import Data.Function    ( on )
import Data.List        ( sortBy )
import System.IO
import Text.Printf      ( printf )

-- |Data type for a command handler.
data Command = Command {
    -- |A function taking a list of command arguments and performing the
    -- command within the IO monad.
      action    :: [String] -> IO Bool

    -- |The name of the command.
    , name      :: String

    -- |A brief one-line description.
    , brief     :: String

    -- |A filler multi-line description. One line per element of the list.
    , full      :: [String]
    }

-- |Return the Command record corresponding to the given name.
named :: [Command] -> String -> Maybe Command
named cs n =
    case matching of
        []      -> Nothing
        [c]     -> Just c

        -- this case should never happen if the command structure is
        -- appropriately initialised.
        (_:_)   -> error (printf "Multiple commands named: %s" n)
    where
        matching = filter ((== n) . name) cs

-- |Return a string listing all commands and including a brief descriptions.
descTable :: [Command] -> String
descTable cs =
        unlines $ map descStr (sortBy (compare `on` name) cs)
    where
        maxLen = maximum $ map (length . name) cs
        descStr c = printf "  %*s  %s" maxLen (name c) (brief c)

-- |Call a command given a name and arguments list. Return the a flag
-- indicating success or Nothing if no command matching that name could be
-- found.
run :: [Command] -> String -> [String] -> IO (Maybe Bool)
run cs n args =
        case mc of
            Just c  -> fmap Just (action c args)
            Nothing -> return Nothing
    where
        mc = named cs n
