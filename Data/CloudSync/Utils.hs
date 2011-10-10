module Data.CloudSync.Utils where

import Control.Monad
import System.FilePath
import System.Directory
import System.Posix.Files
import System.Posix.Types

-- |Given a file path, return a list where the first element is the file path
-- and the remaining are the parents up until the root
parents :: FilePath -> [FilePath]
parents fp =
        ps ++ take 1 rest
    where
        -- an infinite list of parents where the root directory is repeated
        allps = iterate takeDirectory fp

        -- snip the list after the first repeat
        (ps, rest) = span (\p -> p /= takeDirectory p) allps

-- |Given a file path, return a list as with parents but stop when the device
-- ID changes, i.e. include only those directories on the same filesystem as
-- the original filepath.
parentsOnSameDevice :: FilePath -> IO [FilePath]
parentsOnSameDevice fp = do
    -- get the device ID of the file
    dID <- fmap deviceID (getFileStatus fp)

    -- get the paths and device IDs of all parents
    pdIDs <- mapM (\pfp -> getFileStatus pfp >>= \s -> return (pfp, deviceID s)) (parents fp)

    -- return the first of those which match the original device ID
    return $ map fst $ takeWhile (\(_, pdID) -> pdID == dID) pdIDs

