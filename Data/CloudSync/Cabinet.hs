module Data.CloudSync.Cabinet where

import Control.Monad        ( when, unless, liftM2 )
import System.Directory
import System.FilePath
import System.IO
import Text.Printf          ( printf )

data Cabinet = Cabinet {
      root :: FilePath
    } deriving (Show)

-- |Return the root of all 'special' directories inside the cabinet.
specialRoot :: Cabinet -> FilePath
specialRoot c = joinPath [root c, ".cs"]

-- |Return all directories which must exist for a cabinet to be deemed to
-- 'exist', including the root directory of the cabinet itself.
existDirs :: Cabinet -> [FilePath]
existDirs c = root c : map (\p -> joinPath [root c, ".cs", p]) ["", "objects"]

-- |Return true iff all special directories exist within a cabinet dir.
isValidCabinet :: Cabinet -> IO Bool
isValidCabinet c = do
    if isRelative (root c)
        then return False
        else do
            existFlags <- mapM doesDirectoryExist (existDirs c)
            return $ all (==True) existFlags

-- |Initialise a cabinet from an existing directory.
fromDirectory :: FilePath -> IO Cabinet
fromDirectory p = do
    -- Where should the cabinet's root-dir be?
    rp <- canonicalizePath p

    -- Create Cabinet record
    let c = Cabinet rp

    valid <- isValidCabinet c
    if valid
        then return c
        else error (printf "%s: not a cabinet directory" (root c))

-- |Initialise a directory to be a cabinet and return the cabinet.
init :: FilePath -> IO Cabinet
init p = do
    -- Where should the cabinet's root-dir be?
    root <- canonicalizePath p

    -- Create Cabinet record
    let c = Cabinet root

    -- Is the passed filepath an existing directory?
    e <- doesDirectoryExist p
    unless e (error (printf "%s: not a directory" p))

    -- Is it an existing cabinet?
    ce <- isValidCabinet c
    when ce (error (printf "%s: already a cabinet" p))

    -- Make all the directories
    mapM_ (createDirectoryIfMissing True) (existDirs c)

    return c
