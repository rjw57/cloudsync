module Cabinet  ( Cabinet

                , createCabinetDir
                , ensureCabinetDir
                , openCabinetDir

                , ensureObjectStore
                , objectStore

                ) where

import Control.Monad        ( unless, when )

import System.Directory     ( canonicalizePath
                            , createDirectory
                            , doesDirectoryExist
                            )

import System.FilePath      ( isRelative, joinPath )

data Cabinet = Cabinet FilePath
               deriving (Show)

-- |Return the result of joining the relative path p, to the cabinet root.
cabinetFilePath :: Cabinet -> FilePath -> FilePath
cabinetFilePath (Cabinet p) rp | isRelative rp = joinPath [p, rp]
cabinetFilePath           _ rp                 = error (shows rp ": non-relative path")

-- |Return the location of the object store within a cabinet directory.
objectStore :: Cabinet -> FilePath
objectStore c = cabinetFilePath c "objects"

-- |Return the location of the object store within a cabinet directory ensuring
-- it exists in the process.
ensureObjectStore :: Cabinet -> IO FilePath
ensureObjectStore c = do
    let os = objectStore c
    os_exists <- doesDirectoryExist os
    unless os_exists (createDirectory os)
    return os

-- |Open a cabinet directory, attempting to create it if necessary.
ensureCabinetDir :: FilePath -> IO Cabinet
ensureCabinetDir p = do
    -- open or create depending on existence
    p_exists <- doesDirectoryExist p
    if p_exists then openCabinetDir p else createCabinetDir p

-- |Create a cabinet directory, failing if it already exists
createCabinetDir :: FilePath -> IO Cabinet
createCabinetDir p = do
    -- check cabinet store does not already exist
    p_exists <- doesDirectoryExist p
    when p_exists (fail (shows p ": already exists"))

    -- create it
    createDirectory p

    -- open it
    openCabinetDir p

-- |Open an existing cabinet directory, failing if it doesn't exist
openCabinetDir :: FilePath -> IO Cabinet
openCabinetDir p = do
    -- check directory exists
    p_exists <- doesDirectoryExist p
    unless p_exists (fail (shows p ": is not a directory"))

    -- canonicalise the path
    p <- canonicalizePath p

    -- create the cabinet store record
    let c = Cabinet p

    -- make the object store directory if necessary
    ensureObjectStore c

    return c

-- vim:sw=4:sts=4:et
