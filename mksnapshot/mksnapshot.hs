import Control.Monad

import Data.ByteString              as BS       (ByteString, pack, unpack)
import Data.ByteString.Base16       as BS16     (encode)
import Data.ByteString.Char8        as BSC8     (pack, unpack)

import Data.ByteString.Lazy         as BSL      (pack, unpack, hPut)
import Data.ByteString.Lazy.Char8   as BSLC8    (pack, unpack)
import Data.ByteString.Base16.Lazy  as BSL16    (encode)

import Data.Digest.SHA256           as Digest   (hash)

import Data.Text.Lazy               as T        (pack)
import Data.Text.Lazy.Encoding                  (encodeUtf8)

import Data.Word                                (Word8)

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Files

import Text.Printf

import Cabinet

-- |Add an object whose content is the specified String to the store. The
-- contents are encoded as UTF8.
createStringObject :: Cabinet -> ObjectType -> String -> IO Object
createStringObject c typ cnts = createObject c typ octets
    where octets = BSL.unpack $ encodeUtf8 $ T.pack cnts

-- |Write an object header to a file handle
writeHeader :: ObjectType -> Int -> Handle -> IO ()
writeHeader t s file = hPutStr file $ printf "%s %d" (describeType t) s

-- |Add an object whose content is the specified list of octets to the store.
createObject :: Cabinet -> ObjectType -> [Word8] -> IO Object
createObject c typ cnts =
    do
        -- ensure that the directory containing the object files exists
        ensureObjectPaths object

        -- write the header
        withFile (header paths) WriteMode $ writeHeader typ len

        -- write the contents
        withFile (contents paths) WriteMode $ \h -> BSL.hPut h (BSL.pack cnts)

        -- return the object
        return object

    where digest    = Digest.hash cnts                           -- digest octets
          name      = BSC8.unpack $ BS16.encode $ BS.pack digest -- digest hex string
          len       = length cnts                                -- length of contents
          object    = Object c name
          paths     = objectPaths object

data Object = Object Cabinet String
              deriving (Show)

data ObjectType = ObjectType String

describeType :: ObjectType -> String
describeType (ObjectType t) = t

data ObjectPaths = ObjectPaths String String
                   deriving (Show)

header :: ObjectPaths -> String
header (ObjectPaths _ p) = p

contents :: ObjectPaths -> String
contents (ObjectPaths p _) = p

-- |Return the paths containing the contents and header for the specified object name
objectPaths :: Object -> ObjectPaths
objectPaths o =
    ObjectPaths (addExtension base "contents") (addExtension base "header")
    where (dir, file) = objectBase o
          base = joinPath [dir, file]

-- |The directory containing the object's files and the remainder of the name
objectBase :: Object -> (FilePath, String)
objectBase (Object c n) = (joinPath [objectStore c, take 2 n], drop 2 n)

-- Ensure the directory containing the object files exists
ensureObjectDir :: Object -> IO ()
ensureObjectDir o =
    do
        exists <- doesDirectoryExist dir
        unless exists $ createDirectory dir
    where (dir, _) = objectBase o

-- Like objectPaths but ensures that the directory containing the header and
-- contents files exists.
ensureObjectPaths :: Object -> IO ObjectPaths
ensureObjectPaths o = ensureObjectDir o >> return (objectPaths o)

main :: IO()
main = do
    c <- ensureCabinetDir "hello"
    print c
    obj <- createStringObject c (ObjectType "blob") "ngsjngr"
    print $ objectPaths obj

-- vim:sw=4:sts=4:et
