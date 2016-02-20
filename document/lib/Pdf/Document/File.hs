{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- PDF file as container for objects

module Pdf.Document.File
( File (..)
, NotFound (..)
, withBuffer
, EncryptionStatus (..)
)
where

import Pdf.Core hiding (trailer, rawStreamContent)
import qualified Pdf.Core as Core
import Pdf.Core.Util
import Pdf.Core.IO.Buffer (Buffer)

import Pdf.Document.Encryption

import Data.Typeable
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)

-- | PDF file
--
data File = File
  { object :: Ref -> IO Object
  , streamContent :: Ref -> Stream -> IO (InputStream ByteString)
  , rawStreamContent :: Ref -> Stream -> IO (InputStream ByteString)
  , trailer :: IO Dict
  , setDecryptor :: Decryptor -> IO ()
  , setUserPassword :: ByteString -> IO Bool
  , encryptionStatus :: IO EncryptionStatus
  }

data EncryptionStatus
  = Encrypted
  | NotEncrypted
  | Decrypted
  deriving (Show, Eq)

data File_ = File_
  { _lastXRef :: XRef
  , _buffer :: Buffer
  , _filters :: [StreamFilter]
  , _decrRef :: IORef (Maybe Decryptor)
  }

-- | PDF file from buffer
withBuffer :: [StreamFilter] -> Buffer -> IO File
withBuffer filters buf = do
  xref <- lastXRef buf
  decrRef <- newIORef Nothing
  let file = File_
        { _lastXRef = xref
        , _buffer = buf
        , _filters = filters
        , _decrRef = decrRef
        }
  return File
    { object = findObject file
    , streamContent = getStreamContent file
    , rawStreamContent = getRawStreamContent file
    , trailer = getTrailer file
    , setDecryptor = writeIORef decrRef . Just
    , setUserPassword = setUserPassword_ file
    , encryptionStatus = getEncryptionStatus file
    }

-- | Recursively load indirect object
deref :: File_ -> Object -> IO Object
deref file (Ref ref) = findObject file ref
deref _ o = return o

-- | Set the password to be user for decryption
--
-- Returns False when the password is wrong
setUserPassword_ :: File_ -> ByteString -> IO Bool
setUserPassword_ file pass = message "setUserPassword" $ do
  tr <- getTrailer file
  enc <-
    case HashMap.lookup "Encrypt" tr of
      Nothing -> throwIO (Unexpected "document is not encrypted" [])
      Just o -> do
        o' <- deref file o
        case o' of
          Dict d -> return d
          Null -> throwIO (Corrupted "encryption encryption dict is null" [])
          _ -> throwIO (Corrupted "document Encrypt should be a dictionary" [])
  let either_res = mkStandardDecryptor tr enc
        (ByteString.take 32 $ pass `mappend` defaultUserPassword)
  case either_res of
    Left err -> throwIO $ Corrupted err []
    Right Nothing -> return False
    Right (Just decr) -> do
      writeIORef (_decrRef file) (Just decr)
      return True

getTrailer :: File_ -> IO Dict
getTrailer file = do
  Core.trailer (_buffer file) (_lastXRef file)

findObject :: File_ -> Ref -> IO Object
findObject file ref = do
  (obj, decrypted) <- (lookupEntryRec file ref >>= readObjectForEntry file)
      -- unknown type should be interpreted as reference to null object
    `catch` \(UnknownXRefStreamEntryType _) -> return (Null, False)
  if decrypted
    then return obj
    else decrypt file ref obj

getEncryptionStatus :: File_ -> IO EncryptionStatus
getEncryptionStatus file = do
  enc <- encrypted file
  if enc
    then do
      maybe_decr <- readIORef (_decrRef file)
      case maybe_decr of
        Nothing -> return Encrypted
        Just _ -> return Decrypted
    else return NotEncrypted

-- | Whether the PDF file it encrypted
encrypted :: File_ -> IO Bool
encrypted file = message "encrypted" $ do
  tr <- getTrailer file
  case HashMap.lookup "Encrypt" tr of
    Nothing -> return False
    Just _ -> return True

decrypt :: File_ -> Ref -> Object -> IO Object
decrypt file ref o = do
  maybe_decr <- readIORef (_decrRef file)
  case maybe_decr of
    Nothing -> return o
    Just decr -> decryptObject decr ref o

decryptStream
  :: File_
  -> Ref
  -> InputStream ByteString
  -> IO (InputStream ByteString)
decryptStream file ref is = do
  maybe_decryptor <- readIORef (_decrRef file)
  case maybe_decryptor of
    Nothing -> return is
    Just decryptor -> decryptor ref DecryptStream is

getStreamContent :: File_ -> Ref -> Stream -> IO (InputStream ByteString)
getStreamContent file ref s = do
  is <- getRawStreamContent file ref s
  decodeStream knownFilters s is

getRawStreamContent :: File_ -> Ref -> Stream -> IO (InputStream ByteString)
getRawStreamContent file ref (S dict pos) = do
  len <- do
    obj <- sure $ HashMap.lookup "Length" dict `notice` "Length missing in stream"
    case obj of
      Number _ -> sure $ intValue obj `notice` "Length should be an integer"
      Ref r -> do
        o <- findObject file r
        sure $ intValue o `notice` "Length should be an integer"
      _ -> throwIO $ Corrupted "Length should be an integer" []
  is <- Core.rawStreamContent (_buffer file) len pos
  decryptStream file ref is

readObjectForEntry :: File_-> Entry -> IO (Object, Bool)

readObjectForEntry _file EntryFree{} = return (Null, False)

readObjectForEntry file (EntryUsed off gen) = do
  (R _ gen', obj) <- readObjectAtOffset (_buffer file) off
  unless (gen' == gen) $
    throwIO (Corrupted "readObjectForEntry" ["object generation missmatch"])
  return (obj, False)

readObjectForEntry file (EntryCompressed index num) = do
  objStream@(S dict _) <- do
    o <- findObject file (R index 0)
    sure $ streamValue o `notice` "Compressed entry should be in stream"
  first <- sure $ (HashMap.lookup "First" dict >>= intValue)
      `notice` "First should be an integer"
  raw <- getStreamContent file (R index 0) objStream
  decoded <- decodeStream (_filters file) objStream raw
  obj <- readCompressedObject decoded (fromIntegral first) num
  return (obj, True)

lookupEntryRec :: File_ -> Ref -> IO Entry
lookupEntryRec file ref = loop (_lastXRef file)
  where
  loop xref = do
    res <- lookupEntry file ref xref
    case res of
      Just e -> return e
      Nothing -> do
        prev <- prevXRef (_buffer file) xref
        case prev of
          Just p -> loop p
          Nothing -> throwIO (NotFound $ "The Ref not found: " ++ show ref)

lookupEntry :: File_ -> Ref -> XRef -> IO (Maybe Entry)
lookupEntry file ref xref@(XRefTable _) =
  lookupTableEntry (_buffer file) xref ref
lookupEntry file ref (XRefStream _ s@(S dict _)) = do
  raw <- getStreamContent file ref s
  decoded <- decodeStream (_filters file) s raw
  lookupStreamEntry dict decoded ref

-- | Thrown when can't find an object
data NotFound = NotFound String
  deriving (Show, Typeable)

instance Exception NotFound
