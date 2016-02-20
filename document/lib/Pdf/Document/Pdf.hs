{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Pdf.Document.Pdf
(
  Pdf,
  pdfWithFile,
  pdfWithHandle,
  document,
  lookupObject,
  streamContent,
  rawStreamContent,
  deref,
  isEncrypted,
  setUserPassword,
  EncryptedError (..)
)
where

import Pdf.Core hiding (rawStreamContent)
import qualified Pdf.Core.IO.Buffer as Buffer

import Pdf.Document.File (File)
import qualified Pdf.Document.File as File
import Pdf.Document.Internal.Types

import Data.Typeable
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Exception hiding (throw)
import System.IO (Handle)
import System.IO.Streams (InputStream)

-- | Make Pdf with interface to pdf file
pdfWithFile :: File -> IO Pdf
pdfWithFile f = return (Pdf f)

-- | Make Pdf with seekable handle
pdfWithHandle :: Handle -> IO Pdf
pdfWithHandle h = do
  buf <- Buffer.fromHandle h
  File.withBuffer knownFilters buf >>= pdfWithFile

file :: Pdf -> File
file (Pdf f) = f

-- | Get PDF document
document :: Pdf -> IO Document
document pdf = do
  status <- File.encryptionStatus (file pdf)
  case status of
    File.Encrypted -> throwIO $
      EncryptedError "File is encrypted, use 'setUserPassword'"
    _ -> return ()

  Document pdf <$> File.trailer (file pdf)

-- | Find object by it's reference
lookupObject :: Pdf -> Ref -> IO Object
lookupObject pdf ref = File.object (file pdf) ref

-- | Get stream content, decoded and decrypted
--
-- Note: length of the content may differ from the raw one
streamContent :: Pdf
              -> Ref
              -> Stream
              -> IO (InputStream ByteString)
streamContent pdf ref s = File.streamContent (file pdf) ref s

-- | Get stream content without decrypting or decoding it
rawStreamContent
  :: Pdf
  -> Ref
  -> Stream
  -> IO (InputStream ByteString)
rawStreamContent pdf ref s =
  File.rawStreamContent (file pdf) ref s

-- | Recursively load indirect object
deref :: Pdf -> Object -> IO Object
deref pdf (Ref ref) = do
  o <- lookupObject pdf ref
  deref pdf o
deref _ o = return o

-- | Whether the PDF document it encrypted
isEncrypted :: Pdf -> IO Bool
isEncrypted pdf = do
  status <- File.encryptionStatus (file pdf)
  case status of
    File.NotEncrypted -> return False
    File.Encrypted -> return True
    File.Decrypted -> return True

-- | Set the password to be user for decryption
--
-- Returns False when the password is wrong
setUserPassword :: Pdf -> ByteString -> IO Bool
setUserPassword pdf pass = do
  File.setUserPassword (file pdf) pass

-- | File is encrypted
data EncryptedError = EncryptedError Text
  deriving (Show, Typeable)

instance Exception EncryptedError
