{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Namespace handling.
module Cardano.Types.Namespace (
  Namespace,
  fromText,
  unsafeFromText,
  asText,
  toFilePath,
  parseFilePath,
  DecodeError (..),
  asBytes,
  parseBytes,
  asString,
  humanFileNameFor,
  fromSymbol,
) where

import Control.Exception
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Char
import Data.Proxy (Proxy)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Simple newtype wrapper around Text for namespaces.
newtype Namespace = Namespace {asText :: Text}
  deriving (Eq, Ord)
  deriving (Show)

instance IsString Namespace where
  fromString = fromText . T.pack

{- | Construct a 'Namespace' from 'Text'. Trims leading and trailing whitespace.

Prefer using qualified.
-}
fromText :: Text -> Namespace
fromText = Namespace . T.strip

{- | Construct a 'Namespace' from Text, does not perform
an input cleanup.
-}
unsafeFromText :: Text -> Namespace
unsafeFromText = Namespace

-- | Construct a 'Namespace' from a type-level symbol.
fromSymbol :: (KnownSymbol n) => Proxy n -> Namespace
fromSymbol = fromText . T.pack . symbolVal

{- | Convert Namespace to a safe 'FilePath' by base16 encoding
the content.
-}
toFilePath :: Namespace -> FilePath
toFilePath ns = T.unpack (toSerializable ns)

-- | Parse safe FilePath representation.
parseFilePath :: FilePath -> Either DecodeError Namespace
parseFilePath fp = fromSerializable (T.pack fp)

{- | Encode a 'Namespace' into a filename-safe 'Text'.
The encoding used is lowercase hexadecimal of the UTF-8 bytes.
-}
toSerializable :: Namespace -> Text
toSerializable (Namespace t) =
  T.decodeUtf8 (B16.encode (T.encodeUtf8 t))

-- | Represent 'Namespace' as bytes.
asBytes :: Namespace -> BS.ByteString
asBytes (Namespace t) = T.encodeUtf8 t

{- | Parse a 'Namespace' from UTF-8 encoded bytes.
Returns 'Left' with a 'DecodeError' if the bytes are not valid UTF-8.
-}
parseBytes :: BS.ByteString -> Either DecodeError Namespace
parseBytes bs = Namespace <$> first InvalidUtf8 (T.decodeUtf8' bs)

-- | Represent 'Namespace' as 'String'.
asString :: Namespace -> String
asString = T.unpack . asText

-- | Errors that can occur during decoding of a 'Namespace'.
data DecodeError
  = -- | There is an encoded byte that is not a valid UTF-8.
    InvalidUtf8 UnicodeException
  | -- | Hex encoding contains invalid characters.
    InvalidHex String
  deriving (Show)
  deriving (Exception)

{- | Decode a filename-safe 'Text' back into a 'Namespace'.
Returns 'Left' with an error message on invalid input (non-hex or invalid UTF-8).
-}
fromSerializable :: Text -> Either DecodeError Namespace
fromSerializable txt = do
  let bs = T.encodeUtf8 txt
  decoded <- first InvalidHex (B16.decode bs)
  Namespace <$> first InvalidUtf8 (T.decodeUtf8' decoded)

{- | Create a public name for the file, it's not useful to keep
base16 encoding here, because it will make even proper namespace files
unreadable. So we apply the one way rule where we change all the "bad" chars
to '_', so for example `utxo/v0` namespace becomes `utxo_v0`, that is pretty
much readable.
-}
humanFileNameFor :: Namespace -> FilePath
humanFileNameFor (Namespace ns) = base <> ".scls"
 where
  base = T.unpack (T.map convert ns)
  convert c
    | isAlphaNum c = c
    | c `elem` ['-'] = c
    | otherwise = '_'
