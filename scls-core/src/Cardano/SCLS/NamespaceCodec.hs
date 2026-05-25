{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | Namespace-versioned canonical CBOR encoding/decoding.

This module provides a mechanism to associate data types with versioned namespaces
at the type level. This is useful for handling different eras of Cardano ledger,
where each era might have different data structures with their own encoding schemes.
-}
module Cardano.SCLS.NamespaceCodec (
  KnownNamespace (..),
  CanonicalCBOREntryEncoder (..),
  CanonicalCBOREntryDecoder (..),
  NamespaceKeySize,
  namespaceKeySize,
  encodeKeyToBytes,
  decodeKeyFromBytes,
  encodeEntryToBytes,
  decodeEntryFromBytes,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (getRawDecoder), CanonicalEncoding (getRawEncoding))
import Cardano.SCLS.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.NamespaceKey
import Cardano.SCLS.Versioned (Versioned (..))
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.MemPack (StateT (runStateT), Unpack (runUnpack), packWithByteArray)
import Data.MemPack.Buffer (pinnedByteArrayToByteString)
import Data.MemPack.Error (SomeError)
import Data.MemPack.Extra (ByteStringSized (..), RawBytes (RawBytes), runDecode)
import GHC.TypeLits (KnownNat)

{- | Encode a value to canonical CBOR with a specific namespace.

A namespace identifies the specific version of the encoding for a data type.
For example, "utxo/v0" might represent a certain era's UTxO format, while
"utxo/v1" represents the following era's format.

The namespace parameter is a type-level string (Symbol) that constrains the
encoding at compile-time. The key size is automatically determined by the
'NamespaceKeySize' type family based on the namespace.
-}
class CanonicalCBOREntryEncoder ns a where
  -- | Encode an entry value to canonical CBOR for the given namespace.
  encodeEntry :: a -> CanonicalEncoding

{- | Decode a value from canonical CBOR with a specific namespace.

Complements 'CanonicalCBOREntryEncoder' for deserialization . Returns a 'Versioned'
wrapper that tracks the namespace information.
-}
class CanonicalCBOREntryDecoder ns a where
  -- | Decode a value from canonical CBOR with a specific namespace.
  decodeEntry :: CanonicalDecoder s (Versioned ns a)

encodeEntryToBytes :: forall ns a. (CanonicalCBOREntryEncoder ns a) => a -> RawBytes
encodeEntryToBytes a =
  RawBytes $ toStrictByteString $ getRawEncoding $ encodeEntry @ns a

decodeEntryFromBytes :: forall ns a. (CanonicalCBOREntryDecoder ns a) => RawBytes -> Either DeserialiseFailure (Versioned ns a)
decodeEntryFromBytes (RawBytes bs) =
  fmap snd $ deserialiseFromBytes (getRawDecoder $ decodeEntry @ns) $ BSL.fromStrict bs

{- | A type class that associates a namespace with its key and entry types.
This type class acts as a bridge between the namespace/version, its key type,
its entry type, and the encoding/decoding mechanisms.

Defining an instance of 'KnownNamespace' for a specific namespace requires specifying:
  - 'NamespaceKey': The type used as the key for entries in this namespace.
  - 'NamespaceEntry': The type of the entries stored in this namespace.
  - 'CanonicalCBOREntryEncoder' and 'CanonicalCBOREntryDecoder' instances for the entry type.
  - 'NamespaceKeySize' type family instance for the namespace.
  - 'IsKey' instance for the key type.
-}
class
  ( IsKey (NamespaceKey ns)
  , KnownNat (NamespaceKeySize ns) -- ensures type family NamespaceKeySize is defined for this namespace
  , CanonicalCBOREntryEncoder ns (NamespaceEntry ns)
  , CanonicalCBOREntryDecoder ns (NamespaceEntry ns)
  ) =>
  KnownNamespace ns
  where
  type NamespaceKey ns
  type NamespaceEntry ns

-- | Encode a namespace key to a 'ByteStringSized' of the appropriate size.
encodeKeyToBytes :: forall ns. (KnownNamespace ns, Typeable (NamespaceKey ns)) => NamespaceKey ns -> ByteStringSized (NamespaceKeySize ns)
encodeKeyToBytes key =
  ByteStringSized . pinnedByteArrayToByteString $ packWithByteArray True (show (typeRep (Proxy @(NamespaceKey ns)))) (keySize @(NamespaceKey ns)) (packKeyM key)

{- | Decode a namespace key from a 'ByteString'.
Returns 'Nothing' if decoding fails.
-}
decodeKeyFromBytes ::
  forall ns.
  ( KnownNamespace ns
  , Typeable (NamespaceKey ns)
  ) =>
  Proxy ns ->
  ByteString ->
  Either SomeError (NamespaceKey ns)
decodeKeyFromBytes _ bs = do
  let unpacker = unpackKeyM
  second fst $ runDecode $ runStateT (runUnpack unpacker bs) 0
