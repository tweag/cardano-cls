{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Module defining an existential wrapper for known namespace symbols.
module Cardano.SCLS.NamespaceSymbol (
  SomeNamespaceSymbol (..),
  toString,
  KnownSpec (..),
  mkNamespaceSymbol,
) where

import Cardano.SCLS.NamespaceKey (NamespaceKeySize)
import Codec.CBOR.Cuddle.Huddle (Huddle)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, KnownSymbol, symbolVal)

{- | An instance of 'KnownSpec' connects a type-level namespace symbol @ns@
  with its corresponding specification encoded as a 'Huddle' value. Instances
  of this class should be defined for each namespace.
-}
class (KnownSymbol ns) => KnownSpec ns where
  namespaceSpec :: proxy ns -> Huddle

-- | Existential wrapper for known namespace symbols.
data SomeNamespaceSymbol = forall ns. (KnownSymbol ns, KnownNat (NamespaceKeySize ns), KnownSpec ns) => SomeNamespaceSymbol (Proxy ns)

instance Eq SomeNamespaceSymbol where
  (SomeNamespaceSymbol (_ :: proxy ns1)) == (SomeNamespaceSymbol (_ :: proxy ns2)) =
    symbolVal (Proxy @ns1) == symbolVal (Proxy @ns2)

instance Ord SomeNamespaceSymbol where
  compare (SomeNamespaceSymbol (_ :: proxy ns1)) (SomeNamespaceSymbol (_ :: proxy ns2)) =
    compare (symbolVal (Proxy @ns1)) (symbolVal (Proxy @ns2))

instance Show SomeNamespaceSymbol where
  show (SomeNamespaceSymbol (_ :: proxy ns)) = symbolVal (Proxy @ns)

-- | Create a 'SomeNamespaceSymbol' from a type-level namespace.
mkNamespaceSymbol :: forall ns. (KnownSpec ns, KnownNat (NamespaceKeySize ns)) => SomeNamespaceSymbol
mkNamespaceSymbol = SomeNamespaceSymbol (Proxy @ns)

-- | Convert a 'SomeNamespaceSymbol' to its string representation.
toString :: SomeNamespaceSymbol -> String
toString (SomeNamespaceSymbol (_ :: proxy ns)) = symbolVal (Proxy @ns)
