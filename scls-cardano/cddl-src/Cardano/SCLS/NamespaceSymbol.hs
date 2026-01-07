-- | Module defining an existential wrapper for known namespace symbols.
module Cardano.SCLS.NamespaceSymbol (
  SomeNamespaceSymbol (..),
  toString,
) where

import Cardano.SCLS.NamespaceKey (NamespaceKeySize)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, KnownSymbol, symbolVal)

-- | Existential wrapper for known namespace symbols.
data SomeNamespaceSymbol = forall ns. (KnownSymbol ns, KnownNat (NamespaceKeySize ns)) => SomeNamespaceSymbol (Proxy ns)

instance Eq SomeNamespaceSymbol where
  (SomeNamespaceSymbol (_ :: proxy ns1)) == (SomeNamespaceSymbol (_ :: proxy ns2)) =
    symbolVal (Proxy @ns1) == symbolVal (Proxy @ns2)

instance Ord SomeNamespaceSymbol where
  compare (SomeNamespaceSymbol (_ :: proxy ns1)) (SomeNamespaceSymbol (_ :: proxy ns2)) =
    compare (symbolVal (Proxy @ns1)) (symbolVal (Proxy @ns2))

instance Show SomeNamespaceSymbol where
  show (SomeNamespaceSymbol (_ :: proxy ns)) = symbolVal (Proxy @ns)

-- | Convert a 'SomeNamespaceSymbol' to its string representation.
toString :: SomeNamespaceSymbol -> String
toString (SomeNamespaceSymbol (_ :: proxy ns)) = symbolVal (Proxy @ns)
