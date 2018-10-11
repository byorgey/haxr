--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

--------------------------------------------------

{-| Compatibility module.

Forwards-Compatibility and Backwards-Compatibility for:

* the <https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid SemigroupMonoid> proposal.

Exports 'Data.Monoid' and (when present) 'Data.Semigroup'.

Since `base-4.11.0.0`, i.e. `ghc-8.4`+, `Semigroup` is a superclass of `Monoid`, and `mappend` becomes redundant (with default implementation `mappend = (<>)`).

-}

module Network.XmlRpc.Compat

#if MIN_VERSION_base(4,9,0)
  ( module Data.Monoid
  , module Data.Semigroup
#else
  ( module Data.Monoid
#endif
  ) where

--------------------------------------------------

#if MIN_VERSION_base(4,9,0)
import Data.Monoid    hiding ((<>))
import Data.Semigroup               (Semigroup(..))
#else
import Data.Monoid
#endif

--------------------------------------------------