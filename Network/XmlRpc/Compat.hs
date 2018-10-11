----------------------------------------

{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE ConstraintKinds #-}
#endif

----------------------------------------

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

----------------------------------------

{-| Compatibility module.

Forwards-Compatibility and Backwards-Compatibility for:

* the <https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid SemigroupMonoid> proposal.

    Exports 'Data.Monoid' and (when present) 'Data.Semigroup'.

    Since `base-4.11.0.0`, i.e. `ghc-8.4`+, `Semigroup` is a superclass of `Monoid`, and `mappend` becomes redundant (with default implementation `mappend = (<>)`).

* the <https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail MonadFail> proposal.

    Exports 'Control.Monad' and (when present) 'Control.Monad.Fail'.

    `ghc-8.0.*` bundles `base-4.9.0.0`, which introduces `Control.Monad.Fail`.

-}

module Network.XmlRpc.Compat

#if MIN_VERSION_base(4,9,0)
  ( module Data.Monoid
  , module Data.Semigroup
  , module Control.Monad
  , module Control.Monad.Fail
#else
  ( module Data.Monoid
  , module Control.Monad
  , module Network.XmlRpc.Compat
#endif
  ) where

----------------------------------------

#if MIN_VERSION_base(4,9,0)
import Data.Monoid    hiding ((<>))
import Data.Semigroup               (Semigroup(..))
#else
import Data.Monoid
#endif

----------------------------------------

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad      hiding (fail)
#else
import Control.Monad
#endif

----------------------------------------

#if !MIN_VERSION_base(4,9,0)
type MonadFail = Monad
#endif

----------------------------------------