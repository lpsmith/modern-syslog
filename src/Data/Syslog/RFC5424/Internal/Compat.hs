{-# LANGUAGE CPP #-}
-- | This is a module of its own, partly because it uses the CPP extension,
-- which doesn't play well with backslash-broken string literals.
module Data.Syslog.RFC5424.Internal.Compat
    ( (<>)
    , toPico
    , fromPico
    ) where

import Data.Monoid

import Data.Fixed (Pico)
#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed(MkFixed))
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

#if !MIN_VERSION_base(4,5,0)
infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

#if MIN_VERSION_base(4,7,0)

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

#else

toPico :: Integer -> Pico
toPico = unsafeCoerce

fromPico :: Pico -> Integer
fromPico = unsafeCoerce

#endif
