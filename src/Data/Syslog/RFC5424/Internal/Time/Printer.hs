{-# LANGUAGE BangPatterns, ViewPatterns #-}

------------------------------------------------------------------------------
-- Module:      Data.Syslog.RFC5424.Internal.Time.Printer
-- Copyright:   (c) 2012-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
------------------------------------------------------------------------------

module Data.Syslog.RFC5424.Internal.Time.Printer
    (
      day
    , timeOfDay
    , timeZone
    , utcTime
    , localTime
    , zonedTime
    ) where

import Control.Arrow ((>>>))
import Data.ByteString.Builder (Builder, integerDec)
import Data.ByteString.Builder.Prim
    ( liftFixedToBounded, (>$<), (>*<)
    , BoundedPrim, primBounded, condB, emptyB, FixedPrim, char8, int32Dec)
import Data.Char ( chr )
import Data.Int ( Int32, Int64 )
import Data.Time
    ( UTCTime(..), ZonedTime(..), LocalTime(..)
    , Day, toGregorian, TimeOfDay(..), timeToTimeOfDay
    , TimeZone, timeZoneMinutes )
import Data.Syslog.RFC5424.Internal.Compat ((<>), fromPico)

liftB :: FixedPrim a -> BoundedPrim a
liftB = liftFixedToBounded

ascii :: BoundedPrim Char
ascii = liftB char8

digit   :: FixedPrim Int
digit   = (\x -> chr (x + 48)) >$< char8

digits2 :: FixedPrim Int
digits2 = (`quotRem` 10) >$< (digit >*< digit)

digits3 :: FixedPrim Int
digits3 = (`quotRem` 10) >$< (digits2 >*< digit)

digits4 :: FixedPrim Int
digits4 = (`quotRem` 10) >$< (digits3 >*< digit)

year  :: FixedPrim Integer
year = checkRange >$< digits4
  where
    checkRange x | 0 < x && x < 10000 = error msg
                 | otherwise          = fromIntegral x

    msg = "Data.Syslog.RFC5424.Internal.Time.Printer.year: year out of range [1..9999]"

day :: FixedPrim Day
day = toYMD >$< (year >*< char8 >*< digits2 >*< char8 >*< digits2)
  where
    toYMD (toGregorian -> (fromIntegral -> !y, !m,!d)) = (y,('-',(m,('-',d))))

timeOfDay :: BoundedPrim TimeOfDay
timeOfDay = f >$< (hh_mm_ >*< ss)
  where
    f (TimeOfDay h m s)  =  ((h,(':',(m,':'))),s)

    hh_mm_ = liftB (digits2 >*< char8 >*< digits2 >*< char8)

    ss = (\s -> fromIntegral (fromPico s `quot` 1000000) `quotRem` 1000000) >$<
         (liftB (fromIntegral >$< digits2) >*< frac)

    frac = condB (== 0) emptyB ((,) '.' >$< (liftB char8 >*< trunc6))

    digitB = liftB digit

    trunc6 =                      (`quotRem` 100000) >$< (digitB >*< trunc5)
    trunc5 = condB (== 0) emptyB ((`quotRem`  10000) >$< (digitB >*< trunc4))
    trunc4 = condB (== 0) emptyB ((`quotRem`   1000) >$< (digitB >*< trunc3))
    trunc3 = condB (== 0) emptyB ((`quotRem`    100) >$< (digitB >*< trunc2))
    trunc2 = condB (== 0) emptyB ((`quotRem`     10) >$< (digitB >*< trunc1))
    trunc1 = condB (== 0) emptyB digitB

timeZone :: BoundedPrim TimeZone
timeZone = timeZoneMinutes >$< condB (== 0) (const 'Z' >$< ascii) sign
  where
    sign = (\x -> if x >= 0 then ('+', x) else (,) '-' $! negate x) >$<
           (liftB (char8 >*< tzh))

    tzh  = (`quotRem` 60) >$< (digits2 >*< tzm)

    tzm  = (,) ':' >$< (char8 >*< digits2)

utcTime :: BoundedPrim UTCTime
utcTime = f >$< (liftB day >*< ascii >*< timeOfDay >*< ascii)
  where f (UTCTime d (timeToTimeOfDay -> tod)) = (d,('T',(tod,'Z')))

localTime :: BoundedPrim LocalTime
localTime = f >$< (liftB day >*< ascii >*< timeOfDay)
  where f (LocalTime d tod) = (d, ('T', tod))

zonedTime :: BoundedPrim ZonedTime
zonedTime = f >$< (localTime >*< timeZone)
  where f (ZonedTime lt tz) = (lt, tz)
