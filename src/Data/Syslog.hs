{-# LANGUAGE DeriveDataTypeable #-}

module Data.Syslog
     ( Facility(..)
     , Priority(..)
     , syslogCode
     ) where

import Data.Typeable

data Priority =
     EMERGENCY
   | ALERT
   | CRITICAL
   | ERROR
   | WARNING
   | NOTICE
   | INFO
   | DEBUG
     deriving (Eq, Ord, Bounded, Enum, Show, Read, Typeable)

data Facility =
     KERN
   | USER
   | MAIL
   | DAEMON
   | AUTH
   | SYSLOG
   | LPR
   | NEWS
   | UUCP
   | CRON
   | AUTHPRIV
   | FTP
   | NTP
   | LOG_AUDIT
   | LOG_ALERT
   | CLOCK
   | LOCAL0
   | LOCAL1
   | LOCAL2
   | LOCAL3
   | LOCAL4
   | LOCAL5
   | LOCAL6
   | LOCAL7
     deriving (Eq, Ord, Bounded, Enum, Show, Read, Typeable)

syslogCode :: Integral n => Facility -> Priority -> n
syslogCode x y = 8 * fromIntegral (fromEnum x) + fromIntegral (fromEnum y)
{-# INLINE syslogCode #-}
