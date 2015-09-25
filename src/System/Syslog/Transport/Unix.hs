{-# LANGUAGE CPP #-}

module System.Syslog.Transport.Unix
    ( Options(..), defaultOptions
    , openlog, openlogOptions, closelog, syslog
    ) where

import           Control.Concurrent.MVar
import           Control.Exception
import qualified Data.ByteString.Builder as Bu
import qualified Data.ByteString.Lazy as BL
import           System.Mem.Weak ( addFinalizer )
import           Network.Socket

import qualified Data.Syslog.RFC5424 as RFC5424
import           System.Syslog.Transport.Unix.Internal

data Options = Options { unixAddress :: !String } deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options {
#ifdef mingw32_HOST_OS
       unixAddress = error "windows not supported"
#elif  darwin_HOST_OS
       unixAddress = "/var/run/syslog"
#else
       unixAddress = "/dev/log"
#endif
    }

openlog :: IO SyslogHandle
openlog = openlogOptions defaultOptions

openlogOptions :: Options -> IO SyslogHandle
openlogOptions opt = do
    s <- bracketOnError
           (socket AF_UNIX Datagram 0)
           (sClose)  -- only done if there's an error
           (\s -> do
                    connect s (SockAddrUnix (unixAddress opt))
                    addFinalizer s (sClose s)
                    return s
           )
    lock <- newMVar ()
    return $! SyslogHandle s lock

closelog :: SyslogHandle -> IO ()
closelog h = withMVar (lock h) $ \_ -> sClose (sock h)

syslog :: SyslogHandle -> RFC5424.SyslogMsg -> IO ()
syslog h msgb = syslogWrite h msg
  where msg = BL.toStrict (Bu.toLazyByteString (RFC5424.runSyslogMsg msgb))
