{-# LANGUAGE BangPatterns #-}

module System.Syslog.Transport.Unix.Internal where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as B
import           Foreign.Ptr (plusPtr)
import           Foreign.ForeignPtr (withForeignPtr)
import           Network.Socket

data SyslogHandle = SyslogHandle {
      sock :: !Socket
    , lock :: !(MVar ())
    }

sWriteOnce :: Socket -> ByteString -> IO Int
sWriteOnce s (PS fp off len) =
    withForeignPtr fp $ \ ptr ->
        sendBuf s (ptr `plusPtr` off) len

syslogWrite :: SyslogHandle -> ByteString -> IO ()
syslogWrite h !str = withMVar (lock h) $ \_ -> void (sWriteOnce (sock h) str)
