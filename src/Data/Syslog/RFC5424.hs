{-# LANGUAGE DeriveDataTypeable, BangPatterns, ViewPatterns #-}

------------------------------------------------------------------------------
-- Module:      Data.Syslog.RFC5424
-- Copyright:   (c) 2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
------------------------------------------------------------------------------

module Data.Syslog.RFC5424
     ( SyslogMsg,  runSyslogMsg
     , syslogMsg
     , Header
     , header
     , Facility(..)
     , Priority(..)
     , RFC5424Time(..)
     , RFC5424TimePrim, runRFC5424TimePrim, runRFC5424Time
     , getCurrentTime
     , getZonedTime
     , Hostname
     , AppName
     , ProcId
     , MsgId
     , SDData
     , SDID
     , sdElement
     , SDName
     , SDParams
     , sdParam
     , Msg
     , msgAny
     , msgUtf8
     ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Bu
import           Data.ByteString.Builder.Prim ((>$<),(>*<))
import qualified Data.ByteString.Builder.Prim as BP
import           Data.Monoid (Monoid(..))
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime, ZonedTime, getZonedTime)
import           Data.Syslog
import           Data.Syslog.RFC5424.Internal
import           Data.Syslog.RFC5424.Internal.Compat((<>))
import qualified Data.Syslog.RFC5424.Internal.Time.Printer as TPP


syslogMsg :: Header -> SDData -> Maybe Msg -> SyslogMsg
syslogMsg h d mm = SyslogMsg $
       runHeader h
    <> Bu.char8 ' '
    <> runSDData d
    <> case mm of
         Nothing -> mempty
         Just m  -> Bu.char8 ' ' <> runMsg m

-- | Either "-" or from 1 to 255 printing ASCII characters.
type Hostname = ByteString
-- | Either "-" or from 1 to  48 printing ASCII characters.
type AppName  = ByteString
-- | Either "-" or from 1 to 128 printing ASCII characters.
type ProcId   = ByteString
-- | Either "-" or from 1 to  32 printing ASCII characters.
type MsgId    = ByteString

ascii :: BP.BoundedPrim Char
ascii = BP.liftFixedToBounded BP.char8

header :: RFC5424Time t
       => Facility -> Priority -> t
       -> Hostname -> AppName -> ProcId -> MsgId -> Header
header fac pri time hostname appname procid msgid = Header $
     let !code = syslogCode fac pri
      in BP.primBounded (codePrefix >*< timestamp) (code, time)
   <> checkHeaderSyntax 255 hostname <> Bu.char8 ' '
   <> checkHeaderSyntax  48 appname  <> Bu.char8 ' '
   <> checkHeaderSyntax 128 procid   <> Bu.char8 ' '
   <> checkHeaderSyntax  32 msgid
  where
    codePrefix = (\x -> ('<',(x,('>',('1',' '))))) >$<
                 (ascii >*< BP.word8Dec >*< ascii >*< ascii >*< ascii)

    timestamp  = (\t -> (t, ' ')) >$<
                 (runRFC5424TimePrim rfc5424TimePrim >*< ascii)


-- | Must contain from 1 to 32 printing ASCII characters,  except '='
--   ']', and '"'.    Technically,  any SDIDs you define yourself and
--   haven't been registered with IANA must include an \@ character
--   followed by a private enterprise number.
type SDID      = ByteString

sdElement :: SDID -> SDParams -> SDData
sdElement sdid params = SDData $
    case checkSDName sdid of
      Nothing    -> mempty
      Just sdid' -> Bu.char8 '[' <> sdid' <> runSDParams params <> Bu.char8 ']'

sdParam :: SDName -> Text -> SDParams
sdParam key val =
    case checkSDName key of
      Nothing   -> SDParamsEmpty
      Just key' -> SDParams $ key'
                           <> BP.primFixed (BP.char8 >*< BP.char8) ('=','"')
                           <> escapeSDParamText val
                           <> Bu.char8 '"'

-- | Note that arbitrary data is allowed,  but it must not start with the
--   Byte Order Mark @0xEF 0xBB 0xBF@.

msgAny :: ByteString -> Maybe Msg
msgAny s
    |    B.length s >= 3
      && B.index s 0 == 0xEF
      && B.index s 1 == 0xBB
      && B.index s 2 == 0xBF

         = Nothing

    | otherwise

         = Just (Msg (Bu.byteString s))

msgUtf8 :: Text -> Msg
msgUtf8 s = Msg (BP.primFixed bom () <> TE.encodeUtf8Builder s)

class RFC5424Time t where
  rfc5424TimePrim :: RFC5424TimePrim t

instance RFC5424Time UTCTime   where
  rfc5424TimePrim = RFC5424TimePrim TPP.utcTime

instance RFC5424Time ZonedTime where
  rfc5424TimePrim = RFC5424TimePrim TPP.zonedTime

runRFC5424Time :: RFC5424Time t => t -> Builder
runRFC5424Time = BP.primBounded (runRFC5424TimePrim rfc5424TimePrim)
