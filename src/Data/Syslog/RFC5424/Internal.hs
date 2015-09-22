{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- Module:      Data.Syslog.RFC5424.Internal
-- Copyright:   (c) 2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
------------------------------------------------------------------------------

module Data.Syslog.RFC5424.Internal
     ( SyslogMsg(..)
     , Header(..)
     , RFC5424TimePrim(..)
     , SDData(..),  runSDData
     , SDName
     , SDParams(..), runSDParams
     , Msg(..)
     , checkSDName
     , escapeSDParamText
     , bom
     , checkHeaderSyntax
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
import           Data.Typeable
import           Data.Word ( Word8 )
import           Data.Syslog.RFC5424.Internal.Compat ((<>), fromPico)

newtype SyslogMsg = SyslogMsg { runSyslogMsg :: Builder } deriving (Typeable)

newtype Header = Header { runHeader :: Builder }
        deriving (Typeable)

newtype RFC5424TimePrim t = RFC5424TimePrim
    { runRFC5424TimePrim :: BP.BoundedPrim t }

-- | An abstract representation of the syntax of zero or more structured-data
--   datums.

data SDData = SDDataEmpty | SDData !Builder deriving (Typeable)

instance Monoid SDData where
    mempty = SDDataEmpty
    mappend SDDataEmpty sy = sy
    mappend (SDData x)  sy = SDData $ mappend x $ case sy of
                                                   SDDataEmpty -> mempty
                                                   SDData y    -> y

runSDData :: SDData -> Builder
runSDData sx = case sx of
                 SDDataEmpty -> Bu.char8 '-'
                 SDData x    -> x

-- | Must contain from 1 to 32 printing ASCII characters,  except '='
--   ']', and '"'
type SDName    = ByteString

-- | An abstract representation of the syntax of a structured parameter.
data SDParams = SDParamsEmpty
              | SDParams !Builder
                deriving (Typeable)

instance Monoid SDParams where
    mempty = SDParamsEmpty
    mappend SDParamsEmpty sy = sy
    mappend (SDParams x)  sy = SDParams (mappend x (runSDParams sy))

newtype Msg = Msg { runMsg :: Builder } deriving (Typeable)

runSDParams :: SDParams -> Builder
runSDParams sx = case sx of
                   SDParamsEmpty -> mempty
                   (SDParams x)  -> Bu.char8 ' ' <> x

checkSDName :: SDName -> Maybe Builder
checkSDName str_ = let str = B.take 32 str_
                    in if B.all validChar str
                       then checkNull str
                       else checkNull $! B.filter validChar str
   where
     validChar c = (34 < c && c < 127 && c /= 61 && c /= 93) || c == 33
     checkNull str =
         if B.null str
         then Just $! Bu.byteString str
         else Nothing

escapeSDParamText :: Text -> Builder
escapeSDParamText = TE.encodeUtf8BuilderEscaped escapeChars
   where
     escapeChars =
         BP.condB
             (\c -> c == 34 || c == 92 || c == 93)
             ((,) '\\' >$< BP.liftFixedToBounded (BP.char8 >*< BP.word8))
             (BP.liftFixedToBounded BP.word8)

bom :: BP.FixedPrim a
bom = const (0xEF, (0xBB, 0xBF)) >$< (BP.word8 >*< BP.word8 >*< BP.word8)

checkHeaderSyntax :: Int -> ByteString -> Builder
checkHeaderSyntax n str_ =
    let str = B.take n str_
     in if B.all validChar str
        then checkNull str
        else checkNull $! B.filter validChar str
  where
    validChar c = 32 < c && c < 127
    checkNull str =
         if B.null str
         then Bu.char8 '-'
         else Bu.byteString str
