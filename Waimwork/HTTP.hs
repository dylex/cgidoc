-- |Parsers and generators for HTTP header data.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Waimwork.HTTP
  ( encodePathSegments'
  , encodePath'
  , splitHTTP
  , quoteHTTP
  , unquoteHTTP
  , formatHTTPDate
  , parseHTTPDate
  , ETag(..)
  , ETags(..)
  , renderETag
  , etagParser
  , parseETag
  , renderETags
  , parseETags
  , strongETagEq
  , weakETagEq
  , matchETag
  ) where

import           Control.Applicative ((<**>), (<|>), optional)
import           Control.Monad (msum)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import           Data.Function (on)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Format (FormatTime, ParseTime, formatTime, parseTimeM, defaultTimeLocale)
import           Network.HTTP.Types (Query, encodePathSegments, renderQueryBuilder)

-- |Same as 'encodePathSegments', but generates absolute paths (empty results in a single slash)
encodePathSegments' :: [T.Text] -> BSB.Builder
encodePathSegments' [] = BSB.char8 '/'
encodePathSegments' p = encodePathSegments p

-- |Same as 'encodePath' but using 'encodePathSegments''
encodePath' :: [T.Text] -> Query -> BSB.Builder
encodePath' p [] = encodePathSegments' p
encodePath' p q = encodePathSegments' p <> renderQueryBuilder True q

ctls, separators :: String
ctls = "\0-\31\DEL"
separators = "()<>@,;:\\\"/[]?={} \t"

quotedString :: AP.Parser BS.ByteString
quotedString = APC.char '"' *> qdtext <* APC.char '"' where
  qdtext = APC.takeTill (\c -> c == '"' || c == '\\')
    <**> AP.option id (flip BS.append <$> qpair)
  qpair = BS.cons <$> (APC.char '\\' *> AP.anyWord8) <*> qdtext

token :: AP.Parser BS.ByteString
token = AP.takeWhile1 $ AP.notInClass $ separators ++ ctls 

element :: AP.Parser BS.ByteString
element = quotedString <|> token

list :: AP.Parser a -> AP.Parser [a]
list p = catMaybes <$> AP.sepBy (APC.skipSpace *> optional p <* APC.skipSpace) (APC.char ',')

-- |Parse a comma-separated list of quoted string or unquoted tokens
splitHTTP :: BS.ByteString -> [BS.ByteString]
splitHTTP = either (const []) id . AP.parseOnly (list element <* AP.endOfInput)

-- |Attempt to parse a quoted string, otherwise just take the input as-is.
unquoteHTTP :: BS.ByteString -> BS.ByteString
unquoteHTTP s = either (const s) id $ AP.parseOnly (APC.skipSpace *> element <* APC.skipSpace <* AP.endOfInput) s

-- |Quote a string.
quoteHTTP :: BS.ByteString -> BS.ByteString
quoteHTTP = BSC.pack . ('"':) . quote . BSC.unpack where
  quote "" = "\""
  quote ('\\':r) = '\\':'\\':quote r
  quote ('"':r) = '\\':'"':quote r
  quote (c:r)
    | APC.inClass ctls c = '\\':c:quote r
    | otherwise = c:quote r

dateFmts :: [String]
-- rfc1123Date, rfc850Date, asctimeDate
dateFmts = ["%a, %d %b %Y %T GMT", "%A, %d-%b-%y %T GMT", "%a %b %e %T %Y"]

defaultDateFmt :: String
defaultDateFmt = head dateFmts

-- |Render an HTTP date string in rfc1128 format.
formatHTTPDate :: FormatTime t => t -> BS.ByteString
formatHTTPDate = BSC.pack . formatTime defaultTimeLocale defaultDateFmt

-- |Parse an HTTP date string in any valid format: rfc1123, rfc850, asctime
parseHTTPDate :: ParseTime t => BS.ByteString -> Maybe t
parseHTTPDate b = msum $ map (\f -> parseTimeM True defaultTimeLocale f s) dateFmts where s = BSC.unpack b

-- |An HTTP entity tag
data ETag
  = WeakETag{ eTag :: !BS.ByteString } -- ^@W/...@
  | StrongETag{ eTag :: !BS.ByteString }
  deriving (Eq)

instance Show ETag where
  show = BSC.unpack . renderETag

data ETags
  = ETags [ETag]
  | AnyETag -- ^@*@

instance Monoid ETags where
  mempty = ETags []
  mappend AnyETag _ = AnyETag
  mappend _ AnyETag = AnyETag
  mappend (ETags a) (ETags b) = ETags (mappend a b)

instance Show ETags where
  show = BSC.unpack . renderETags

etag :: AP.Parser ETag
etag = AP.option StrongETag (WeakETag <$ AP.string "W/") <*> quotedString

etagParser :: AP.Parser ETag
etagParser = etag

parseETag :: BS.ByteString -> Either String ETag
parseETag = AP.parseOnly (etag <* AP.endOfInput)

renderETag :: ETag -> BS.ByteString
renderETag (WeakETag t) = "W/" <> quoteHTTP t
renderETag (StrongETag t) = quoteHTTP t

parseETags :: BS.ByteString -> ETags
parseETags = either (const $ ETags []) id . AP.parseOnly
  (APC.skipSpace *> (AnyETag <$ APC.char '*' <|> ETags <$> list etag) <* APC.skipSpace <* AP.endOfInput)

renderETags :: ETags -> BS.ByteString
renderETags (ETags l) = BS.intercalate "," $ map renderETag l
renderETags AnyETag = "*"

-- |The strong etag comparison function.
strongETagEq :: ETag -> ETag -> Bool
strongETagEq (StrongETag x) (StrongETag y) = x == y
strongETagEq _ _ = False

-- |The weak etag comparison function.
weakETagEq :: ETag -> ETag -> Bool
weakETagEq = on (==) eTag

-- |Test an ETag against a list.  Should be used as @'matchETag' . 'strongETagEq'@ or @'matchETag' . 'weakETagEq'@.
matchETag :: (ETag -> Bool) -> ETags -> Bool
matchETag _ AnyETag = True
matchETag e (ETags l) = any e l
