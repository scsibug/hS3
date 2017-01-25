-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.Authentication
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Implements authentication and low-level communication with Amazon
-- Web Services, such as S3, EC2, and others.
-- API Version 2006-03-01
-- <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/>
-----------------------------------------------------------------------------

module Network.AWS.Authentication (
   -- * Function Types
   runAction, isAmzHeader, preSignedURI,
   -- * Data Types
   S3Action(..),
   -- * Misc functions
   mimeEncodeQP, mimeDecode
   ) where

import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.AWS.ArrowUtils
import Network.HTTP as HTTP hiding (simpleHTTP_)
import Network.HTTP.HandleStream (simpleHTTP_)
import Network.Stream (Result)
import Network.URI as URI
import qualified Data.ByteString.Lazy.Char8 as L

import Data.ByteString.Char8 (pack, unpack)

import Data.HMAC
import Codec.Binary.Base64 (encode, decode)
import Codec.Utils (Octet)

import Data.Char (intToDigit, digitToInt, ord, chr, toLower)
import Data.Bits ((.&.))
import qualified Codec.Binary.UTF8.String as US

import Data.List (sortBy, groupBy, intersperse, isInfixOf)
import Data.Maybe

import System.Time
import System.Locale

import Text.Regex

import Control.Arrow
import Control.Arrow.ArrowTree
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlOptions
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.ReadDocument

rightToMaybe = either (const Nothing) Just
encodeBS = unpack . encode . pack
decodeBS = rightToMaybe . unpack . decode . pack

-- | An action to be performed using S3.
data S3Action =
    S3Action {
      -- | Connection and authentication information
      s3conn :: AWSConnection,
      -- | Name of bucket to act on (URL encoded)
      s3bucket :: String,
      -- | Name of object to act on (URL encoded)
      s3object :: String,
      -- | Query parameters (requires a prefix of @?@)
      s3query :: String,
      -- | Additional header fields to send
      s3metadata :: [(String, String)],
      -- | Body of action, if sending data
      s3body :: L.ByteString,
      -- | Type of action, 'PUT', 'GET', etc.
      s3operation :: RequestMethod
    } deriving (Show)

-- | Transform an 'S3Action' into an HTTP request.  Does not add
--   authentication or date information, so it is not suitable for
--   sending directly to AWS.
requestFromAction :: S3Action -- ^ Action to transform
                  -> HTTP.HTTPRequest L.ByteString -- ^ Action represented as an HTTP Request.
requestFromAction a =
    Request { rqURI = URI { uriScheme = "",
                            uriAuthority = Nothing,
                            uriPath = qpath,
                            uriQuery = s3query a,
                            uriFragment = "" },
              rqMethod = s3operation a,
              rqHeaders = Header HdrHost (s3Hostname a) :
                          headersFromAction a,
              rqBody = (s3body a)
            }
    where qpath = '/' : s3object a

-- | Create 'Header' objects from an action.
headersFromAction :: S3Action
                  -> [Header]
headersFromAction = map (\(k,v) -> case k of
                                    "Content-Type" -> Header HdrContentType v
                                    "Content-Length" -> Header HdrContentLength v
                                    "Content-MD5" -> Header HdrContentMD5 v
                                    otherwise -> Header (HdrCustom k) (mimeEncodeQP v))
                    . s3metadata

-- | Inspect HTTP body, and add a @Content-Length@ header with the
--   correct length, if it does not already exist.
addContentLengthHeader :: HTTP.HTTPRequest L.ByteString -> HTTP.HTTPRequest L.ByteString
addContentLengthHeader req = insertHeaderIfMissing HdrContentLength conlength req
    where conlength = show (L.length (rqBody req))

-- | Add AWS authentication header to an HTTP request.
addAuthenticationHeader :: S3Action     -- ^ Action with authentication data
                        -> HTTP.HTTPRequest L.ByteString -- ^ Request to transform
                        -> HTTP.HTTPRequest L.ByteString -- ^ Authenticated request
addAuthenticationHeader act req = insertHeader HdrAuthorization auth_string req
    where auth_string = "AWS " ++ awsAccessKey conn ++ ":" ++ signature
          signature = (makeSignature conn (stringToSign act req))
          conn = s3conn act

-- | Sign a string using the given authentication data
makeSignature :: AWSConnection -- ^ Action with authentication data
              -> String -- ^ String to sign
              -> String -- ^ Base-64 encoded signature
makeSignature c s =
        encodeBS (hmac_sha1 keyOctets msgOctets)
        where keyOctets = string2words (awsSecretKey c)
              msgOctets = string2words s

-- | Generate text that will be signed and subsequently added to the
--   request.
stringToSign :: S3Action -> HTTP.HTTPRequest L.ByteString -> String
stringToSign a r =
    canonicalizeHeaders r ++
    canonicalizeAmzHeaders r ++
    canonicalizeResource a

-- | Extract header data needed for signing.
canonicalizeHeaders :: HTTP.HTTPRequest L.ByteString -> String
canonicalizeHeaders r =
    http_verb ++ "\n" ++
    hdr_content_md5 ++ "\n" ++
    hdr_content_type ++ "\n" ++
    dateOrExpiration ++ "\n"
        where http_verb = show (rqMethod r)
              hdr_content_md5 = get_header HdrContentMD5
              hdr_date = get_header HdrDate
              hdr_content_type = get_header HdrContentType
              get_header h = fromMaybe "" (findHeader h r)
              dateOrExpiration = fromMaybe hdr_date (findHeader HdrExpires r)

-- | Extract @x-amz-*@ headers needed for signing.
--   find all headers with type HdrCustom that begin with amzHeader
--   lowercase key names
--   sort lexigraphically by key name
--   combine headers with same name
--   unfold multi-line headers
--   trim whitespace around the header
canonicalizeAmzHeaders :: HTTP.HTTPRequest L.ByteString -> String
canonicalizeAmzHeaders r =
    let amzHeaders = filter isAmzHeader (rqHeaders r)
        amzHeaderKV = map headerToLCKeyValue amzHeaders
        sortedGroupedHeaders = groupHeaders (sortHeaders amzHeaderKV)
        uniqueHeaders = combineHeaders sortedGroupedHeaders
    in concatMap (\a -> a ++ "\n") (map showHeader uniqueHeaders)

-- | Give the string representation of a (key,value) header pair.
--   Uses rules for authenticated headers.
showHeader :: (String, String) -> String
showHeader (k,v) = k ++ ":" ++ removeLeadingTrailingWhitespace(fold_whitespace v)

-- | Replace CRLF followed by whitespace with a single space
fold_whitespace :: String -> String
fold_whitespace s = subRegex (mkRegex "\n\r( |\t)+") s " "

-- | strip leading/trailing whitespace
removeLeadingTrailingWhitespace :: String -> String
removeLeadingTrailingWhitespace s = subRegex (mkRegex "^\\s+") (subRegex (mkRegex "\\s+$") s "") ""

-- | Combine same-named headers.
combineHeaders :: [[(String, String)]] -> [(String, String)]
combineHeaders = map mergeSameHeaders

-- | Headers with same name should have values merged.
mergeSameHeaders :: [(String, String)] -> (String, String)
mergeSameHeaders h@(x:_) = let values = map snd h
                     in ((fst x), (concat $ intersperse "," values))

-- | Group headers with the same name.
groupHeaders :: [(String, String)] -> [[(String, String)]]
groupHeaders = groupBy (\a b -> fst a == fst b)

-- | Sort by key name.
sortHeaders :: [(String, String)] -> [(String, String)]
sortHeaders = sortBy (\a b -> fst a `compare` fst b)

-- | Make 'Header' easier to work with, and lowercase keys.
headerToLCKeyValue :: Header -> (String, String)
headerToLCKeyValue (Header k v) = (map toLower (show k), v)

-- | Determine if a header belongs in the StringToSign
isAmzHeader :: Header -> Bool
isAmzHeader h =
    case h of
      Header (HdrCustom k) _ -> isPrefix amzHeader k
      otherwise -> False

-- | is the first list a prefix of the second?
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix a b = a == take (length a) b

-- | Prefix used by Amazon metadata headers
amzHeader :: String
amzHeader = "x-amz-"

-- | Extract resource name, as required for signing.
canonicalizeResource :: S3Action -> String
canonicalizeResource a = bucket ++ uri ++ subresource
    where uri = '/' : s3object a
          bucket = case (s3bucket a) of
                     b@(_:_) -> '/' : map toLower b
                     otherwise -> ""
          subresource = case (subresource_match) of
                          [] -> ""
                          x:_ -> x
          subresource_match = filter (\sr -> isInfixOf sr (s3query a))
                              ["?versioning", "?torrent", "?logging", "?acl", "?location"]

-- | Add a date string to a request.
addDateToReq :: HTTP.HTTPRequest L.ByteString -- ^ Request to modify
             -> String       -- ^ Date string, in RFC 2616 format
             -> HTTP.HTTPRequest L.ByteString-- ^ Request with date header added
addDateToReq r d = r {HTTP.rqHeaders =
                          HTTP.Header HTTP.HdrDate d : HTTP.rqHeaders r}

-- | Add an expiration date to a request.
addExpirationToReq :: HTTP.HTTPRequest L.ByteString -> String -> HTTP.HTTPRequest L.ByteString
addExpirationToReq r = addHeaderToReq r . HTTP.Header HTTP.HdrExpires

-- | Attach an HTTP header to a request.
addHeaderToReq :: HTTP.HTTPRequest L.ByteString -> Header -> HTTP.HTTPRequest L.ByteString
addHeaderToReq r h = r {HTTP.rqHeaders = h : HTTP.rqHeaders r}

-- | Get hostname to connect to. Needed for european buckets
s3Hostname :: S3Action -> String
s3Hostname a =
    let s3host = awsHost (s3conn a) in
    case (s3bucket a) of
        b@(_:_) -> b ++ "." ++ s3host
        otherwise -> s3host

-- | Get current time in HTTP 1.1 format (RFC 2616)
--   Numeric time zones should be used, but I'd rather not subvert the
--   intent of ctTZName, so we stick with the name format.  Otherwise,
--   we could send @+0000@ instead of @GMT@.
--   see:
--   <http://www.ietf.org/rfc/rfc2616.txt>
--   <http://www.ietf.org/rfc/rfc1123.txt>
--   <http://www.ietf.org/rfc/rfc822.txt>
httpCurrentDate :: IO String
httpCurrentDate =
    do c <- getClockTime
       let utc_time = (toUTCTime c) {ctTZName = "GMT"}
       return $ formatCalendarTime defaultTimeLocale rfc822DateFormat utc_time

-- | Convenience for dealing with HMAC-SHA1
string2words :: String -> [Octet]
string2words = US.encode

-- | Construct the request specified by an S3Action, send to Amazon,
--   and return the response.  Todo: add MD5 signature.
runAction :: S3Action -> IO (AWSResult (HTTPResponse L.ByteString))
runAction a = runAction' a (s3Hostname a)

runAction' :: S3Action -> String -> IO (AWSResult (HTTPResponse L.ByteString))
runAction' a hostname = do
        c <- (openTCPConnection hostname (awsPort (s3conn a)))
--bufferOps = lazyBufferOp
        cd <- httpCurrentDate
        let aReq = addAuthenticationHeader a $
                   addContentLengthHeader $
                   addDateToReq (requestFromAction a) cd
        --print aReq -- Show request header
        result <- simpleHTTP_ c aReq
        -- Show result header and body
        --print result
        --case result of
        --  Left a -> print ""
        --  Right a -> print (rspBody a)
        close c
        createAWSResult a result

-- | Construct a pre-signed URI, but don't act on it.  This is useful
--   for when an expiration date has been set, and the URI needs to be
--   passed on to a client.
preSignedURI :: S3Action -- ^ Action with resource
             -> Integer -- ^ Expiration time, in seconds since
                        --   00:00:00 UTC on January 1, 1970
             -> URI -- ^ URI of resource
preSignedURI a e =
    let c = (s3conn a)
        srv = (awsHost c)
        pt = (show (awsPort c))
        accessKeyQuery = "AWSAccessKeyId=" ++ awsAccessKey c
        beginQuery = case (s3query a) of
                  "" -> "?"
                  x -> x ++ "&"
        expireQuery = "Expires=" ++ show e
        toSign = "GET\n\n\n" ++ show e ++ "\n/" ++ s3bucket a ++ "/" ++ s3object a
        sigQuery = "Signature=" ++ urlEncode (makeSignature c toSign)
        q = beginQuery ++ accessKeyQuery ++ "&" ++
                expireQuery ++ "&" ++ sigQuery
    in URI { uriScheme = "http:",
             uriAuthority = Just (URIAuth "" srv (':' : pt)),
             uriPath = "/" ++ s3bucket a ++ "/" ++ s3object a,
             uriQuery = q,
             uriFragment = ""
           }


-- | Inspect a response for network errors, HTTP error codes, and
--   Amazon error messages.
--   We need the original action in case we get a 307 (temporary redirect)
createAWSResult :: S3Action -> Result (HTTPResponse L.ByteString) -> IO (AWSResult (HTTPResponse L.ByteString))
createAWSResult a b = either handleError handleSuccess b
    where handleError = return . Left . NetworkError
          handleSuccess s = case (rspCode s) of
                              (2,_,_) -> return (Right s)
                              -- temporary redirect
                              (3,0,7) -> case (findHeader HdrLocation s) of
                                                Just l -> runAction' a (getHostname l)
                                                Nothing -> return (Left $ AWSError "Temporary Redirect" "Redirect without location header")  -- not good
                              (4,0,4) -> return (Left $ AWSError "NotFound" "404 Not Found")  -- no body, so no XML to parse
                              otherwise -> do e <- parseRestErrorXML (L.unpack (rspBody s))
                                              return (Left e)
          -- Get hostname part from http url.
          getHostname :: String -> String
          getHostname h = case parseURI h of
                             Just u -> case (uriAuthority u) of
                                           Just auth -> (uriRegName auth)
                                           Nothing -> ""
                             Nothing -> ""
-- | Find the errors embedded in an XML message body from Amazon.
parseRestErrorXML :: String -> IO ReqError
parseRestErrorXML x =
    do e <- runX (readString [withValidate no] x
                                 >>> processRestError)
       case e of
         [] -> return (AWSError "NoErrorInMsg"
                       ("HTTP Error condition, but message body"
                        ++ "did not contain error code."))
         x:xs -> return x


-- | Find children of @Error@ entity, use their @Code@ and @Message@
--   entities to create an 'AWSError'.
processRestError = deep (isElem >>> hasName "Error") >>>
                   split >>> first (text <<< atTag "Code") >>>
                   second (text <<< atTag "Message") >>>
                   unsplit (\x y -> AWSError x y)

--- mime header encoding
mimeEncodeQP, mimeDecode :: String -> String
-- | Decode a mime string, we know about quoted printable and base64 encoded UTF-8
-- S3 may convert quoted printable to base64
mimeDecode a
    | isPrefix utf8qp a =
        mimeDecodeQP $ encoded_payload utf8qp a
    | isPrefix utf8b64 a =
        mimeDecodeB64 $ encoded_payload utf8b64 a
    | otherwise =
        a
    where
      utf8qp  = "=?UTF-8?Q?"
      utf8b64 = "=?UTF-8?B?"
      -- '=?UTF-8?Q?foobar?=' -> 'foobar'
      encoded_payload prefix = reverse . drop 2 . reverse . drop (length prefix)

mimeDecodeQP :: String -> String
mimeDecodeQP =
    US.decodeString . mimeDecodeQP'

mimeDecodeQP' :: String -> String
mimeDecodeQP' ('=':a:b:rest) =
    chr (16 * digitToInt a + digitToInt b)
            : mimeDecodeQP' rest
mimeDecodeQP' (h:t) =h : mimeDecodeQP' t
mimeDecodeQP' [] = []

mimeDecodeB64 :: String -> String
mimeDecodeB64 s =
    case decodeBS s of
        Nothing -> ""
        Just a ->  US.decode a

 -- Encode a String into quoted printable, if needed.
 -- eq: =?UTF-8?Q?=aa?=
mimeEncodeQP s =
    if any reservedChar s
    then "=?UTF-8?Q?" ++ (mimeEncodeQP' $ US.encodeString s) ++ "?="
    else s

mimeEncodeQP' :: String -> String
mimeEncodeQP' [] = []
mimeEncodeQP' (h:t) =
    let str = if reservedChar h then escape h else [h]
    in str ++ mimeEncodeQP' t
    where
        escape x =
            let y = ord x in
            [ '=', intToDigit ((y `div` 16) .&. 0xf), intToDigit (y .&. 0xf) ]

-- Char needs escaping?
reservedChar :: Char -> Bool
reservedChar x
    -- from space (0x20) till '~' everything is fine. The rest are control chars, or high bit.
    | xi >= 0x20 && xi <= 0x7e = False
    | otherwise = True
    where xi = ord x
