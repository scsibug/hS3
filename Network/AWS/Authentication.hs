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
import Network.HTTP as HTTP
import Network.URI as URI

import Data.HMAC
import Codec.Binary.Base64 (encode, decode)
import Codec.Utils (Octet)

import Data.Char (isSpace, intToDigit, digitToInt, ord, chr, toLower)
import Data.Bits ((.&.))
import qualified Codec.Binary.UTF8.String as US
import Codec.Utils (Octet)

import Data.List (sortBy, groupBy, intersperse)
import Data.Maybe

import System.Time
import System.Locale

import Text.Regex

import Control.Arrow
import Text.XML.HXT.Arrow

-- | An action to be performed using S3.
data S3Action =
    S3Action {
      -- | Connection and authentication information
      s3conn :: AWSConnection,
      -- | Name of bucket to act on
      s3bucket :: String,
      -- | Name of object to act on
      s3object :: String,
      -- | Query parameters (requires a prefix of @?@)
      s3query :: String,
      -- | Additional header fields to send
      s3metadata :: [(String, String)],
      -- | Body of action, if sending data
      s3body :: String,
      -- | Type of action, 'PUT', 'GET', etc.
      s3operation :: RequestMethod
    } deriving (Show)

-- | Transform an 'S3Action' into an HTTP request.  Does not add
--   authentication or date information, so it is not suitable for
--   sending directly to AWS.
requestFromAction :: S3Action -- ^ Action to transform
                  -> HTTP.Request -- ^ Action represented as an HTTP Request.
requestFromAction a =
    Request { rqURI = URI { uriScheme = "",
                            uriAuthority = Nothing,
                            uriPath = canonicalizeResource a,
                            uriQuery = s3query a,
                            uriFragment = "" },
              rqMethod = s3operation a,
              rqHeaders = [Header HdrHost (awsHost (s3conn a))] ++
                          (headersFromAction a),
              rqBody = s3body a
            }
    where path = (s3bucket a) ++ "/" ++ (s3object a)

-- | Create 'Header' objects from an action.
headersFromAction :: S3Action
                  -> [Header]
headersFromAction a = map (\(k,v) -> case k of
                                       "Content-Type" -> Header HdrContentType v
                                       otherwise -> (Header (HdrCustom k)) (mimeEncodeQP v))
                                            (s3metadata a)

-- | Inspect HTTP body, and add a @Content-Length@ header with the
--   correct length.
addContentLengthHeader :: HTTP.Request -> HTTP.Request
addContentLengthHeader req = insertHeader HdrContentLength (show (length (rqBody req))) req

-- | Add AWS authentication header to an HTTP request.
addAuthenticationHeader :: S3Action     -- ^ Action with authentication data
                        -> HTTP.Request -- ^ Request to transform
                        -> HTTP.Request -- ^ Authenticated request
addAuthenticationHeader act req = insertHeader HdrAuthorization auth_string req
    where auth_string = "AWS " ++ (awsAccessKey conn) ++ ":" ++ signature
          signature = makeSignature conn (stringToSign act req)
          conn = s3conn act

-- | Sign a string using the given authentication data
makeSignature :: AWSConnection -- ^ Action with authentication data
              -> String -- ^ String to sign
              -> String -- ^ Base-64 encoded signature
makeSignature c s =
    encode (hmac_sha1 keyOctets msgOctets)
        where keyOctets = string2words (awsSecretKey c)
              msgOctets = string2words s

-- | Generate text that will be signed and subsequently added to the
--   request.
stringToSign :: S3Action -> HTTP.Request -> String
stringToSign a r =
    (canonicalizeHeaders r) ++
    (canonicalizeAmzHeaders r) ++
    (canonicalizeResource a)

-- | Extract header data needed for signing.
canonicalizeHeaders :: HTTP.Request -> String
canonicalizeHeaders r =
    http_verb ++ "\n" ++
    hdr_content_md5 ++ "\n" ++
    hdr_content_type ++ "\n" ++
    dateOrExpiration ++ "\n"
        where http_verb = show (rqMethod r)
              hdr_content_md5 = get_header HdrContentMD5
              hdr_date = get_header HdrDate
              hdr_content_type = get_header HdrContentType
              get_header h = maybe "" id (findHeader h r)
              dateOrExpiration = maybe hdr_date id (findHeader HdrExpires r)

-- | Extract @x-amz-*@ headers needed for signing.
--   find all headers with type HdrCustom that begin with amzHeader
--   lowercase key names
--   sort lexigraphically by key name
--   combine headers with same name
--   unfold multi-line headers
--   trim whitespace around the header
canonicalizeAmzHeaders :: HTTP.Request -> String
canonicalizeAmzHeaders r =
    let amzHeaders = filter isAmzHeader (rqHeaders r)
        amzHeaderKV = map headerToLCKeyValue amzHeaders
        sortedGroupedHeaders = groupHeaders (sortHeaders amzHeaderKV)
        uniqueHeaders = combineHeaders sortedGroupedHeaders
    in concat (map (\a -> a ++ "\n") (map showHeader uniqueHeaders))

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
combineHeaders h = map mergeSameHeaders h

-- | Headers with same name should have values merged.
mergeSameHeaders :: [(String, String)] -> (String, String)
mergeSameHeaders h@(x:xs) = let values = map snd h
                     in ((fst x), (concat $ intersperse "," values))

-- | Group headers with the same name.
groupHeaders :: [(String, String)] -> [[(String, String)]]
groupHeaders = groupBy (\a b -> (fst a) == (fst b))

-- | Sort by key name.
sortHeaders :: [(String, String)] -> [(String, String)]
sortHeaders = sortBy (\a b -> (fst a) `compare` (fst b))

-- | Make 'Header' easier to work with, and lowercase keys.
headerToLCKeyValue :: Header -> (String, String)
headerToLCKeyValue (Header k v) = (map toLower (show k), v)

-- | Determine if a header belongs in the StringToSign
isAmzHeader :: Header -> Bool
isAmzHeader h =
    case h of
      Header (HdrCustom k) v -> isPrefix amzHeader k
      otherwise -> False

-- | is the first list a prefix of the second?
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix a b = a == take (length a) b

-- | Prefix used by Amazon metadata headers
amzHeader = "x-amz-"

-- | Extract resource name, as required for signing.
canonicalizeResource :: S3Action -> String
canonicalizeResource a = bucket ++ uri
    where uri = case (s3object a) of
                  "" -> ""
                  otherwise -> "/" ++ (s3object a)
          bucket = case (s3bucket a) of
                     b@(x:xs) -> "/" ++ b
                     otherwise ->  case uri of
                                     [] -> "/" -- at the least we have a "/"
                                     otherwise -> ""
-- | Add a date string to a request.
addDateToReq :: HTTP.Request -- ^ Request to modify
             -> String       -- ^ Date string, in RFC 2616 format
             -> HTTP.Request -- ^ Request with date header added
addDateToReq r d = r {HTTP.rqHeaders =
                          (HTTP.Header HTTP.HdrDate d) : (HTTP.rqHeaders r)}

-- | Add an expiration date to a request
addExpirationToReq :: HTTP.Request -> String -> HTTP.Request
addExpirationToReq r e = addHeaderToReq r (HTTP.Header HTTP.HdrExpires e)

addHeaderToReq :: HTTP.Request -> Header -> HTTP.Request
addHeaderToReq r h = r {HTTP.rqHeaders = h : (HTTP.rqHeaders r)}

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
runAction :: S3Action -> IO (AWSResult Response)
runAction a = do c <- openTCPPort (awsHost (s3conn a)) (awsPort (s3conn a))
                 cd <- httpCurrentDate
                 let aReq = addAuthenticationHeader a $
                            addContentLengthHeader $
                            addDateToReq (requestFromAction a) cd
                 result <- (simpleHTTP_ c aReq)
                 createAWSResult result

-- | Construct a pre-signed URI, but don't act on it.  This is useful
--   for when an expiration date has been set, and the URI needs to be
--   passed on to a client.
preSignedURI :: S3Action -- ^ Action with resource
             -> Integer -- ^ Expiration time, in seconds since
                        --   00:00:00 UTC on January 1, 1970
             -> URI -- ^ URI of resource
preSignedURI a e =
    let c = (s3conn a)
        server = (awsHost c)
        port = (show (awsPort c))
        accessKeyQuery = "AWSAccessKeyId=" ++ (awsAccessKey c)
        secretKey = (awsSecretKey c)
        beginQuery = case (s3query a) of
                  "" -> "?"
                  x -> x ++ "&"
        expireQuery = "Expires=" ++ (show e)
        toSign = "GET\n\n\n" ++ (show e) ++ "\n/" ++ (s3bucket a) ++ "/" ++ (s3object a)
        sigQuery = "Signature=" ++ (urlEncode (makeSignature c toSign))
        query = beginQuery ++ accessKeyQuery ++ "&" ++
                expireQuery ++ "&" ++ sigQuery
    in URI { uriScheme = "http:",
             uriAuthority = Just (URIAuth "" server (":" ++ port)),
             uriPath = "/" ++ (s3bucket a) ++ "/" ++ (s3object a),
             uriQuery = query,
             uriFragment = ""
           }


-- | Inspect a response for network errors, HTTP error codes, and
--   Amazon error messages.
createAWSResult :: Result Response -> IO (AWSResult Response)
createAWSResult b = either (handleError) (handleSuccess) b
    where handleError x = return (Left (NetworkError x))
          handleSuccess x = case (rspCode x) of
                              (2,y,z) -> return (Right x)
                              (4,0,4) -> return (Left $ AWSError "NotFound" "404 Not Found")  -- no body, so no XML to parse
                              otherwise -> do err <- parseRestErrorXML (rspBody x)
                                              return (Left err)

-- | Find the errors embedded in an XML message body from Amazon.
parseRestErrorXML :: String -> IO ReqError
parseRestErrorXML x =
    do e <- runX (readString [(a_validate,v_0)] x
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

-- | Decode a mime string, we only know about quoted printable UTF-8
mimeDecode a =
    if isPrefix utf8qp a
        -- =?UTF-8?Q?....?= -> decoded UTF-8 string
    then US.decodeString $ mimeDecode' $ reverse $ drop 2 $ reverse $ drop (length utf8qp) a
    else a
    where
      utf8qp = "=?UTF-8?Q?"

mimeDecode' :: String -> String
mimeDecode' ('=':a:b:rest) =
    chr (16 * digitToInt a + digitToInt b)
            : mimeDecode' rest
mimeDecode' (h:t) = h : mimeDecode' t
mimeDecode' [] = []

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
