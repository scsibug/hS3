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
   runAction, isAmzHeader,
   -- * Data Types
   S3Action(..)
   ) where

import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.AWS.ArrowUtils
import Network.HTTP as HTTP
import Network.URI as URI

import Data.HMAC
import Codec.Binary.Base64 (encode, decode)
import Codec.Utils (Octet)

import Data.Char (ord, toLower)
import Data.List (sortBy, groupBy, intersperse)
import Data.Maybe

import System.Time
import System.Locale

import Text.Regex

import Control.Arrow
import Text.XML.HXT.Arrow

-- | An action to be performed using S3.
data S3Action =
    S3Action { s3conn :: AWSConnection, -- ^ Connection and authentication
                                        --   information
               s3bucket :: String, -- ^ Name of bucket to act on
               s3object :: String, -- ^ Name of object to act on
               s3query :: String, -- ^ Query parameters, requires a prefix of @?@
               s3metadata :: [(String, String)], -- ^ Additional header fields to send
               s3body :: String, -- ^ Body of action, if sending data
               s3operation :: RequestMethod -- ^ Type of action, 'PUT', 'GET', etc.
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
headersFromAction a = map (\(k,v) -> (Header (HdrCustom k)) v)
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
          signature = encode (hmac_sha1 keyOctets msgOctets)
          keyOctets = string2words (awsSecretKey conn)
          msgOctets = string2words (stringToSign act req)
          conn = s3conn act

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
    hdr_date ++ "\n"
        where http_verb = show (rqMethod r)
              hdr_content_md5 = get_header HdrContentMD5
              hdr_date = get_header HdrDate
              hdr_content_type = get_header HdrContentType
              get_header h = maybe "" id (findHeader h r)

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
showHeader (k,v) = k ++ ":" ++ removeLeadingWhitespace(fold_whitespace v)

-- | Replace CRLF followed by whitespace with a single space
fold_whitespace :: String -> String
fold_whitespace s = subRegex (mkRegex "\n\r( |\t)+") s " "

-- | strip leading tabs/spaces
removeLeadingWhitespace :: String -> String
removeLeadingWhitespace s = subRegex (mkRegex "^( |\t)+") s ""

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
string2words = map (fromIntegral . ord)


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

-- | Inspect a response for network errors, HTTP error codes, and
--   Amazon error messages.
createAWSResult :: Result Response -> IO (AWSResult Response)
createAWSResult b = either (handleError) (handleSuccess) b
    where handleError x = return (Left (NetworkError x))
          handleSuccess x = case (rspCode x) of
                              (2,y,z) -> return (Right x)
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


