-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.S3Bucket
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Bucket interface for Amazon S3
-- API Version 2006-03-01
-- <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/>
-----------------------------------------------------------------------------

module Network.AWS.S3Bucket (
               -- * Function Types
               createBucket, createBucketWithPrefix, deleteBucket, listBuckets, listObjects,
               -- * Data Types
               Bucket(Bucket, bucket_name, bucket_creation_date),
               ListRequest(..),
               ListResult(..)
              ) where

import Network.AWS.Authentication as Auth
import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.AWS.ArrowUtils

import Network.HTTP as HTTP
import Network.URI as URI
import Network.Stream

import Data.Char (toLower)

import Text.XML.HXT.Arrow
import Control.Arrow

import Control.Monad
import System.Random (randomIO)
import Codec.Utils
import Data.Digest.MD5
import Codec.Text.Raw

data Bucket = Bucket { bucket_name :: String,
                       bucket_creation_date :: String
                     } deriving (Show, Eq)

-- | Create a new bucket on S3 with the given prefix, and a random
--   suffix.  This can be used to programatically create buckets
--   without of naming conflicts.
createBucketWithPrefix :: AWSConnection -- ^ AWS connection information
                       -> String -- ^ Bucket name prefix
                       -> IO (AWSResult String) -- ^ Server response, if
                                                --   successful, the bucket
                                                --   name is returned.

createBucketWithPrefix aws pre =
    do suffix <- randomName
       let name = pre ++ "-" ++ suffix
       res <- createBucket aws name
       either (\x -> case x of
                       AWSError c m -> createBucketWithPrefix aws pre
                       otherwise -> return (Left x))
                  (\x -> return (Right name)) res

randomName :: IO String
randomName =
    do rdata <- randomIO
       return $ take 10 $ show $ hexdumpBy "" 999  (hash (toOctets 10 (abs rdata)))

-- | Create a new bucket on S3 with the given name.
createBucket :: AWSConnection -- ^ AWS connection information
             -> String -- ^ Proposed bucket name
             -> IO (AWSResult ()) -- ^ Server response
createBucket aws bucket =
    do res <- Auth.runAction (S3Action aws bucket "" "" [] "" PUT)
       -- throw away the server response, return () on success
       return (either (Left) (\x -> Right ()) res)

-- | Delete a bucket with the given name on S3.  The bucket must be
--   empty for deletion to succeed.
deleteBucket :: AWSConnection -- ^ AWS connection information
             -> String -- ^ Bucket name to delete
             -> IO (AWSResult ()) -- ^ Server response
deleteBucket aws bucket =
    do res <- Auth.runAction (S3Action aws bucket "" "" [] "" DELETE)
       return (either (Left) (\x -> Right ()) res)

-- | Return a list of all bucket names and creation dates.  S3
--   allows a maximum of 100 buckets per user.
listBuckets :: AWSConnection -- ^ AWS connection information
           -> IO (AWSResult [Bucket]) -- ^ Server response
listBuckets aws =
    do res <- Auth.runAction (S3Action aws "" "" "" [] "" GET)
       case res of
         Left x -> do return (Left x)
         Right y -> do bs <- parseBucketListXML (rspBody y)
                       return (Right bs)

parseBucketListXML :: String -> IO [Bucket]
parseBucketListXML x = runX (readString [(a_validate,v_0)] x >>> processBuckets)

processBuckets = deep (isElem >>> hasName "Bucket") >>>
                 split >>> first (text <<< atTag "Name") >>>
                 second (text <<< atTag "CreationDate") >>>
                 unsplit (\x y -> Bucket x y)

-- | List request parameters
data ListRequest =
    ListRequest { prefix :: String,
                  marker :: String,
                  delimiter :: String,
                  max_keys :: Int
                }

instance Show ListRequest where
    show x = "prefix=" ++ (urlEncode (prefix x)) ++ "&" ++
             "marker=" ++ (urlEncode (marker x)) ++ "&" ++
             "delimiter=" ++ (urlEncode (delimiter x)) ++ "&" ++
             "max-keys=" ++ (show (max_keys x))

-- | Result from listing objects.
data ListResult =
    ListResult {
      key :: String, -- ^ Name of object
      last_modified :: String, -- ^ Last modification date
      etag :: String, -- ^ MD5
      size :: Integer -- ^ Bytes of object data
    } deriving (Show)

-- | List objects in a bucket, based on parameters from 'ListRequest'.  See
--   the Amazon S3 developer resources for in depth explanation of how
--   the fields in 'ListRequest' can be used to query for objects.
--   <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/ListingKeysRequest.html>
listObjects :: AWSConnection -- ^ AWS connection information
            -> String -- ^ Bucket name to search
            -> ListRequest -- ^ List parameters
            -> IO (AWSResult [ListResult]) -- ^ Server response
listObjects aws bucket lp =
    do res <- Auth.runAction (S3Action aws bucket ""
                                           ("?" ++ (show lp)) [] "" GET)
       case res of
         Left x -> do return (Left x)
         Right y -> do let objs = rspBody y
                       tr <- isListTruncated objs
                       lr <- getListResults objs
                       case tr of
                         True -> do let last_result = (key . head . reverse) lr
                                    next_set <- listObjects aws bucket
                                                (lp {marker = last_result,
                                                              max_keys = (max_keys lp) - (length lr)})
                                    either (\x -> return (Left x))
                                           (\x -> return (Right (lr ++ x))) next_set
                         False -> do return (Right lr)

-- | Determine if ListBucketResult is truncated.  It would make sense
--   to combine this with the query for list results, so we didn't
--   have to parse the XML twice.
isListTruncated :: String -> IO Bool
isListTruncated s =
    do results <- runX (readString [(a_validate,v_0)] s >>> processTruncation)
       return $ case results of
                  [] -> False
                  x:xs -> x

processTruncation = (text <<< atTag "IsTruncated")
                    >>> arr (\x -> case (map toLower x) of
                                     "true" -> True
                                     "false" -> False
                                     otherwise -> False)


getListResults :: String -> IO [ListResult]
getListResults s = runX (readString [(a_validate,v_0)] s >>> processListResults)

-- | Learning arrows on the job.
processListResults = deep (isElem >>> hasName "Contents") >>>
                     ((text <<< atTag "Key") &&&
                      (text <<< atTag "LastModified") &&&
                      (text <<< atTag "ETag") &&&
                      (text <<< atTag "Size")) >>>
                     arr (\(a,(b,(c,d))) -> ListResult a b (unquote . HTTP.urlDecode $ c) (read d))

-- | Remove quote characters from a 'String'.
unquote :: String -> String
unquote = filter (/= '"')
