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
               createBucketIn, createBucket, createBucketWithPrefixIn,
               createBucketWithPrefix, deleteBucket, getBucketLocation,
               emptyBucket, listBuckets, listObjects, listAllObjects,
               isBucketNameValid, getObjectStorageClass,
               getVersioningConfiguration, setVersioningConfiguration,
               -- * Data Types
               S3Bucket(S3Bucket, bucket_name, bucket_creation_date),
               ListRequest(..),
               ListResult(..),
               IsTruncated,
               VersioningConfiguration(..),
               VersioningStatus(..)
              ) where

import Network.AWS.Authentication as Auth
import Network.AWS.AWSResult
import Network.AWS.S3Object
import Network.AWS.AWSConnection
import Network.AWS.ArrowUtils

import Network.HTTP as HTTP
import Network.Stream()

import qualified Data.ByteString.Lazy.Char8 as L

import Data.Char (toLower, isAlphaNum)
import Data.List (isInfixOf)

import qualified Data.Tree.NTree.TypeDefs

import Control.Monad
import System.Random (randomIO)
import Codec.Utils
import Data.Digest.MD5
import Codec.Text.Raw

import Control.Arrow
import Control.Arrow.ArrowTree
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlOptions
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.DOM.TypeDefs

data S3Bucket = S3Bucket { bucket_name :: String,
                           bucket_creation_date :: String
                         } deriving (Show, Eq)

data VersioningConfiguration = VersioningConfiguration {
      versioningStatus :: VersioningStatus,
      mfaDeleteEnabled :: Bool
}
  deriving (Read, Show, Eq)

data VersioningStatus = VersioningDisabled | VersioningEnabled | VersioningSuspended
                        deriving (Read, Show, Eq)

-- | Create a new bucket on S3 with the given prefix, and a random
--   suffix.  This can be used to programatically create buckets
--   without of naming conflicts.
createBucketWithPrefixIn :: AWSConnection -- ^ AWS connection information
                       -> String -- ^ Bucket name prefix
                       -> String -- ^ Location ("US", "EU", "us-west-1", "ap-southeast-1")
                       -> IO (AWSResult String) -- ^ Server response, if
                                                --   successful, the bucket
                                                --   name is returned.
createBucketWithPrefixIn aws pre location =
    do suffix <- randomName
       let name = pre ++ "-" ++ suffix
       res <- createBucketIn aws name location
       either (\x -> case x of
                       AWSError _ _ -> createBucketWithPrefixIn aws pre location
                       otherwise -> return (Left x))
                  (\_ -> return (Right name)) res

-- | see createBucketWithPrefixIn, but hardcoded for the US
createBucketWithPrefix :: AWSConnection -- ^ AWS connection information
                       -> String -- ^ Bucket name prefix
                       -> IO (AWSResult String) -- ^ Server response, with bucket name
createBucketWithPrefix aws pre =
    createBucketWithPrefixIn aws pre "US"

randomName :: IO String
randomName =
    do rdata <- randomIO :: IO Integer
       return $ take 10 $ show $ hexdumpBy "" 999
                  (hash (toOctets (10::Integer) (abs rdata)))

-- | Create a new bucket on S3 with the given name.
createBucketIn :: AWSConnection -- ^ AWS connection information
             -> String -- ^ Proposed bucket name
             -> String -- ^ Location ("US", "EU", "us-west-1", "ap-southeast-1")
             -> IO (AWSResult ()) -- ^ Server response
createBucketIn aws bucket location =
    let constraint = if location == "US"
                        then "" -- US == no body
                        else "<CreateBucketConfiguration><LocationConstraint>" ++ location ++ "</LocationConstraint></CreateBucketConfiguration>"
    in
    do res <- Auth.runAction (S3Action aws bucket "" "" [] (L.pack constraint) PUT Nothing)
       -- throw away the server response, return () on success
       return (either Left (\_ -> Right ()) res)

-- | Create a new bucket on S3 with the given name.
createBucket :: AWSConnection -- ^ AWS connection information
             -> String -- ^ Proposed bucket name
             -> IO (AWSResult ()) -- ^ Server response
createBucket aws bucket =
    createBucketIn aws bucket "US"

-- | Physical location of the bucket. "US" or "EU"
getBucketLocation :: AWSConnection  -- ^ AWS connection information
                  -> String  -- ^ Bucket name
                  -> IO (AWSResult String) -- ^ Server response ("US", "EU", "us-west-1", "ap-southeast-1", etc.)
getBucketLocation aws bucket =
    do res <- Auth.runAction (S3Action aws bucket "?location" "" [] L.empty GET Nothing)
       case res of
         Left x -> return (Left x)
         Right y -> do bs <- parseBucketLocationXML (L.unpack (rspBody y))
                       return (Right bs)

parseBucketLocationXML :: String -> IO String
parseBucketLocationXML s =
    do results <- runX (readString [withValidate no] s >>> processLocation)
       return $ case results of
                  [] -> "US"    -- not specified by S3, but they are in the US
                  x:_ -> x

processLocation :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) String
processLocation = (text <<< atTag "LocationConstraint")
                    >>> arr id

-- | Delete a bucket with the given name on S3.  The bucket must be
--   empty for deletion to succeed.
deleteBucket :: AWSConnection -- ^ AWS connection information
             -> String -- ^ Bucket name to delete
             -> IO (AWSResult ()) -- ^ Server response
deleteBucket aws bucket =
    do res <- Auth.runAction (S3Action aws bucket "" "" [] L.empty DELETE Nothing)
       return (either Left (\_ -> Right ()) res)

-- | Empty a bucket of all objects.  Iterates through all objects
--   issuing delete commands, so time is proportional to number of
--   objects in the bucket.  At this time, delete requests are free
--   from Amazon.
emptyBucket :: AWSConnection -- ^ AWS connection information
            -> String -- ^ Bucket name to empty
            -> IO (AWSResult ()) -- ^ Server response
emptyBucket aws bucket =
    do res <- listAllObjects aws bucket (ListRequest "" ""  "" 0)
       let objFromRes x = S3Object bucket (key x) "" [] L.empty
       case res of
         Left x -> return (Left x)
         Right y -> deleteObjects aws (map objFromRes y)

-- | Delete a list of objects, stop as soon as an error is encountered.
deleteObjects :: AWSConnection
              -> [S3Object]
              -> IO (AWSResult ())
deleteObjects _ [] = return (Right ())
deleteObjects aws (x:xs) =
    do dr <- deleteObject aws x
       case dr of
         Left o -> return (Left o)
         Right _ -> deleteObjects aws xs

-- | Return a list of all bucket names and creation dates.  S3
--   allows a maximum of 100 buckets per user.
listBuckets :: AWSConnection -- ^ AWS connection information
           -> IO (AWSResult [S3Bucket]) -- ^ Server response
listBuckets aws =
    do res <- Auth.runAction (S3Action aws "" "" "" [] L.empty GET Nothing)
       case res of
         Left x -> return (Left x)
         Right y -> do bs <- parseBucketListXML (L.unpack (rspBody y))
                       return (Right bs)

parseBucketListXML :: String -> IO [S3Bucket]
parseBucketListXML x = runX (readString [withValidate no] x >>> processBuckets)

processBuckets :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) S3Bucket
processBuckets = deep (isElem >>> hasName "Bucket") >>>
                 split >>> first (text <<< atTag "Name") >>>
                 second (text <<< atTag "CreationDate") >>>
                 unsplit (\x y -> S3Bucket x y)

-- | List request parameters
data ListRequest =
    ListRequest { prefix :: String,
                  marker :: String,
                  delimiter :: String,
                  max_keys :: Int
                }

instance Show ListRequest where
    show x = "prefix=" ++ urlEncode (prefix x) ++ "&" ++
             "marker=" ++ urlEncode (marker x) ++ "&" ++
             "delimiter=" ++ urlEncode (delimiter x) ++ "&" ++
             "max-keys=" ++ show (max_keys x)

-- | Result from listing objects.
data ListResult =
    ListResult {
      key :: String, -- ^ Name of object
      last_modified :: String, -- ^ Last modification date
      etag :: String, -- ^ MD5
      size :: Integer, -- ^ Bytes of object data
      storageClass :: StorageClass -- ^ Storage class of the object
    } deriving (Show)

-- | Is a result set response truncated?
type IsTruncated = Bool

-- | List objects in a bucket, based on parameters from 'ListRequest'.  See
--   the Amazon S3 developer resources for in depth explanation of how
--   the fields in 'ListRequest' can be used to query for objects.
--   <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/ListingKeysRequest.html>
listObjects :: AWSConnection -- ^ AWS connection information
            -> String -- ^ Bucket name to search
            -> ListRequest -- ^ List parameters
            -> IO (AWSResult (IsTruncated, [ListResult])) -- ^ Server response
listObjects aws bucket lreq =
    do res <- Auth.runAction (S3Action aws bucket ""
                                           ('?' : show lreq) [] L.empty GET Nothing)
       case res of
         Left x -> return (Left x)
         Right y -> do let objs = L.unpack (rspBody y)
                       tr <- isListTruncated objs
                       lr <- getListResults objs
                       return (Right (tr, lr))

-- | Repeatedly query the server for all objects in a bucket, ignoring the @max_keys@ field.
listAllObjects :: AWSConnection -- ^ AWS connection information
               -> String -- ^ Bucket name to search
               -> ListRequest -- ^ List parameters
               -> IO (AWSResult [ListResult]) -- ^ Server response
listAllObjects aws bucket lp =
    do let lp_max = lp {max_keys = 1000}
       res <- listObjects aws bucket lp_max
       case res of
         Left x -> return (Left x)
         Right y -> case y of
                      (True,lr) -> do let last_result = (key . last) lr
                                      next_set <- listAllObjects aws bucket
                                                  (lp_max {marker = last_result})
                                      either (\x -> return (Left x))
                                             (\x -> return (Right (lr ++ x))) next_set
                      (False,lr) -> return (Right lr)

-- | Retrieve the storage class of an object from S3.
--   For checking more than one object's storage class efficiently,
--   use listObjects.
getObjectStorageClass :: AWSConnection
                      -> S3Object
                      -> IO (AWSResult StorageClass)
getObjectStorageClass c obj =
    do res <- listObjects c (obj_bucket obj) (ListRequest (obj_name obj) "" "" 1)
       return (either Left (\(t,xs) -> Right (head (map storageClass xs))) res)

-- | Determine if ListBucketResult is truncated.  It would make sense
--   to combine this with the query for list results, so we didn't
--   have to parse the XML twice.
isListTruncated :: String -> IO Bool
isListTruncated s =
    do results <- runX (readString [withValidate no] s >>> processTruncation)
       return $ case results of
                  [] -> False
                  x:_ -> x

processTruncation :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Bool
processTruncation = (text <<< atTag "IsTruncated")
                    >>> arr (\x -> case (map toLower x) of
                                     "true" -> True
                                     "false" -> False
                                     otherwise -> False)

getListResults :: String -> IO [ListResult]
getListResults s = runX (readString [withValidate no] s >>> processListResults)

processListResults :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) ListResult
processListResults = deep (isElem >>> hasName "Contents") >>>
                     ((text <<< atTag "Key") &&&
                      (text <<< atTag "LastModified") &&&
                      (text <<< atTag "ETag") &&&
                      (text <<< atTag "Size") &&&
                      (text <<< atTag "StorageClass")) >>>
                     arr (\(a,(b,(c,(d,e)))) -> ListResult a b ((unquote . HTTP.urlDecode) c) (read d) (read e))

-- | Check Amazon guidelines on bucket naming.  (missing test for IP-like names)
isBucketNameValid :: String -> Bool
isBucketNameValid n = and checks where
    checks = [(length n >= 3),
              (length n <= 63),
              (isAlphaNum $ head n),
              (not (elem '_' n)),
              (not (isInfixOf ".-" n)),
              (not (isInfixOf "-." n)),
              ((last n) /= '-')]

-- | Set the versioning configuration of a bucket (MFA not yet supported).
setVersioningConfiguration :: AWSConnection -- ^ AWS connection information
                           -> String -- ^ Bucket to modify
                           -> VersioningConfiguration -- ^ Desired versioning configuration
                           -> IO (AWSResult ()) -- ^ Server response
setVersioningConfiguration aws bucket vc =
    do res <- Auth.runAction (S3Action aws bucket "" "?versioning" [] (L.pack (versioningConfigurationToXML vc)) PUT Nothing)
       case res of
         Left x -> return (Left x)
         Right y -> return (Right ())

versioningConfigurationToXML :: VersioningConfiguration -> String
versioningConfigurationToXML vc =
    case vc of
      VersioningConfiguration VersioningEnabled _ -> versioningConfigXml "Enabled"
      VersioningConfiguration _ _ -> versioningConfigXml "Suspended"

versioningConfigXml :: String -> String
versioningConfigXml status =
  "<VersioningConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Status>" ++ status ++  "</Status></VersioningConfiguration>"

-- | Check versioning and MFA configuration of a bucket.
getVersioningConfiguration :: AWSConnection -- ^ AWS connection information
                           -> String -- ^ Bucket name to inquire on
                           -> IO (AWSResult VersioningConfiguration) -- ^ Server response
getVersioningConfiguration aws bucket =
    do res <- Auth.runAction (S3Action aws bucket "" "?versioning" [] L.empty GET Nothing)
       case res of
         Left x -> return (Left x)
         Right y -> do vc <- parseVersionConfigXML (L.unpack (rspBody y))
                       return (Right vc)

parseVersionConfigXML :: String -> IO (VersioningConfiguration)
parseVersionConfigXML s =
    do results <- runX (readString [withValidate no] s >>> processVersionConfig)
       return $ case results of
                  [] -> (VersioningConfiguration VersioningSuspended True)
                  x:_ -> x

processVersionConfig =
  deep (isElem >>> hasName "VersioningConfiguration") >>>
    ((text <<< atTag "Status")
    >>> arr (\v -> case (map toLower v) of
                    "suspended" -> (VersioningConfiguration VersioningSuspended False)
                    "enabled" -> (VersioningConfiguration VersioningEnabled False)
            ))
    <+>
    arr (\x -> (VersioningConfiguration VersioningDisabled False))

-- | Remove quote characters from a 'String'.
unquote :: String -> String
unquote = filter (/= '"')
