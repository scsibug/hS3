-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.S3Object
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Object interface for Amazon S3
-- API Version 2006-03-01
-- <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/>
-----------------------------------------------------------------------------

module Network.AWS.S3Object (
  -- * Function Types
  sendObject, copyObject, copyObjectWithReplace, getObject,
  getObjectInfo, deleteObject, publicUriForSeconds,
  publicUriUntilTime, setStorageClass, getStorageClass,
  rewriteStorageClass,
  -- * Data Types
  S3Object(..), StorageClass(..)
  ) where

import Network.AWS.Authentication as Auth
import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.HTTP
import Network.URI
import System.Time
import Data.List.Utils

import qualified Data.ByteString.Lazy.Char8 as L

-- | An object that can be stored and retrieved from S3.
data S3Object =
    S3Object { -- | Name of the bucket containing this object
               obj_bucket :: String,
               -- | URI of the object.  Subresources ("?acl" or
               -- | "?torrent") should be suffixed onto this name.
               obj_name :: String,
               -- | A standard MIME type describing the format of the
               --   contents.  If not specified, @binary/octet-stream@ is
               --   used.
               content_type :: String,
               -- | Object metadata in (key,value) pairs.  Key names
               --   should use the prefix @x-amz-meta-@ to be stored with
               --   the object.  The total HTTP request must be under 4KB,
               --   including these headers.
               obj_headers :: [(String, String)],
               -- | Object data.
               obj_data :: L.ByteString
    } deriving (Read, Show)

data StorageClass = STANDARD | REDUCED_REDUNDANCY
  deriving (Read, Show, Eq)

-- Amazon header key for storage class
storageHeader = "x-amz-storage-class"

-- | Add required headers for the storage class.
--   Use this in combination with 'sendObject' for new objects.  To
--   modify the storage class of existing objects, use
--   'rewriteStorageClass'.  Using reduced redundancy for object storage
--   trades off redundancy for storage costs.
setStorageClass :: StorageClass -- ^ Storage class to request
                -> S3Object -- ^ Object to modify
                -> S3Object -- ^ Object with storage class headers set, ready to be sent
setStorageClass sc obj = obj {obj_headers = addToAL
                                            (obj_headers obj)
                                            storageHeader (show sc)}

-- | Retrieve the storage class of a local S3Object.
--   Does not work for objects retrieved with 'getObject', since the
--   required header values are not returned.  Use
--   'getObjectStorageClass' or 'listObjects' from S3Bucket module to
--   determine storage class of existing objects.
getStorageClass :: S3Object -- ^ Object to inspect
                -> Maybe StorageClass -- ^ Requested storage class, Nothing if unspecified
getStorageClass obj = case stg_values of
                        [] -> Nothing
                        x -> Just (read (head x))
    where
      hdrs = obj_headers obj
      stg_hdrs = filter (\x -> fst x == storageHeader) hdrs
      stg_values = map fst stg_hdrs

-- | Change the storage class (and only the storage class) of an existing object.
--   This actually performs a copy to the same location, preserving metadata.
--   It is not clear to me whether ACLs are preserved when copying to the same location.
--   For best performance, we must not change other headers during storage class
--   changes.
rewriteStorageClass :: AWSConnection -- ^ AWS connection information
                    -> StorageClass -- ^ New storage class for object
                    -> S3Object -- ^ Object to modify
                    -> IO (AWSResult S3Object) -- ^ Server response
rewriteStorageClass aws sc obj =
    copyObject aws obj (setStorageClass sc (obj {obj_headers = []}))

-- | Send data for an object.
sendObject :: AWSConnection      -- ^ AWS connection information
           -> S3Object           -- ^ Object to add to a bucket
           -> IO (AWSResult ())  -- ^ Server response
sendObject aws obj =
    do res <- Auth.runAction (S3Action aws (urlEncode (obj_bucket obj))
                              (urlEncode (obj_name obj))
                              ""
                              (("Content-Type", (content_type obj)) :
                               obj_headers obj)
                              (obj_data obj) PUT)
       return (either Left (\_ -> Right ()) res)

-- | Create a pre-signed request URI.  Anyone can use this to request
--   an object until the specified date.
publicUriUntilTime :: AWSConnection -- ^ AWS connection information
                  -> S3Object -- ^ Object to be made available
                  -> Integer -- ^ Expiration time, in seconds since
                             --   00:00:00 UTC on January 1, 1970
                  -> URI -- ^ URI for the object
publicUriUntilTime c obj time =
    let act = S3Action c (urlEncode (obj_bucket obj)) (urlEncode (obj_name obj)) "" [] L.empty GET
    in preSignedURI act time

-- | Create a pre-signed request URI.  Anyone can use this to request
--   an object for the number of seconds specified.
publicUriForSeconds :: AWSConnection -- ^ AWS connection information
                    -> S3Object -- ^ Object to be made available
                    -> Integer -- ^ How many seconds until this
                               --   request expires
                    -> IO URI -- ^ URI for the object
publicUriForSeconds c obj time =
    do TOD ctS _ <- getClockTime -- GHC specific, todo: get epoch within h98.
       return (publicUriUntilTime c obj (ctS + time))

-- | Retrieve an object.
getObject :: AWSConnection            -- ^ AWS connection information
          -> S3Object                 -- ^ Object to retrieve
          -> IO (AWSResult S3Object)  -- ^ Server response
getObject = getObjectWithMethod GET

-- | Get object info without retrieving content body from server.
getObjectInfo :: AWSConnection            -- ^ AWS connection information
              -> S3Object                 -- ^ Object to retrieve information on
              -> IO (AWSResult S3Object)  -- ^ Server response
getObjectInfo = getObjectWithMethod HEAD

-- | Get an object with specified method.
getObjectWithMethod :: RequestMethod -- ^ Method to use for retrieval (GET/HEAD)
                    -> AWSConnection -- ^ AWS connection
                    -> S3Object      -- ^ Object to request
                    -> IO (AWSResult S3Object)
getObjectWithMethod m aws obj =
    do res <- Auth.runAction (S3Action aws (urlEncode (obj_bucket obj))
                                           (urlEncode (obj_name obj))
                                           ""
                                           (obj_headers obj)
                                           L.empty m)
       return (either Left (\x -> Right (populate_obj_from x)) res)
           where
             populate_obj_from x =
                 obj { obj_data = (rspBody x),
                       obj_headers = (headersFromResponse x) }

headersFromResponse :: HTTPResponse L.ByteString -> [(String,String)]
headersFromResponse r =
    let respheaders = rspHeaders r
    in map (\x -> case x of
                    Header (HdrCustom name) val -> (name, (mimeDecode val))
           ) (filter isAmzHeader respheaders)

-- | Delete an object.  Only bucket and object name need to be
--   specified in the S3Object.  Deletion of a non-existent object
--   does not return an error.
deleteObject :: AWSConnection      -- ^ AWS connection information
             -> S3Object           -- ^ Object to delete
             -> IO (AWSResult ())  -- ^ Server response
deleteObject aws obj = do res <- Auth.runAction (S3Action aws (urlEncode (obj_bucket obj))
                                                              (urlEncode (obj_name obj))
                                                              ""
                                                              (obj_headers obj)
                                                              L.empty DELETE)
                          return (either Left (\_ -> Right ()) res)

-- | Copy object from one bucket to another (or the same bucket), preserving the original headers.
--   Headers from @destobj@ are sent, while only the
--   bucket and name of @srcobj@ are used.  For the best
--   performance, when changing headers during a copy, use the
--   'copyObjectWithReplace' function.  For conditional copying, the
--   following headers set on the destination object may be used:
--   @x-amz-copy-source-if-match@, @x-amz-copy-source-if-none-match@,
--   @x-amz-copy-source-if-unmodified-since@, or
--   @x-amz-copy-source-if-modified-since@.  See
--   <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/API/index.html?RESTObjectCOPY.html>
--   for more details.
copyObject :: AWSConnection            -- ^ AWS connection information
              -> S3Object                 -- ^ Source object (bucket+name only)
              -> S3Object                 -- ^ Destination object
              -> IO (AWSResult S3Object)  -- ^ Server response, headers include version information
copyObject aws srcobj destobj =
    do res <- Auth.runAction (S3Action aws (urlEncode (obj_bucket destobj))
                                           (urlEncode (obj_name destobj))
                                           ""
                                           (copy_headers)
                                           L.empty PUT)
       return (either Left (\x -> Right (populate_obj_from x)) res)
           where
             populate_obj_from x =
                 destobj { obj_data = (rspBody x),
                           obj_headers = (headersFromResponse x) }
             copy_headers = [("x-amz-copy-source",
                              ("/"++ (urlEncode (obj_bucket srcobj))
                               ++ "/" ++ (urlEncode (obj_name srcobj))))]
                            ++ (obj_headers destobj)

-- | Copy object from one bucket to another (or the same bucket), replacing headers.
--   Any headers from @srcobj@ are ignored, and only those
--   set in @destobj@ are used.
copyObjectWithReplace :: AWSConnection            -- ^ AWS connection information
                      -> S3Object                 -- ^ Source object (bucket+name only)
                      -> S3Object                 -- ^ Destination object
                      -> IO (AWSResult S3Object)  -- ^ Server response, headers include version information
copyObjectWithReplace aws srcobj destobj =
    copyObject aws srcobj (destobj {obj_headers =
                                    (addToAL (obj_headers destobj)
                                     "x-amz-metadata-directive"
                                     "REPLACE")
                                   })
