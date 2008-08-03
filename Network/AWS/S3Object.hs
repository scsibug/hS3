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
  sendObject, getObject, getObjectInfo, deleteObject,
  publicUriForSeconds, publicUriUntilTime,
  -- * Data Types
  S3Object(..)
  ) where

import Network.AWS.Authentication as Auth
import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.HTTP
import Network.URI
import System.Time

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
               obj_data :: String
    } deriving (Read, Show)

-- | Send data for an object.
sendObject :: AWSConnection      -- ^ AWS connection information
           -> S3Object           -- ^ Object to add to a bucket
           -> IO (AWSResult ())  -- ^ Server response
sendObject aws obj =
    do res <- Auth.runAction (S3Action aws (obj_bucket obj)
                              (obj_name obj)
                              ""
                              (("Content-Type", (content_type obj)) :
                               (obj_headers obj))
                              (obj_data obj) PUT)
       return (either (Left) (\_ -> Right ()) res)

-- | Create a pre-signed request URI.  Anyone can use this to request
--   an object until the specified date.
publicUriUntilTime :: AWSConnection -- ^ AWS connection information
                  -> S3Object -- ^ Object to be made available
                  -> Integer -- ^ Expiration time, in seconds since
                             --   00:00:00 UTC on January 1, 1970
                  -> URI -- ^ URI for the object
publicUriUntilTime c obj time =
    let act = S3Action c (obj_bucket obj) (obj_name obj) "" [] "" GET
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
    do res <- Auth.runAction (S3Action aws (obj_bucket obj)
                                           (obj_name obj)
                                           ""
                                           (obj_headers obj)
                                           "" m)
       return (either (Left) (\x -> Right (populate_obj_from x)) res)
           where
             populate_obj_from x =
                 obj { obj_data = (rspBody x),
                       obj_headers = (headersFromResponse x) }

headersFromResponse :: Response -> [(String,String)]
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
deleteObject aws obj = do res <- Auth.runAction (S3Action aws (obj_bucket obj)
                                                              (obj_name obj)
                                                              ""
                                                              (obj_headers obj)
                                                              "" DELETE)
                          return (either (Left) (\_ -> Right ()) res)


