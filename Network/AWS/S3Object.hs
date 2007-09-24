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
  sendObject, getObject, deleteObject,
  -- * Data Types
  S3Object(..)
  ) where

import Network.AWS.Authentication as Auth
import Network.AWS.AWSResult
import Network.AWS.AWSConnection
import Network.HTTP

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
    } deriving (Show)

-- | Send data for an object.
sendObject :: AWSConnection      -- ^ AWS connection information
           -> S3Object           -- ^ Object to add to a bucket
           -> IO (AWSResult ())  -- ^ Server response
sendObject aws obj = do res <- Auth.runAction (S3Action aws (obj_bucket obj)
                                                            (obj_name obj)
                                                            ""
                                                            (obj_headers obj)
                                                            (obj_data obj) PUT)
                        return (either (Left) (\x -> Right ()) res)

-- | Retrieve an object.
getObject :: AWSConnection            -- ^ AWS connection information
          -> S3Object                 -- ^ Object to retrieve
          -> IO (AWSResult S3Object)  -- ^ Server response
getObject aws obj =
    do res <- Auth.runAction (S3Action aws (obj_bucket obj)
                                           (obj_name obj)
                                           ""
                                           (obj_headers obj)
                                           "" GET)
       return (either (Left) (\x -> Right (populate_obj_from x)) res)
           where
             populate_obj_from x = obj {obj_data = (rspBody x)}

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
                          return (either (Left) (\x -> Right ()) res)


