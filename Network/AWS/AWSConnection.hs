-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.AWSConnection
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Connection and authentication info for an Amazon AWS request.
-----------------------------------------------------------------------------
module Network.AWS.AWSConnection (
   -- * Constants
   defaultAmazonS3Host, defaultAmazonS3Port,
   -- * Function Types
   amazonS3Connection, amazonS3ConnectionFromEnv,
   -- * Data Types
   AWSConnection(..)
   ) where

import System.Environment

-- | An Amazon Web Services connection.  Everything needed to connect
--   and authenticate requests.
data AWSConnection =
    AWSConnection { awsHost :: String, -- ^ Service provider hostname
                    awsPort :: Int,    -- ^ Service provider port number
                    awsAccessKey :: String, -- ^ Access Key ID
                    awsSecretKey :: String  -- ^ Secret Access Key
                  } deriving (Show)

-- | Hostname used for connecting to Amazon's production S3 service (@s3.amazonaws.com@).
defaultAmazonS3Host :: String
defaultAmazonS3Host = "s3.amazonaws.com"

-- | Port number used for connecting to Amazon's production S3 service (@80@).
defaultAmazonS3Port :: Int
defaultAmazonS3Port = 80

-- | Create an AWSConnection to Amazon from credentials.  Uses the
--   production service.
amazonS3Connection :: String -- ^ Access Key ID
                   -> String -- ^ Secret Access Key
                   -> AWSConnection -- ^ Connection to Amazon S3
amazonS3Connection = AWSConnection defaultAmazonS3Host defaultAmazonS3Port

-- | Retrieve Access and Secret keys from environment variables
--   AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY, respectively.
--   Either variable being undefined or empty will result in
--   'Nothing'.
amazonS3ConnectionFromEnv :: IO (Maybe AWSConnection)
amazonS3ConnectionFromEnv =
    do ak <- getEnvKey "AWS_ACCESS_KEY_ID"
       sk0 <- getEnvKey "AWS_ACCESS_KEY_SECRET"
       sk1 <- getEnvKey "AWS_SECRET_ACCESS_KEY"
       return $ case (ak, sk0, sk1) of
                  ("",  _,  _) -> Nothing
                  ( _, "", "") -> Nothing
                  ( _, "",  _) -> Just (amazonS3Connection ak sk1)
                  ( _,  _,  _) -> Just (amazonS3Connection ak sk0)
    where getEnvKey s = fmap (maybe "" id . lookup s) getEnvironment

