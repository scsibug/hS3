#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  List Buckets
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- List all buckets for an S3 account
-- Usage:
--    listBuckets.hs
--
-- This requires the following environment variables to be set with
-- your Amazon keys:
--   AWS_ACCESS_KEY_ID
--   AWS_ACCESS_KEY_SECRET
-----------------------------------------------------------------------------

import Network.AWS.S3Bucket
import Network.AWS.S3Object
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import System.Environment
import Data.Maybe

main = do mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          res <- listBuckets conn
          either print (mapM_ (putStrLn . bucket_name)) res
