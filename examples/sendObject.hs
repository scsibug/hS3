#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Send Object
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Create a new public object with some content.  This can be viewed through
-- a web browser afterwards by visiting http://bucket.s3.amazonaws.com/object
-- Usage:
--    listObjects.hs bucket-name object-name "Some Object Content."
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

main = do argv <- getArgs
          let bucket : key : content : xs = argv
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          let obj = S3Object bucket key "text/html" [("x-amz-acl", "public-read")] content
          res <- sendObject conn obj
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Creation of " ++ key ++ " successful."))
                 res
