#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Delete Object
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Delete an object in a bucket with a given name.
-- Usage:
--    deleteObject.hs bucket-name object-name
--
-- This requires the following environment variables to be set with
-- your Amazon keys:
--   AWS_ACCESS_KEY_ID
--   AWS_ACCESS_KEY_SECRET
-----------------------------------------------------------------------------

import Bucket
import S3Object
import Authentication
import S3Auth(aws_id, aws_key)
import System.Environment
import AWSResult

workingConn = AWSConnection defaultAmazonHost defaultAmazonPort aws_id aws_key

main = do argv <- getArgs
          let bucket : key : xs = argv
          let obj = S3Object bucket key "" [] ""
          res <- deleteObject workingConn obj
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Key " ++ key ++ " has been removed, if it existed before."))
                 res