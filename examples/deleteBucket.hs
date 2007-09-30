#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Delete Bucket
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Delete a bucket with a given name.
-- Usage:
--    deleteBucket.hs bucket-name
--
-- This requires the following environment variables to be set with
-- your Amazon keys:
--   AWS_ACCESS_KEY_ID
--   AWS_ACCESS_KEY_SECRET
-----------------------------------------------------------------------------

import Network.AWS.S3Bucket
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import System.Environment
import Data.Maybe

main = do argv <- getArgs
          let bucket = head argv
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          putStrLn ("Deleting bucket with name: " ++ bucket)
          res <- deleteBucket conn bucket
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Deletion of " ++ bucket ++ " successful."))
                 res
