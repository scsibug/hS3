#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  List Objects
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- List all objects in a bucket
-- Usage:
--    listObjects.hs bucket-name
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
          let bucket : xs = argv
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          res <- listAllObjects conn bucket (ListRequest "" ""  "" 1000)
          print (ListRequest "" ""  "" 1000)
          either (putStrLn . prettyReqError)
                 (\x -> do putStrLn ("Key list from bucket " ++
                                     bucket ++
                                     " has been retrieved.  Key/Etag follows:")
                           mapM_ (\x -> putStrLn (key x ++
                                                  " " ++
                                                  etag x)) x
                 ) res
