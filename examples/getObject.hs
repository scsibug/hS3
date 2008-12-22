#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Get Object
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Retrieve the contents of an object in a bucket.
-- Usage:
--    getObject.hs bucket-name object-name
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
import qualified Data.ByteString.Lazy.Char8 as L

main = do argv <- getArgs
          let bucket : key : xs = argv
          let obj = S3Object bucket key "" [] L.empty
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          res <- getObject conn obj
          either (putStrLn . prettyReqError)
                 (\x -> do putStrLn ("Key " ++ key ++ " has been retrieved.  Content follows:")
                           L.putStrLn (obj_data x))
                 res