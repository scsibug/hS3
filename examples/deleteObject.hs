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

import Network.AWS.S3Bucket
import Network.AWS.S3Object
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import System.Environment
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

main = do argv <- getArgs
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          let bucket : key : xs = argv
          let obj = S3Object bucket key "" [] L.empty
          res <- deleteObject conn obj
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Key " ++ key ++ " has been removed, if it existed before."))
                 res