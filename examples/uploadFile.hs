#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Upload File
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Upload a file to S3 with a given bucket and object name.
-- Usage:
--    uploadFile.hs bucket-name object-name filename
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
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L
import System.Posix.Files

main = do argv <- getArgs
          let bucket : key : filename : xs = argv
          f <- L.readFile filename
          contentFS <- getFileStatus filename
          let offset = fileSize contentFS
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          print ("offset is "++(show offset))
          let obj = S3Object bucket key "text/plain" [("Content-Length",(show offset))] f
          res <- sendObject conn obj
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Creation of " ++ key ++ " successful."))
                 res