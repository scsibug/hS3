#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  Upload File
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Upload a file to S3 with a given bucket and object name, with MD5 integrity check.
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
import qualified Data.ByteString.Lazy as L
import System.Posix.Files
import Data.Digest.MD5(hash)
import Codec.Binary.Base64 (encode, decode)

main = do argv <- getArgs
          let bucket : key : filename : xs = argv
          f <- L.readFile filename
          contentFS <- getFileStatus filename
          let offset = fileSize contentFS
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          let headers = [("Content-Length", (show offset)), ("Content-MD5", (mkMD5 f))]
          print headers
          let obj = S3Object bucket key "text/plain" headers f
          res <- sendObject conn obj
          either (putStrLn . prettyReqError)
                 (const $ putStrLn ("Creation of " ++ key ++ " successful."))
                 res

mkMD5 = encode . hash . L.unpack