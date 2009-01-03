#!/usr/local/bin/runhaskell

-----------------------------------------------------------------------------
-- |
-- Program     :  PreSigned Request
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Generate a pre-signed object request, suitable for sending to a 3rd party.
-- Usage:
--    preSignedRequest.hs bucket-name object-name seconds-request-valid
--
-- This requires the following environment variables to be set with
-- your Amazon keys:
--   AWS_ACCESS_KEY_ID
--   AWS_ACCESS_KEY_SECRET
-----------------------------------------------------------------------------

import Network.AWS.S3Object
import Network.AWS.AWSConnection
import System.Environment
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

main = do argv <- getArgs
          let bucket : key : seconds : xs = argv
          mConn <- amazonS3ConnectionFromEnv
          let conn = fromJust mConn
          let obj = S3Object bucket key "" [] L.empty
          uri <- (publicUriForSeconds conn obj (read seconds))
          print uri
