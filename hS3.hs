-----------------------------------------------------------------------------
-- |
-- Module      :  hS3
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Command-line program for interacting with S3.
-----------------------------------------------------------------------------
module Main where

import Network.AWS.S3Bucket
import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import System.Environment
import System.IO
import Data.Maybe
import Network.AWS.S3Object
import qualified Data.ByteString.Lazy.Char8 as L

withConn :: ( AWSConnection -> IO (AWSResult a)) -> IO a
withConn f = do
  mConn <- amazonS3ConnectionFromEnv
  case mConn of
    Just c -> fmap (either (error . prettyReqError) -- failure
                    id ) $ f c
    Nothing -> error "couldn't connect"

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- create / delete bucket
    ["cb", name, location] -> withConn $ \g -> createBucketIn g name location
    ["db", name ] ->  withConn $ \g -> deleteBucket g name
    -- objects
    ["go", bucket, gkey ] ->
        do c <- withConn $ \g -> getObject g $ S3Object bucket gkey "" [] L.empty
           L.putStr $ obj_data c
    ["so", bucket, skey ] ->
        (\c ->  withConn $ \g -> sendObject g $ S3Object bucket skey "" [] c)
            =<< L.getContents
    ["los", bucket] ->
        do l <- withConn $ \g -> listObjects g bucket (ListRequest "" "" "" 1000)
           print l
    ["lbs"] -> withConn listBuckets >>= mapM_ (putStrLn . bucket_name)
    _ -> usage

usage :: IO ()
usage = putStr $ unlines [
         "export AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET"
        , ""
        , "cb <name> [EU, US] : create bucket"
        , "db <name> : delete bucket"
        , "do <bucket> <key> : delete object"
        , ""
        , "lbs : list buckets"
        , "so : send object"
        , "go : get object"
        , "los : list objects" ]
