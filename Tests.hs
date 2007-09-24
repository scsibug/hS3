-----------------------------------------------------------------------------
-- |
-- Module      :  AWS S3 Tests
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Test hS3 library against Amazon S3.  This requires the following
-- environment variables to be set with your Amazon keys:
--   AWS_ACCESS_KEY_ID
--   AWS_ACCESS_KEY_SECRET
-----------------------------------------------------------------------------

module Main(main) where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Object
import Network.AWS.S3Bucket
import Data.Maybe (fromJust)

import Test.HUnit

-- | Run the tests
main = runTestTT tests

tests =
   TestList [s3operationsTest]

testBucket = "hS3-test"

testObjectTemplate = S3Object testBucket "hS3-object-test" "text/plain"
                     [("x-amz-foo", "bar")] "Hello S3!"

-- | A sequence of several operations.
s3operationsTest =
    TestCase (
              do c <- getConn
                 -- Bucket Creation
                 bucket <- testCreateBucket c
                 let testObj = testObjectTemplate {obj_bucket = bucket}
                 -- Object send
                 testSendObject c testObj
                 -- Object get
                 testGetObject c testObj
                 -- Object list
                 testListObject c bucket 1
                 -- Object delete
                 testDeleteObject c testObj
                 -- Delete bucket
                 testDeleteBucket c bucket
             )

testCreateBucket :: AWSConnection -> IO String
testCreateBucket c =
    do r <- createBucketWithPrefix c testBucket
       either (\x -> do assertFailure (show x)
                        return ""
              )
              (\x -> do assertBool "bucket creation" True
                        return x
              ) r

testSendObject :: AWSConnection -> S3Object -> IO ()
testSendObject c o =
    do r <- sendObject c o
       either (\x -> assertFailure (show x))
              (const $ assertBool "object send" True) r

testGetObject :: AWSConnection -> S3Object -> IO ()
testGetObject c o =
    do r <- getObject c o
       either (\x -> assertFailure (show x))
              (\x -> do assertEqual "object get body"
                                        (obj_data x) (obj_data o)
                        assertEqual "object get metadata"
                                        (obj_headers x) (obj_headers o)
              ) r

-- test that a bucket has a given number of objects
testListObject :: AWSConnection -> String -> Int -> IO ()
testListObject c bucket count =
    do r <- listObjects c bucket (ListRequest "" "" "" 100)
       either (\x -> assertFailure (show x))
              (\x -> assertEqual "object list" count (length x)) r

testDeleteObject :: AWSConnection -> S3Object -> IO ()
testDeleteObject c o =
    do r <- deleteObject c o
       either (\x -> assertFailure (show x))
              (const $ assertBool "object delete" True) r

testDeleteBucket :: AWSConnection -> String -> IO ()
testDeleteBucket c bucket =
    do r <- deleteBucket c bucket
       either (\x -> assertFailure (show x))
              (const $ assertBool "bucket deletion" True) r

getConn = do mConn <- amazonS3ConnectionFromEnv
             return (fromJust mConn)
