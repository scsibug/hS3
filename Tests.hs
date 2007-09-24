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
                 -- Object get info
                 testGetObjectInfo c testObj
                 -- Object list
                 testListObject c bucket 1
                 -- Object delete
                 testDeleteObject c testObj
                 -- Delete bucket
                 testDeleteBucket c bucket
             )

failOnError :: (Show a) =>
               Either a b  -- ^ AWS Result to inspect
            -> t           -- ^ Value to return on failure
            -> (b -> IO t) -- ^ Assertions to run on success
            -> IO t
failOnError r f d = do either (\x -> do assertFailure (show x)
                                        return f)
                          (\x -> do d x) r

testCreateBucket :: AWSConnection -> IO String
testCreateBucket c =
    do r <- createBucketWithPrefix c testBucket
       failOnError r "" (\x -> assertBool "bucket creation" True >> return x)

testSendObject :: AWSConnection -> S3Object -> IO ()
testSendObject c o =
    do r <- sendObject c o
       failOnError r ()
              (const $ assertBool "object send" True)

testGetObject :: AWSConnection -> S3Object -> IO ()
testGetObject c o =
    do r <- getObject c o
       failOnError r ()
              (\x -> do assertEqual "object get body"
                                        (obj_data x) (obj_data o)
                        assertEqual "object get metadata"
                                        (obj_headers x) (obj_headers o)
              )

testGetObjectInfo :: AWSConnection -> S3Object -> IO ()
testGetObjectInfo c o =
    do r <- getObject c o
       failOnError r ()
              (\x -> do assertEqual "object info get metadata"
                                        (obj_headers x) (obj_headers o)
              )

-- test that a bucket has a given number of objects
testListObject :: AWSConnection -> String -> Int -> IO ()
testListObject c bucket count =
    do r <- listObjects c bucket (ListRequest "" "" "" 100)
       failOnError r ()
              (\x -> assertEqual "object list" count (length x))

testDeleteObject :: AWSConnection -> S3Object -> IO ()
testDeleteObject c o =
    do r <- deleteObject c o
       failOnError r ()
              (const $ assertBool "object delete" True)

testDeleteBucket :: AWSConnection -> String -> IO ()
testDeleteBucket c bucket =
    do r <- deleteBucket c bucket
       failOnError r ()
              (const $ assertBool "bucket deletion" True)

getConn = do mConn <- amazonS3ConnectionFromEnv
             return (fromJust mConn)
