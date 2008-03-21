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
                     [("x-amz-meta-foo", "bar"),
                      ("x-amz-meta-french", "Bonjour, ça va?"),
                      ("x-amz-meta-smiley", "☺")
                      ] "Hello S3!"


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
                 -- Object list (should have 1 object in bucket)
                 testListAllObjects c bucket 1
                 -- Object delete
                 testDeleteObject c testObj
                 -- Object send, and then bucket empty
                 testSendObject c testObj
                 testEmptyBucket c bucket
                 -- Bucket should now be empty
                 testListAllObjects c bucket 0
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
       failOnError r "" (\x -> do assertBool "bucket creation" True
                                  return x
                        )

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
                                        (obj_data o) (obj_data x)
                        assertEqual "object get metadata"
                                        (obj_headers o)
                                        (realMetadata (obj_headers x))
              )

testGetObjectInfo :: AWSConnection -> S3Object -> IO ()
testGetObjectInfo c o =
    do r <- getObject c o
       failOnError r ()
              (\x -> assertEqual "object info get metadata"
                          (obj_headers o) (realMetadata (obj_headers x))
              )

-- test that a bucket has a given number of objects
testListAllObjects :: AWSConnection -> String -> Int -> IO ()
testListAllObjects c bucket count =
    do r <- listAllObjects c bucket (ListRequest "" "" "" 100)
       failOnError r ()
              (\x -> assertEqual "object list" count (length x))

testEmptyBucket :: AWSConnection -> String -> IO ()
testEmptyBucket c b =
    do r <- emptyBucket c b
       failOnError r ()
               (const $ assertBool "bucket empty" True)

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

-- These headers get added by amazon, but ignore them for
-- testing metadata storage.
headersToIgnore = ["x-amz-id-2", "x-amz-request-id"]

realMetadata :: [(String, b)] -> [(String, b)]
realMetadata = filter (\x -> (fst x) `notElem` headersToIgnore)

