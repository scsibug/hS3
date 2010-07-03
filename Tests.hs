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
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Exception(finally)
import IO(bracket)

import Test.HUnit

-- | Run the tests
main = runTestTT tests

tests =
    TestList
    [
     TestLabel "S3 Operations Test" s3OperationsTest,
     TestLabel "S3 Copy Test" s3CopyTest,
     TestLabel "S3 Location Test" s3LocationTest,
     TestLabel "Bucket Naming Test" bucketNamingTest,
     TestLabel "Reduced Redundancy Test" reducedRedundancyTest
    ]

testBucket = "hs3-test"

testObjectTemplate = S3Object testBucket "hS3-object-test" "text/plain"
                     [("x-amz-meta-foo", "bar"),
                      ("x-amz-meta-french", "Bonjour, ça va?"),
                      ("x-amz-meta-smiley", "☺")
                      ] (L.pack "Hello S3!")

testSourceTemplate = S3Object testBucket "hS3-object-source" "text/plain"
                         [] (L.pack "testing")
testDestinationTemplate = testSourceTemplate {obj_name = "hS3-object-destination"}

-- | A sequence of several operations.
s3OperationsTest =
    TestCase (
              do c <- getConn
                 -- Bucket Creation
                 bucket <- testCreateBucket c
                 testGetBucketLocation c bucket "US"
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
                 -- Bucket should be gone
                 testBucketGone c bucket
             )

s3LocationTest =
    TestCase (
              do c <- getConn
                 -- European buckets
                 bracket (testCreateBucketIn c "EU")
                             (\b ->
                                  do testEmptyBucket c b
                                     testDeleteBucket c b
                             )
                             (\b ->
                                  do testGetBucketLocation c b "EU"
                                     let euTestObj = testObjectTemplate {obj_bucket = b}
                                     testSendObject c euTestObj
                                     testGetObject c euTestObj
                                     testDeleteObject c euTestObj
                             )
                 -- US buckets
                 bracket (testCreateBucketIn c "US")
                             (\b -> testDeleteBucket c b)
                             (\b -> testGetBucketLocation c b "US")
             )

bucketNamingTest =
    TestList
    [
     (nameNotValidTC "At least 3 chars" "ab"),
     (nameValidTC "At least 3 chars" "abc"),
     (nameNotValidTC "63 chars or fewer" (replicate 64 'a')),
     (nameNotValidTC "Starts with alphanum char" "."),
     (nameNotValidTC "Starts with alphanum char" "_"),
     (nameNotValidTC "Starts with alphanum char" "-"),
     (nameNotValidTC "No underscores" "ab_cd"),
     (nameNotValidTC "Do not end with a dash" "foo-"),
     (nameNotValidTC "Dashes should not be next to periods" "ab.-cd")
    ]

nameValidTC :: String -> String -> Test
nameValidTC msg name = TestCase (assertBool msg (isBucketNameValid name))
nameNotValidTC :: String -> String -> Test
nameNotValidTC msg name = TestCase (assertBool msg (not (isBucketNameValid name)))

s3CopyTest =
    TestCase (
              do c <- getConn
                 -- Bucket Creation
                 b <- testCreateBucket c
                 d <- testCreateBucket c
                 finally (
                       do let srcObj = testSourceTemplate {obj_bucket = d}
                          let destObj = testDestinationTemplate {obj_bucket = b}
                          -- Object send
                          testSendObject c srcObj
                          -- Object copy
                          testCopyObject c srcObj destObj
                          -- Object get info from copied object
                          testGetObjectInfo c destObj
                         ) (
                          -- Empty buckets
                       do testEmptyBucket c b
                          testEmptyBucket c d
                          -- Destroy buckets
                          testDeleteBucket c b
                          testDeleteBucket c d
                         )
             )

failOnError :: (Show a) =>
               Either a b  -- ^ AWS Result to inspect
            -> t           -- ^ Value to return on failure
            -> (b -> IO t) -- ^ Assertions to run on success
            -> IO t
failOnError r f d = either
                    (\x ->
                         do assertFailure (show x)
                            return f)
                    (\x -> d x) r

testCreateNamedBucket :: AWSConnection -> String -> IO ()
testCreateNamedBucket c bucket =
    do r <- createBucket c bucket
       failOnError r ()
              (const $ assertBool "bucket creation" True)

testCreateBucket :: AWSConnection -> IO String
testCreateBucket c =
    do r <- createBucketWithPrefix c testBucket
       failOnError r "" (\x -> do assertBool "bucket creation" True
                                  return x
                        )

testCreateBucketIn :: AWSConnection -> String -> IO String
testCreateBucketIn c location =
    do r <- createBucketWithPrefixIn c testBucket location
       failOnError r "" (\x -> do assertBool ("bucket creation in " ++ location) True
                                  return x
                        )

testGetBucketLocation :: AWSConnection -> String -> String -> IO ()
testGetBucketLocation c bucket expectedLocation =
    do r <- getBucketLocation c bucket
       failOnError r () (\x ->
                         assertEqual ("Bucket in the " ++ expectedLocation)
                                     expectedLocation x)

testSendObject :: AWSConnection -> S3Object -> IO ()
testSendObject c o =
    do r <- sendObject c o
       failOnError r ()
              (const $ assertBool "object send" True)

testCopyObject :: AWSConnection -> S3Object -> S3Object -> IO ()
testCopyObject c srco desto =
    do r <- copyObject c srco desto
       failOnError r ()
               (const $ assertBool "object copied" True)

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

-- test if a bucket is not present
-- It sometimes takes a second or two for a bucket to disappear after a delete,
-- so failing this is not fatal.
testBucketGone :: AWSConnection -> String -> IO ()
testBucketGone c bucket =
    getBucketLocation c bucket >>=
       either (\(AWSError code msg) -> assertEqual "Bucket is gone" "NotFound" code)
              (\x -> do assertFailure "Bucket still there, should be gone (sometimes slow, not fatal)"
                        return ())

reducedRedundancyTest =
    TestCase (
              do c <- getConn
                 b <- testCreateBucket c
                 let rr = "reduced-redundancy"
                 let testObj = testObjectTemplate {obj_bucket = b, obj_name = rr}
                 let rrTestObj = setStorageClass REDUCED_REDUNDANCY testObj
                 testSendObject c rrTestObj
                 r <- listObjects c b (ListRequest rr "" "" 1)
                 failOnError r ()
                        (\(t,xs) -> putStrLn (show xs) >> assertEqual "storage class is reduced-redundancy"
                                   REDUCED_REDUNDANCY (head (map storageClass xs)))
             )

getConn = do mConn <- amazonS3ConnectionFromEnv
             return (fromJust mConn)

-- These headers get added by amazon, but ignore them for
-- testing metadata storage.
headersToIgnore = ["x-amz-id-2", "x-amz-request-id"]

realMetadata :: [(String, b)] -> [(String, b)]
realMetadata = filter (\x -> fst x `notElem` headersToIgnore)