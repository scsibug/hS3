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
import Control.Concurrent(threadDelay)
import Data.List.Utils(hasKeyAL)
import Test.HUnit

-- | Run the tests
main = runTestTT tests

tests =
    TestList
    [
     TestLabel "S3 Operations Test" s3OperationsTest,
     TestLabel "S3 Message Integrity Check" s3MessageIntegrityCheckTest,
     TestLabel "S3 Copy Test" s3CopyTest,
     TestLabel "S3 Copy/Replace Test" s3CopyReplaceTest,
     TestLabel "S3 Location Test" s3LocationTest,
     TestLabel "Bucket Naming Test" bucketNamingTest,
     TestLabel "Reduced Redundancy Creation Test" reducedRedundancyCreateTest,
     TestLabel "Reduced Redundancy Existing Test" reducedRedundancyExistingTest,
     TestLabel "Versioning Test" versioningTest
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
                 threadDelay 3000000 -- sleep 3 sec, since bucket isn't always unavailable immediately
                 testBucketGone c bucket
             )

s3MessageIntegrityCheckTest =
    TestCase (
              do c <- getConn
                 bucket <- testCreateBucket c
                 testGetBucketLocation c bucket "US"
                 let testObj = testObjectTemplate {obj_bucket = bucket}
                 -- Object send
                 testSendObjectMIC c testObj
                 -- Object get
                 testGetObject c testObj
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
                 let srcHeader = "x-amz-meta-src"
                 let srcValue = "foo"
                 finally (
                       do let srcObj = testSourceTemplate {obj_bucket = d, obj_headers = [(srcHeader,srcValue)]}
                          let destObj = testDestinationTemplate {obj_bucket = b}
                          -- Object send
                          testSendObject c srcObj
                          -- Verify headers were set on original object
                          sr <- getObject c srcObj
                          failOnError sr ()
                                          (\x -> assertBool "Original sent object has custom headers"
                                                (hasKeyAL srcHeader (obj_headers x))
                                          )
                          -- Object copy
                          testCopyObject c srcObj destObj
                          -- Verify destination object contains same added header as source object
                          testGetObjectInfo c (destObj {obj_headers = [(srcHeader,srcValue)]})
                         ) (
                          -- Empty buckets
                       do testEmptyBucket c b
                          testEmptyBucket c d
                          -- Destroy buckets
                          testDeleteBucket c b
                          testDeleteBucket c d
                         )
             )

s3CopyReplaceTest =
    TestCase (
              do c <- getConn
                 -- Bucket Creation
                 b <- testCreateBucket c
                 d <- testCreateBucket c
                 let srcHeader = "x-amz-meta-src"
                 let srcValue = "foo"
                 finally (
                       do let srcObj = testSourceTemplate {obj_bucket = d, obj_headers = [(srcHeader,srcValue)]}
                          let destObj = testDestinationTemplate {obj_bucket = b}
                          -- Object send
                          testSendObject c srcObj
                          sr <- getObject c srcObj
                          failOnError sr ()
                                          (\x -> assertBool "Original sent object has custom headers"
                                                (hasKeyAL srcHeader (obj_headers x))
                                          )
                          -- Object copy
                          testCopyObjectWithReplace c srcObj destObj
                          -- Object get info from copied object
                          testGetObjectInfo c destObj
                          dr <- getObject c destObj
                          failOnError dr ()
                                          (\x -> assertBool "Copied object w/ replace does not have source headers"
                                                (not (hasKeyAL srcHeader (obj_headers x)))
                                          )
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

testSendObjectMIC :: AWSConnection -> S3Object -> IO ()
testSendObjectMIC c o =
    do r <- sendObjectMIC c o
       failOnError r ()
              (const $ assertBool "object send" True)

testCopyObject :: AWSConnection -> S3Object -> S3Object -> IO ()
testCopyObject c srco desto =
    do r <- copyObject c srco desto
       failOnError r ()
               (const $ assertBool "object copied" True)

testCopyObjectWithReplace :: AWSConnection -> S3Object -> S3Object -> IO ()
testCopyObjectWithReplace c srco desto =
    do r <- copyObjectWithReplace c srco desto
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

-- Test to ensure an object on S3 matches the headers passed to this function.
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

reducedRedundancyCreateTest =
    TestCase (
              do c <- getConn
                 b <- testCreateBucket c
                 let rr = "reduced-redundancy"
                 let testObj = testObjectTemplate {obj_bucket = b, obj_name = rr}
                 let rrTestObj = setStorageClass REDUCED_REDUNDANCY testObj
                 testSendObject c rrTestObj
                 r <- getObjectStorageClass c testObj
                 failOnError r ()
                        (\sc -> assertEqual "storage class is reduced-redundancy"
                               REDUCED_REDUNDANCY sc)
             )

reducedRedundancyExistingTest =
    TestCase (
              do c <- getConn
                 b <- testCreateBucket c
                 let rr = "reduced-redundancy"
                 let testObj = testObjectTemplate {obj_bucket = b, obj_name = rr}
                 testSendObject c testObj
                 rewriteStorageClass c REDUCED_REDUNDANCY testObj
                 r <- getObjectStorageClass c testObj
                 failOnError r ()
                        (\sc -> assertEqual "storage class is reduced-redundancy"
                               REDUCED_REDUNDANCY sc)
                 -- Set storage class back to STANDARD
                 rewriteStorageClass c STANDARD testObj
                 s <- getObjectStorageClass c testObj
                 failOnError s ()
                        (\sc -> assertEqual "storage class switched back to standard"
                               STANDARD sc)
             )

versioningTest =
    TestCase (
              do c <- getConn
                 b <- testCreateBucket c
                 r <- getVersioningConfiguration c b
                 failOnError r ()
                     (\vc -> assertEqual "versioning is disabled by default"
                            (VersioningConfiguration VersioningDisabled False) vc)
                 sr <- setVersioningConfiguration c b (VersioningConfiguration VersioningEnabled False)
                 failOnError sr ()
                     (\const -> assertBool "versioning set without error" True)
                 ur <- getVersioningConfiguration c b
                 failOnError ur ()
                     (\vc -> assertEqual "versioning is now enabled"
                            (VersioningConfiguration VersioningEnabled False) vc)
             )

getConn = do mConn <- amazonS3ConnectionFromEnv
             return (fromJust mConn)

-- These headers get added by amazon, but ignore them for
-- testing metadata storage.
headersToIgnore = ["x-amz-id-2", "x-amz-request-id"]

realMetadata :: [(String, b)] -> [(String, b)]
realMetadata = filter (\x -> fst x `notElem` headersToIgnore)