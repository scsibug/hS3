-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.AWSResult
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD3
--
-- Results from a query to Amazon Web Services.
-- API Version 2006-03-01
-- <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/>
-----------------------------------------------------------------------------

module Network.AWS.AWSResult (
                  -- * Data Types
                  AWSResult,
                  ReqError(..),
                  prettyReqError
                 ) where

import Network.Stream as Stream

-- | A result from processing a request to S3.  Either some success
--   value, or a 'ReqError'.
type AWSResult a = Either ReqError a

-- | An error from an S3 request, either at the network layer, or from
--   S3 itself.
data ReqError =
    -- | Connection error at the network layer.
    NetworkError Stream.ConnError |
    -- | @AWSError code message@ constructs an error message from S3
    --   itself.  See
    --   <http://docs.amazonwebservices.com/AmazonS3/2006-03-01/ErrorCodeList.html>
    --   for a detailed list of possible codes.
    AWSError String String
             deriving (Show, Eq)

-- | Pretty print an error message.
prettyReqError :: ReqError -> String
prettyReqError r = case r of
                     AWSError a b -> b
                     NetworkError c -> show c