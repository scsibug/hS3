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

type AWSResult a = Either ReqError {- some kind of failure to process the request -}
                          a {- result -}
data ReqError =
    NetworkError Stream.ConnError |
    AWSError String String -- AWS error code and message
             deriving (Show, Eq)

prettyReqError :: ReqError -> String
prettyReqError r = case r of
                     AWSError a b -> b
                     NetworkError c -> show c