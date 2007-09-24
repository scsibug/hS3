{-# OPTIONS -fno-monomorphism-restriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.AWS.ArrowUtils
-- Copyright   :
-- License     :
--
-- Helper functions for working with HXT.  Scraped from <haskell.org>.
-----------------------------------------------------------------------------

module Network.AWS.ArrowUtils (
  split, unsplit, atTag, text
)

where

import Control.Arrow
import Text.XML.HXT.Arrow

-- misc. functions for working with arrows (and HXT)

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText
