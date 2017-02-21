module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List.Split
import Data.Maybe
import Control.Lens hiding (deep)
import Data.List (intercalate)

-- See https://www.hackerrank.com/challenges/stack-exchange-scraper

main = do
  let doc = fromUrl "http://stackoverflow.com/questions/tagged/haskell"
  contents <- runX $ doc
    >>> css "div[class=summary]"
    >>> (css "a[class~=question-hyperlink]" >>> (getAttrValue "href" &&& deep getText))
    &&& (css "span[class=relativetime]" /> getText)
    >>> arr format
  mapM_ print contents
  where format ((url, questionText), questionTime) = intercalate ";" [getIdentifier url, questionText, questionTime]
        getIdentifier url = fromMaybe "" (splitOn "/" url ^? element 2)
