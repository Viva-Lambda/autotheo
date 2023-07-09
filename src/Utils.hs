-- utility modules for non empty list
module Utils(concatLN) where

import qualified Data.List.NonEmpty as LN

concatLN :: (LN.NonEmpty (LN.NonEmpty a)) -> LN.NonEmpty a

concatLN xs = let listOfList = LN.toList (LN.map LN.toList xs)
              in LN.fromList (concat listOfList)
