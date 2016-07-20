module Test.Wormhole.Page
( test
) where

import Data.Foldable (all, elem)
import Data.String as String
import Prelude
import Test.QuickCheck (quickCheck)
import Wormhole.Page

test = do
  quickCheck $ idempotent makeNamespace runNamespace
  quickCheck $ idempotent makeSlug runSlug
  quickCheck $ pattern makeNamespace runNamespace
  quickCheck $ pattern makeSlug runSlug
  where idempotent :: forall a. (String -> a) -> (a -> String) -> String -> Boolean
        idempotent m r s = go (go s) == go s where go = r <<< m

        pattern :: forall a. (String -> a) -> (a -> String) -> String -> Boolean
        pattern m r s = all (_ `elem` okChars) (String.toCharArray (r (m s)))
          where okChars = String.toCharArray (letters <> digits <> "-")
                letters = "abcdefghijklmnopqrstuvwxyz"
                digits = "0123456789"
