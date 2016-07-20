module Test.Wormhole.Page
( test
) where

import Control.Monad.Eff (runPure)
import Data.Foldable (all, elem)
import Data.Maybe (isJust, isNothing, Maybe(Just, Nothing))
import Data.String as String
import Prelude
import Test.QuickCheck (quickCheck)
import Wormhole.Page

test = do
  testSlugNamespace
  testLocate

testSlugNamespace = do
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

testLocate = do
  quickCheck $ noSuitableLocator
  quickCheck $ suitableLocatorNoPage
  quickCheck $ suitableLocatorPage
  where noSuitableLocator n s = isNothing (runPure (locate (const Nothing) n s))
        suitableLocatorNoPage n s = isNothing (runPure (locate ll n s))
          where ll n' | n' == n   = Just (const (pure Nothing))
                      | otherwise = Nothing
        suitableLocatorPage n s = isJust (runPure (locate ll n s))
          where ll n' | n' == n   = Just l
                      | otherwise = Nothing
                l s' | s' == s   = pure (Just {title: "", body: HTML ""})
                     | otherwise = pure Nothing
