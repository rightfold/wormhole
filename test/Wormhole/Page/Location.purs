module Test.Wormhole.Page.Location
( test
) where

import Control.Monad.Eff (runPure)
import Data.Maybe (isJust, isNothing, Maybe(Just, Nothing))
import Prelude
import Test.QuickCheck (quickCheck)
import Wormhole.Page (Body(HTML), Namespace, Slug)
import Wormhole.Page.Location

test = do
  quickCheck $ noSuitableLocator
  quickCheck $ suitableLocatorNoPage
  quickCheck $ suitableLocatorPage
  where noSuitableLocator :: Namespace -> Slug -> Boolean
        noSuitableLocator n s = isNothing (runPure (locate (const Nothing) n s))

        suitableLocatorNoPage :: Namespace -> Slug -> Boolean
        suitableLocatorNoPage n s = isNothing (runPure (locate ll n s))
          where ll n' | n' == n   = Just (const (pure Nothing))
                      | otherwise = Nothing

        suitableLocatorPage :: Namespace -> Slug -> Boolean
        suitableLocatorPage n s = isJust (runPure (locate ll n s))
          where ll n' | n' == n   = Just l
                      | otherwise = Nothing
                l s' | s' == s   = pure (Just {title: "", body: HTML ""})
                     | otherwise = pure Nothing
