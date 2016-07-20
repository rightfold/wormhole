module Test.Main where

import Prelude
import Test.Wormhole.Page as Wormhole.Page
import Test.Wormhole.Page.Location as Wormhole.Page.Location

main = do
  Wormhole.Page.test
  Wormhole.Page.Location.test
