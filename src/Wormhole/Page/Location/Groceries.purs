module Wormhole.Page.Location.Groceries
( locate
) where

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Iff (Iff)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Partial.Unsafe (unsafeCrashWith)
import Prelude
import Wormhole.Page (Body(HTML), Page, runSlug, Slug)

locate :: forall eff
        . (String -> Iff eff (Either Error (Maybe String)))
       -> Slug
       -> Iff eff (Either Error (Maybe Page))
locate getList slug = do
  list <- getList (runSlug slug)
  case list of
    Left err       -> pure (Left err)
    Right Nothing  -> pure (Right Nothing)
    Right (Just l) -> pure $ Right $ Just $ page slug l

page :: Slug -> String -> Page
page slug list = {title: "Groceries: " <> runSlug slug, body: body}
  where body = list # parse # compute # render

parse :: String -> Array {from :: String, to :: String, amount :: Int}
parse = String.split "\n" >>> map (String.split "\t") >>> (_ >>= parseRecord)
  where parseRecord [from, to, _, amount'] =
          Int.fromString amount' # maybe [] (\amount -> [{from, to, amount}])
        parseRecord _ = []

compute :: Array {from :: String, to :: String, amount :: Int}
        -> Array {from :: String, to :: String, amount :: Int}
compute = map sortFromTo >>> group >>> map fold >>> map unsortFromTo
  where sortFromTo {from, to, amount}
          | from < to = {from: from, to: to,   amount:        amount}
          | otherwise = {from: to,   to: from, amount: negate amount}
        unsortFromTo {from, to, amount}
          | amount > 0 = {from: from, to: to,   amount:        amount}
          | otherwise  = {from: to,   to: from, amount: negate amount}
        group =     Array.sortBy (\a b -> compare a.from b.from <> compare a.to b.to)
                >>> Array.groupBy (\a b -> a.from == b.from && a.to == b.to)
        fold xs = case Array.head xs of
                    Just {from, to} -> xs # map _.amount # sum # {from, to, amount: _}
                    Nothing -> unsafeCrashWith "groupBy returns empty element"

render :: Array {from :: String, to :: String, amount :: Int} -> Body
render _ = HTML ""
