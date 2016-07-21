module Wormhole.Page.Location.Groceries
( locate
) where

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Iff (Iff)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
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
compute = id

render :: Array {from :: String, to :: String, amount :: Int} -> Body
render _ = HTML ""
