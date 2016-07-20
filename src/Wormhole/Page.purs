module Wormhole.Page
( Page
, Body(..)

, Namespace, makeNamespace, runNamespace
, Slug, makeSlug, runSlug

, locate
, Locator
) where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(Nothing), maybe)
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

type Page = { title :: String
            , body  :: Body
            }

data Body
  = HTML String
  | InternalRedirect Namespace Slug
  | ExternalRedirect String

newtype Namespace = Namespace String
makeNamespace = slugify >>> Namespace
runNamespace (Namespace s) = s
derive instance eqNamespace :: Eq Namespace
derive instance ordNamespace :: Ord Namespace
instance arbitraryNamespace :: Arbitrary Namespace where arbitrary = makeNamespace <$> arbitrary

newtype Slug = Slug String
makeSlug = slugify >>> Slug
runSlug (Slug s) = s
derive instance eqSlug :: Eq Slug
derive instance ordSlug :: Ord Slug
instance arbitrarySlug :: Arbitrary Slug where arbitrary = makeSlug <$> arbitrary

foreign import slugify :: String -> String

locate :: forall eff. (Namespace -> Maybe (Locator eff)) -> Namespace -> Locator eff
locate ll ns s = maybe (pure Nothing) (_ $ s) (ll ns)

type Locator eff = Slug -> Eff eff (Maybe Page)
