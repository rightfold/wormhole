module Wormhole.Page
( Page
, Body(..)

, Namespace, makeNamespace, runNamespace
, Slug, makeSlug, runSlug

, locate
, Locator
) where

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

locate :: forall namespace slug f page
        . (Applicative f)
       => (namespace -> Maybe (slug -> f (Maybe page)))
       -> namespace -> slug -> f (Maybe page)
locate ll ns s = maybe (pure Nothing) (_ $ s) (ll ns)

type Locator f = Slug -> f (Maybe Page)
