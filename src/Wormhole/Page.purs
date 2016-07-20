module Wormhole.Page
( Page
, Body(..)

, Namespace, makeNamespace, runNamespace
, Slug, makeSlug, runSlug
) where

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
