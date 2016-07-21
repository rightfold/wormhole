module Wormhole.Page.Location
( locate
) where

import Control.Plus (empty, class Plus)
import Data.Maybe (Maybe, maybe)
import Prelude

locate :: forall namespace slug f m page
        . (Applicative f, Plus m)
       => (namespace -> Maybe (slug -> f (m page)))
       -> namespace -> slug -> f (m page)
locate ll ns s = maybe (pure empty) (_ $ s) (ll ns)
