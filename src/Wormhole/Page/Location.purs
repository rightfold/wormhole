module Wormhole.Page.Location
( locate
) where

import Data.Maybe (Maybe(Nothing), maybe)
import Prelude

locate :: forall namespace slug f page
        . (Applicative f)
       => (namespace -> Maybe (slug -> f (Maybe page)))
       -> namespace -> slug -> f (Maybe page)
locate ll ns s = maybe (pure Nothing) (_ $ s) (ll ns)
