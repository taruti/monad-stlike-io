{-# OPTIONS_HADDOCK prune #-}
-- | Unsafe interface for implementators.
--   Purposefully not documented.
module Control.Monad.STLike.Unsafe
    (STLike(..), STLikeImpl, NotShared(..), unsafeRemoveRegion
    ) where

import Control.Monad.STLike.Internal