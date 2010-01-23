module Control.Monad.STLike.Internal where

import Control.DeepSeq
import Control.Monad
import Foreign

-- | Regioned variables.
-- A regioned variable is /safe/ i.e. no references to
-- it may escape the current IOS.
newtype Regioned s t = R t

instance Monad (Regioned s) where
    return      = R
    _     >>  b = b
    (R v) >>= f = f v
instance Functor (Regioned s) where
    fmap f (R v) = R (f v)

-- | Run a computation on regioned data
-- and return the result in a strict fashion.
runRegion :: (NotShared ty, STLikeImpl m) => Regioned s ty -> STLike m s ty
runRegion = runRegionImpl


class NFData ty => NotShared ty where
    runRegionImpl :: STLikeImpl m => Regioned s ty -> STLike m s ty
    runRegionImpl (R v) = v `deepseq` return v

instance NotShared Bool
instance NotShared Char
instance NotShared Double
instance NotShared Float
instance NotShared Int
instance NotShared Int8
instance NotShared Int16
instance NotShared Int32
instance NotShared Int64
instance NotShared Integer
instance NotShared Word
instance NotShared Word8
instance NotShared Word16
instance NotShared Word32
instance NotShared Word64
instance NotShared ()
--instance NotShared IntSet
instance NotShared a => NotShared ([] a)
--instance (Integral a, NotShared a) => NotShared (Ratio a)
--instance (RealFloat a, NotShared a) => NotShared (Complex a)
instance NotShared a => NotShared (Maybe a)
--NotShared a => NotShared (IntMap a)
--NotShared a => NotShared (Tree a)
--NotShared a => NotShared (Set a)
instance (NotShared a, NotShared b) => NotShared (Either a b)
instance (NotShared a, NotShared b) => NotShared ((,) a b)
--(Ix a, NotShared a, NotShared b) => NotShared (Array a b)
--(NotShared k, NotShared a) => NotShared (Map k a)
instance (NotShared a, NotShared b, NotShared c) => NotShared ((,,) a b c)
instance (NotShared a, NotShared b, NotShared c, NotShared d) => NotShared ((,,,) a b c d)
instance (NotShared a1, NotShared a2, NotShared a3, NotShared a4, NotShared a5) => NotShared ((,,,,) a1 a2 a3 a4 a5)
--instance NotShared B.ByteString where
--    runRegion (R bs) = return $! B.copy bs


unsafeRemoveRegion :: STLikeImpl m => Regioned s r -> STLike m s r
unsafeRemoveRegion (R x) = return x

newtype STLike m s t = STLike (m t)

class Monad m => STLikeImpl m

instance STLikeImpl m => Monad (STLike m s) where
    (STLike a) >> (STLike b)   = STLike (a >> b)
    (STLike a) >>= b  = STLike $ do v <- a
                                    let STLike r = b v
                                    r
    return x = STLike (return x)

instance (STLikeImpl m) => Functor (STLike m s) where
    fmap f (STLike m) = STLike (liftM f m)

