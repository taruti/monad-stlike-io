module Control.Monad.STLike.IO
    (-- * IOS monad
     IOS, io, runIOS
     -- * Regioned monad
    ,Regioned, runRegion, region
     -- * Utilities
    ,RegionMonad, (:<), withRIOR, rbsFromPtr,rbsToBS,withRbsPtr,rbsMapLookup
    ) where

import qualified Control.Exception as E
import Control.Monad.STLike.Internal
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map as M
import Foreign


-- | Monad for scoped IO computations
-- The underlying monad must be strict here.
type IOS s t = STLike IO s t
instance STLikeImpl IO

instance MonadIO (STLike IO s) where
    liftIO x = io x

-- | Lift IO computations into IOS. liftIO also works.
io :: IO t -> IOS s t
io x = STLike x

-- | Run an IOS computation in the IO monad.
runIOS :: (forall s. IOS s t) -> IO t
runIOS x = let STLike v = x in v

{-
-- | Use a resource with IOS. Like /bracket/.
withRIOS :: (forall s. IOS (s :< o) (resource s))            -- ^ Open the resource
            (forall s. (resource s -> IOS (s :< o) ()))      -- ^ Close it.
            (forall s. (resource s -> IOS (s :< o) result))  -- ^ Compute with it.
         -> IOS o result
withRIOS (STLike open, close, work) = STLike (E.bracket open ioclose iowork)
    where iowork  x = let STLike w = work  x in w
          ioclose x = let STLike w = close x in w
-}
-- | Use a resource with IOS. Like /bracket/.
withRIOR :: IOS o resource           -- ^ Open the resource
         -> (resource -> IOS o ())   -- ^ Close it.
         -> (forall s. Regioned s resource -> IOS (s :< o) result) -- ^ Compute with it.
         -> IOS o result
withRIOR (STLike open) close work = STLike (E.bracket open ioclose iowork)
    where iowork  x = let STLike w = work  (R x) in w
          ioclose x = let STLike w = close x in w

-- | Create a ByteString representing the pointer and length. 
--   No copying done, O(1).
rbsFromPtr :: Ptr a -> Int -> IOS s (Regioned s B.ByteString)
rbsFromPtr ptr len = io $ fmap R $ B.unsafePackCStringLen (castPtr ptr,len)

-- | Create a copy of a regioned ByteString as a normal ByteString. O(n).
rbsToBS :: RegionMonad m s reg => Regioned s B.ByteString -> STLike m reg B.ByteString
rbsToBS (R b) = return $! B.copy b

-- | Use a regioned ByteString as a pointer. O(1). 
--   The pointer points to the region contents,
--   so be cafeful with it.
withRbsPtr :: Regioned s B.ByteString -> (Ptr any -> Int -> IOS s t) -> IOS s t
--withRbsPtr (R b) act = io $ B.unsafeUseAsCStringLen b (\(p,l) -> unsafeSTToIO (act (castPtr p) l))
withRbsPtr (R b) act = io $ B.unsafeUseAsCStringLen b (\(p,l) -> let STLike v = (act (castPtr p) l) in v)

-- | Lookup inside a Map with a regioned ByteString.
rbsMapLookup :: (RegionMonad m s reg, Ord key) => Regioned s key -> M.Map key value -> STLike m reg (Maybe value)
rbsMapLookup (R k) m = case M.lookup k m of
                      Just x  -> return (Just x)
                      Nothing -> return Nothing

-- | Regions a value. Synonym for /return/.
region :: t -> Regioned s t
region = R
