module FunctorM where

import Array

class FunctorM f where
    fmapM :: Monad m => (a -> m b) -> f a -> m (f b)


instance FunctorM [] where
    fmapM f xs = mapM f xs

instance FunctorM Maybe where
    fmapM _ Nothing = return Nothing
    fmapM f (Just x) = f x >>= return . Just 

instance Ix i => FunctorM (Array i) where
    fmapM f a = sequence [ f e >>= return . (,) i | (i,e) <- assocs a] >>= return . array b  where
        b = bounds a


