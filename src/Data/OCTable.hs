module Data.OCTable where
    import Control.Monad
    import Control.Monad.Trans
    
    newtype OCTableT a m b = OCTableT {
        runOCTableT::([a], [a])->m ([a], [a], b)
    }
    
    instance Monad m => Functor (OCTableT a m) where
        fmap f t = do
            x <- t
            return $ f x

    instance Monad m => Applicative (OCTableT a m) where
        (<*>) = ap
        pure = return

    instance Monad m=>Monad (OCTableT a m) where
        return x = OCTableT (\(o, c)->return (o, c, x))
        lhs >>= rhs = OCTableT f where
            f (o, c) = do
                (o', c', r') <- runOCTableT lhs (o, c)
                runOCTableT (rhs r') (o', c')
                
    instance MonadTrans (OCTableT a) where
        lift m = OCTableT (\(o, c) -> do
            x <- m
            return (o, c, x)
            )
            
    popTaskT::(Eq a, Monad m)=>OCTableT a m a
    popTaskT = OCTableT f where
        f ([], c) = undefined
        f ((h:xs), c) = return (xs, h:c, h)
        
    pushTaskT::(Eq a, Monad m)=>a->OCTableT a m ()
    pushTaskT x = OCTableT f where
        f (o, c) = if x `elem` c || x `elem` o then
                return (o, c, ())
            else
                return (x:o, c, ())
        
    untilExhaustedT::[a]->OCTableT a IO ()->IO ()
    untilExhaustedT o t = untilExhaustedT' o [] t
    untilExhaustedT'::[a]->[a]->OCTableT a IO ()->IO ()
    untilExhaustedT' [] _ _ = return ()
    untilExhaustedT' o@(h:xs) c t = do
        (o', c', r') <- runOCTableT t (o, c)
        return r' >> untilExhaustedT' o' c' t
    