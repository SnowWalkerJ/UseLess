{-# LANGUAGE BangPatterns #-}
module Data.Queue where
    import Control.Monad
    import Control.Monad.Trans
    
    data Queue a b = Queue {
        wrapped::[a]->([a], b)
    }

    instance Functor (Queue a) where
        fmap f q = do
            s <- q
            return $ f s

    instance Applicative (Queue a) where
        pure = return
        m1 <*> m2 = do
            func <- m1
            val <- m2
            return $ func val

    instance Monad (Queue a) where
        return x = Queue (\a->(a, x))
        m1 >>= m2 = Queue func where
            func1 = wrapped m1
            func lst = func2 lst' where
                (lst', result1) = func1 lst
                func2 = (wrapped (m2 result1))

    push::a->Queue a ()
    push (!a) = Queue (\lst->(a:lst, ())) 
    
    pop::Queue a a
    pop = Queue (\(h:xs)->(xs, h))

    untilExhausted::[a]->Queue a (IO ())->IO ()
    untilExhausted lst q =
        let func = wrapped q
            (lst', result) = func lst
        in case lst' of
            [] -> result
            _  -> result >> untilExhausted lst' q
            
    runOnce::[a]->Queue a (IO ())->IO ()
    runOnce lst q = 
        let func = wrapped q
            (lst', result) = func lst
        in result