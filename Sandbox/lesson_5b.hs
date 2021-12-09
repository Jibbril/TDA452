import Control.Applicative (Applicative(..))
import Control.Monad(liftM,ap)

-- instance Functor MyMonad where fmap = liftM
-- instance Applicative MyMonad where
--   pure  = return
--   (<*>) = ap

{-
  a <- act  <=> act >>= \a -> do rest
  do act    <=> act
  (>>=) :: m a -> (a -> m b) -> m b
-}

-- 3 Equivalent (prob wrong)
-- take10 = do 
--   filename <- getLine
--   contents <- readFile
--   putStr
-- take10' = getLine >>= \filename -> readFile filename >>= \contents -> putStr (take 10 contents)
-- take10''= getLine >>= readFile >>= putStr . take 10


-- Maybe implementation
-- instance Monad Maybe' where
--   Just x  >>= f = f x
--   Nothing >>= _ = Nothing
--   return        = Just
--   fail s        = Nothing