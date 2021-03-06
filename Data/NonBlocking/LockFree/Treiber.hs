{-# LANGUAGE BangPatterns, MagicHash #-}

{-|
Module      : Treiber
Description : Implementation of a Treiber stack.
License     : BSD3
Maintainer  : Julian Sutherland (julian.sutherland10@imperial.ac.uk)

An implementation of Treiber stacks, a lock free stack. Works with any monad that has atomically modificable references.
-}

module Data.NonBlocking.LockFree.Treiber(TreiberStack(), TreiberStackIO, TreiberStackSTM, newTreiberStack, pushTreiberStack, popTreiberStack) where

import Control.Concurrent.STM (STM())
import Control.Concurrent.STM.TVar (TVar())
import Control.Monad(join, when)
import Control.Monad.Loops(whileM_)
import Control.Monad.Ref(MonadAtomicRef, newRef, readRef, writeRef, atomicModifyRef)
import Data.IORef(IORef)
import GHC.Exts (Int(I#))
import GHC.Prim (reallyUnsafePtrEquality#)

-- |TreiberStack inside the 'IO' 'Monad'.
type TreiberStackIO a = TreiberStack IORef a
-- |TreiberStack inside the 'STM' 'Monad'.
type TreiberStackSTM a = TreiberStack TVar a

-- |A lock-free concurrent Treiber stack usable in any 'Monad', m, that is paired with a reference type, r, by an instance of 'MonadAtomicRef'. Can use Specializations 'TreiberStackIO' and 'TreiberStackSTM'.
data TreiberStack r a = TreiberStack (r (TreiberElem r a))
data TreiberElem r a = TreiberElem a (r (TreiberElem r a)) | End

instance Eq (TreiberElem r a) where
  End == End = True
  (TreiberElem x rest1) == (TreiberElem y rest2) = (ptrEq rest1 rest2)

{-# SPECIALIZE newTreiberStack :: IO (TreiberStackIO a) #-}
{-# SPECIALIZE newTreiberStack :: STM (TreiberStackSTM a) #-}
-- |Creates a new empty instance of the 'TreiberStack'. Internally implemented with a reference of type r, which is why they must be atomically modifiable. Initially empty.
newTreiberStack :: (MonadAtomicRef r m) => m (TreiberStack r a)
newTreiberStack = do
  ref <- newRef End
  return (TreiberStack ref)

{-# SPECIALIZE pushTreiberStack :: TreiberStackIO a -> a -> IO () #-}
{-# SPECIALIZE pushTreiberStack :: TreiberStackSTM a -> a -> STM () #-}
-- |Pushes an element on to a 'TreiberStack' in a lock-free manner.
pushTreiberStack :: (MonadAtomicRef r m) => TreiberStack r a -> a -> m ()
pushTreiberStack (TreiberStack x) v = do
  let partConstr = TreiberElem v
  b <- newRef False
  whileM_ (readRef b >>= return . not) $ do
    z <- readRef x
    res <- newRef z
    suc <- cas x z (partConstr res)
    writeRef b suc

{-# SPECIALIZE popTreiberStack :: TreiberStackIO a -> IO (Maybe a) #-}
{-# SPECIALIZE popTreiberStack :: TreiberStackSTM a -> STM (Maybe a) #-}
-- |Pops an element of a 'TreiberStack' in a lock-free manner. Returns 'Nothing' if the stack is empty.
popTreiberStack :: (MonadAtomicRef r m) => TreiberStack r a -> m (Maybe a)
popTreiberStack (TreiberStack x) = do
  b <- newRef False
  ret <- newRef Nothing
  whileM_ (readRef b >>= return . not) $ do
    y <- readRef x
    case y of
      End -> writeRef b True
      (TreiberElem elem rest) -> do
        z <- readRef rest
        suc <- cas x y z
        t <- readRef x
        writeRef b suc
        when suc $ writeRef ret (Just elem)
  readRef ret

{-# SPECIALIZE cas :: (Eq a) => IORef a -> a -> a -> IO Bool #-}
{-# SPECIALIZE cas :: (Eq a) => TVar a -> a -> a -> STM Bool #-}
cas :: (MonadAtomicRef r m, Eq a) => r a -> a -> a -> m Bool
cas ref comp rep = atomicModifyRef ref (\val -> let b = val == comp in (if b then rep else val, b))

{-# NOINLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1
