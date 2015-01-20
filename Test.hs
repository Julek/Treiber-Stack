import Data.NonBlocking.LockFree.Treiber

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Data.IORef
import Data.Maybe
import System.Random

main :: IO ()
main = do
  stck <- newTreiberStack
  stp <- newIORef False
  done <- newIORef False
  forkIO (writer stck stp 10)
  forkIO (writer stck stp 10)
  forkIO (reader stck stp done)
  whileM_ (fmap not $ readIORef done) yield

reader :: TreiberStack IORef Int -> IORef Bool -> IORef Bool -> IO ()
reader stck stp done = do
  gotNothing <- newIORef False
  whileM_ (readIORef stp >>= \wd -> readIORef gotNothing >>= \gn -> return . not $ (wd && gn)) $ do
    val <- popTreiberStack stck
    when (isNothing val) (writeIORef gotNothing True)
    print val
  writeIORef done True

writer :: TreiberStack IORef Int -> IORef Bool -> Int -> IO ()
writer stck stp 0 = writeIORef stp True
writer stck stp val = do
  print val  
  pushTreiberStack stck val
  writer stck stp (val - 1)
  
