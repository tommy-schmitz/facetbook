{-# LANGUAGE GADTs #-}
module FIO where

{-
import Control.Applicative
import Data.ByteString.Char8(unpack)
import Data.List(find)
import Data.String(fromString)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Control.Monad(liftM, ap)
import Data.IORef

class Lattice a where
  leq :: a -> a -> Bool
  bot :: a

data Fac l a where
  Undefined :: Fac l a
  Raw :: a -> Fac l a
  Fac :: l -> Fac l a -> Fac l a -> Fac l a
  BindFac :: Fac l a -> (a -> Fac l b) -> Fac l b
instance Functor (Fac l) where
  fmap = liftM
instance Applicative (Fac l) where
  pure  = return
  (<*>) = ap
instance Monad (Fac l) where
  return = Raw
  (>>=) = BindFac

data FIORef l a =
  FIORef (IORef (Fac l a))

data FIO l a where
  Return     :: a -> FIO l a
  BindFIO    :: FIO l a -> (a -> FIO l b) -> FIO l b
  Swap       :: Fac l (FIO l a) -> FIO l (Fac l a)
  IO         :: IO a -> FIO l a                   -- Potentially unsafe, use with care
  New        :: a -> FIO l (FIORef l a)
  Read       :: FIORef l a -> FIO l (Fac l a)
  Write      :: FIORef l a -> Fac l a -> FIO l ()

instance Functor (FIO l) where
  fmap = liftM
instance Applicative (FIO l) where
  pure  = return
  (<*>) = ap
instance Monad (FIO l) where
  return = Return
  (>>=) = BindFIO

data PC l =
    UpwardClosure l
  | Singleton l
  | AllViews

ffacet :: PC l -> Fac l a -> Fac l a -> Fac l a
ffacet pc a b =
  case pc of
    AllViews        -> a
    UpwardClosure k -> Fac k a b
    Singleton k     -> error "one"

consistentWithAdding pc k =
  case pc of
    AllViews -> True
    UpwardClosure k' -> True
    Singleton k' -> leq k k'
consistentWithSubtracting pc k =
  case pc of
    AllViews -> k /= bot
    UpwardClosure k' -> not (leq k k')
    Singleton k' -> not (leq k k')

runFIO :: (Eq l, Lattice l) => PC l -> FIO l a -> IO a
runFIO pc x =
  case x of
    Return a ->
      return a
    BindFIO a b -> do  --IO
      c <- runFIO pc a
      runFIO pc (b c)
    Swap Undefined ->
      return Undefined
    Swap (Raw a) -> do  --IO
      b <- runFIO pc a
      return (Raw b)
    Swap (BindFac Undefined b) ->
      return Undefined
    Swap (BindFac (Raw a) b) ->
      runFIO pc (Swap (b a))
    Swap (BindFac (Fac k a b) c) ->
      let fv =
           if not (pc `consistentWithSubtracting` k) then
             BindFac a c
           else if not (pc `consistentWithAdding` k) then
             BindFac b c
           else
             Fac k (BindFac a c) (BindFac b c)             in
      runFIO pc (Swap fv)
    Swap (BindFac (BindFac a b) c) ->
      runFIO pc (Swap (BindFac a (\d -> BindFac (b d) c)))
    Swap (Fac k a b) -> do  --IO
      if not (pc `consistentWithSubtracting` k) then
        runFIO pc (Swap a)
      else if not (pc `consistentWithAdding` k) then
        runFIO pc (Swap b)
      else
        error "two"
    IO ia -> do
      a <- ia
      return a
    New a -> do  --IO
      b <- newIORef (ffacet pc (Raw a) Undefined)
      return (FIORef b)
    Read (FIORef a) -> do  --IO
      readIORef a
    Write (FIORef a) b -> do  --IO
      c <- readIORef a
      writeIORef a (ffacet pc b c)
