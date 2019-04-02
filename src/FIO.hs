{-# LANGUAGE GADTs #-}
module FIO where
import Control.Monad(liftM, ap)
import Data.IORef
import Data.Either(partitionEithers)
import Data.List(foldl')
class Lattice a where
  leq :: a -> a -> Bool
  lub :: a -> a -> a
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
  IO         :: l -> IO a -> FIO l a
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
data PC l = PC [l] [l]
subsumes (PC ks1 ks2) k =
  all (`leq` k) ks1
  &&  not (any (`leq` k) ks2)
ffacet :: PC l -> Fac l a -> Fac l a -> Fac l a
ffacet pc a b =
  case pc of
    PC [] []       -> a
    PC (k:ks1) []  -> Fac k (ffacet (PC ks1 []) a b) b
    PC ks1 (k:ks2) -> Fac k b (ffacet (PC ks1 ks2) a b)
inconsistent (PC ks1 ks2) =
  let l_c = foldl' lub bot ks1  in
  any (`leq` l_c) ks2
plus (PC ks1 ks2) k =
  PC (k : ks1) ks2
minus (PC ks1 ks2) k =
  PC ks1 (k : ks2)
runFIO :: Lattice l => PC l -> FIO l a -> IO a
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
           if inconsistent (pc `plus` k) then
             BindFac b c
           else if inconsistent (pc `minus` k) then
             BindFac a c
           else
             Fac k (BindFac a c) (BindFac b c)       in
      runFIO pc (Swap fv)
    Swap (BindFac (BindFac a b) c) ->
      runFIO pc (Swap (BindFac a (\d -> BindFac (b d) c)))
    Swap (Fac k a b) -> do  --IO
      if inconsistent (pc `plus` k) then
        runFIO pc (Swap b)
      else if inconsistent (pc `minus` k) then
        runFIO pc (Swap a)
      else do  --IO
        a' <- runFIO (pc `plus`  k) (Swap a)
        b' <- runFIO (pc `minus` k) (Swap b)
        return (Fac k a' b')
    IO k ia ->
      if pc `subsumes` k then
        ia
      else
        return undefined
    New a -> do  --IO
      b <- newIORef (ffacet pc (Raw a) Undefined)
      return (FIORef b)
    Read (FIORef a) -> do  --IO
      readIORef a
    Write (FIORef a) b -> do  --IO
      c <- readIORef a
      writeIORef a (ffacet pc b c)
