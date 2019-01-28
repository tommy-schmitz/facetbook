{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Control.Applicative
import Control.Monad(liftM, ap)
import Data.ByteString.Char8(unpack)
import Data.IORef
import Data.List(find)
import Data.String(fromString)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))

type Post = String
type User = String

data Label =
    Whitelist [User]
  | Bot
  deriving (Show, Eq)
leq :: Label -> Label -> Bool
leq Bot            _               = True
leq _              Bot             = False
leq _              (Whitelist [])  = True
leq (Whitelist []) _               = False
leq (Whitelist us) (Whitelist [u]) = find (u==) us /= Nothing
leq k1             k2              = k1==k2

data Fac a where
  Undefined :: Fac a
  Raw :: a -> Fac a
  Fac :: Label -> Fac a -> Fac a -> Fac a
  BindFac :: Fac a -> (a -> Fac b) -> Fac b
instance Functor Fac where
  fmap = liftM
instance Applicative Fac where
  pure  = return
  (<*>) = ap
instance Monad Fac where
  return = Raw
  (>>=) = BindFac

data FIORef a =
  FIORef (IORef (Fac a))

data FIO a where
  Return     :: a -> FIO a
  BindFIO    :: FIO a -> (a -> FIO b) -> FIO b
  Swap       :: Fac (FIO a) -> FIO (Fac a)
  IO         :: IO a -> FIO a                   -- Potentially unsafe, use with care
  New        :: a -> FIO (FIORef a)
  Read       :: FIORef a -> FIO (Fac a)
  Write      :: FIORef a -> Fac a -> FIO ()

instance Functor FIO where
  fmap = liftM
instance Applicative FIO where
  pure  = return
  (<*>) = ap
instance Monad FIO where
  return = Return
  (>>=) = BindFIO

data PC =
    UpwardClosure Label
  | Singleton Label
  | Everything

ffacet :: PC -> Fac a -> Fac a -> Fac a
ffacet pc a b =
  case pc of
    Everything      -> a
    UpwardClosure k -> Fac k a b
    Singleton k     -> undefined

consistentWithAdding pc k =
  case pc of
    Everything -> True
    UpwardClosure k' -> True
    Singleton k' -> leq k k'
consistentWithSubtracting pc k =
  case pc of
    Everything -> k /= Bot
    UpwardClosure k' -> not (leq k k')
    Singleton k' -> not (leq k k')

runFIO :: PC -> FIO a -> IO a
runFIO pc x = z x where
  z :: FIO a -> IO a
  z x =
    case x of
      Return a ->
        return a
      BindFIO a b -> do  --IO
        c <- z a
        z (b c)
      Swap Undefined ->
        return Undefined
      Swap (Raw a) -> do  --IO
        b <- z a
        return (Raw b)
      Swap (BindFac Undefined b) ->
        return Undefined
      Swap (BindFac (Raw a) b) ->
        z (Swap (b a))
      Swap (BindFac (Fac k a b) c) ->
        z (Swap (Fac k (BindFac a c) (BindFac b c)))
      Swap (BindFac (BindFac a b) c) ->
        z (Swap (BindFac a (\d -> BindFac (b d) c)))
      Swap (Fac k a b) -> do  --IO
        if pc `consistentWithAdding` k then
          z (Swap a)
        else if pc `consistentWithSubtracting` k then
          z (Swap b)
        else
          undefined
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

-- This is the password-checking function.
-- Currently, it takes the username from the URL parameters.
-- Currently, it always succeeds without any password.
check_credentials request =
  case lookup "username" (WAI.queryString request) of
    Just (Just username) ->
      Just username
    _ ->
      Nothing

type App a = FIORef a -> WAI.Request -> (WAI.Response -> FIO ()) -> FIO ()

run_server :: Int -> App (FList Post) -> IO ()
run_server port app = do  --IO
  database <- runFIO Everything $ New Nil
  Warp.run port $ \request respond -> do  --IO
    let fio_respond = \x -> IO $ do  --IO
         respond x
         return ()
    let handle pc = do  --IO
         runFIO pc (app database request fio_respond)
         return ResponseReceived
    if WAI.pathInfo request == ["login"] then
      handle Everything
    else
      case check_credentials request of
        Nothing ->
          handle Everything
        Just username ->
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  handle (UpwardClosure (Whitelist (unpack username : words (unpack permissions))))
                _ ->
                  handle (Singleton (Whitelist [unpack username]))
            ["read-all-posts"] ->
              handle (Singleton (Whitelist [unpack username]))
            _ ->
              handle (Singleton (Whitelist [unpack username]))

main = run_server 3000 facetbook

--------------------------------
--  Above is in the TCB.      --
--------------------------------
--  Below is not in the TCB.  --
--------------------------------

data FList a =
    Nil
  | Cons a (Fac (FList a))

flatten :: Fac (FList a) -> Fac [a]
flatten ffl = do  --Fac
  fl <- ffl
  case fl of
    Nil ->
      Raw []
    Cons x ffl -> do  --Fac
      xs <- flatten ffl
      Raw (x:xs)

-- "facetbook" code must not use "runFIO" or "IO :: IO a -> FIO a".
-- This can be enforced using Haskell's module system.
facetbook :: App (FList Post)
facetbook database request respond = do  --FIO
  let headers = [("Content-Type", "text/html")]
  if WAI.pathInfo request == ["login"] then
    respond $ WAI.responseLBS status200 headers "boring login page"
  else
    case check_credentials request of
      Nothing ->
        respond $ WAI.responseLBS status403 headers "bad credentials"
      Just username ->
        case WAI.pathInfo request of
          ["post"] ->
            case lookup "content" (WAI.queryString request) of
              Just (Just p) -> do  --FIO
                case lookup "permissions" (WAI.queryString request) of
                  Just (Just permissions) -> do  --FIO
                    d <- Read database
                    let d' = Raw $ Cons (unpack p) d
                    Write database d'
                    respond $ WAI.responseLBS status200 headers "post successful"
                  _ ->
                    respond $ WAI.responseLBS status400 headers "bad post (missing permissions)"
              _ ->
                respond $ WAI.responseLBS status400 headers "bad post (missing content)"
          ["read-all-posts"] -> do  --FIO
            d <- Read database
            Swap $ do  --Fac
              all_posts <- flatten d
              Raw $ do  --FIO
                respond $ WAI.responseLBS status200 headers $ escape (show all_posts)
            return ()
          _ ->
            respond $ WAI.responseLBS status404 headers "bad request"

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])
