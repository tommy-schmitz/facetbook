{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Control.Applicative
import Control.Concurrent(forkIO)
import Control.Monad(liftM, ap)
import Data.ByteString.Char8(unpack)
import Data.IORef
import Data.Monoid((<>))
import Data.List(find)
import Data.String

import qualified Network.Wai.Handler.Warp as Warp (run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI

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

data FIORef a =
  FIORef (IORef [(Label, a)])

-- This FIO monad implements Trapeze (the serverless faceted system).
-- It's quite different from our usual FIO monad.
-- There is no Faceted monad anymore.
data FIO a where
  Return     :: a -> FIO a
  Bind       :: FIO a -> (a -> FIO b) -> FIO b
  IO         :: IO a -> FIO a                   -- Potentially unsafe, use with care
  RaiseLabel :: Label -> FIO a -> FIO ()
  New        :: a -> FIO (FIORef a)
  Read       :: FIORef a -> FIO a
  --Read       :: FIORef a -> FIO [(Label,a)]
  Write      :: FIORef a -> a -> FIO ()

instance Functor FIO where
  fmap = liftM
instance Applicative FIO where
  pure  = return
  (<*>) = ap
instance Monad FIO where
  return = Return
  (>>=) = Bind

runFIO :: Label -> FIO a -> IO a
runFIO k fa = case fa of
  Return a -> do  --IO
    return a
  Bind fb c -> do  --IO
    b <- runFIO k fb
    runFIO k (c b)
  IO ia ->
    ia
  RaiseLabel k' fa ->
    if leq k k' then do  --IO
      forkIO (runFIO k' fa >> return ())
      return ()
    else
      return ()
  New b -> do  --IO
    r <- newIORef [(k, b)]
    return (FIORef r)
  Read (FIORef r) -> do  --IO
    list <- readIORef r
    return (list_last list k)
  Write (FIORef r) b -> do  --IO
    list <- readIORef r
    writeIORef r (list_write list k b)

-- Helpers for runFIO
list_last :: [(Label, a)] -> Label -> a
list_last [] k =
  undefined
  --[]
list_last ((k',a):list) k =
  if leq k' k then
    a
    --(k',a):list_last list k
  else
    list_last list k
list_remove :: [(Label, a)] -> Label -> [(Label, a)]
list_remove [] k =
  []
list_remove ((k',a):list) k =
  if leq k k' then
    list_remove list k
  else
    (k',a) : list_remove list k
list_write :: [(Label, a)] -> Label -> a -> [(Label, a)]
list_write list k a =
  (k,a) : list_remove list k

-- This is the password-checking function.
-- Currently, it takes the username from the URL parameters.
-- Currently, it always succeeds without any password.
check_credentials request =
  case lookup "username" (WAI.queryString request) of
    Just (Just username) ->
      Just username
    _ ->
      Nothing

type App a token = FIORef a -> WAI.Request -> (WAI.Response -> FIO token) -> FIO token

run_server :: Int -> App (FIORef (FList Post)) WAI.ResponseReceived -> IO ()
run_server port app = do  --IO
  database <- runFIO Bot $ do  --FIO
    r <- New Nil
    New r
  Warp.run port $ \request respond -> do  --IO
    let fio_respond = \x -> IO $ respond x
    let handle k = runFIO k (app database request fio_respond)
    if WAI.pathInfo request == ["login"] then
      handle Bot
    else
      case check_credentials request of
        Nothing ->
          handle Bot
        Just username ->
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  handle (Whitelist (unpack username : words (unpack permissions)))
                _ ->
                  handle (Whitelist [unpack username])
            ["read-all-posts"] ->
              handle (Whitelist [unpack username])
            _ ->
              handle (Whitelist [unpack username])

main = run_server 3000 facetbook

--------------------------------
--  Above is in the TCB.      --
--------------------------------
--  Below is not in the TCB.  --
--------------------------------

data FList a =
    Nil
  | Cons a (FIORef (FList a))

-- "facetbook" code must not use "runFIO" or "IO".
-- This can be enforced using Haskell's module system.
facetbook :: App (FIORef (FList Post)) token
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
                  Just (Just permissions) -> do
                    r <- Read database
                    d' <- New (Cons (unpack p) r)
                    Write database d'
                    respond $ WAI.responseLBS status200 headers "post successful"
                  _ ->
                    respond $ WAI.responseLBS status400 headers "bad post (missing permissions)"
              _ ->
                respond $ WAI.responseLBS status400 headers "bad post (missing content)"
          ["read-all-posts"] -> do
            let loop flist =
                 case flist of
                   Nil -> do  --FIO
                     return []
                   Cons p r -> do  --FIO
                     d <- Read r
                     ps <- loop d
                     return (p : ps)
            all_posts <- do  --FIO
              r <- Read database
              flist <- Read r
              loop flist
            respond $ WAI.responseLBS status200 headers $ escape (show all_posts)
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
