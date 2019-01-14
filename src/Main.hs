{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Control.Applicative
import Control.Concurrent(forkIO)
import Control.Monad(liftM, ap)
import Control.Monad.Cont(ContT, runContT)
import Control.Monad.Reader(ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Data.Monoid((<>))
import Data.List(find)
import Data.String

import Web.Scotty
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
leq = undefined

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
list_last ((k',a):list) k =
  if leq k' k then
    a
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
facetbook :: App (FList Post) token
facetbook database request respond = do  --FIO
  if WAI.pathInfo request == ["login"] then
    respond $ WAI.responseLBS status200 [] "boring login page"
  else
    case check_credentials request of
      Nothing ->
        respond $ WAI.responseLBS status403 [] "bad credentials"
      Just username ->
        case WAI.pathInfo request of
          ["post"] ->
            case lookup "content" (WAI.queryString request) of
              Just (Just p) -> do  --FIO
                d <- Read database
                r <- New d
                let d' = Cons (show p) r
                Write database d'
                respond $ WAI.responseLBS status200 [] "post successful"
              _ ->
                respond $ WAI.responseLBS status400 [] "bad post"
          ["read-all-posts"] -> do
            d <- Read database
            let loop flist =
                 case flist of
                   Nil      -> do  --FIO
                     return []
                   Cons p r -> do  --FIO
                     d <- Read r
                     ps <- loop d
                     return (p : ps)
            all_posts <- loop d
            respond $ WAI.responseLBS status200 [] (escape (show all_posts))
          _ ->
            respond $ WAI.responseLBS status404 [] "bad request"

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

--------------------------------
--  Above is not in the TCB.  --
--------------------------------
--  Below is in the TCB.      --
--------------------------------

run_server :: Int -> App (FList Post) WAI.ResponseReceived -> IO ()
run_server port app = do  --IO
  database <- runFIO Bot $ New Nil
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
                  handle (Whitelist (show username : words (show permissions)))
                _ ->
                  handle (Whitelist [show username])
            ["read-all-posts"] ->
              handle (Whitelist [show username])
            _ ->
              handle (Whitelist [show username])

main = run_server 3000 facetbook
