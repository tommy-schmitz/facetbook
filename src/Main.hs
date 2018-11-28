{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Control.Applicative
import Control.Monad(liftM, ap)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Data.Monoid((<>))
import Data.List(find)
import Data.String

import Web.Scotty

type User = String
type Permissions = [User]
data Label =
    Whitelist [User]
  | Bot
  deriving (Show, Eq)
leq :: Label -> Label -> Bool
leq Bot            _               = True
leq _              Bot             = False
leq (Whitelist us) (Whitelist [u]) = find (u==) us /= Nothing
leq k1             k2              = k1 == k2

data Fac a where
  Raw     ::                         a      -> Fac a
  Fac     :: Label -> Fac a ->     Fac a    -> Fac a
  BindFac ::          Fac a -> (a -> Fac b) -> Fac b
instance Functor Fac where
  fmap = liftM
instance Applicative Fac where
  pure  = return
  (<*>) = ap
instance Monad Fac where
  return = Raw
  (>>=) = BindFac
-- Unsafe function
project :: Label -> Fac a -> a
project k1 = g where
  g :: Fac a -> a
  g (Raw a) =
    a
  g (Fac k2 fa1 fa2) =
    if leq k2 k1 then
      g fa1
    else
      g fa2
  g (BindFac fb1 c) =
    g (c (g fb1))

-------------------------------
--  Above is in TCB          --
-------------------------------
--  Below may not be in TCB  --
-------------------------------

data PostList =
    Nil
  | Cons (Fac String) (Fac PostList)

flatten :: Fac PostList -> Fac [String]
flatten fpl = do  --Fac
  pl <- fpl
  case pl of
    Nil ->
      Raw []
    Cons fs fpl -> do  --Fac
      s <- fs
      ss <- flatten fpl
      Raw (s:ss)

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

display_redirect_page username =
  html $ "\
    \<meta http-equiv=\"refresh\" content=\"0; url="<> escape username <>"\" />\
    \"
display_main_page username d =
  html $ "\
    \Hello, "<> escape username <>"\
    \<form method=\"post\" action=\"/\">\
    \  Username: <input name=\"username\" value=\""<> escape username <>"\"></input><br />\
    \  Permissions: <input name=\"permissions\"></input><br />\
    \  Content:<br />\
    \  <textarea name=\"content\"></textarea><br />\
    \  <input type=\"submit\"></input><br />\
    \</form>\
    \<br />\
    \Your view of the database:<br />\
    \"<> escape (show d) <> "<br /><br />\
    \"

-------------------------------
--  Above may not be in TCB  --
-------------------------------
--  Below is in TCB          --
-------------------------------

main = do  --IO
  database <- newIORef (Raw Nil)
  scotty 3000 $ do  --Scotty
    get "/" $ do  --ScottyIO
      html $ "\
        \Please log in by typing your username at the end of the URL.\
        \"
    post "/" $ do  --ScottyIO
      username <- param "username"
      p <- param "permissions"
      let permissions = username : words p
      content <- param "content"
      fpl <- lift $ readIORef database
      lift $ writeIORef database $ Fac (Whitelist permissions) (Raw (Cons (Raw content) fpl)) fpl
      display_redirect_page username
    get "/:username" $ do  --ScottyIO
      username <- param "username"
      fpl <- lift $ readIORef database
      let d = project (Whitelist [username]) (flatten fpl)
      display_main_page username d
