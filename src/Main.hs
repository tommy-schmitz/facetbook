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

type Post = String
type User = String

data LatticeState = LS [(User, User)]
data Label =
    Top
  | U User
  | Bot
  deriving Show
leq :: LatticeState -> Label -> Label -> Bool
leq s Bot    _      = True
leq s _      Bot    = False
leq s _      Top    = True
leq s Top    _      = False
leq s (U u1) (U u2) =
  let LS edges = s  in
  find ((u1,u2)==) edges /= Nothing

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
project :: LatticeState -> Label -> Fac a -> a
project lattice k1 = g where
  g :: Fac a -> a
  g (Raw a) =
    a
  g (Fac k2 fa1 fa2) =
    if leq lattice k2 k1 then
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
    \<meta http-equiv=\"refresh\" content=\"0; url=/read-all-posts/"<> escape username <>"\" />\
    \"
display_main_page username d =
  html $ "\
    \Hello, "<> escape username <>"\
    \<form method=\"post\" action=\"/add-friend\">\
    \  Username: <input name=\"username\" value=\""<> escape username <>"\"></input><br />\
    \  Add friend: <input name=\"friend\" value=\"\"></input><br />\
    \  <input type=\"submit\"></input><br />\
    \</form>\
    \<hr />\
    \<form method=\"post\" action=\"/remove-friend\">\
    \  Username: <input name=\"username\" value=\""<> escape username <>"\"></input><br />\
    \  Remove friend: <input name=\"enemy\" value=\"\"></input><br />\
    \  <input type=\"submit\"></input><br />\
    \</form>\
    \<hr />\
    \<form method=\"post\" action=\"/post\">\
    \  Username: <input name=\"username\" value=\""<> escape username <>"\"></input><br />\
    \  New post:<br /><textarea name=\"content\"></textarea><br />\
    \  <input type=\"submit\"></input><br />\
    \</form>\
    \<hr />\
    \Your view of the database:<br />\
    \"<> escape (show d) <> "<br /><br />\
    \"

-------------------------------
--  Above may not be in TCB  --
-------------------------------
--  Below is in TCB          --
-------------------------------

type FDatabase = (Fac (FList Post), LatticeState)

main = do  --IO
  database <- newIORef ((Raw Nil, LS []) :: FDatabase)
  scotty 3000 $ do  --Scotty
    post "/add-friend" $ do  --ScottyIO
      username <- param "username"
      friend <- param "friend"
      (posts, LS lattice) <- lift $ readIORef database
      let new_lattice = (username, friend) : lattice
      database <- lift $ writeIORef database (posts, LS new_lattice)
      display_redirect_page username
    post "/remove-friend" $ do  --ScottyIO
      username <- param "username"
      enemy <- param "enemy"
      (posts, LS lattice) <- lift $ readIORef database
      let new_lattice = filter (/= (username, enemy)) lattice
      database <- lift $ writeIORef database (posts, LS new_lattice)
      display_redirect_page username
    post "/post" $ do  --ScottyIO
      username <- param "username"
      content <- param "content"
      (posts, lattice) <- lift $ readIORef database
      let new_posts = Fac (U username) (Raw (Cons content posts)) posts
      lift $ writeIORef database (new_posts, lattice)
      display_redirect_page username
    get "/read-all-posts/:username" $ do  --ScottyIO
      username <- param "username"
      (flist, lattice) <- lift $ readIORef database
      let posts = project lattice (U username) (flatten flist)
      display_main_page username posts

type Database = [(Label, String)]

filter_database :: String -> Database -> [String]
filter_database username db =
  case db of
    [] ->
      []
    (k, s) : db_tail ->
      let filtered_tail = filter_database username db_tail  in
      if k `leq` Whitelist [username] then
        s : filtered_tail
      else
        filtered_tail

main = do  --IO
  database <- newIORef []
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
      db <- lift $ readIORef database
      let new_db = (Whitelist permissions, content) : db
      lift $ writeIORef database new_db
      display_redirect_page username
    get "/:username" $ do  --ScottyIO
      username <- param "username"
      db <- lift $ readIORef database
      let posts = filter_database username db
      display_main_page username posts
