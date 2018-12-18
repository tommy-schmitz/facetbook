{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import Control.Applicative
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
import Network.HTTP.Types.Status(status200)
import Network.Wai

-- Database API:
--   CreatePost(Username, Content)
--   AddFriend(Username, Username)
--   RemoveFriend(Username, Username)

type Post = String
type User = String

data LatticeState = LS [(User, User)]
data Label =
    Top
  | U User
  | Bot
  deriving (Show, Eq)
leq :: LatticeState -> Label -> Label -> Bool
leq s Bot    _      = True
leq s _      Bot    = False
leq s _      Top    = True
leq s Top    _      = False
leq s (U u1) (U u2) =
  if u1==u2 then
    True
  else
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

redirect_page username =
  responseLBS status200 [] $ "\
    \<meta http-equiv=\"refresh\" content=\"0; url=/read-all-posts/"<> escape username <>"\" />\
    \"
login_page =
  responseLBS status200 [] "boring login page"
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

-- Faceted section --

type FDatabase = Fac (FList Post)

{-
faceted_handler database username request =
  let req = pathInfo request !! 1  in
  if req == "read-all-posts" then do
    flist <- readIORef database
    posts <- lift $ project lattice (U username) (flatten flist)
    display_main_page username posts
  else if req == "post" then do
    let new_posts = Fac (U username) (Raw (Cons p posts)) posts
    lift $ writeIORef database new_posts
    display_redirect_page username
  else
    undefined

-- This is the password-checker function.
-- Currently, password-checking always succeeds.
check_credentials :: Request -> Maybe Username
check_credentials request =
  Just (pathInfo request !! 0)

main_faceted = do  --IO
  database <- newIORef (Raw Nil)
  Warp.run 3000 $ \request respond -> do  --IO
    if ["login"] == pathInfo request then
      respond login_page
    else
      case check_credentials request of
        Just username ->
          let response = faceted_handler database username request  in
          respond login_page
        Nothing ->
          respond login_page
-}

-- An issue.
-- Should the FIO interface allow running a server inside a server?
-- Answer: no, who cares about that?
-- So then what should the top-level interface be like?
-- Built-in database functionality at the top-level?

main_faceted = do
  Warp.run 3000 $ \request respond -> do
    Warp.run 3001 $ \request respond ->
      respond $ responseLBS status200 [] "3001"
    respond $ responseLBS status200 [] "3000"

{-
  database <- newIORef (Raw Nil :: FDatabase)
  scotty 3000 $ do  --Scotty
    get "login" $ do  --ScottyIO
      display_login_page
    notFound $ do  --ScottyIO
      username <- param "username"
      
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
      posts <- lift $ project lattice (U username) (flatten flist)
      display_main_page username posts
-}

----------------------------

-- Non-faceted section --

{-

type Database = ([(Label, Post)], LatticeState)

filter_database :: User -> Database -> [Post]
filter_database username (posts, lattice) =
  let x = filter (\(k,p) -> leq lattice k (U username)) posts  in
  map snd x

main_nonfaceted = do  --IO
  database <- newIORef ([], LS [])
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
      let new_posts = (U username, content) : posts
      lift $ writeIORef database (new_posts, lattice)
      display_redirect_page username
    get "/read-all-posts/:username" $ do  --ScottyIO
      username <- param "username"
      db <- lift $ readIORef database
      let posts = filter_database username db
      display_main_page username posts
-}

main = main_faceted
