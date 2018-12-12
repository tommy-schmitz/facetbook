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
join :: LatticeState -> Label -> Label -> Label
join s Bot    k      = k
join s k      Bot    = k
join s _      Top    = Top
join s Top    _      = Top
join s (U u1) (U u2) =
  let LS edges = s  in
  if u1==u2 then
    U u1
  else if find ((u1,u2)==) edges /= Nothing then
    U u2
  else if find ((u2,u1)==) edges /= Nothing then
    U u1
  else
    Top

data Fac a where
  Raw     ::                         a      -> Fac a
  Fac     :: Label -> Fac a ->     Fac a    -> Fac a
  BindFac ::          Fac a -> (a -> Fac b) -> Fac b

data FIORef a =
  FIORef (IORef (Fac a))

data FIO a where
  Return :: a -> FIO a
  BindFIO :: FIO a -> (a -> FIO b) -> FIO b
  Swap :: Fac (FIO a) -> FIO (Fac a)
  New :: a -> FIO (FIORef a)
  Read :: FIORef a -> FIO (Fac a)
  Write :: FIORef a -> Fac a -> FIO ()

type PC = (Label, [Label])
consistent :: LatticeState -> PC -> Bool
consistent s (k1, []) = k1 /= Top
consistent s (k1, k2:ks) = not (leq s k2 k1) && consistent s (k1, ks)
add_pc :: LatticeState -> PC -> Label -> PC
add_pc s (k, ks) k' = (join s k k', ks)
subtract_pc :: LatticeState -> PC -> Label -> PC
subtract_pc _ (k, ks) k' = (k, k':ks)

type M = ContT () (ReaderT PC IO)

run :: LatticeState -> FIO a -> M a
run s (Return x) =
  return x
run s (BindFIO x f) = do
  y <- run s x
  run s (f y)
run s (Swap (Raw ia)) = do
  a <- run s ia
  return (Raw a)
run s (Swap (BindFac (Raw b) f)) =
  run s (Swap (f b))
run s (Swap (BindFac (Fac k fb1 fb2) f)) =
  run s (Swap (Fac k (BindFac fb1 f) (BindFac fb2 f)))
run s (Swap (BindFac (BindFac c g) f)) =
  run s (Swap (BindFac c (\x -> BindFac (g x) f)))
run s (Swap (Fac k fia1 fia2)) = do
  pc <- ask
  if   not (consistent s (subtract_pc s pc k))   then
    run s (Swap fia1)
  else if   not (consistent s (add_pc s pc k))   then
    run s (Swap fia2)
  else
    error (show pc ++ show k)

instance Functor Fac where
  fmap = liftM
instance Applicative Fac where
  pure  = return
  (<*>) = ap
instance Monad Fac where
  return = Raw
  (>>=) = BindFac

project :: LatticeState -> Label -> Fac a -> IO a
project s k fa = do
  temp <- newIORef undefined
  runReaderT (runContT (run s (Swap $ BindFac fa (\a -> Raw (Return a)))) (\(Raw a) -> do
      lift $ writeIORef temp a
      return ()
    )) (k, [])
  readIORef temp

-- Unsafe function
{-
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
-}

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

-- Faceted section --

type FDatabase = (Fac (FList Post), LatticeState)

main_faceted = do  --IO
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
      posts <- lift $ project lattice (U username) (flatten flist)
      display_main_page username posts

----------------------------

-- Non-faceted section --

type Database = ([(Label, Post)], LatticeState)

filter_database :: User -> Database -> [Post]
filter_database username (posts, lattice) =
  case posts of
    [] ->
      []
    (k, p) : posts_tail ->
      let filtered_tail = filter_database username (posts_tail, lattice)  in
      if leq lattice k (U username) then
        p : filtered_tail
      else
        filtered_tail

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

main = main_faceted
