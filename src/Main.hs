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
type PostId = Int

data LatticeState = State {
  permissions :: [[User]]
} deriving Show
data Label =
    Top
  | U User
  | P PostId
  | Bot
  deriving Show

leq :: LatticeState -> Label -> Label -> Bool
leq s Bot    _      = True
leq s _      Bot    = False
leq s _      Top    = True
leq s Top    _      = False
leq s (U u1) (U u2) = u1 == u2
leq s (P p1) (P p2) = p1 == p2
leq s (U _ ) (P _ ) = False
leq s (P p ) (U u ) =
  let us = permissions s !! p  in
  find (u==) us /= Nothing

join :: LatticeState -> Label -> Label -> Label
join s Bot    k      = k
join s k      Bot    = k
join s _      Top    = Top
join s Top    _      = Top
join s (U u1) (U u2) = if u1 == u2 then U u1 else Top
join s (P p1) (P p2) = if p1 == p2 then P p1 else Top
join s (U u ) (P p ) = join s (P p) (U u)
join s (P p ) (U u ) =
  let us = permissions s !! p  in
  if find (u==) us /= Nothing then
    U u
  else
    Top

data Fac a where
  Raw     ::                         a      -> Fac a
  Fac     :: Label -> Fac a ->     Fac a    -> Fac a
  BindFac ::          Fac a -> (a -> Fac b) -> Fac b
instance Show a => Show (Fac a) where
  show (Raw a)         = "Raw (" ++ show a ++ ")"
  show (Fac k fa1 fa2) = "Fac (" ++ show k ++ ") (" ++ show fa1 ++ ") (" ++ show fa2 ++ ")"
  show (BindFac fa f)  = "BindFac (...) (...)"

instance Functor Fac where
  fmap = liftM
instance Applicative Fac where
  pure  = return
  (<*>) = ap
instance Monad Fac where
  return = Raw
  (>>=) = BindFac

project :: LatticeState -> Label -> Fac a -> Fac a
project s k1 = g where
  g :: Fac a -> Fac a
  g (Raw a) =
    Raw a
  g (Fac k2 fa1 fa2) =
    if leq s k2 k1 then
      g fa1
    else
      g fa2
  g (BindFac fb1 c) =
    let Raw b = g fb1  in
    g (c b)

data FIO a where
  Return :: a -> FIO a
  BindFIO :: FIO a -> (a -> FIO b) -> FIO b
  Run :: Fac (FIO a) -> FIO (Fac a)
type PC = (Label, [Label])

data PostList =
    Nil
  | Cons (Fac String) (Fac PostList)
instance Show PostList where
  show Nil = "Nil"
  show (Cons fs fpl) = "Cons (" ++ show fs ++ ") (" ++ show fpl ++ ")"

flatten :: Faceted PostList -> [String]
flatten fpl = do  --Fac
  pl <- fpl
  case pl of
    Nil ->
      Raw []
    Cons fs fpl -> do  --Fac
      s <- fs
      ss <- flatten fpl
      Raw (s:ss)

{-
project_pl s k (Raw Nil) =
  Raw Nil
project_pl s k (Raw (Cons fs fd)) =
  Raw (Cons (project s k fs) (project_pl s k fd))
project_pl s k fd = project_pl s k (project s k fd)
-}

project_pl s k fpl = project s k $ facpostlist_to_faclist fpl

{-
example = do
  let da = Fac ["a"] (Raw "a:42") (Raw "censored")
  let db = Fac ["b"] (Raw "b:43") (Raw "censored")
  let dc = Fac ["c"] (Raw "c:44") (Raw "censored")

  let d0 = Raw Nil
  let d1 = Fac ["a"] (Raw (Cons da d0)) d0
  let d2 = Fac ["b"] (Raw (Cons db d1)) d1
  let d3 = Fac ["c"] (Raw (Cons dc d2)) d2

  d3
-}

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

display_redirect_page =
  html $ "\
    \<meta http-equiv=\"refresh\" content=\"0; url="<> escape username <>"\" />\
    \"
display_main_page =
  html $ "\
    \"<> escape (show n) <>". Hello, "<> escape username <>"\
    \<form method=\"post\" action=\"/\">\
    \  Username: <input name=\"username\" value=\""<> escape username <>"\"></input><br />\
    \  Permissions: <input name=\"permissions\"></input><br />\
    \  Content:<br />\
    \  <textarea name=\"content\"></textarea><br />\
    \  <input type=\"submit\"></input><br />\
    \</form>\
    \Your view of the database:<br />\
    \"<> escape (show (project_pl s (U username) fd)) <> "<br /><br />\
{-
    \The lattice:<br />\
    \"<> escape (show s) <> "<br /><br />\
    \The real database:<br />\
    \"<> escape (show fd) <> "\
-}
    \"

main = do
  lattice <- newIORef (State [])
  database <- newIORef (Raw Nil)
  scotty 3000 $ do
    get "/" $ do
      html $ "\
        \Please log in by typing your username at the end of the URL.\
        \"
    post "/" $ do
      username <- param "username"
      p <- param "permissions"
      let perms = username : words p
      content <- param "content"
      s <- lift $ readIORef lattice
      fd <- lift $ readIORef database
      let post_id = length (permissions s)
      lift $ writeIORef lattice $ State (permissions s ++ [perms])
      lift $ writeIORef database $ Fac (P post_id) (Raw (Cons (Raw content) fd)) fd
      redirect_to_main_page
    get "/:username" $ do
      username <- param "username"
      s <- lift $ readIORef lattice
      fd <- lift $ readIORef database
      let d = project s (U username) (flatten fd)
      display_main_page d
