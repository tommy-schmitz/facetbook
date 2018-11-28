{-# LANGUAGE OverloadedStrings #-}
module Main where

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
leq s Bot     _     = True
leq s _       Bot   = False
leq s _       Top   = True
leq s Top     _     = False
leq s (U u1) (U u2) = u1 == u2
leq s (P p1) (P p2) = p1 == p2
leq s (U _ ) (P _ ) = False
leq s (P p ) (U u ) =
  let us = permissions s !! p  in
  find (u==) us /= Nothing

data Faceted a =
    Raw a
  | Faceted Label (Faceted a) (Faceted a)
  deriving Show
project s k1 (Raw a) =
  Raw a
project s k1 (Faceted k2 fa1 fa2) =
  if leq s k2 k1 then
    project s k1 fa1
  else
    project s k1 fa2

data PostList =
    Nil
  | Cons (Faceted String) (Faceted PostList)
  deriving Show
project_pl s k (Raw Nil) =
  Raw Nil
project_pl s k (Raw (Cons fs fd)) =
  Raw (Cons (project s k fs) (project_pl s k fd))
project_pl s k fd = project_pl s k (project s k fd)

{-
example = do
  let da = Faceted ["a"] (Raw "a:42") (Raw "censored")
  let db = Faceted ["b"] (Raw "b:43") (Raw "censored")
  let dc = Faceted ["c"] (Raw "c:44") (Raw "censored")

  let d0 = Raw Nil
  let d1 = Faceted ["a"] (Raw (Cons da d0)) d0
  let d2 = Faceted ["b"] (Raw (Cons db d1)) d1
  let d3 = Faceted ["c"] (Raw (Cons dc d2)) d2

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

main = do
  counter <- newIORef 0
  let inc = do
        n <- lift $ readIORef counter
        lift $ writeIORef counter (n+1)
        return (n+1)
  lattice <- newIORef (State [])
  database <- newIORef (Raw Nil)
  scotty 3000 $ do
    get "/" $ do
      html $ "\
        \Please log in.\
        \"
    post "/" $ do
      n <- inc
      username <- param "username"
      p <- param "permissions"
      let perms = username : words p
      content <- param "content"
      s <- lift $ readIORef lattice
      fd <- lift $ readIORef database
      let post_id = length (permissions s)
      lift $ writeIORef lattice $ State (permissions s ++ [perms])
      lift $ writeIORef database $ Faceted (P post_id) (Raw (Cons (Raw content) fd)) fd
      html $ "\
        \<meta http-equiv=\"refresh\" content=\"0; url="<>escape username<>"\" />\
        \"
    get "/:username" $ do
      n <- inc
      username <- param "username"
      s <- lift $ readIORef lattice
      fd <- lift $ readIORef database
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
        \The lattice:<br />\
        \"<> escape (show s) <> "<br /><br />\
        \The real database:<br />\
        \"<> escape (show fd) <> "\
        \"
