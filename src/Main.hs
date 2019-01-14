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
import Network.HTTP.Types.Status(status200, status403)
import qualified Network.Wai as WAI

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
{-
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
-}
leq :: Label -> Label -> Bool
leq = undefined

data Fac a where
  Undefined ::                                 Fac a
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

data FIORef a =
  FIORef (IORef (Fac a))
data FIO a where
  Return :: a -> FIO a
  BindFIO :: FIO a -> (a -> FIO b) -> FIO b
  Swap :: Fac (FIO a) -> FIO (Fac a)
  New :: a -> FIO (FIORef a)
  Read :: FIORef a -> FIO (Fac a)
  Write :: FIORef a -> Fac a -> FIO ()

instance Functor FIO where
  fmap = liftM
instance Applicative FIO where
  pure  = return
  (<*>) = ap
instance Monad FIO where
  return = Return
  (>>=) = BindFIO

runFIO :: Label -> FIO a -> IO (Fac a)
runFIO k x = z x where
 z :: FIO a -> IO (Fac a)
 z x =
  case x of
    Return a ->
      return (Raw a)
    BindFIO a b -> do  --IO
      c <- z a
      d <- z $ Swap $ do  --Fac
        e <- c
        return (b e)
      return $ BindFac d id
    Swap Undefined ->
      return Undefined
    Swap (Raw a) -> do  --IO
      b <- z a
      return (Raw b)
    Swap (BindFac Undefined b) ->
      return Undefined
    Swap (BindFac (Raw a) b) ->
      z (Swap (b a))
    Swap (BindFac (Fac k' a b) c) ->
      z (Swap (Fac k' (BindFac a c) (BindFac b c)))
    Swap (BindFac (BindFac a b) c) ->
      z (Swap (BindFac a (\d -> BindFac (b d) c)))
    Swap (Fac k' a b) -> do  --IO
      if leq k' k then
        z (Swap a)
      else
        z (Swap b)
    New a -> do  --IO
      b <- newIORef (Fac k (Raw a) Undefined)
      return (Raw (FIORef b))
    Read (FIORef a) -> do  --IO
      b <- readIORef a
      return (Raw b)
    Write (FIORef a) b -> do  --IO
      c <- readIORef a
      writeIORef a (Fac k b c)
      return (Raw ())

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
  WAI.responseLBS status200 [] $ "\
    \<meta http-equiv=\"refresh\" content=\"0; url=/read-all-posts/"<> escape username <>"\" />\
    \"
login_page =
  WAI.responseLBS status200 [] "boring login page"
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

type App a = FIORef a -> WAI.Request -> (String -> FIO ()) -> FIO ()

run_server :: Int -> App a -> IO ()
run_server port_number handler = do  --IO
  database_ioref <- newIORef Undefined
  let database_fioref = FIORef database_ioref
  Warp.run port_number $ \request respond -> do  --IO
    let simplified_respond = \x -> respond $ WAI.responseLBS status200 [] x
    let handle k = run_sandboxed k (handler database request simplified_respond)
    case WAI.pathInfo request of
      ["login"] ->

        -- THE BIG QUESTION.
        -- WHAT KIND OF SANDBOX DOES THIS CODE RUN IN?
        -- WHAT KINDS OF CAPABILITIES DOES THIS CODE HAVE?

        -- respond label = Bot
        -- request label = Bot
        -- PC = []
      _ ->
        case check_credentials request of
          Just username ->
            case WAI.pathInfo request of
              ["post"] ->
                -- respond label = username
                -- request label = username + permissions
                -- PC = [username + permissions]
              ["read-all-posts"] ->
                -- respond label = username
                -- request label = Bot
                -- PC = [username]
              _ ->
                -- respond label = username
                -- request label = Bot
                -- PC = [username]
          Nothing ->
            -- respond label = Bot
            -- request label = Bot
            -- PC = []
            respond $ responseLBS status403 [] "Bad credentials"

app_facetbook :: App (FList Post)
app_facetbook database request respond = do  --FIO
  case WAI.pathInfo request of
    ["login"] ->
      respond login_page
    ["post"] -> do
      x <- readFIORef database
      let Just (Just p) = lookup "content" (WAI.queryString request)
      writeFIORef database $ Cons p x

main_faceted = do  --IO
  run_server 3000 app_facetbook

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

main = return ()
