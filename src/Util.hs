{-# LANGUAGE OverloadedStrings, GADTs #-}
module Util where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Data.String(fromString)
import Network.HTTP.Types.Header(ResponseHeaders)
import Data.IORef
import Data.ByteString.Char8(unpack)
import qualified Network.Wai as WAI
import Data.List(foldl', intersect)

import FIO(Lattice(leq, bot, lub), FIORef, FIO, Fac)

type Post = String
type User = String

data Label =
    Whitelist [User]
  | Bot
  deriving (Show, Eq)
instance Lattice Label where
  leq Bot             _               = True
  leq _               Bot             = False
  leq (Whitelist us1) (Whitelist us2) = us2 `subset` us1  where
    subset xs ys = all (\x -> x `elem` ys) xs

  lub Bot             k               = k
  lub k               Bot             = k
  lub (Whitelist us1) (Whitelist us2) = Whitelist (intersect us1 us2)

  bot = Bot

-- This is the password-checking function.
-- Currently, it takes the username from the URL parameters.
-- Currently, it always succeeds without any password.
check_credentials request =
  case lookup "username" (WAI.queryString request) of
    Just (Just u) ->
      let username = unpack u  in
      if all (\c -> (c>='0' && c<='9') || (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_') username then
        Just username
      else
        Nothing
    _ ->
      Nothing

data FList a =
    Nil
  | Cons a (Fac Label (FList a))

data TicTacToe = TicTacToe {
  players :: [User],
  player_assignment :: User -> Maybe Bool,  --True means X, False means O
  turn :: Maybe Bool,  -- 'Nothing' means game hasn't started yet.
  board :: Int -> Int -> Maybe Bool,
  history :: [String]
}
type Database = (IORef [(Label, Post)], IORef [TicTacToe])
type App = Database -> WAI.Request -> (WAI.Response -> IO ()) -> IO ()

headers :: ResponseHeaders
headers = [("Content-Type", "text/html")]

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f ('\n':cs) a = f cs (reverse "<br />" ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

navbar username =
  "<div><a href=\"login\">Logout</a></div>"
