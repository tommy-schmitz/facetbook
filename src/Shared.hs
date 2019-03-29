{-# LANGUAGE OverloadedStrings #-}
module Shared where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Data.IORef
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import Network.HTTP.Types.Header(ResponseHeaders)
import qualified Network.Wai as WAI(Request, Response, queryString)
import qualified Data.List as List(intersect)

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
  lub (Whitelist us1) (Whitelist us2) = Whitelist (List.intersect us1 us2)

  bot = Bot

valid_username :: String -> Bool
valid_username s =
  s /= ""  &&
  all (\c -> (c>='0' && c<='9') ||
             (c>='a' && c<='z') ||
             (c>='A' && c<='Z') ||
             c=='_'                ) s

-- This is the password-checking function.
-- Currently, it takes the username from the URL parameters.
-- Currently, it always succeeds without any password.
check_credentials request =
  let username = get_parameter request "username"  in
  if valid_username username then
    Just username
  else
    Nothing

data FList a =
    Nil
  | Cons a (Fac Label (FList a))

get_parameter :: WAI.Request -> String -> String
get_parameter request key =
  case lookup (fromString key) (WAI.queryString request) of
    Just (Just value) ->
      unpack value
    _ ->
      ""

type Ref = IORef
type PostList = [(Label, Post)]
type XIO = IO

data TicTacToe = TicTacToe {
  players :: [User],
  player_assignment :: User -> Maybe Bool,  -- 'True' means X, 'False' means O
  turn :: Maybe Bool,  -- 'Nothing' means game hasn't started yet.
  board :: Int -> Int -> Maybe Bool,
  history :: [String]
}
type Database = (Ref PostList, Ref [TicTacToe])
type Handler = Database -> (WAI.Response -> XIO ()) -> XIO ()

headers :: ResponseHeaders
headers = [("Content-Type", "text/html")]

navbar username =
  "<div><a href=\"login\">Logout</a></div>"

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
