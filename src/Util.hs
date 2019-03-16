{-# LANGUAGE OverloadedStrings, GADTs #-}
module Util where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Data.String(fromString)
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

valid_username :: String -> Bool
valid_username s =
  length s > 0  &&
  all (\c -> (c>='0' && c<='9') ||
             (c>='a' && c<='z') ||
             (c>='A' && c<='Z') ||
             c=='_'                ) s

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

get_parameter :: WAI.Request -> String -> String
get_parameter request key =
  case lookup (fromString key) (WAI.queryString request) of
    Just (Just value) ->
      unpack value
    _ ->
      ""
