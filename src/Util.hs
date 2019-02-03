{-# LANGUAGE OverloadedStrings, GADTs #-}
module Util where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.ByteString.Char8(unpack)
import Data.IORef
import Data.String(fromString)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import qualified Network.Wai as WAI
import Data.List(find)

import FIO(Lattice(leq, bot), FIORef, FIO, Fac)

type Post = String
type User = String

data Label =
    Whitelist [User]
  | Bot
  deriving (Show, Eq)
instance Lattice Label where
  leq Bot            _               = True
  leq _              Bot             = False
  leq _              (Whitelist [])  = True
  leq (Whitelist []) _               = False
  leq (Whitelist us) (Whitelist [u]) = find (u==) us /= Nothing
  leq k1             k2              = k1==k2
  bot = Bot

type App a = FIORef Label a -> WAI.Request -> (WAI.Response -> FIO Label ()) -> FIO Label ()

-- This is the password-checking function.
-- Currently, it takes the username from the URL parameters.
-- Currently, it always succeeds without any password.
check_credentials request =
  case lookup "username" (WAI.queryString request) of
    Just (Just username) ->
      Just username
    _ ->
      Nothing

data FList a =
    Nil
  | Cons a (Fac Label (FList a))

data TicTacToe = TicTacToe {
  players :: [User],
  player_assignment :: [Maybe Bool],
  board :: Int -> Int -> Maybe Bool,
  sequence_number :: Int
}
data Database = Database {
  game_list :: [TicTacToe],
  posts :: Fac Label (FList Post)
}
