{-# LANGUAGE OverloadedStrings #-}
module Shared where
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import qualified Network.Wai as WAI(Request, queryString)
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
