{-# LANGUAGE OverloadedStrings, GADTs #-}
module FacetBook where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import Data.List(find)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Data.Monoid((<>))
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI

import Util(App, Post, User, check_credentials, Label, FList(Nil, Cons))
import FIO(FIO(Read, Write, Swap), Fac)

headers = [("Content-Type", "text/html")]

navbar username =
  "<div><a href=\"login\">Logout</a></div>"

login :: App (FList Post)
login database request respond =
  respond $ WAI.responseLBS status200 headers $
      "Username:\n" <>
      "<form action=\"read-all-posts\">\n" <>
      "<input name=\"username\"></input>" <>
      "</form>\n"

authentication_failed :: App (FList Post)
authentication_failed database request respond =
  respond $ WAI.responseLBS status403 headers "bad credentials"

post :: User -> [User] -> App (FList Post)
post username users database request respond =
  case lookup "content" (WAI.queryString request) of
    Just (Just p) -> do  --FIO
      d <- Read database
      let d' = return $ Cons (unpack p) d
      Write database d'
      respond $ WAI.responseLBS status200 headers "post successful"
    _ ->
      respond $ WAI.responseLBS status400 headers "bad post (missing content)"

post_err_permissions :: App (FList Post)
post_err_permissions database request respond =
  respond $ WAI.responseLBS status400 headers "bad post (missing permissions)"

flatten :: Fac Label (FList a) -> Fac Label [a]
flatten ffl = do  --Fac
  fl <- ffl
  case fl of
    Nil ->
      return []
    Cons x ffl -> do  --Fac
      xs <- flatten ffl
      return (x:xs)

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

read_all_posts :: User -> App (FList Post)
read_all_posts username database request respond = do  --FIO
  d <- Read database
  Swap $ do  --Fac
    all_posts <- flatten d
    return $ do  --FIO
      respond $ WAI.responseLBS status200 headers $
          navbar username <>
          "Posts:<br />" <>
          escape (show all_posts)
  return ()

bad_request :: App (FList Post)
bad_request database request respond =
  respond $ WAI.responseLBS status404 headers "bad request"
