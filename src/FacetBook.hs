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
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI

import Util(App, Post, check_credentials, Label)
import FIO(FIO(Read, Write, Swap), Fac)

data FList a =
    Nil
  | Cons a (Fac Label (FList a))

flatten :: Fac Label (FList a) -> Fac Label [a]
flatten ffl = do  --Fac
  fl <- ffl
  case fl of
    Nil ->
      return []
    Cons x ffl -> do  --Fac
      xs <- flatten ffl
      return (x:xs)

-- "facetbook" code must not use "runFIO" or "IO :: IO a -> FIO a".
-- This can be enforced using Haskell's module system.
facetbook :: App (FList Post)
facetbook database request respond = do  --FIO
  let headers = [("Content-Type", "text/html")]
  if WAI.pathInfo request == ["login"] then
    respond $ WAI.responseLBS status200 headers "boring login page"
  else
    case check_credentials request of
      Nothing ->
        respond $ WAI.responseLBS status403 headers "bad credentials"
      Just username ->
        case WAI.pathInfo request of
          ["post"] ->
            case lookup "content" (WAI.queryString request) of
              Just (Just p) -> do  --FIO
                case lookup "permissions" (WAI.queryString request) of
                  Just (Just permissions) -> do  --FIO
                    d <- Read database
                    let d' = return $ Cons (unpack p) d
                    Write database d'
                    respond $ WAI.responseLBS status200 headers "post successful"
                  _ ->
                    respond $ WAI.responseLBS status400 headers "bad post (missing permissions)"
              _ ->
                respond $ WAI.responseLBS status400 headers "bad post (missing content)"
          ["read-all-posts"] -> do  --FIO
            d <- Read database
            Swap $ do  --Fac
              all_posts <- flatten d
              return $ do  --FIO
                respond $ WAI.responseLBS status200 headers $ escape (show all_posts)
            return ()
          _ ->
            respond $ WAI.responseLBS status404 headers "bad request"

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])
