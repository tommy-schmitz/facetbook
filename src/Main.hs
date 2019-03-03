{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
-}
import Data.Monoid((<>))
import Network.HTTP.Types.Status(status200)
import Data.IORef
import Data.ByteString.Char8(unpack)
import Data.List(find)
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
import qualified Data.ByteString.Lazy.Char8 as ByteString(intercalate)

import Util(check_credentials, Post, User, Label(Whitelist), headers, escape, navbar)
import FIO(Lattice(leq))
import qualified FacetBook as FacetBook(login, authentication_failed, create_post, other_request)
import FacetBook(App)

do_create_post :: User -> [User] -> App
do_create_post username users database request respond =
  case lookup "content" (WAI.queryString request) of
    Just (Just c) -> do  --IO
      let content = unpack c
      d <- readIORef (fst database)
      writeIORef (fst database) $ (Whitelist (username : users), username ++ ": " ++ content) : d
      respond $ WAI.responseLBS status200 headers $
          "<meta http-equiv=\"refresh\" content=\"0; url=/dashboard?username="<>escape username<>"\" />"
    _ ->
      FacetBook.create_post username database request respond

filter_posts :: Label -> [(Label, a)] -> [a]
filter_posts k d =
  map snd $ filter (\(k', p) -> leq k' k) $ d

dashboard :: User -> App
dashboard username database request respond = do  --IO
      d <- readIORef (fst database)
      let all_posts = filter_posts (Whitelist [username]) d
      respond $ WAI.responseLBS status200 headers $
          navbar username <>
          "<br /><a href=\"tictactoe?username=" <>
          escape username <>
          "\">Play TicTacToe</a><br />" <>
          "<a href=\"/post?username="<>escape username<>"\">Create post</a><br />" <>
          "Recent posts:<br />" <>
          ByteString.intercalate "<hr />" (map escape (take 20 all_posts))

main = do  --IO
  r1 <- newIORef []
  r2 <- newIORef []
  let database = (r1, r2)
  let censored_database = (undefined, r2)
  let port = 3000
  Warp.run port $ \request respond -> do  --IO
    putStrLn (show (WAI.rawPathInfo request))
    putStrLn (show (WAI.rawQueryString request))
    let io_respond = \x -> do  --IO
         respond x
         return ()
    let delegate database app_handler = do  --IO
         app_handler database request io_respond
         return ResponseReceived
    if WAI.pathInfo request == ["login"] then
      delegate undefined $
          FacetBook.login
    else
      case check_credentials request of
        Nothing ->
          delegate undefined $
              FacetBook.authentication_failed
        Just user ->
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  let users = words (unpack permissions)  in
                  delegate database $
                      do_create_post user users
                _ ->
                  delegate censored_database $
                      FacetBook.create_post user
            ["dashboard"] ->
              delegate database $
                  dashboard user
            _ ->
              delegate censored_database $
                  FacetBook.other_request user
