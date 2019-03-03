{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
-}
import Data.Monoid((<>))
import Network.HTTP.Types.Status(status200, status404)
import Data.IORef
import Data.ByteString.Char8(unpack)
import qualified Data.List as List(findIndex)
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
import qualified Data.ByteString.Lazy.Char8 as ByteString(intercalate)

import Util(check_credentials, Post, User, Label(Whitelist), headers, escape, navbar, TicTacToe(TicTacToe, players, player_assignment, board, turn, history))
import FIO(Lattice(leq))
import qualified FacetBook as FacetBook(login, authentication_failed, create_post, render_tictactoe, delete_at, update_game)
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

other_request :: User -> App
other_request username database request respond =
  if WAI.pathInfo request /= ["tictactoe"] then
        respond $ WAI.responseLBS status404 headers "bad request"
  else do  --IO
        game_list <- readIORef (snd database)
        case lookup "partner" (WAI.queryString request) of
          Just (Just p) ->
            let partner = unpack p  in
            if partner == username then
              respond $ WAI.responseLBS status200 headers $
                  "Sorry, but playing a game with yourself is not supported."
            else
              case List.findIndex (\game -> username `elem` players game && partner `elem` players game) game_list of
                Nothing -> do  --IO
                  let new_game = TicTacToe {
                    players = [username, partner],
                    player_assignment = \_ -> Nothing,
                    turn = Nothing,
                    board = \_ _ -> Nothing,
                    history = []
                  }
                  writeIORef (snd database) $ new_game : game_list
                  respond $ WAI.responseLBS status200 headers $ FacetBook.render_tictactoe new_game username partner
                Just index -> do  --IO
                  let game = game_list !! index
                  let new_game =
                       case lookup "action" (WAI.queryString request) of
                         Just (Just a) ->
                           case readsPrec 0 (unpack a) of
                             [(action, "")] ->
                               FacetBook.update_game game action username partner
                             _ ->
                               game
                         _ ->
                           game
                  writeIORef (snd database) $ new_game : FacetBook.delete_at index game_list
                  d <- readIORef (fst database)
                  let all_posts = filter_posts (Whitelist [username]) d
                  respond $ WAI.responseLBS status200 headers $
                    FacetBook.render_tictactoe new_game username partner <>
                    "<br /><br />Recent posts:<br />" <>
                    ByteString.intercalate "<hr />" (map escape (take 20 all_posts))
          _ -> do  --IO
            respond $ WAI.responseLBS status200 headers $
                "<form action=\"tictactoe\">" <>
                "  Partner:<br />" <>
                "  <input type=\"hidden\" name=\"username\" value=\""<>
                escape username <>
                "\"></input>" <>
                "  <input name=\"partner\"></input>" <>
                "</form>"

main = do  --IO
  r1 <- newIORef []
  r2 <- newIORef []
  let database = (r1, r2)
  let posts_database = (r1, undefined)
  let tictactoe_database = (undefined, r2)
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
                  delegate posts_database $
                      do_create_post user users
                _ ->
                  delegate undefined $
                      FacetBook.create_post user
            ["dashboard"] ->
              delegate posts_database $
                  dashboard user
            _ ->
              delegate database $
                  other_request user
