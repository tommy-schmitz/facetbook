{-# LANGUAGE OverloadedStrings #-}
module TCB where
import Data.IORef
import Data.Monoid((<>))
import qualified Data.ByteString.Lazy.Char8 as ByteString(intercalate)
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI(Request, pathInfo, responseLBS)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
import Network.HTTP.Types.Status(status200)
import FIO
import Shared
import qualified UCB
do_create_post :: User -> Post -> [User] -> Handler
do_create_post username content users database respond = do  --IO
  d <- readIORef (fst database)
  let labeled_data = (Whitelist (username : users), username ++ ": " ++ content)
  writeIORef (fst database) (labeled_data : d)
  respond $ WAI.responseLBS status200 headers $
      "<meta http-equiv=\"refresh\" content=\"0; url=/dashboard?username="<>escape username<>"\" />"
flatten :: [(Label, Post)] -> [Post]
flatten = map snd
filter_posts :: Label -> PostList -> PostList
filter_posts k = filter (\(k',p) -> leq k' k)
dashboard :: User -> Handler
dashboard username database respond = do  --IO
  labeled_posts <- readIORef (fst database)
  let d = filter_posts (Whitelist [username]) labeled_posts
  let posts = flatten d
  respond $ WAI.responseLBS status200 headers $
          navbar username <>
          "<h2>Dashboard</h2>" <>
          "<a href=\"/post?username="<>escape username<>"\">Create post</a><br />" <>
          "<br /><a href=\"tictactoe?username=" <>
          escape username <>
          "\">Play TicTacToe</a><br />" <>
          "Recent posts:<hr />" <>
          ByteString.intercalate "<hr />" (map escape (take 20 posts))
handle_request :: WAI.Request -> Handler
handle_request request =
  let sandbox h = \database respond ->
       let censored = (undefined, snd database)  in
       h censored respond  in
  if WAI.pathInfo request == ["login"] then
    sandbox $ UCB.login
  else case check_credentials request of
    Nothing ->
      sandbox $ UCB.authentication_failed
    Just username -> case WAI.pathInfo request of
      ["post"] ->
        let content = get_parameter request "content"  in
        let permissions = get_parameter request "permissions"  in
        let users = words permissions  in
        if content /= "" && all valid_username users then
          do_create_post username content users
        else
          sandbox $ UCB.compose_post username
      ["dashboard"] ->
        dashboard username
      ["tictactoe"] ->
        let partner = get_parameter request "partner"  in
        if valid_username partner then
          let action = get_parameter request "action"  in
          sandbox $ UCB.tictactoe_play username partner action
        else
          sandbox $ UCB.tictactoe_select_partner username
      _ ->
        sandbox $ UCB.not_found
main :: IO ()
main = do  --IO
  r1 <- newIORef []
  r2 <- newIORef []
  let database = (r1, r2)
  let port = 3000
  Warp.run port $ \request respond -> do  --IO
    let unit_respond = \x -> do  --IO
         respond x
         return ()
    handle_request request database unit_respond
    return ResponseReceived
