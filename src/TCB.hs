{-# LANGUAGE OverloadedStrings #-}
module TCB where
import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI(Request, pathInfo)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
import Shared(check_credentials, Post, Label(Whitelist, Bot), get_parameter, valid_username)
import FIO(leq)
import qualified UCB
do_create_post username content users database respond = do  --IO
  d <- readIORef (fst database)
  let labeled_data = (Whitelist (username : users), username ++ ": " ++ content)
  writeIORef (fst database) (labeled_data : d)
  respond $ UCB.do_create_post_response username
flatten :: [(Label, Post)] -> [Post]
flatten = map snd
filter_posts k = filter (\(k',p) -> leq k' k)
dashboard username database respond = do  --IO
  labeled_posts <- readIORef (fst database)
  let d = filter_posts (Whitelist [username]) labeled_posts
  let posts = flatten d
  respond $ UCB.dashboard_response username posts
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
