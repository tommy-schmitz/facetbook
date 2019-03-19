{-# LANGUAGE OverloadedStrings #-}
module TCB where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Data.ByteString.Char8(unpack)
import Data.List(find)
-}
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI(Request, pathInfo)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))

import Shared(check_credentials, Post, Label(Whitelist, Bot), FList(Nil), get_parameter, valid_username)
import FIO(Lattice(leq), FIO(IO, New), Fac(Raw, Fac, Undefined), FIORef, runFIO, PC(Constraints, Singleton))
import qualified UCB as UCB(handle_request)

policy :: WAI.Request -> (Label, Label)
policy request =
  if WAI.pathInfo request == ["login"] then
    (Bot, Bot)
  else case check_credentials request of
    Nothing ->
      (Bot, Bot)
    Just username -> case WAI.pathInfo request of
      ["post"] ->
        let permissions = get_parameter request "permissions"  in
        let users = words permissions  in
        if all valid_username users then
          (Whitelist (username : users), Whitelist [username])
        else
          (Whitelist [username], Whitelist [username])
      _ ->
        (Bot, Whitelist [username])

main :: IO ()
main = do  --IO
  database <- runFIO (Constraints [] []) $ do  --FIO
    r1 <- New Nil
    r2 <- New []
    return (r1, r2)
  let port = 3000
  Warp.run port $ \request respond -> do  --IO
    let (k1, k2) = policy request
    let fio_respond = \x -> IO k2 $ do  --IO
         respond x
         return ()
    let faceted_request = Fac k1 (Raw request) Undefined
    runFIO (Constraints [] []) $
        UCB.handle_request database faceted_request fio_respond
    return ResponseReceived
