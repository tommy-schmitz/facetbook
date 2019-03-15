{-# LANGUAGE OverloadedStrings #-}
module TCB where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import Data.ByteString.Char8(unpack)
import Data.List(find)
-}
import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI(Request, pathInfo)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))

import Shared(check_credentials, Post, Label(Whitelist, Bot), FList(Nil), get_parameter, valid_username)
import FIO(Lattice(leq), FIO(IO, New), Fac(Raw, Fac, Undefined), FIORef, runFIO, PC(Constraints, Singleton))
import qualified UCB as UCB(handle_request)

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
    UCB.handle_request request database unit_respond
    return ResponseReceived
