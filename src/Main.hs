{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
-}
import Data.IORef
import Data.ByteString.Char8(unpack)
import Data.List(find)
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))

import Util(check_credentials, Post)
--import FIO(Lattice(leq), FIO(IO, New), Fac, FIORef, runFIO, PC(Constraints, Singleton))
import qualified FacetBook as FacetBook(login, authentication_failed, do_create_post, create_post, dashboard, other_request)

main = do  --IO
  r1 <- newIORef []
  r2 <- newIORef []
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
                      FacetBook.do_create_post user users
                _ ->
                  delegate undefined $
                      FacetBook.create_post user
            ["dashboard"] ->
              delegate posts_database $
                  FacetBook.dashboard user
            _ ->
              delegate tictactoe_database $
                  FacetBook.other_request user
