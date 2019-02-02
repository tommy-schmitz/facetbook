{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import Data.String(fromString)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
-}
import Data.ByteString.Char8(unpack)
import Data.List(find)
import qualified Network.Wai.Handler.Warp as Warp(run)
import qualified Network.Wai as WAI
import Network.Wai.Internal(ResponseReceived(ResponseReceived))

import Util(check_credentials, Post, Label(Whitelist), App, FList(Nil))
import FIO(Lattice(leq), FIO(IO, New), Fac, FIORef, runFIO, PC(AllViews, Singleton, UpwardClosure))
import qualified FacetBook as FacetBook(login, authentication_failed, post, post_err_permissions, read_all_posts, bad_request)

main = do  --IO
  database <- runFIO AllViews $ do  --FIO
    New Nil
  let port = 3000
  Warp.run port $ \request respond -> do  --IO
    let fio_respond = \x -> IO $ do  --IO
         respond x
         return ()
    let sandbox pc app_handler = do  --IO
         runFIO pc (app_handler database request fio_respond)
         return ResponseReceived
    if WAI.pathInfo request == ["login"] then
      sandbox AllViews $
          FacetBook.login
    else
      case check_credentials request of
        Nothing ->
          sandbox AllViews $
              FacetBook.authentication_failed
        Just username ->
          let user = unpack username  in
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  let users = words (unpack permissions)  in
                  sandbox (UpwardClosure (Whitelist (user : users))) $
                      FacetBook.post user users
                _ ->
                  sandbox (Singleton (Whitelist [unpack username])) $
                      FacetBook.post_err_permissions
            ["read-all-posts"] ->
              sandbox (Singleton (Whitelist [unpack username])) $
                  FacetBook.read_all_posts user
            _ ->
              sandbox (Singleton (Whitelist [unpack username])) $
                  FacetBook.bad_request
