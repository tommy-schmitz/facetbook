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

import Util(check_credentials, Post, Label(Whitelist, Bot), FList(Nil))
import FIO(Lattice(leq), FIO(IO, New), Fac, FIORef, runFIO, PC(Constraints, Singleton))
import qualified FacetBook as FacetBook(login, authentication_failed, post, post_err_permissions, read_all_posts, other_request)

main = do  --IO
  database <- runFIO (Constraints [] []) $ do  --FIO
    r1 <- New Nil
    r2 <- New []
    return (r1, r2)
  let port = 3000
  Warp.run port $ \request respond -> do  --IO
    putStrLn (show (WAI.rawPathInfo request))
    putStrLn (show (WAI.rawQueryString request))
    let fio_respond k = \x -> IO k $ do  --IO
         respond x
         return ()
    let sandbox k pc app_handler = do  --IO
         runFIO pc (app_handler database request (fio_respond k))
         return ResponseReceived
    if WAI.pathInfo request == ["login"] then
      sandbox Bot (Constraints [] []) $
          FacetBook.login
    else
      case check_credentials request of
        Nothing ->
          sandbox Bot (Constraints [] []) $
              FacetBook.authentication_failed
        Just username ->
          let user = unpack username  in
          let sandbox' = sandbox (Whitelist [user])  in
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  let users = words (unpack permissions)  in
                  sandbox' (Constraints [Whitelist (user : users)] []) $
                      FacetBook.post user users
                _ ->
                  sandbox' (Singleton (Whitelist [user])) $
                      FacetBook.post_err_permissions
            ["read-all-posts"] ->
              sandbox' (Singleton (Whitelist [user])) $
                  FacetBook.read_all_posts user
            _ ->
              sandbox' (Constraints [] []) $
                  FacetBook.other_request user
