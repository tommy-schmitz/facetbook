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

import Util(check_credentials, Label(Whitelist), App)
import FIO(Lattice(leq), FIO(IO, New), Fac, FIORef, runFIO, PC(Everything, Singleton, UpwardClosure))
import FacetBook(FList(Nil), facetbook)

type Post = String
type User = String

run_server :: Int -> App (FList Post) -> IO ()
run_server port app = do  --IO
  database <- runFIO Everything $ New Nil
  Warp.run port $ \request respond -> do  --IO
    let fio_respond = \x -> IO $ do  --IO
         respond x
         return ()
    let handle pc = do  --IO
         runFIO pc (app database request fio_respond)
         return ResponseReceived
    if WAI.pathInfo request == ["login"] then
      handle Everything
    else
      case check_credentials request of
        Nothing ->
          handle Everything
        Just username ->
          case WAI.pathInfo request of
            ["post"] ->
              case lookup "permissions" (WAI.queryString request) of
                Just (Just permissions) ->
                  handle (UpwardClosure (Whitelist (unpack username : words (unpack permissions))))
                _ ->
                  handle (Singleton (Whitelist [unpack username]))
            ["read-all-posts"] ->
              handle (Singleton (Whitelist [unpack username]))
            _ ->
              handle (Singleton (Whitelist [unpack username]))

main = run_server 3000 facetbook
