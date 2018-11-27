{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid((<>))

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    get "/:username" $ do
--      username <- param "username"
--      text $ "hello " <> username <> "!"
      html $ "\
        <input>\
        </input>\
        "
