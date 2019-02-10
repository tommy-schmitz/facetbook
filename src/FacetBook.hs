{-# LANGUAGE OverloadedStrings, GADTs #-}
module FacetBook where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import qualified Data.List as List
import Data.Monoid((<>))
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI

import Util(App, Post, User, check_credentials, Label, FList(Nil, Cons), Database, TicTacToe(TicTacToe, players, player_assignment, board, sequence_number))
import FIO(FIO(Read, Write, Swap), Fac)

headers = [("Content-Type", "text/html")]

navbar username =
  "<div><a href=\"login\">Logout</a></div>"

login :: App
login database request respond =
  respond $ WAI.responseLBS status200 headers $
      "Username:\n" <>
      "<form action=\"read-all-posts\">\n" <>
      "<input name=\"username\"></input>" <>
      "</form>\n"

authentication_failed :: App
authentication_failed database request respond =
  respond $ WAI.responseLBS status403 headers "bad credentials"

post :: User -> [User] -> App
post username users database request respond =
  case lookup "content" (WAI.queryString request) of
    Just (Just c) -> do  --FIO
      let content = unpack c
      d <- Read (fst database)
      Write (fst database) $ return $ Cons content d
      respond $ WAI.responseLBS status200 headers "post successful"
    _ ->
      respond $ WAI.responseLBS status400 headers "bad post (missing content)"

post_err_permissions :: App
post_err_permissions database request respond =
  respond $ WAI.responseLBS status400 headers "bad post (missing permissions)"

flatten :: Fac Label (FList a) -> Fac Label [a]
flatten ffl = do  --Fac
  fl <- ffl
  case fl of
    Nil ->
      return []
    Cons x ffl -> do  --Fac
      xs <- flatten ffl
      return (x:xs)

escape s = fromString s' where
  f ('<' :cs) a = f cs (reverse "&lt;"   ++ a)
  f ('>' :cs) a = f cs (reverse "&gt;"   ++ a)
  f ('&' :cs) a = f cs (reverse "&amp;"  ++ a)
  f ('"' :cs) a = f cs (reverse "&quot;" ++ a)
  f ('\'':cs) a = f cs (reverse "&#39;"  ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

read_all_posts :: User -> App
read_all_posts username database request respond = do  --FIO
  d <- Read (fst database)
  Swap $ do  --Fac
    all_posts <- flatten d
    return $ do  --FIO
      respond $ WAI.responseLBS status200 headers $
          navbar username <>
          "Posts:<br />" <>
          escape (show all_posts) <>
          "<br /><a href=\"tictactoe?username=" <>
          escape username <>
          "\">Play TicTacToe</a>"
  return ()

get_winner :: TicTacToe -> Either Bool ()
get_winner game = do  --Either Bool
  let check (x1, y1) (x2, y2) (x3, y3) = do  --Either Bool
       let s1 = board game x1 y1
       let s2 = board game x2 y2
       let s3 = board game x3 y3
       if s1 /= s2 then
         Right ()
       else if s2 /= s3 then
         Right ()
       else case s1 of
         Nothing -> Right ()
         Just s -> Left s
  check (0, 0) (1, 0) (2, 0)  -- Row 0
  check (0, 1) (1, 1) (2, 1)  -- Row 1
  check (0, 2) (1, 2) (2, 2)  -- Row 2
  check (0, 0) (0, 1) (0, 2)  -- Column 0
  check (1, 0) (1, 1) (1, 2)  -- Column 1
  check (2, 0) (2, 1) (2, 2)  -- Column 2
  check (0, 0) (1, 1) (2, 2)  -- Descending diagonal
  check (2, 0) (1, 1) (0, 2)  -- Ascending diagonal

render_tictactoe game =
  let winner = get_winner game  in
  let sq x y =
       let content =
            case board game x y of
              Just True ->
                "x"
              Just False ->
                "o"
              Nothing ->
                if winner == Right () then
                  "<a href onclick=\"return false\">#</a>"
                else
                  ""  in
       "<div style=\"position: absolute; left: "
       <> escape (show (32 * x)) <>
       "px; top: "
       <> escape (show (32 * y)) <>
       "px; border: 1px solid black; width: 28px; height: 28px; \">"
       <> content <>
       "</div>"  in
  sq 0 0  <>
  sq 0 1  <>
  sq 0 2  <>
  sq 1 0  <>
  sq 1 1  <>
  sq 1 2  <>
  sq 2 0  <>
  sq 2 1  <>
  sq 2 2

delete_at index list =
  let (list_1, list_2) = List.splitAt index list  in
  list_1 ++ List.drop 1 list_2

other_request :: User -> App
other_request username database request respond =
  if WAI.pathInfo request /= ["tictactoe"] then
    respond $ WAI.responseLBS status404 headers "bad request"
  else do  --FIO
    d <- Read (snd database)
    Swap $ do  --Fac
      game_list <- d
      case lookup "partner" (WAI.queryString request) of
        Just (Just p) -> do  --Fac
          let partner = unpack p
          case List.findIndex (\game -> username `elem` players game && partner `elem` players game) game_list of
            Nothing -> do  --Fac
              let new_game = TicTacToe {
                players = [username, partner],
                player_assignment = [Nothing, Nothing],
                board = \_ _ -> Nothing,
                sequence_number = 0
              }
              return $ do  --FIO
                Write (snd database) $ return $ new_game : game_list
                respond $ WAI.responseLBS status200 headers $ render_tictactoe new_game
            Just index -> do  --Fac
              case lookup "action" (WAI.queryString request) of
                Just (Just a) -> do  --Fac
                  let action = unpack a
                  let game = game_list !! index
                  --Write (snd database) $ return $ new_game : delete_at index game_list
                  error "three"
                _ ->
                  let game = game_list !! index  in
                  return $ do  --FIO
                    respond $ WAI.responseLBS status200 headers $ render_tictactoe game
        _ ->
          return $ do  --FIO
            respond $ WAI.responseLBS status200 headers $
                "<form action=\"tictactoe\">" <>
                "  Partner:<br />" <>
                "  <input type=\"hidden\" name=\"username\" value=\""<>
                escape username <>
                "\"></input>" <>
                "  <input name=\"partner\"></input>" <>
                "</form>"
    return ()

--    case lookup "content" (WAI.queryString request) of
