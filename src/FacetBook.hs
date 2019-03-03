{-# LANGUAGE OverloadedStrings, GADTs #-}
module FacetBook where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import Data.IORef
import qualified Data.List as List
import Data.Monoid((<>))
import Data.String(fromString)
import Data.ByteString.Char8(unpack)
import Network.HTTP.Types.Status(status200, status400, status403, status404)
import qualified Network.Wai as WAI

import Util(Post, User, check_credentials, Label(Whitelist), headers, escape, navbar, TicTacToe(TicTacToe, turn, board, player_assignment, history, players))
import FIO(leq)

data Action =
    Iam Bool
  | Reset
  | Move Int Int
  | Noop
  deriving (Read, Show)
type Database = (IORef [(Label, Post)], IORef [TicTacToe])
type App = Database -> WAI.Request -> (WAI.Response -> IO ()) -> IO ()

login :: App
login database request respond =
  respond $ WAI.responseLBS status200 headers $
      "Username:\n" <>
      "<form action=\"dashboard\">\n" <>
      "<input name=\"username\"></input>" <>
      "</form>\n"

authentication_failed :: App
authentication_failed database request respond =
  respond $ WAI.responseLBS status200 headers $
      "<meta http-equiv=\"refresh\" content=\"0; url=/login\" />"

create_post :: User -> App
create_post username database request respond =
  respond $ WAI.responseLBS status200 headers $
      navbar username <>
      "<form action=\"post\">\n" <>
      "<input type=\"hidden\" name=\"username\" value=\"" <>
      escape username <>
      "\"></input>" <>
      "Permissions: <input name=\"permissions\"></input><br />" <>
      "Content:<br /><textarea name=\"content\"></textarea><br />" <>
      "<input type=\"submit\"></input>" <>
      "</form>\n"

get_winner :: TicTacToe -> Either (Maybe Bool) ()
get_winner game = do  --Either (Maybe Bool)
  let check (x1, y1) (x2, y2) (x3, y3) = do  --Either (Maybe Bool)
       let s1 = board game x1 y1
       let s2 = board game x2 y2
       let s3 = board game x3 y3
       if s1 /= s2 then
         Right ()
       else if s2 /= s3 then
         Right ()
       else case s1 of
         Nothing -> Right ()
         Just x -> Left (Just x)
  check (0, 0) (1, 0) (2, 0)  -- Row 0
  check (0, 1) (1, 1) (2, 1)  -- Row 1
  check (0, 2) (1, 2) (2, 2)  -- Row 2
  check (0, 0) (0, 1) (0, 2)  -- Column 0
  check (1, 0) (1, 1) (1, 2)  -- Column 1
  check (2, 0) (2, 1) (2, 2)  -- Column 2
  check (0, 0) (1, 1) (2, 2)  -- Descending diagonal
  check (2, 0) (1, 1) (0, 2)  -- Ascending diagonal
  let cats = do  --Maybe
       sequence $ flip map [0, 1, 2] $ \y ->
         sequence $ flip map [0, 1, 2] $ \x ->
           board game x y
  if cats == Nothing then
    Right ()
  else
    Left Nothing  -- Cats game

my_turn game username = turn game == player_assignment game username

render_square game username x y =
  let content =
       case board game x y of
         Just True ->
           "X"
         Just False ->
           "O"
         Nothing ->
           if get_winner game == Right () && turn game /= Nothing && my_turn game username then
             "<a href onclick=\"return request('Move+" <>
             escape (show x) <> "+" <> escape (show y) <>
             "')\">#</a>"
           else
             ""  in
  "<div style=\"position: absolute; left: "
  <> escape (show (32 * x)) <>
  "px; top: "
  <> escape (show (32 * y)) <>
  "px; border: 1px solid black; width: 28px; height: 28px; \">\n"
  <> content <>
  "\n</div>\n"

render_roles game username partner =
  let my_role =
       if turn game == Nothing then
         escape username <> ": " <>
         (case player_assignment game username of
           Just True -> "<span style=\"background-color: #80ff80;\">X</span>"
           _ -> "<a href onclick=\"return request('Iam+True')\">X</a>"
         ) <>
         " " <>
         (case player_assignment game username of
           Just False -> "<span style=\"background-color: #80ff80;\">O</span>"
           _ -> "<a href onclick=\"return request('Iam+False')\">O</a>"
         )
       else
         "<div>" <> escape username <> ": " <>
         (case player_assignment game username of
           Nothing -> "undecided"
           Just True -> "X"
           Just False -> "O"
         ) <>
         "</div>"  in
  my_role <>
  "<div>" <> escape partner <> ": " <>
  (case player_assignment game partner of
    Nothing -> "undecided"
    Just True -> "X"
    Just False -> "O"
  ) <>
  "</div>"

render_tictactoe game username partner =
  let sq x y = render_square game username x y  in
  navbar username <>
  "<script>\n" <>
  "  (function() {\n\n\n" <>
  "  document.body.style.margin = '0';\n" <>
  "  setInterval(function () {\n" <>
  "    request('Noop', 0);\n" <>
  "  }, 3000);\n" <>
  "  request = function(action) {\n" <>
  "    const xhr = new XMLHttpRequest();\n" <>
  "    xhr.addEventListener('load', function() {\n" <>
  "      document.body.innerHTML = xhr.responseText;\n" <>
  "    });\n" <>
  "    xhr.open('GET',\n" <>
  "      'tictactoe?username=" <>
         escape username <>
         "&partner=" <>
         escape partner <>
         "&action='+action\n" <>
  "    );\n" <>
  "    xhr.send();\n" <>
  "    return false;\n" <>
  "  };\n" <>
  "  \n\n}());\n" <>
  "</script>\n" <>
  render_roles game username partner <>
  "<div style=\"position: relative; height: 100px;\">\n" <>
  sq 0 0  <>
  sq 0 1  <>
  sq 0 2  <>
  sq 1 0  <>
  sq 1 1  <>
  sq 1 2  <>
  sq 2 0  <>
  sq 2 1  <>
  sq 2 2  <>
  "</div>\n" <>
  "<a href onclick=\"return request('Reset')\">Reset</a><br />" <>
  fromString (List.intercalate "\n<br />\n" (history game))

update_game :: TicTacToe -> Action -> User -> User -> TicTacToe
update_game game action username partner =
  case action of
    Noop ->
      game
    Iam b ->
      if turn game /= Nothing then
        game
      else
        let pa = \u -> if u == username then Just b else player_assignment game u  in
        let game_can_start =
             case map pa (players game) of
               [Just True, Just False] -> True
               [Just False, Just True] -> True
               _                       -> False  in
        game {
          player_assignment = pa,
          turn = if game_can_start then Just True else Nothing,
          history =
            if game_can_start then
              let p0 = players game !! 0  in
              let p1 = players game !! 1  in
              let pa0 = if pa p0 == Just True then "X" else "O"  in
              let pa1 = if pa p1 == Just True then "X" else "O"  in
              let s = "Started game where " ++ p0 ++ " = " ++ pa0 ++ " and " ++ p1 ++ " = " ++ pa1  in
              s : take 4 (history game)
            else
              history game
        }
    Reset ->
      game {
        player_assignment = \_ -> Nothing,
        turn = Nothing,
        board = \_ _ -> Nothing,
        history = (username ++ " reset the game.") : take 4 (history game)
      }
    Move mx my ->
      if turn game /= Nothing && my_turn game username && get_winner game == Right () then
        let intermediate_game = game {
          turn = fmap not (turn game),
          board = \x y ->
            if x == mx && y == my then
              turn game
            else
              board game x y
        }  in
        let victory_info =
             case get_winner intermediate_game of
               Right ()          -> []
               Left (Just True)  -> ["X wins!"]
               Left (Just False) -> ["O wins!"]
               Left Nothing      -> ["Cats game!"]  in
        intermediate_game {
          history = victory_info ++ (username ++ " put " ++ (if turn game == Just True then "X" else "O") ++ " at " ++ show mx ++ ", " ++ show my) : take 4 (history game)
        }
      else
        game

delete_at index list =
  let (list_1, list_2) = List.splitAt index list  in
  list_1 ++ List.drop 1 list_2

do_tictactoe username database request = do  --IO
        game_list <- readIORef (snd database)
        case lookup "partner" (WAI.queryString request) of
          Just (Just p) ->
            let partner = unpack p  in
            if partner == username then
              return "Sorry, but playing a game with yourself is not supported."
            else
              case List.findIndex (\game -> username `elem` players game && partner `elem` players game) game_list of
                Nothing -> do  --IO
                  let new_game = TicTacToe {
                    players = [username, partner],
                    player_assignment = \_ -> Nothing,
                    turn = Nothing,
                    board = \_ _ -> Nothing,
                    history = []
                  }
                  writeIORef (snd database) $ new_game : game_list
                  return (FacetBook.render_tictactoe new_game username partner)
                Just index -> do  --IO
                  let game = game_list !! index
                  let new_game =
                       case lookup "action" (WAI.queryString request) of
                         Just (Just a) ->
                           case readsPrec 0 (unpack a) of
                             [(action, "")] ->
                               FacetBook.update_game game action username partner
                             _ ->
                               game
                         _ ->
                           game
                  writeIORef (snd database) $ new_game : FacetBook.delete_at index game_list
                  return (FacetBook.render_tictactoe new_game username partner)
          _ -> do  --IO
            return $
                "<form action=\"tictactoe\">" <>
                "  Partner:<br />" <>
                "  <input type=\"hidden\" name=\"username\" value=\""<>
                escape username <>
                "\"></input>" <>
                "  <input name=\"partner\"></input>" <>
                "</form>"
