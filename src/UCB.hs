{-# LANGUAGE OverloadedStrings #-}
module UCB where

{-
import Control.Applicative
import Control.Monad(liftM, ap)
import Data.IORef
import qualified Network.Wai.Handler.Warp as Warp(run)
import Network.Wai.Internal(ResponseReceived(ResponseReceived))
-}
import qualified Data.List as List(intercalate, splitAt, drop, findIndex)
import Data.Monoid((<>))
import Data.String(fromString)
import qualified Data.ByteString.Lazy.Char8 as ByteString(intercalate)
import Network.HTTP.Types.Status(status200, status404)
import qualified Network.Wai as WAI(Request, pathInfo, Response, responseLBS)

import Shared(Post, User, check_credentials, Label, FList(Nil, Cons), get_parameter, valid_username)
import FIO(FIO(Read, Write, Swap), Fac(), FIORef)

type Ref = FIORef Label
type PostList = FList Post
type XIO = FIO Label

data Action =
    Iam Bool
  | Reset
  | Move Int Int
  | Noop
  deriving (Read, Show)
data TicTacToe = TicTacToe {
  players :: [User],
  player_assignment :: User -> Maybe Bool,  -- 'True' means X, 'False' means O
  turn :: Maybe Bool,  -- 'Nothing' means game hasn't started yet.
  board :: Int -> Int -> Maybe Bool,
  history :: [String]
}
type Database = (Ref PostList, Ref [TicTacToe])
type Handler = Database -> (WAI.Response -> XIO ()) -> XIO ()

headers = [("Content-Type", "text/html")]

navbar username =
  "<div><a href=\"login\">Logout</a></div>"

login :: Handler
login database respond =
  respond $ WAI.responseLBS status200 headers $
      "Username:\n" <>
      "<form action=\"dashboard\">\n" <>
      "<input name=\"username\"></input>" <>
      "</form>\n"

authentication_failed :: Handler
authentication_failed database respond =
  respond $ WAI.responseLBS status200 headers $
      "<meta http-equiv=\"refresh\" content=\"0; url=/login\" />"

do_create_post :: User -> Post -> [User] -> Handler
do_create_post username content users database respond = do  --FIO
  d <- Read (fst database)
  Write (fst database) $ return $ Cons (username ++ ": " ++ content) d
  respond $ WAI.responseLBS status200 headers $
      "<meta http-equiv=\"refresh\" content=\"0; url=/dashboard?username="<>escape username<>"\" />"

compose_post :: User -> Handler
compose_post username database respond =
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

dashboard :: User -> Handler
dashboard username database respond = do  --FIO
  d <- Read (fst database)
  Swap $ do  --Fac
    posts <- flatten d
    return $ do  --FIO
      respond $ WAI.responseLBS status200 headers $
          navbar username <>
          "<br /><a href=\"tictactoe?username=" <>
          escape username <>
          "\">Play TicTacToe</a><br />" <>
          "<a href=\"/post?username="<>escape username<>"\">Create post</a><br />" <>
          "Recent posts:<br />" <>
          ByteString.intercalate "<hr />" (map escape (take 20 posts))
  return ()

not_found :: Handler
not_found _ respond = do  --FIO
  respond $ WAI.responseLBS status404 [] "404 bad request"

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
  f ('\n':cs) a = f cs (reverse "<br />" ++ a)
  f (c   :cs) a = f cs (c:a)
  f []        a = a
  s' = reverse (f s [])

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

read_ref :: Ref a -> (a -> XIO ()) -> XIO ()
read_ref r g = do
  fx <- Read r
  Swap $ do  --Fac
    x <- fx
    return (g x)
  return ()

tictactoe_play username partner action database respond =
  if partner == username then
    respond $ WAI.responseLBS status200 headers $
        "Sorry, but playing a game with yourself is not supported."
  else
    read_ref (snd database) $ \game_list ->
      let maybe_game = List.findIndex (\game ->
             username `elem` players game  &&
             partner  `elem` players game
           ) game_list  in
      case maybe_game of
        Nothing -> do  --FIO
          let new_game = TicTacToe {
            players = [username, partner],
            player_assignment = \_ -> Nothing,
            turn = Nothing,
            board = \_ _ -> Nothing,
            history = []
          }
          Write (snd database) $ return $ new_game : game_list
          respond $ WAI.responseLBS status200 headers $ render_tictactoe new_game username partner
        Just index -> do  --FIO
          let game = game_list !! index
          let new_game =
               case readsPrec 0 action of
                 [(parsed_action, "")] ->
                   update_game game parsed_action username partner
                 _ ->
                   game
          Write (snd database) $ return $ new_game : delete_at index game_list
          respond $ WAI.responseLBS status200 headers $ render_tictactoe new_game username partner

tictactoe_select_partner username database respond =
  respond $ WAI.responseLBS status200 headers $
      "<form action=\"tictactoe\">" <>
      "  Partner:<br />" <>
      "  <input type=\"hidden\" name=\"username\" value=\""<>
      escape username <>
      "\"></input>" <>
      "  <input name=\"partner\"></input>" <>
      "</form>"

parse_request :: WAI.Request -> Handler
parse_request request =
  if WAI.pathInfo request == ["login"] then
    login
  else case check_credentials request of
    Nothing ->
      authentication_failed
    Just username -> case WAI.pathInfo request of
      ["post"] ->
        let content = get_parameter request "content"  in
        let permissions = get_parameter request "permissions"  in
        let users = words permissions  in
        if content /= "" && all valid_username users then
          do_create_post username content users
        else
          compose_post username
      ["dashboard"] ->
        dashboard username
      ["tictactoe"] ->
        let partner = get_parameter request "partner"  in
        if valid_username partner then
          let action = get_parameter request "action"  in
          tictactoe_play username partner action
        else
          tictactoe_select_partner username
      _ ->
        not_found

handle_request :: Fac Label WAI.Request -> Handler
handle_request faceted_request database respond = do  --FIO
  Swap $ do  --Fac
    request <- faceted_request
    return $ do  --FIO
      let handler = parse_request request
      handler database respond
  return ()
