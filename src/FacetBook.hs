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

import Util(Post, User, check_credentials, Label, FList(Nil, Cons))
import FIO(FIO(Read, Write, Swap), Fac, FIORef)

data Action =
    Iam Bool
  | Reset
  | Move Int Int
  | Noop
  deriving (Read, Show)
data TicTacToe = TicTacToe {
  players :: [User],
  player_assignment :: [Maybe Bool],  --True means X, False means O
  turn :: Maybe Bool,  -- 'Nothing' means game hasn't started yet.
  board :: Int -> Int -> Maybe Bool,
  history :: [String]
}
type Database = (FIORef Label (FList Post), FIORef Label [TicTacToe])
type App = Database -> WAI.Request -> (WAI.Response -> FIO Label ()) -> FIO Label ()

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
  respond $ WAI.responseLBS status200 headers $
      "<meta http-equiv=\"refresh\" content=\"0; url=/login\" />"

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

my_turn game username = (turn game == (player_assignment game !! 0)) == (players game !! 0 == username)

render_tictactoe game username partner =
  let winner = get_winner game  in
  let sq x y =
       let content =
            case board game x y of
              Just True ->
                "X"
              Just False ->
                "O"
              Nothing ->
                if winner == Right () && turn game /= Nothing && my_turn game username then
                  "<a href onclick=\"return request('" <>
                  escape (show (Move x y)) <>
                  "')\">#</a>"
                else
                  ""  in
       "<div style=\"position: absolute; left: "
       <> escape (show (32 * x)) <>
       "px; top: "
       <> escape (show (32 * y)) <>
       "px; border: 1px solid black; width: 28px; height: 28px; \">\n"
       <> content <>
       "\n</div>\n"  in
  navbar username <>
  "<script>\n" <>
  "  document.body.style.margin = '0';\n" <>
  "  (function() {\n\n\n" <>
  "  let preempted = false;\n" <>
  "  const handle = setInterval(function () {\n" <>
  "    request('Noop');\n" <>
  "  }, 3000);\n" <>
  "  request = function(action, priority) {\n" <>
  "    const xhr = new XMLHttpRequest();\n" <>
  "    xhr.addEventListener('load', function() {\n" <>
  "      clearInterval(handle);\n" <>
  "      if(!preempted)\n" <>
  "        document.body.innerHTML = xhr.responseText;\n" <>
  "      preempted = true;\n" <>
  "    });\n" <>
  "    xhr.open('GET',\n" <>
  "      'tictactoe?username=" <>
         escape username <>
         "&partner=" <>
         escape partner <>
         "&action='+action" <>
  "    );\n" <>
  "    xhr.send();\n" <>
  "  };\n" <>
  "  }());\n" <>
  "</script>\n" <>
  "<div>" <> escape partner <> ": " <>
  (case 
    
  ) <>
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
  fromString (List.intercalate "\n<br />\n" (history game))

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
                turn = Nothing,
                board = \_ _ -> Nothing,
                history = []
              }
              return $ do  --FIO
                Write (snd database) $ return $ new_game : game_list
                respond $ WAI.responseLBS status200 headers $ render_tictactoe new_game username partner
            Just index ->
              let game = game_list !! index  in
              case lookup "action" (WAI.queryString request) of
                Just (Just a) ->
                  case readsPrec 0 (unpack a) of
                    [(action, "")] ->
                      let new_game = case action of
                           Noop ->
                             game
                           Iam b ->
                             if turn game /= Nothing then
                               game
                             else
                               let pa =
                                    if players game !! 0 == username then
                                      [Just b, player_assignment game !! 1]
                                    else
                                      [player_assignment game !! 0, Just b]          in
                               let game_can_start =
                                    case pa of
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
                                     let pa0 = if (player_assignment game !! 0) == Just True then "X" else "O"  in
                                     let pa1 = if (player_assignment game !! 1) == Just True then "X" else "O"  in
                                     let s = "Started game where " ++ p0 ++ " = " ++ pa0 ++ " and " ++ p1 ++ " = " ++ pa1  in
                                     s : take 4 (history game)
                                   else
                                     history game
                               }
                           Reset ->
                             game {
                               player_assignment = [Nothing, Nothing],
                               turn = Nothing,
                               board = \_ _ -> Nothing,
                               history = (username ++ " reset the game.") : take 4 (history game)
                             }
                           Move mx my ->
                             if turn game /= Nothing && my_turn game username && get_winner game == Right () then
                               game {
                                 turn = fmap not (turn game),
                                 board = \x y ->
                                   if x == mx && y == my then
                                     turn game
                                   else
                                     board game x y,
                                 history = (username ++ " put " ++ (if turn game == Just True then "X" else "O") ++ " at " ++ show mx ++ ", " ++ show my) : take 4 (history game)
                               }
                             else
                               game  in
                      return $ do  --FIO
                        Write (snd database) $ return $ new_game : delete_at index game_list
                        respond $ WAI.responseLBS status200 headers $ render_tictactoe new_game username partner
                    _ ->
                      return $ do  --FIO
                        undefined
                _ ->
                  let game = game_list !! index  in
                  return $ do  --FIO
                    respond $ WAI.responseLBS status200 headers $ render_tictactoe game username partner
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
