(ns web-tic-tac-toe.generate_html
  (:require [clojure.string :as str]
            [ttt-clojure.board :as board]
            [ttt-clojure.game :as game]
            [ttt-clojure.ui :as ui]))


(def generate-css
  (str "<style>"
       "body { padding: 10px;}"
       "h1 { margin: 0;}"
       "button {"
       "  width: 60px;"
       "  height: 60px;"
       "  font-size: 24px;"
       "  text-align: center;"
       "  vertical-align: middle;"
       "  line-height: 60px;"
       "  margin: 2px;"
       "  border: 1px solid #000;"
       "}"
       ".new-game-btn {"
       "  width: 120px;"
       "  height: 80px;"
       "  font-size: 16px;"
       "  text-align: center;"
       "  vertical-align: middle;"
       "  background-color: #4CAF50;"
       "  color: white;"
       "  padding: 10px 20px;"
       "  border: none;"
       "  cursor: pointer;"
       "}"
       "</style>"))

(def generate-tictactoe-form
  (str "<h1>Choose your Tic Tac Toe Options</h1>"
       "<form action=\"/tictactoe\" method=\"post\">"
       "<p>Please choose which board you want to play with</p>"
       "<input type=\"radio\" id=\"3x3\" name=\"size\" value=\"3x3\">"
       "<label for=\"3x3\">3x3</label><br>"
       "<input type=\"radio\" id=\"4x4\" name=\"size\" value=\"4x4\">"
       "<label for=\"4x4\">4x4</label>"
       "<br><br>"

       "<p>Please Select Player 1</p>"
       "<input type=\"radio\" id=\"human1\" name=\"player_1\" value=\"human\">"
       "<label for=\"human1\">human</label><br>"
       "<input type=\"radio\" id=\"ai_easy1\" name=\"player_1\" value=\"ai_easy\">"
       "<label for=\"easy ai1\">easy ai</label><br>"
       "<input type=\"radio\" id=\"ai_medium1\" name=\"player_1\" value=\"ai_medium\">"
       "<label for=\"medium ai1\">medium ai</label><br>"
       "<input type=\"radio\" id=\"ai_hard1\" name=\"player_1\" value=\"ai_hard\">"
       "<label for=\"hard ai1\">hard ai</label><br><br>"

       "<p>Please Select Player 2</p>"
       "<input type=\"radio\" id=\"human2\" name=\"player_2\" value=\"human\">"
       "<label for=\"human2\">human</label><br>"
       "<input type=\"radio\" id=\"ai_easy2\" name=\"player_2\" value=\"ai_easy\">"
       "<label for=\"easy ai2\">easy ai</label><br>"
       "<input type=\"radio\" id=\"ai_medium2\" name=\"player_2\" value=\"ai_medium\">"
       "<label for=\"medium ai2\">medium ai</label><br>"
       "<input type=\"radio\" id=\"ai_hard2\" name=\"player_2\" value=\"ai_hard\">"
       "<label for=\"hard ai2\">hard ai</label><br><br>"
       "<br>"

       "<input type=\"submit\" value=\"Submit\">"
       "</form>"))


(defn game-buttons [board side]
  (apply str
         (map-indexed
           (fn [index token]
             (let [button-value (if (string? token)
                                  (str token)
                                  (+ index 1))]
               (str "<button type='submit' name='move' value='" button-value "'>"
                    (if (string? token) token " ") "</button>"
                    (when (= (dec side) (mod index side)) "<br>"))))
           board)))

(defn player-value [player]
  (let [kind (:kind player)
        difficulty (:difficulty player)]
    (if difficulty
      (str (name kind) "_" (name difficulty))
      (name kind))))
(defn hidden-map [game]
  (let [player-1 (:player-1 game)
        player-2 (:player-2 game)]
    (str "<input type='hidden' name='game-id' value='" (:game-id game) "'>"
         "<input type='hidden' name='player_1' value='" (player-value player-1) "'>"
         "<input type='hidden' name='player_2' value='" (player-value player-2) "'>"

         "<input type='hidden' name='size' value='" (name (:size game)) "'>"
         "<input type='hidden' name='moves' value='" (clojure.string/join "," (:moves game)) "'>")))

(def new-button
  (str "<form action=\"/tictactoe\" method=\"post\">"
       "<button type=\"submit\" class=\"new-game-btn\" name=\"newGame\" value=\"true\">New Game</button>"
       "</form>"))

(defn display-game-board [board side]
  (apply str
         (map-indexed (fn [i token]
                        (let [content (if (string? token) token " ")]
                          (str "<div class='cell' style='width: 60px; height: 60px; line-height: 60px; "
                               "display: inline-block; border: 1px solid #000; text-align: center; "
                               "vertical-align: middle;'>"
                               content "</div>"
                               (when (= (dec side) (mod i side)) "<br>"))))
                      board)))

(defn generate-html [game]
  (let [board (game/convert-moves-to-board game)
        game-over? (board/game-over? board game)
        side (Math/sqrt (count board))]
    (str "<!DOCTYPE html><html><head>"
         generate-css
         "</head><body><h1>Tic Tac Toe</h1>"
         (if game-over?
           (str "<div class='game-result'>"
                (display-game-board board side)
                "</div>"
                (ui/endgame-result board "X" "O"))
           (str "<form method='POST'>"
                (game-buttons board side)
                (hidden-map game)
                "</form>"))
         new-button
         "</body></html>")))
