(ns web_tic_tac_toe.ttt-handler
  (:require [clojure.string :as str]
            [ttt-clojure.board :as board]
            [ttt-clojure.game :as game]
            [ttt-clojure.game-modes :as gm]
            [ttt-clojure.ui :as ui])

  (:import (server FilePathHandler)))

;(def ui-type (atom :web))

(defn get-value [params-map key default]
  (let [value (get params-map key)]
    (or (case value
          "3x3" :3x3
          "4x4" :4x4
          "ai_hard" {:kind :ai :difficulty :hard}
          "ai_medium" {:kind :ai :difficulty :medium}
          "ai_easy" {:kind :ai :difficulty :easy}
          "human" {:kind :human}
          nil)
        default)))

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
(defn parse-params [params-str]
  (->> (str/split params-str #"&")
       (map (fn [param-str] (str/split param-str #"=")))
       (reduce (fn [accumulator [key val]] (assoc accumulator key val)) {})))

(defn game-buttons [board side]
  (apply str
         (map-indexed (fn [index token]
                        (let [button-value (if (string? token)
                                             (str token)
                                             (+ index 1))]
                          (str "<button type='submit' name='move' value='" button-value "'>"
                               (if (string? token) token " ") "</button>"
                               (when (= (dec side) (mod index side)) "<br>"))))
                      board)))

(defn player-value [player]                                 ;test
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

(defn parse-moves [moves-str]
  (if (str/blank? moves-str)
    []
    (mapv #(Integer/parseInt %) (str/split moves-str #"%2C"))))

(defn wrong-move [move]
  (or (= move "-1") (= move "X") (= move "O")))

(defn get-players [game moves]
  (if (even? (count moves))
    [(:player-1 game) (:player-2 game)]
    [(:player-2 game) (:player-1 game)]))

(defn get-move [params-map game moves]
  (let [[current-player opponent] (get-players game moves)
        game (assoc game :moves moves)
        board (game/convert-moves-to-board game)]
    (if (= :ai (:kind current-player))
      (gm/get-move current-player opponent board)
      (get params-map "move" "-1"))))

(defn update-moves [params-map game]
  (let [moves (parse-moves (get params-map "moves" ""))
        move (get-move params-map game moves)]
    (if (wrong-move move)
      moves
      (conj moves (if (integer? move) move (Integer/parseInt move))))))

(defn update-game [body]
  (let [params-map (parse-params body)
        size (get-value params-map "size" :3x3)
        game-id (+ 1 1)                                     ;change
        player-1-info (get-value params-map "player_1" {:kind :human})
        player-2-info (get-value params-map "player_2" {:kind :human})
        initial-game {:game-id  game-id
                      :player-1 (assoc player-1-info :token "X")
                      :player-2 (assoc player-2-info :token "O")
                      :size     size :moves []}
        moves (update-moves params-map initial-game)
        game (assoc initial-game :moves moves)]
    game))

(defn generate-response [input]
  (FilePathHandler/generateResponse "Tic Tac Toe" input))

(defn handle-tictactoe [request]                            ;test
  (let [body (.getBody request)
        params-map (parse-params body)
        new-game (get params-map "newGame" false)
        game (update-game body)]
    (if (or (empty? body) new-game)
      (generate-response generate-tictactoe-form)
      (generate-response (generate-html game)))))






;"<br>"
;"<input type=\"radio\" id=\"3x3x3\" name=\"board_size\" value=\"3x3x3\">"
;"<label for=\"3x3x3\">3x3x3</label>"
