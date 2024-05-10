(ns web_tic_tac_toe.ttt-handler
  (:require [clojure.string :as str]
            [ttt-clojure.board :as board]
            [ttt-clojure.game :as game]
            [ttt-clojure.ui :as ui])

  (:import (server FilePathHandler)))

;(def ui-type (atom :web))

(defn get-value [params-map key default]
  (let [value (get params-map key)]
    (or (case value
          "3x3" :3x3
          "4x4" :4x4
          "ai_hard" :hard
          "ai_medium" :medium
          "ai_easy" :easy
          ;
          "hard" :hard
          "medium" :medium
          "easy" :easy
          nil)
        default)))

(defn generate-css []                                       ;make a def - constant
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
       "</form>"
       ))
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
(defn hidden-map [game]
  (prn "(:kind (:player-1 game)):" (:kind (:player-1 game)))
  (prn "(:kind (:player-2 game)):" (:kind (:player-2 game)))
  (str "<input type='hidden' name='game-id' value='" (:game-id game) "'>"
       "<input type='hidden' name='player_1' value='" (name (:kind (:player-1 game))) "'>" ;
       "<input type='hidden' name='player_2' value='" (name (:kind (:player-2 game))) "'>"
       "<input type='hidden' name='size' value='" (name (:size game)) "'>"
       "<input type='hidden' name='moves' value='" (clojure.string/join "," (:moves game)) "'>"))
(defn new-button []                                         ;change to constant def
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
         (generate-css)
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
         (new-button)
         "</body></html>")))

(defn parse-moves [moves-str]
  (if (empty? moves-str)                                    ; use str/blank? when working with strings
    []
    ; use mapv
    (vec (map #(Integer/parseInt %) (str/split moves-str #"%2C")))))

(defn wrong-move [move]
  (or (= move "-1") (= move "X") (= move "O")))

(defn get-move []
  )

(defn update-moves [params-map]
  (let [move (get params-map "move" "-1");
        moves-str (get params-map "moves" "")
        moves (parse-moves moves-str)
        ;if human do above, else get-move from ai

        ]
    (if (wrong-move move)
      moves
      (conj moves (Integer/parseInt move)))))

(defn update-game [body]
  (let [params-map (parse-params body)
        _ (prn "params-map:" params-map)
        size (get-value params-map "size" :3x3)
        player-1 (get-value params-map "player_1" :human);assoc x
        player-2 (get-value params-map "player_2" :human)
        _ (prn "player-1:" player-1)
        __ (prn "player-2:" player-2)
        moves (update-moves params-map)
        ;game-id (data/get-next-game-id)
        game-id (+ 1 1)
        game {:game-id  game-id
              :player-1 {:kind player-1 :token "X"}
              :player-2 {:kind player-2 :token "O"}
              :size     size :moves moves}]
    game))


#_(defmethod mouse-clicked :play [state mouse]
    (let [{:keys [moves player-1 player-2]} (:game state)
          [player opponent] (if (board/player1? moves)
                              [player-1 player-2]
                              [player-2 player-1])
          size (size (:game state))
          index (get-index size mouse)
          game (:game state)
          board (game/convert-moves-to-board game)
          move (if (= :ai (:kind player))
                 (gm/get-move player opponent board)
                 (inc index))
          token (get board index)
          new-moves (conj (:moves game) move)]
      (cond
        (board/game-over? board game)
        (assoc state :screen :again)

        (= :ai (:kind player))
        (do
          (data/save! (assoc game :moves new-moves))
          (assoc-in state [:game :moves] new-moves))

        (available-move? token size move)
        (do
          (data/save! (assoc game :moves new-moves))
          (assoc-in state [:game :moves] new-moves))

        :else state)))

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










; "<p>Please Select Player 1</p>"
;       "<input type=\"radio\" id=\"human1\" name=\"player_1\" value=\"human\">"
;       "<label for=\"human1\">human</label><br>"
;       "<input type=\"radio\" id=\"ai_easy1\" name=\"player_1\" value=\"ai_easy\">"
;       "<label for=\"easy ai1\">easy ai</label><br>"
;       "<input type=\"radio\" id=\"ai_medium1\" name=\"player_1\" value=\"ai_medium\">"
;       "<label for=\"medium ai1\">medium ai</label><br>"
;       "<input type=\"radio\" id=\"ai_hard1\" name=\"player_1\" value=\"ai_hard\">"
;       "<label for=\"hard ai1\">hard ai</label><br><br>"
;       "<p>Please Select Player 2</p>"
;       "<input type=\"radio\" id=\"human2\" name=\"player_2\" value=\"human\">"
;       "<label for=\"human2\">human</label><br>"
;       "<input type=\"radio\" id=\"ai_easy2\" name=\"player_2\" value=\"ai_easy\">"
;       "<label for=\"easy ai2\">easy ai</label><br>"
;       "<input type=\"radio\" id=\"ai_medium2\" name=\"player_2\" value=\"ai_medium\">"
;       "<label for=\"medium ai2\">medium ai</label><br>"
;       "<input type=\"radio\" id=\"ai_hard2\" name=\"player_2\" value=\"ai_hard\">"
;       "<label for=\"hard ai2\">hard ai</label><br><br>"
;"<br>"
;"<input type=\"radio\" id=\"3x3x3\" name=\"board_size\" value=\"3x3x3\">"
;"<label for=\"3x3x3\">3x3x3</label>"
