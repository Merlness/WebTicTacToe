(ns web_tic_tac_toe.ttt-handler
  (:require [clojure.string :as str]
            [ttt-clojure.game :as game]
            [ttt-clojure.game-modes :as gm]
            [web-tic-tac-toe.generate_html :as gh])
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

(defn parse-moves [moves-str]
  (if (str/blank? moves-str)
    []
    (mapv #(Integer/parseInt %) (str/split moves-str #"%2C"))))

(defn parse-params [params-str]
  (->> (str/split params-str #"&")
       (map (fn [param-str] (str/split param-str #"=")))
       (reduce (fn [accumulator [key val]] (assoc accumulator key val)) {})))

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
      (generate-response gh/generate-tictactoe-form)
      (generate-response (gh/generate-html game)))))






;"<br>"
;"<input type=\"radio\" id=\"3x3x3\" name=\"board_size\" value=\"3x3x3\">"
;"<label for=\"3x3x3\">3x3x3</label>"
