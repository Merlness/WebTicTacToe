(ns web-tic-tac-toe.ttt-handler
  (:require [clojure.string :as str]
            [ttt-clojure.game :as game]
            [ttt-clojure.game-modes :as gm]
            [web-tic-tac-toe.generate_html :as gh])
  (:import (server FilePathHandler)))

;(def ui-type (atom :web))

(defn get-player [value default]
  (let [[kind difficulty] (str/split (str value) #"_")]
    (cond
      (and (= "ai" kind) difficulty) {:kind :ai :difficulty (keyword difficulty)}
      (= "human" kind) {:kind :human}
      :else default)))

(defn get-board-size [params-map key default]
  (case (get params-map key)
    "3x3" :3x3
    "4x4" :4x4
    default))

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
        size (get-board-size params-map "size" :3x3)
        game-id (+ 1 1)                                     ;change
        player-1-info (get-player (params-map "player_1") {:kind :human})
        player-2-info (get-player (params-map "player_2") {:kind :human})
        initial-game {:game-id  game-id
                      :player-1 (assoc player-1-info :token "X")
                      :player-2 (assoc player-2-info :token "O")
                      :size     size :moves []}
        moves (update-moves params-map initial-game)
        game (assoc initial-game :moves moves)]
    game))

(defn generate-response [input]
  (FilePathHandler/generateResponse "Tic Tac Toe" input))

(defn- respond-with-game [body]
  (-> body update-game gh/generate-html generate-response))

(defn- new-game? [body]
  (or (empty? body)
      (= body "newGame=true")))

;"newGame=true&size=4x4&player_1=ai_predictable&player_2=human&moves=1%2C2"
;"newGame=TRUE" ;ignore case
;"newGame=TrUe"
;"newGame=false"
;"newGame=False"

(defn handle-tictactoe [request]                            ;test
  (let [body (.getBody request)]
    (if (new-game? body)
      (generate-response gh/generate-tictactoe-form)
      (respond-with-game body)))
  #_(let [body (.getBody request)
          params-map (parse-params body)
          new-game (get params-map "newGame" false)
          game (update-game body)]
      (if (or (empty? body) new-game)
        (generate-response gh/generate-tictactoe-form)
        (generate-response (gh/generate-html game)))))






;"<br>"
;"<input type=\"radio\" id=\"3x3x3\" name=\"board_size\" value=\"3x3x3\">"
;"<label for=\"3x3x3\">3x3x3</label>"
