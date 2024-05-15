(ns web-tic-tac-toe.ttt-handler-spec
  (:require [speclj.core :refer :all]
            [ttt-clojure.game-modes :as gm]
            [ttt-clojure.ui :as ui]
            [web-tic-tac-toe.ttt-handler :as sut]
            [web-tic-tac-toe.generate_html :as gh]
    ;[web-tic-tac-toe.generate_html :as gh]
            )
  (:import (server FilePathHandler Response)))

(def base-game {:game-id  2
                :player-1 {:kind :human :token "X"}
                :player-2 {:kind :human :token "O"}
                :size     :3x3 :moves []})


(describe "Parsing request body and constructing game state map"
  (with-stubs)

  (it "should construct the correct game state map from the given request body"
    (let [request-body "board_size=4x4&player_1=ai_hard&player_2=ai_medium"
          expected-game-state {:game-id  2
                               :player-1 {:kind :hard :token "X"}
                               :player-2 {:kind :medium :token "O"}
                               :size     :4x4
                               :moves    []}
          params (sut/parse-params "board_size=4x4&player_1=ai_hard&player_2=ai_medium")
          ]



      #_(should= expected-game-state
                 (->> request-body
                      (sut/parse-request-body)
                      (sut/construct-game-state)))))

  (it "parses parameter string into a map"
    (let [params-str "board_size=4x4&player_1=ai_hard&player_2=ai_medium"
          expected-map {"board_size" "4x4" "player_1" "ai_hard" "player_2" "ai_medium"}]
      (should= expected-map (sut/parse-params params-str))))

  (it "converts board size into keywords"
    (let [params-map {"board_size" "4x4" "player_1" "ai_hard" "player_2" "ai_medium"}]
      (should= :4x4 (sut/get-board-size params-map "board_size" ""))))

  (it "converts players into maps"
    (should= {:kind :ai :difficulty :hard} (sut/get-player "ai_hard" ""))
    (should= {:kind :ai :difficulty :foo} (sut/get-player "ai_foo" "default"))
    (should= "default" (sut/get-player "ai_" "default")))

  (it "updates with no new moves and a valid new move"
    (let [params-map {"move" "5" "moves" ""}
          expected-moves [5]]
      (should= expected-moves (sut/update-moves params-map base-game))))

  (it "updates with previous moves and a valid new move"
    (let [params-map {"move" "3" "moves" "1%2C2"}
          expected-moves [1 2 3]]
      (should= expected-moves (sut/update-moves params-map base-game))))

  (it "does not update with previous moves and an invalid new move"
    (let [params-map {"move" "-1" "moves" "1%2C2"}
          expected-moves [1 2]]
      (should= expected-moves (sut/update-moves params-map base-game))))

  (it "does not update with previous moves and no new move"
    (let [params-map {"moves" "1%2C2%2C3"}
          expected-moves [1 2 3]]
      (should= expected-moves (sut/update-moves params-map base-game))))

  (it "returns an empty vector when the input string is empty"
    (should= [] (sut/parse-moves "")))

  (it "returns a vector of integers when the input string is populated"
    (should= [1 2 3] (sut/parse-moves "1%2C2%2C3")))

  (it "identifies '-1' as an invalid move"
    (should= true (sut/wrong-move "-1"))
    (should= true (sut/wrong-move "X"))
    (should= true (sut/wrong-move "O"))
    (should= false (sut/wrong-move "5"))
    (should= false (sut/wrong-move "0")))


  (it "does not update with previous moves and invalid string as new move"
    (let [params-map {"move" "X" "moves" "1%2C2"}
          expected-moves [1 2]]
      (should= expected-moves (sut/update-moves params-map base-game))))

  (it "updates the game based on empty body input"
    (let [body ""
          expected-game base-game]
      (should= expected-game (sut/update-game body))))

  (it "updates the game based on request body"
    (let [body "size=4x4&player_1=human&player_2=ai_easy&move=3&moves=1%2C2"
          expected-game (assoc base-game
                          :player-2 {:kind :ai :difficulty :easy :token "O"}
                          :size :4x4
                          :moves [1 2 3])]
      (should= expected-game (sut/update-game body))))

  (it "handles missing size parameter"
    (let [body "player_1=human&player_2=ai_hard&move=2&moves=1"
          expected-game (assoc base-game
                          :player-2 {:kind :ai :difficulty :hard :token "O"}
                          :moves [1 5])]
      (should= expected-game (sut/update-game body))))

  (it "generateResponse"
    (let [response (FilePathHandler/generateResponse "Tic Tac Toe" "the-body")]
      (should= 200 (.getStatusCode response))
      (should= {"Content-Type" "text/html" "Server" "Example Server"} (.getHeader response))
      (should-contain "<title>Tic Tac Toe</title>" (.getBody response))
      (should-contain "<body>the-body</body>" (.getBody response))))

  (it "return a medium move"
    (let [params-map {"move" "4"}
          game (assoc base-game
                 :player-2 {:kind :ai :difficulty :medium :token "O"}
                 :moves [1])
          moves [1]]
      (should= 5 (sut/get-move params-map game moves))))

  (it "return a hard move"
    (let [params-map {"move" "4"}
          game (assoc base-game
                 :player-2 {:kind :ai :difficulty :hard :token "O"}
                 :moves [1 5 2])
          moves [1 5 2]]
      (should= 3 (sut/get-move params-map game moves))))

  (it "returns an easy move"
    (with-redefs [gm/get-move (constantly 9)]
      (let [params-map {"move" "4"}
            game (assoc base-game
                   :player-2 {:kind :ai :difficulty :easy :token "O"}
                   :moves [1])
            moves [1]]
        (should= 9 (sut/get-move params-map game moves)))))

  (it "returns move from params-map for human player"
    (let [params-map {"move" "4"}
          game (assoc base-game :moves [1 2])
          moves [1 2]]
      (should= "4" (sut/get-move params-map game moves))))

  (it "returns default move for human player when no move is provided"
    (let [params-map {}
          game (assoc base-game :moves [1 2])
          moves [1 2]]
      (should= "-1" (sut/get-move params-map game moves))))

  (it "generates an HTML response"
    (let [input "<p>Hello, World!</p>"
          response (sut/generate-response input)
          expected-body "\r\n<!DOCTYPE html><html><head><title>Tic Tac Toe</title></head><body><p>Hello, World!</p></body></html>"]
      (should= expected-body (.getBody response))
      (should= 200 (.getStatusCode response))))

  )
