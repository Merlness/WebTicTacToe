(ns web-tic-tac-toe.ttt-handler-spec
  (:require [speclj.core :refer :all]
            [ttt-clojure.board :as board]
            [ttt-clojure.game :as game]
            [ttt-clojure.ui :as ui]
            [web_tic_tac_toe.ttt-handler :as sut])
  (:import (server FilePathHandler)))



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

  (it "converts specific values in the map"
    (let [params-map {"board_size" "4x4" "player_1" "ai_hard" "player_2" "ai_medium"}
          expected-map {"board_size" :4x4 "player_1" :hard "player_2" :medium}]
      (doseq [[key expected-value] expected-map]
        (should= expected-value (sut/get-value params-map key "")))))

  (it "checks CSS generation"
    (let [expected-css (str "<style>body { padding: 10px;}h1 { margin: 0;}"
                            "button {  width: 60px;  height: 60px;  font-size: 24px;  "
                            "text-align: center;  vertical-align: middle;  line-height: 60px;  margin: 2px;  border: 1px solid #000;}"
                            ".new-game-btn {  width: 120px;  height: 80px;  font-size: 16px;  text-align: center;  vertical-align: middle;  "
                            "background-color: #4CAF50;  color: white;  padding: 10px 20px;  border: none;  cursor: pointer;}</style>")]
      (should= expected-css (sut/generate-css))))

  (it "checks ttt form generation"
    (let [output sut/generate-tictactoe-form]
      (should-contain "<h1>Choose your Tic Tac Toe Options</h1>" output)
      (should-contain "<form action=\"/tictactoe\" method=\"post\">" output)
      (should-contain "<input type=\"radio\" id=\"3x3\" name=\"size\" value=\"3x3\">" output)
      (should-contain "<label for=\"3x3\">3x3</label>" output)
      (should-contain "<input type=\"submit\" value=\"Submit\">" output)))

  (it "checks game buttons"
    (let [board [1 "X" 3 "O" 5 6 "X" 8 "O"]
          side 3
          output (sut/game-buttons board side)]
      (should-contain "<button type='submit' name='move' value='1'> " output)
      (should-contain "<button type='submit' name='move' value='X'>X</button>" output)
      (should-contain "<button type='submit' name='move' value='O'>O</button>" output)
      (should-contain "<br>" output)))

  (it "checks hidden map"
    (let [game {:game-id  1
                :player-1 {:kind :human :token "X"}
                :player-2 {:kind :human :token "O"}
                :size     :3x3 :moves [1 2 3]}
          expected-map (str "<input type='hidden' name='game-id' value='1'>"
                            "<input type='hidden' name='player_1' value='human'>"
                            "<input type='hidden' name='player_2' value='human'>"
                            "<input type='hidden' name='size' value='3x3'>"
                            "<input type='hidden' name='moves' value='1,2,3'>")]
      (should= expected-map (sut/hidden-map game))))

  (it "checks for end html in O win"
    (let [board ["X" 2 "O" "X" "O" 6 7 "X" "O"]
          side 3
          output (sut/display-game-board board side)]
      (should= 9 (count (re-seq #"<div class='cell'" output)))
      (should (> (> (count (re-seq #">X</div>" output)) 0)))
      (should (> (> (count (re-seq #">O</div>" output)) 0)))))

  (it "checks new game button"
    (let [expected-button (str "<form action=\"/tictactoe\" method=\"post\"><button type=\"submit\" "
                               "class=\"new-game-btn\" name=\"newGame\" value=\"true\">New Game</button></form>")]
      (should= expected-button (sut/new-button))))

  (it "updates with no new moves and a valid new move"
    (let [params-map {"move" "5" "moves" ""}
          expected-moves [5]]
      (should= expected-moves (sut/update-moves params-map))))

  (it "updates with previous moves and a valid new move"
    (let [params-map {"move" "3" "moves" "1%2C2"}
          expected-moves [1 2 3]]
      (should= expected-moves (sut/update-moves params-map))))

  (it "does not update with previous moves and an invalid new move"
    (let [params-map {"move" "-1" "moves" "1%2C2"}
          expected-moves [1 2]]
      (should= expected-moves (sut/update-moves params-map))))

  (it "does not update with previous moves and no new move"
    (let [params-map {"moves" "1%2C2%2C3"}
          expected-moves [1 2 3]]
      (should= expected-moves (sut/update-moves params-map))))

  (it "does not update with previous moves and invalid string as new move"
    (let [params-map {"move" "X" "moves" "1%2C2"}
          expected-moves [1 2]]
      (should= expected-moves (sut/update-moves params-map))))

  (it "updates the game based on empty body input"
    (let [body ""
          parsed-map {}
          expected-game {:game-id  2,
                         :player-1 {:kind :human :token "X"},
                         :player-2 {:kind :human :token "O"},
                         :size     :3x3,
                         :moves    []}]
      (with-redefs [sut/parse-params (fn [_] parsed-map)
                    sut/get-value (fn [m k d] (get m k d))
                    sut/update-moves (fn [_] [])]
        (should= expected-game (sut/update-game body)))))

  (it "updates the game based on request body"
    (let [body "size=4x4&player_1=human&player_2=ai_easy&move=3&moves=1,2"
          parsed-map {"size" "4x4", "player_1" "human", "player_2" "ai_easy", "move" "3", "moves" "1,2"}
          expected-game {:game-id  2,
                         :player-1 {:kind "human" :token "X"},
                         :player-2 {:kind "ai_easy" :token "O"},
                         :size     "4x4",
                         :moves    [1 2 3]}]
      (with-redefs [sut/parse-params (fn [_] parsed-map)
                    sut/get-value (fn [m k d] (get m k d))
                    sut/update-moves (fn [_] [1 2 3])]
        (should= expected-game (sut/update-game body)))))

  (it "tests generate html with game still going"
    (with-redefs [sut/generate-css (stub :css)
                  sut/game-buttons (stub :game-buttons)
                  sut/hidden-map (stub :hidden-map)
                  sut/new-button (stub :new-button)]


      (let [game {:game-id  1
                  :player-1 {:kind :human :token "X"}
                  :player-2 {:kind :human :token "O"}
                  :size     :3x3 :moves []}]
        (sut/generate-html game)
        (should-have-invoked :css)
        (should-have-invoked :game-buttons)
        (should-have-invoked :hidden-map)
        (should-have-invoked :new-button))))

  (it "tests generate html with game over"
    (with-redefs [sut/generate-css (stub :css)
                  sut/game-buttons (stub :game-buttons)
                  sut/hidden-map (stub :hidden-map)
                  sut/new-button (stub :new-button)
                  sut/display-game-board (stub :display)
                  ui/endgame-result (stub :endgame-result)
                  ]


      (let [game {:game-id  1
                  :player-1 {:kind :human :token "X"}
                  :player-2 {:kind :human :token "O"}
                  :size     :3x3 :moves [1 2 3 4 5 6 7]}]
        (sut/generate-html game)
        (should-have-invoked :css)
        (should-have-invoked :display)
        (should-have-invoked :endgame-result)
        (should-not-have-invoked :game-buttons)
        (should-not-have-invoked :hidden-map)
        (should-have-invoked :new-button))))

  (it "generateResponse"
    (let [response (FilePathHandler/generateResponse "Tic Tac Toe" "the-body")]
      ;(should= 200 (.getStatusCode response))
      ;(should= {"Content-Type" "text/html" "Server" "Example Server"} (.getHeader response))
      (should-contain "<title>Tic Tac Toe</title>" (.getBody response))
      (should-contain "<body>the-body</body>" (.getBody response))))



  #_(it "returns the Tic Tac Toe form for new games or when the body is empty"
    (with-redefs [sut/parse-params (constantly {})
                  sut/generate-tictactoe-form (stub :ttt-form)
                  sut/generate-response (stub :generate-response)
                  sut/generate-html (stub :html)
                  ]
      (let [request (str "POST / HTTP/1.1"
                         "Host: localhost:1234"
                         "Connection: keep-alive"
                         "\n"
                         "body=nothing")]
        (sut/handle-tictactoe request)
        (should-have-invoked :generate-response)
        )
      ))



  ;(it " recap screen"
  ;      (with-redefs [sut/player-display (stub :player-display)
  ;                    sut/moves-display (stub :moves-display)
  ;                    q/line (stub :line)
  ;                    ]
  ;        (let [state {:game   {:game-id  1
  ;                              :player-1 {:kind :human :token "X"}
  ;                              :player-2 {:kind :human :token "O"}
  ;                              :size     :3x3
  ;                              :moves    [1 7]}
  ;                     :screen :recap}]
  ;          (sut/draw-state state)
  ;          (should-have-invoked :player-display {:with                     [{:kind :human :token "X"} 0.25]
  ;                                                {:kind :human :token "O"} 0.75})
  ;          (should-have-invoked :moves-display {:with [[1 7]]}))))

  )
