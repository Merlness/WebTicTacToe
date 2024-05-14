(ns web-tic-tac-toe.generate-html-spec
  (:require [speclj.core :refer :all]
            [ttt-clojure.game-modes :as gm]
            [ttt-clojure.ui :as ui]
            [web-tic-tac-toe.generate_html :as sut])
  (:import (server FilePathHandler)))



(describe "generating html"
  (with-stubs)

  (it "checks CSS generation"
    (let [expected-css (str "<style>body { padding: 10px;}h1 { margin: 0;}"
                            "button {  width: 60px;  height: 60px;  font-size: 24px;  "
                            "text-align: center;  vertical-align: middle;  line-height: 60px;  margin: 2px;  border: 1px solid #000;}"
                            ".new-game-btn {  width: 120px;  height: 80px;  font-size: 16px;  text-align: center;  vertical-align: middle;  "
                            "background-color: #4CAF50;  color: white;  padding: 10px 20px;  border: none;  cursor: pointer;}</style>")]
      (should= expected-css sut/generate-css)))

  (it "checks ttt form generation"
    (let [output sut/generate-tictactoe-form]
      (should-contain "<h1>Choose your Tic Tac Toe Options</h1>" output)
      (should-contain "<form action=\"/tictactoe\" method=\"post\">" output)
      (should-contain "<input type=\"radio\" id=\"3x3\" name=\"size\" value=\"3x3\">" output)
      (should-contain "<label for=\"3x3\">3x3</label>" output)
      (should-contain "<label for=\"4x4\">4x4</label>" output)
      (should-contain "<p>Please Select Player 1</p>" output)
      (should-contain "<label for=\"easy ai1\">easy ai</label>" output)
      (should-contain "<label for=\"medium ai1\">medium ai</label>" output)
      (should-contain "<input type=\"radio\" id=\"ai_hard1\" name=\"player_1\" value=\"ai_hard\">" output)
      (should-contain "<p>Please Select Player 2</p>" output)
      (should-contain "<label for=\"hard ai2\">hard ai</label>" output)
      (should-contain "<input type=\"submit\" value=\"Submit\">" output)))

  (it "generates buttons for a 3x3 board"
    (let [board [1 "X" 3 "O" 5 6 "X" 8 "O"]
          side 3
          output (sut/game-buttons board side)]
      (should-contain "<button type='submit' name='move' value='1'> " output)
      (should-contain "<button type='submit' name='move' value='X'>X</button>" output)
      (should-contain "<button type='submit' name='move' value='O'>O</button>" output)
      (should-contain "<button type='submit' name='move' value='8'> " output)
      (should-contain "<br>" output)))

  (it "generates buttons for an empty 4x4 board"
    (let [board [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]
          side 4
          output (sut/game-buttons board side)]
      (should-contain "<button type='submit' name='move' value='1'> </button>" output)
      (should-contain "<button type='submit' name='move' value='4'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='6'> </button>" output)
      (should-contain "<button type='submit' name='move' value='8'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='11'> </button>" output)
      (should-contain "<button type='submit' name='move' value='12'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='16'> </button><br>" output)))


  (it "checks hidden map with human and ai"
    (let [game {:game-id  1
                :player-1 {:kind :human :token "X"}
                :player-2 {:kind :ai :difficulty :medium :token "O"}
                :size     :3x3 :moves [1 2 3]}
          expected-map (str "<input type='hidden' name='game-id' value='1'>"
                            "<input type='hidden' name='player_1' value='human'>"
                            "<input type='hidden' name='player_2' value='ai_medium'>"
                            "<input type='hidden' name='size' value='3x3'>"
                            "<input type='hidden' name='moves' value='1,2,3'>")]
      (should= expected-map (sut/hidden-map game))))

  (it "returns kind for player without difficulty"
    (let [player {:kind :human}]
      (should= "human" (sut/player-value player))))

  (it "returns kind and difficulty for player with difficulty"
    (let [player-1 {:kind :ai :difficulty :easy}
          player-2 {:kind :ai :difficulty :medium}
          player-3 {:kind :ai :difficulty :hard}]
      (should= "ai_easy" (sut/player-value player-1))
      (should= "ai_medium" (sut/player-value player-2))
      (should= "ai_hard" (sut/player-value player-3))))

  (it "displays a 3x3 board with mixed tokens"
    (let [board ["X" 2 "O" "X" "O" 6 7 "X" "O"]
          side 3
          output (sut/display-game-board board side)]
     ; (prn "output:" output)
      (should-contain "<div class='cell'" output)
      (should-contain ">X</div>" output)
      (should-contain ">O</div>" output)
      (should-contain "<div class='cell' style='width: 60px; height: 60px; line-height: 60px; display: inline-block; border: 1px solid #000; text-align: center; vertical-align: middle;'> </div>" output)
      (should-contain "<br>" output)))

  (it "checks new game button"
    (let [expected-button (str "<form action=\"/tictactoe\" method=\"post\"><button type=\"submit\" "
                               "class=\"new-game-btn\" name=\"newGame\" value=\"true\">New Game</button></form>")]
      (should= expected-button sut/new-button)))

  (it "tests generate html with game still going"
    (with-redefs [sut/game-buttons (stub :game-buttons)
                  sut/hidden-map (stub :hidden-map)]
      (let [game {:game-id  1
                  :player-1 {:kind :human :token "X"}
                  :player-2 {:kind :human :token "O"}
                  :size     :3x3 :moves []}]
        (sut/generate-html game)
        (should-have-invoked :game-buttons)
        (should-have-invoked :hidden-map))))

  (it "tests generate html with game over"
    (with-redefs [sut/game-buttons (stub :game-buttons)
                  sut/hidden-map (stub :hidden-map)
                  sut/display-game-board (stub :display)
                  ui/endgame-result (stub :endgame-result)]
      (let [game {:game-id  1
                  :player-1 {:kind :human :token "X"}
                  :player-2 {:kind :human :token "O"}
                  :size     :3x3 :moves [1 2 3 4 5 6 7]}]
        (sut/generate-html game)
        (should-have-invoked :display)
        (should-have-invoked :endgame-result)
        (should-not-have-invoked :game-buttons)
        (should-not-have-invoked :hidden-map))))

  )
