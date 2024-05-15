(ns web-tic-tac-toe.generate-html-spec
  (:require [speclj.core :refer :all]
            [ttt-clojure.board :as board]
            [ttt-clojure.game :as game]
            [ttt-clojure.game-modes :as gm]
            [ttt-clojure.ui :as ui]
            [web-tic-tac-toe.generate_html :as sut])
  (:import (server FilePathHandler)))

(def base-game
  {:game-id  1
   :player-1 {:kind :human :token "X"}
   :player-2 {:kind :ai :difficulty :easy :token "O"}
   :size     :3x3 :moves []})

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
    (let [expected (str "<h1>Choose your Tic Tac Toe Options</h1>"
                        "<form action=\"/tictactoe\" method=\"post\">"
                        "<p>Please choose which board you want to play with</p>"
                        "<input type=\"radio\" id=\"3x3\" name=\"size\" value=\"3x3\">"
                        "<label for=\"3x3\">3x3</label><br>"
                        "<input type=\"radio\" id=\"4x4\" name=\"size\" value=\"4x4\">"
                        "<label for=\"4x4\">4x4</label>" "<br><br>"
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
                        "<br><input type=\"submit\" value=\"Submit\">" "</form>")]
      (should= expected sut/generate-tictactoe-form)))

  (it "generates buttons for a 3x3 board"
    (let [board [1 "X" 3 "O" 5 6 "X" 8 "O"]
          side 3
          output (sut/game-buttons board side)]
      (should-contain "<button type='submit' name='move' value='1'> " output)
      (should-contain "<button type='submit' name='move' value='X'>X</button>" output)
      (should-contain "<button type='submit' name='move' value='3'> " output)
      (should-contain "<button type='submit' name='move' value='O'>O</button>" output)
      (should-contain "<button type='submit' name='move' value='5'> " output)
      (should-contain "<button type='submit' name='move' value='6'> " output)
      (should-contain "<button type='submit' name='move' value='8'> " output)
      (should-not-contain "<button type='submit' name='move' value='2'> " output)
      (should-not-contain "<button type='submit' name='move' value='4'> " output)
      (should-not-contain "<button type='submit' name='move' value='7'> " output)
      (should-not-contain "<button type='submit' name='move' value='9'> " output)
      (should-contain "<br>" output)))

  (it "generates buttons for an empty 4x4 board"
    (let [board [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]
          side 4
          output (sut/game-buttons board side)]
      (should-not-contain "<button type='submit' name='move' value='X'>X</button>" output)
      (should-not-contain "<button type='submit' name='move' value='O'>O</button>" output)
      (should-contain "<button type='submit' name='move' value='1'> </button>" output)
      (should-contain "<button type='submit' name='move' value='2'> </button>" output)
      (should-contain "<button type='submit' name='move' value='3'> </button>" output)
      (should-contain "<button type='submit' name='move' value='4'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='5'> </button>" output)
      (should-contain "<button type='submit' name='move' value='6'> </button>" output)
      (should-contain "<button type='submit' name='move' value='7'> </button>" output)
      (should-contain "<button type='submit' name='move' value='8'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='9'> </button>" output)
      (should-contain "<button type='submit' name='move' value='10'> </button>" output)
      (should-contain "<button type='submit' name='move' value='11'> </button>" output)
      (should-contain "<button type='submit' name='move' value='12'> </button><br>" output)
      (should-contain "<button type='submit' name='move' value='13'> </button>" output)
      (should-contain "<button type='submit' name='move' value='14'> </button>" output)
      (should-contain "<button type='submit' name='move' value='15'> </button>" output)
      (should-contain "<button type='submit' name='move' value='16'> </button><br>" output)))


  (it "checks hidden map with human and ai"
    (let [game (assoc base-game
                 :player-2 {:kind :ai :difficulty :medium :token "O"}
                 :moves [1 2 3])
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

  (it "returns 'Player 1 human's turn' for an empty moves list"
    (should= "Player 1 human's turn" (sut/turn-message base-game)))

  (it "returns 'Player 2 easy ai's turn' for an odd number of moves"
    (let [game (assoc base-game :moves [1 2 3])]
      (should= "Player 2 easy ai's turn" (sut/turn-message game))))

  (it "returns 'Player 1 human's turn' for an even number of moves"
    (let [game (assoc base-game :moves [1 2])]
      (should= "Player 1 human's turn" (sut/turn-message game))))

  (it "returns 'Player 2 medium ai's turn' for an odd number of moves with medium AI"
    (let [game (assoc base-game :moves [1 2 3]
                                :player-2 {:kind :ai :difficulty :medium :token "O"})]
      (should= "Player 2 medium ai's turn" (sut/turn-message game))))

  (it "returns 'Player 2 hard ai's turn' for an odd number of moves with hard AI"
    (let [game (assoc base-game :moves [1 2 3]
                                :player-2 {:kind :ai :difficulty :hard :token "O"})]
      (should= "Player 2 hard ai's turn" (sut/turn-message game))))

  (it "displays a 3x3 board with mixed tokens"
    (let [board ["X" 2 "O" "X" "O" 6 7 "X" "O"]
          side 3
          output (sut/display-game-board board side)]
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
    (let [output (sut/generate-html base-game)]
      (should-contain "<button type='submit' name='move' value='1'> </button>" output)
      (should-contain "<button type='submit' name='move' value='2'> </button>" output)
      (should-contain "<button type='submit' name='move' value='3'> </button><br>" output)
      (should-contain "</head><body><h1>Tic Tac Toe</h1>" output)
      (should-contain "<div class='turn-message'>" output)
      (should-contain "Player 1 human's turn" output)
      (should-contain "<input type='hidden' name='" output)
      (should-not-contain "Player 2" output)
      (should-not-contain "easy ai's turn" output)
      (should-contain sut/generate-css output)
      (should-contain sut/new-button output)
      (should-not-contain "<div class='cell'" output)
      (should-not-contain ">X</div>" output)
      (should-not-contain ">O</div>" output)))

  (it "tests generate html with game over"
    (let [game (assoc base-game
                 :player-2 {:kind :human :token "O"}
                 :moves [1 2 3 4 5 6 7])
          output (sut/generate-html game)]
      (should-not-contain "<button type='submit' name='move'" output)
      (should-not-contain "Player 2" output)
      (should-not-contain "easy ai's turn" output)
      (should-not-contain "<input type='hidden' name='" output)
      (should-not-contain "<div class='turn-message'>" output)
      (should-contain "</head><body><h1>Tic Tac Toe</h1>" output)
      (should-contain sut/generate-css output)
      (should-contain sut/new-button output)
      (should-contain "<div class='cell'" output)
      (should-contain ">X</div>" output)
      (should-contain ">O</div>" output)))

  )
