(ns web-tic-tac-toe.server-spec
  (:require [speclj.core :refer :all]
            [ttt-clojure.computer :as comp]
            [web-tic-tac-toe.generate_html :as gh]
            [web-tic-tac-toe.server :as sut])
  (:import (Requests Request)
           (server IRequestHandler MyHTTPServer)
           (web_tic_tac_toe.server TheHandler)))

;(.callFunction object param1 param2 ...) ;function call
;(.-property object) ;property
;(Request.) ;constructor

(declare handler)

(defmacro should-be-tictactoe-form [response]
  `(let [response# ~response
         body# (.getBody response#)]
     (should-contain "<title>Tic Tac Toe</title>" body#)
     (should-contain (str "<body>" gh/generate-tictactoe-form "</body>") body#)
     (should= 200 (.getStatusCode response#))))

(defmethod comp/ai-move :predictable [grid _player _opponent _difficulty]
  (first (filter number? grid)))

(def base-game {:game-id  2
                :player-1 {:kind :human :token "X"}
                :player-2 {:kind :ai :difficulty :medium :token "O"}
                :size     :3x3 :moves []})

(defprotocol IServer
  (start [_]))

(deftype FakeServer [started? port handler]
  IServer
  (start [_] (reset! started? true))
  )

(describe "Server"
  (context "server"
    (with-stubs)

    (it "tests MyHTTPServer"
      (let [server-1 (sut/->MyHTTPServer 8080 (TheHandler.))
            server-2 (sut/->MyHTTPServer 9000 (TheHandler.))]
        (should= 8080 (.getPort server-1))
        (should= 9000 (.getPort server-2))
        (should-be-a MyHTTPServer server-1)))

    (it "begins a dummy server"
      (let [server (atom nil)
            started? (atom false)]
        (with-redefs [sut/->MyHTTPServer (fn [port handler]
                                           (reset! server (->FakeServer started? port handler)))]
          (sut/-main)
          (should= 8080 (.-port @server))
          (should @started?)
          (should-be-a TheHandler (.-handler @server)))))
    )

  (context "TheHandler"

    (with handler (sut/->TheHandler))

    (it "can handle /tictactoe"
      (let [ttt-request (Request. "GET" "/tictactoe")
            other-request (Request. "GET" "/ttt")]
        (should (.canHandle @handler ttt-request))
        (should-not (.canHandle @handler other-request))))

    (context "handle"

      (it "creates an empty tic tac toe form on an empty request"
        (let [request (Request. "GET" "/tictactoe")
              response (.handle @handler request)]
          (should-be-tictactoe-form response)))

      (it "creates an empty tic tac toe when newGame is specified"
        (let [request (doto (Request. "GET" "/tictactoe")
                        (.setBody "newGame=true"))
              response (.handle @handler request)]
          (should-be-tictactoe-form response)))

      (it "displays an empty 3x3 game"
        (let [request (doto (Request. "GET" "/tictactoe")
                        (.setBody "size=3x3&player_1=human&player_2=ai_medium"))
              response (.handle @handler request)
              body (.getBody response)]
          (should-contain "<title>Tic Tac Toe</title>" body)
          (should-contain (str "<body>" (gh/generate-html base-game) "</body>") body)
          (should= 200 (.getStatusCode response))))

      (it "displays an 3x3 game with one move"
        (let [request (doto (Request. "GET" "/tictactoe")
                        (.setBody "size=3x3&player_1=human&player_2=ai_medium&moves=1"))
              response (.handle @handler request)
              body (.getBody response)
              game (assoc base-game
                     :moves [1 5])]
          (should-contain "<title>Tic Tac Toe</title>" body)
          (should-contain (str "<body>" (gh/generate-html game) "</body>") body)
          (should= 200 (.getStatusCode response))))

      (it "displays a 4x4 game with two moves"
        (let [request (doto (Request. "GET" "/tictactoe")
                        (.setBody "size=4x4&player_1=ai_predictable&player_2=human&moves=1%2C2"))
              response (.handle @handler request)
              body (.getBody response)
              game {:game-id  2
                    :player-1 {:kind :ai :difficulty :predictable :token "X"}
                    :player-2 {:kind :human :token "O"}
                    :size     :4x4
                    :moves    [1 2 3]}]
          (should-contain "<title>Tic Tac Toe</title>" body)
          (should-contain (str "<body>" (gh/generate-html game) "</body>") body)
          (should= 200 (.getStatusCode response))))))

  )
