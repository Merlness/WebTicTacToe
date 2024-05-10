(ns web-tic-tac-toe.server-spec
  (:require [speclj.core :refer :all]
            [web_tic_tac_toe.Server :as sut])
  (:import (Requests Request)))

(describe "Server"

  (it "can handle /tictactoe"
    (let [handler (sut/->TheHandler)
          ttt-request (Request. "GET" "/tictactoe")
          other-request (Request. "GET" "/ttt")]
      (should (.canHandle handler ttt-request))
      (should-not (.canHandle handler other-request))))

  (it "checks handle-tictactoe"
    (let [handler (sut/->TheHandler)
          request (doto (Request. "GET" "/tictactoe") (.setBody ""))
          ]
      (should (.handle handler request))
      ;should contain tic tac toe
      )
    )

  )
