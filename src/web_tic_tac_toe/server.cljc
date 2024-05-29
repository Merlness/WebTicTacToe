(ns web-tic-tac-toe.server
  (:require [web-tic-tac-toe.ttt-handler :as handler])
  (:import (server MyHTTPServer)
           (RequestHandlers IRequestHandler)))

(deftype TheHandler []
  IRequestHandler
  (handle [_this request]
    (handler/handle-tictactoe request))
  (canHandle [_this request]
    (= "/tictactoe" (.getPath request))))

(defn ->MyHTTPServer [^Integer port ^IRequestHandler handler]
  (MyHTTPServer. port handler))

(defn start-server []
  (let [port 8888
        tic-tac-toe-handler (TheHandler.)
        server (->MyHTTPServer port tic-tac-toe-handler)]
    (.start server)))

(defn -main [& args]
  (start-server))
