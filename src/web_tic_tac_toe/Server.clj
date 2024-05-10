(ns web_tic_tac_toe.Server
  ;(:require [ttt-clojure.sketch :as sketch])
  (:require [ttt-clojure.main-helpers :as helpers]
            [web_tic_tac_toe.ttt-handler :as handler]
            )
  (:import (server MyHTTPServer)
           (RequestHandlers IRequestHandler)))


(deftype TheHandler []
  IRequestHandler
  (handle [_this request]
    (handler/handle-tictactoe request))
  (canHandle [_this request]
    (= "/tictactoe" (.getPath request))))

(defn start-server []
  (let [port 8080
        tic-tac-toe-handler (TheHandler.)
        server (MyHTTPServer. port tic-tac-toe-handler)]
    (.start server)))



(defn -main [& args]
  (start-server))

;(Main/main (into-array String ["-p" "80"]))
;(main/-main)
;(MyHTTPServer. 8080 (TheHandler.))
;(println (helpers/testing :web))
;(println (helpers/testing :default))
;(defmethod helpers/testing :web [type]
;  (str "this is web"))
