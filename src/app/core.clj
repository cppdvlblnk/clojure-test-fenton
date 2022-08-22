(ns app.core
  (:require
   [cheshire.core :as json]
   [clojure.tools.cli :refer [parse-opts]]))

(defn players-mapped
  "converts the list of players to a map keyed by their height in
  inches.  Points to a list of players"
  [p2]
  (reduce
   (fn [acc x]
     (let [h (Integer/parseInt (:h_in x))]
       (update acc h concat (list x))))
   {}
   p2))

(defn find-matches [h players height-indexed-m]
  (reduce (fn [acc x]
            (let [h1 (Integer/parseInt (:h_in x))
                  h2 (- h h1)]
              (if-let [correct-sum (get height-indexed-m h2)]
                (conj acc [x correct-sum])
                acc)))
          []
          players))

(defn clean-found-list [xs]
  (->> xs
       (reduce (fn [acc [p1 p2s]]
                 (mapv
                  (fn [p2]
                    (conj acc
                          (str (:first_name p1) " " (:last_name p1))
                          (str (:first_name p2) " " (:last_name p2))))
                  p2s))
               [])
       distinct))

(def cli-options
  ;; An option with a required argument
  [["-h" "--height HEIGHT" "Combined Height"
    :parse-fn #(Integer/parseInt %)]])

(defn run [height]
  (let [players
        (sort-by :h_in (:values (json/parse-string (slurp "playerlist.json") true)))

        players-mapped (players-mapped players)

        matches-found
        (find-matches height
                      players
                      players-mapped)]

    matches-found))

(defn -main [& args]
  (mapv
   (fn [x]
     (println x))
   (-> (-> (parse-opts args cli-options)
           :options
           :height)
       run
       clean-found-list)))

