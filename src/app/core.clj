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
                (conj acc [(:id x)
                           (map :id correct-sum)])
                acc)))
          []
          players))

(defn denormalize [[p1 p2s]]
  (map (fn [p2] [p1 p2]) p2s))

(defn flat [xs]
  (apply concat (map denormalize xs)))

(def cli-options
  ;; An option with a required argument
  [["-h" "--height HEIGHT" "Combined Height"
    :parse-fn #(Integer/parseInt %)]])

(defn get-player-by-id [id players-with-id]
  (first (filter (fn [x] (= id (:id x))) players-with-id)))

(defn get-names [id-pairs players-with-id]
  (map (fn [[p1-id p2-id]]
         (let [p1 (get-player-by-id p1-id players-with-id)
               p2 (get-player-by-id p2-id players-with-id)]
           (str
            (:first_name p1) " " (:last_name p1)
            "  :  "
            (:first_name p2) " " (:last_name p2))))
       id-pairs))

(defn run [height]
  (let [players
        (:values (json/parse-string (slurp "playerlist.json") true))

        players-with-id
        (map #(assoc  %1 :id %2) players (range (count players)))

        players-mapped (players-mapped players-with-id)

        matches-found
        (find-matches height
                      players-with-id
                      players-mapped)

        flattened     (flat matches-found)

        sub-sorted (map sort flattened)

        uniq (distinct sub-sorted)

        not-self (remove (fn [[p1-id p2-id]] (= p1-id p2-id))  uniq)

        names (get-names uniq players-with-id)]
    names))

(defn -main [& args]
  (->> (let [height (-> (parse-opts args cli-options)
                        :options
                        :height)
             names (run height)]
         (mapv println names))))


