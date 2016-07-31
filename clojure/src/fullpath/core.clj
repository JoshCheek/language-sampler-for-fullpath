(ns fullpath.core
  (:gen-class))

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))


(defn format-paths [paths]
  (let [chomp          #(clojure.string/trim-newline %)
        normalize-path #(str (chomp %) "\n")]
    ; check how many paths there are, if there is only one, we don't want the newline
    (clojure.string/join (map normalize-path paths))))

(defn -main [& argv]
  (let [cwd       (System/getProperty "user.dir")
        paths     (filter-blank argv)
        fullpaths (map #(str cwd "/" %) paths)]
    (print (format-paths fullpaths))
    (flush))) ; <-- ...uhm, why do I have to do this?
