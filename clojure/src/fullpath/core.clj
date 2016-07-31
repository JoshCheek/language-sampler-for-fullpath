(ns fullpath.core)

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))


(defn format-paths [paths]
  (let [chomp          #(clojure.string/trim-newline %)
        normalize-path #(str (chomp %) "\n")]
  (clojure.string/join (map normalize-path paths))))

(defn -main [& argv]
  (let [cwd       (System/getProperty "user.dir")
        paths     (filter-blank argv)
        fullpaths (map #(str cwd "/" %) paths)]
    (print (format-paths fullpaths))))
