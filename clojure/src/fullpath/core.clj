(ns fullpath.core
  (:gen-class))

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))


(defn format-paths [paths]
  (let [chomp             #(clojure.string/trim-newline %)
        ends-with-newline #(str (chomp %) "\n")]
    (if (= 1 (count paths))
        (chomp (first paths))
        (clojure.string/join (map ends-with-newline paths)))))

(defn -main [& argv]
  (let [cwd       (System/getProperty "user.dir")
        paths     (filter-blank argv)
        fullpaths (map #(str cwd "/" %) paths)]
    (print (format-paths fullpaths))
    (flush))) ; <-- ...uhm, why do I have to do this?
