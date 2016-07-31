(ns fullpath.core
  (:gen-class))

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))


(defn format-paths [cwd paths]
  (let [chomp             #(clojure.string/trim-newline %)
        ends-with-newline #(str (chomp %) "\n")
        pathify           #(str cwd "/" %)
        ]
    (if (= 1 (count paths))
        (pathify (chomp (first paths)))
        (clojure.string/join (map #(pathify (ends-with-newline %)) paths)))))

(defn -main [& argv]
  (let [cwd       (System/getProperty "user.dir")
        paths     (filter-blank argv)
        fullpaths (if (= 0 (count paths))
                      (line-seq (java.io.BufferedReader. *in*))
                      paths)
        ]
    (print (format-paths cwd fullpaths))
    (flush))) ; <-- ...uhm, why do I have to do this?
