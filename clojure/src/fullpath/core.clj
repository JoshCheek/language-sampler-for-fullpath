(ns fullpath.core
  (:gen-class))

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))

(defn filter-flags [args]
  (filter #(not (= \- (first %))) args))

(defn format-paths [cwd paths]
  (let [chomp             #(clojure.string/trim-newline %)
        ends-with-newline #(str (chomp %) "\n")
        pathify           #(str cwd "/" %)
        ]
    (if (= 1 (count paths))
        (pathify (chomp (first paths)))
        (clojure.string/join (map #(pathify (ends-with-newline %)) paths)))))

(defn read-lines [instream]
  (line-seq (java.io.BufferedReader. instream)))

(defn print-help []
  (println "usage: fullpath *[relative-paths] [-c]")
  (println)
  (println "  Prints the fullpath of the paths")
  (println "  If no paths are given as args, it will read them from stdin")
  (println)
  (println "  If there is only one path, the trailing newline is omitted")
  (println)
  (println "  The -c flag will copy the results into your pasteboard"))

(defn -main [& argv]
  (let [help?     (or (some #(= "-h" %)     argv)
                      (some #(= "--help" %) argv))
        copy?     (or (some #(= "-c" %)     argv)
                      (some #(= "--copy" %) argv))
        cwd       (System/getProperty "user.dir")
        paths     (filter-flags (filter-blank argv))]
    (if help?
      (print-help)
      (let [fullpaths (if (empty? paths)
                          (filter-blank (read-lines *in*))
                          paths)]
           (print (format-paths cwd fullpaths))))
    (flush))) ; <-- ...uhm, why do I have to do this?
