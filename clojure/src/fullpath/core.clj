(ns fullpath.core)

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))

(defn -main [& argv]
  (let [cwd       (System/getProperty "user.dir")
        paths     (filter-blank argv)
        fullpaths (map #(str cwd "/" %) paths)]
    (println fullpaths)))
