(ns fullpath.core)

(defn -main [& argv]
  (let [cwd (System/getProperty "user.dir")
        fullpaths (map #(str cwd "/" %) argv)]
    (println fullpaths)))
