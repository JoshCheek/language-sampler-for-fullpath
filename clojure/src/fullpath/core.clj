(ns fullpath.core
  (:gen-class))

(require 'clojure.java.shell)

(defn filter-blank [args]
  (filter #(not (clojure.string/blank? %)) args))

(defn filter-flags [args]
  (filter #(not (= \- (first %))) args))

(defn format-paths [cwd paths]
  (let [chomp             #(clojure.string/trim-newline %)
        ends-with-newline #(str (chomp %) "\n")
        pathify           #(str cwd "/" %)]
    (if (= 1 (count paths))
        (pathify (chomp (first paths)))
        (clojure.string/join (map #(pathify (ends-with-newline %)) paths)))))

(defn read-lines [instream]
  (line-seq (java.io.BufferedReader. instream)))

(defn help-screen []
  (str "usage: fullpath *[relative-paths] [-c]\n"
       "\n"
       "  Prints the fullpath of the paths\n"
       "  If no paths are given as args, it will read them from stdin\n"
       "\n"
       "  If there is only one path, the trailing newline is omitted\n"
       "\n"
       "  The -c flag will copy the results into your pasteboard\n"))

(defn -main [& argv]
  (let [help?      (or (some #(= "-h" %) argv) (some #(= "--help" %) argv))
        copy?      (or (some #(= "-c" %) argv) (some #(= "--copy" %) argv))
        cwd        (System/getProperty "user.dir")
        argv-paths (filter-flags (filter-blank argv))]
    (if help?
      (print (help-screen))
      (let [paths           (if (empty? argv-paths) (read-lines *in*) argv-paths)
            formatted-paths (format-paths cwd (filter-blank paths))]
        (print formatted-paths)
        (if copy? (clojure.java.shell/sh "pbcopy" :in formatted-paths))))
    (flush))) ; <-- ...uhm, why do I have to do this?
