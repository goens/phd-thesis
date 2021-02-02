(ns wav-transform.core
  (:gen-class)
  ;;(:import WavTransform)
  )

(defn src []
  [["left" "right"]
  ["leftt" "rightt"]
  ["lefttt" "righttt"]
  ["leftttt" "rightttt"]]
  )

(defn split [s]
  [(nth s 0) (nth s 1)])

(defn fft [x]
  (clojure.string/upper-case x)
  )

(defn ifft [x]
  (clojure.string/lower-case x)
  )

(defn filter [x]
  (clojure.string/replace x #"T" "7")
  )

(defn sink [x y]
  (clojure.string/join #" " [x y])
  )

(defn -main
  "Audio filter example"
  [args]
  (ohua
    (smap
      (algo [s]
            (let [[x y] (split s)]
              (let [[xout yout]
                    [(ifft (filter (fft x)))
                     (ifft (filter (fft y)))]]
                (sink xout yout))))
      (src))))
