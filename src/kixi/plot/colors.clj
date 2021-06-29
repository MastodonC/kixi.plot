(ns kixi.plot.colors
  (:require [clojure2d.color :as color]))

(defn color
  ([c]
   (color/color c))
  ([c a]
   (color/color c a)))

;; from Colorgorical http://vrl.cs.brown.edu/color
;; based on "#29733c", "#50b938", "#59c4b8", "#256cc6", "#fa814c", "#fbe44c"
(def mc-20-palette (into []
                         (map color/color)
                         [;; supplied
                          "#29733c", "#50b938", "#59c4b8", "#256cc6", "#fa814c", "#fbe44c",
                          ;; generated
                          "#7c338b", "#f2c8e8", "#270fe2", "#e18af4", "#474556", "#38b5fc", "#bb15b7", "#92a654", "#b71c41", "#b38491", "#743502", "#ff6b97", "#4e4809", "#ff2a0d"]))
(def mc-dark-green (color/color "#29733c"))
(def mc-light-green (color/color "#50b938"))
(def mc-light-blue (color/color "#59c4b8"))
(def mc-dark-blue (color/color "#256cc6"))
(def mc-orange (color/color "#fa814c"))
(def mc-yellow (color/color "#fbe44c"))

(def orange (nth (color/palette :tableau-20) 2))
(def blue (nth (color/palette :tableau-20) 5))
(def green (nth (color/palette :tableau-20) 4))
(def white (color/color :white))
(def tableau-20-palette (color/palette :tableau-20))
(def points [\V \\
             \^ \|
             \O \/
             \o \A
             \> \x
             \v \S
             \{ \s
             \< \}
             \-])

(defn legend-shape [s]
  (case s
    \^ \v
    \A \V
    \v \^
    \V \A
    \\ \/
    \/ \\
    s))

(defn domain-colors-and-shapes
  ([domain palette]
   (into {}
         (sequence
          (map (fn [d c s]
                 [d {:color c :shape s :legend-shape (legend-shape s)}]))
          domain
          (cycle palette)
          (cycle points))))
  ([domain]
   (domain-colors-and-shapes domain tableau-20-palette)))
