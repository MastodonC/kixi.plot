(ns kixi.plot.colors
  (:require [clojure2d.color :as color]))

(defn color
  ([c]
   (color/color c))
  ([c a]
   (color/color c a)))

(def orange (nth (color/palette :tableau-20) 2))
(def blue (nth (color/palette :tableau-20) 5))
(def green (nth (color/palette :tableau-20) 4))
(def white (color/color :white))
(def palette (color/palette :tableau-20))
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

(defn domain-colors-and-shapes [domain]
  (into {}
        (sequence
         (map (fn [d c s]
                [d {:color c :shape s :legend-shape (legend-shape s)}]))
         domain
         (cycle palette)
         (cycle points))))

(defn colors-and-shapes [historical-transitions]
  (merge (domain-colors-and-shapes (into (sorted-set) (map :setting-1) historical-transitions))
         (domain-colors-and-shapes (into (sorted-set) (map :academic-year-1) historical-transitions))
         (domain-colors-and-shapes (into (sorted-set) (map :need-1) historical-transitions))))
