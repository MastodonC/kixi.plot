(ns kixi.plot
  (:require [cljplot.build :as plotb]
            [cljplot.config :as cfg]
            [cljplot.core :as plot]
            [cljplot.render :as plotr]
            [clojure.string :as s]
            [clojure2d.core :as c]
            [kixi.plot.colors :as colors]
            [kixi.plot.series :as series])
  (:import javax.imageio.ImageIO
           java.awt.image.BufferedImage
           java.io.ByteArrayOutputStream))

(defn ->byte-array [^BufferedImage image]
  (when image
    (with-open [out (ByteArrayOutputStream.)]
      (ImageIO/write image "png" out)
      (.toByteArray out))))

(defn add-watermark [c s]
  (let [text-x-anchor (-  (:w c) 10)
        text-y-anchor (- (:h c) 10)]
    (c/with-canvas-> c
      (c/set-color :gray)
      (c/set-font-attributes 10 :italic)
      (c/text s text-x-anchor text-y-anchor :right))))

(defn zero-index-numerical-y-axes [prepped-data]
  (let [[t [_bottom top]] (get-in prepped-data [:extents :y 0])
        margin 0.025]
    (if (= :numerical t)
      (plotb/update-scale prepped-data :y :domain [0 (+ (* margin top) top)])
      prepped-data)))

(defmulti x-values
  "Pull the x-values out of a series"
  first)

(defmethod x-values :line [series]
  (into #{}
        (map first)
        (second series)))

(defmethod x-values :ci [series]
  (into #{}
        (mapcat (fn [line]
                  (map first line)))
        (second series)))

(defmulti y-values
  "Pull the x-values out of a series"
  first)

(defmethod y-values :line [series]
  (into #{}
        (map second)
        (second series)))

(defmethod y-values :ci [series]
  (into #{}
        (mapcat second)
        (second series)))

(defn update-chart-y-axis-ticks [chart-spec _series]
  (let [[t [_bottom top]] (get-in chart-spec [:extents :y 0])]
    (if (and (= :numerical t) (< top 10))
      (plotb/update-scale chart-spec :y :domain [0 10])
      chart-spec)))

(defn update-chart-x-axis-ticks [chart-spec series]
  (let [x-count (->> (into #{}
                           (mapcat x-values)
                           series)
                     count)
        #_(->> series
               (mapcat (fn [x] (second x)))
               (map (fn [y] (first y)))
               (into #{})
               count)]
    (if (< x-count 10)
      (plotb/update-scale chart-spec :x :ticks x-count)
      chart-spec)))

(defn zero-y-index [{::series/keys [series legend-spec]
                     ::keys [x-axis
                             y-axis
                             size
                             legend-label ;; new
                             legend-font-size
                             title
                             watermark]
                     :or {watermark ""}
                     :as chart-spec}]
  (swap! cfg/configuration
         (fn [c]
           (-> c
               (assoc-in [:legend :font] "Open Sans Bold")
               (assoc-in [:legend :font-size] (or legend-font-size 24)))))
  (let [size (or size {:width 1539 :height 1037 :background colors/white}) ;; 1539x1037 is almost exactly the right size to go into the slide
        title-format (or (::format title) {:font-size 36 :font "Open Sans Bold" :font-style :bold :margin 36})]
    (merge chart-spec
           {::canvas
            (-> (into [[:grid]] series) ;; this is good
                (plotb/preprocess-series)
                (plotb/update-scale :x :fmt (::tick-formatter x-axis))
                (update-chart-x-axis-ticks series)
                (plotb/update-scale :y :fmt (::tick-formatter y-axis))
                (zero-index-numerical-y-axes)
                (update-chart-y-axis-ticks series)
                (plotb/add-axes :bottom {:ticks {:font-size 24 :font-style nil}})
                (plotb/add-axes :left {:ticks {:font-size 24 :font-style nil}})
                (plotb/add-label :bottom (::label x-axis) {:font-size 36 :font "Open Sans" :font-style nil})
                (plotb/add-label :left (::label y-axis) {:font-size 36 :font "Open Sans" :font-style nil})
                (plotb/add-label :top (::label title) title-format)
                (plotb/add-legend legend-label legend-spec)
                (plotr/render-lattice size)
                (add-watermark watermark))})))

(defn show [plot]
  (plot/show plot))

(defn save [plot file-name]
  (plot/save plot file-name))

(defn title->filename [title]
  (s/lower-case (str (s/replace title " " "_") ".png")))

(defn add-series-to-plot [plot {::series/keys [series legend-spec] :as series-and-legend}]
  ;; Still need this as merge-with and -> don't play nicely together and keep the order we want in the conj
  (merge-with (fnil conj []) plot series-and-legend))

(def base-pop-chart-spec
  {::x-axis {::tick-formatter int #_(fn [y] (format "%s/%s" (int (dec y)) (int (- y 2000))))
             ::label          "Calendar Years"}
   ::y-axis {::tick-formatter int
             ::label          "Population"}})

(defn millions-formatter [n]
  (format "%.02f" (/ n (* 1e6))))

(def base-cost-chart-spec
  {::x-axis {::tick-formatter int #_(fn [y] (format "%s/%s" (int (dec y)) (int (- y 2000))))
             ::label          "Calendar Years"}
   ::y-axis {::tick-formatter millions-formatter
             ::label          "Cost £ Million"}})

(def seven-number-summary-legend-items
  [[:line "Median/Actual"
    {:color :black :stroke {:size 4} :font "Open Sans"}]
   [:rect "Interquartile range"
    {:color (colors/color :black 50)}]
   [:rect "90% range"
    {:color (colors/color :black 25)}]])

(def seven-number-summary-legend-scenario-items
  [[:line "Median/Actual Baseline"
    {:color :black :stroke {:size 4} :font "Open Sans"}]
   [:line "Median/Actual Scenario"
    {:color :black :stroke {:size 4 :dash [6.0 3.0]} :font "Open Sans"}]
   [:rect "Interquartile range"
    {:color (colors/color :black 50)}]
   [:rect "90% range"
    {:color (colors/color :black 25)}]])

(defn add-overview-legend-items [{::series/keys [legend-spec] :as chart}]
  (assoc chart ::series/legend-spec (into seven-number-summary-legend-items
                                          legend-spec)))

(defn add-scenario-overview-legend-items [{::series/keys [legend-spec] :as chart}]
  (assoc chart ::series/legend-spec (into seven-number-summary-legend-scenario-items
                                          legend-spec)))


(comment


  (let [data #_(sorted-map-by (comp - compare) :A [1 3] :B [3 8] :C [8 10])
        [["2010" [0 3]]
         ["Joiners" [3 12]]
         ["Leavers" [10 12]]
         ["2011" [0 10]]]]
    (-> (plotb/series
         [:grid]
         [#_:stack-horizontal :stack-vertical [:rbar data {:padding 0.05
                                                           :palette ["#e7ba52" "#9467bd" "#1f77b4" "#c7c7c7" "#aec7e8"]}]])
        (plotb/preprocess-series)
        ;;(plotb/update-scale :y :fmt name)
        (plotb/update-scale :x :fmt str)
        (plotb/add-axes :left)
        (plotb/add-axes :bottom)
        ;;(plotb/add-label :bottom "start, end")
        ;;(plotb/add-label :left "task")
        (plotr/render-lattice {:width 400 :height 200 :border 20})
        (plot/show)))

  )
