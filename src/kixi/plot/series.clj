(ns kixi.plot.series
  (:require [kixi.plot.colors :as colors]
            [tablecloth.api :as tc]))

(defn line-series [{:keys [ds x y color shape stroke size dash]
                    :or {;;shape \O
                         color colors/blue
                         stroke 4
                         size 15}}]
  (let [data (-> ds
                 (tc/select-columns [x y])
                 (tc/rows))
        line-spec {:color color :point {:type shape :size size :stroke {:size stroke}} :stroke {:size stroke}}
        line-spec (if dash (assoc-in line-spec [:stroke :dash] dash) line-spec)]
    [:line
     data
     line-spec]))

(defn ribbon-series [{:keys [ds x low-y high-y color alpha]
                      :or {color colors/blue
                           alpha 50}}]
  (let [high (-> ds
                 (tc/select-columns [x high-y])
                 (tc/rows))
        low (-> ds
                (tc/select-columns [x low-y])
                (tc/rows))]
    [:ci
     [high low]
     {:color (colors/color color alpha)}]))

(defn ds->median-iqr-95-series
  "Takes a filtered or grouped dataset that only has a single series-name"
  ([ds color shape]
   [(line-series {:ds ds
                  :color color
                  :shape shape
                  :x :calendar-year
                  :y :median})
    (ribbon-series {:ds ds
                    :x :calendar-year
                    :color color
                    :alpha 50
                    :high-y :q3
                    :low-y :q1})
    (ribbon-series {:ds ds
                    :x :calendar-year
                    :color color
                    :alpha 25
                    :high-y :high-95
                    :low-y :low-95})])
  ([ds color shape dash]
   [(line-series {:ds ds
                  :color color
                  :shape shape
                  :dash dash
                  :x :calendar-year
                  :y :median})
    (ribbon-series {:ds ds
                    :x :calendar-year
                    :color color
                    :alpha 50
                    :high-y :q3
                    :low-y :q1})
    (ribbon-series {:ds ds
                    :x :calendar-year
                    :color color
                    :alpha 25
                    :high-y :high-95
                    :low-y :low-95})]))

(defn legend-spec [series-name color shape]
  [:shape series-name
   {:color  color
    :shape  shape
    :size   15
    :stroke {:size 4.0}}])

(defn ds->median-iqr-95-series-and-legend [{::keys [colors-and-shapes ds series-name] :as _d}]
  {::series (ds->median-iqr-95-series
             ds
             (-> series-name colors-and-shapes :color)
             (-> series-name colors-and-shapes :shape))
   ::legend-spec [(legend-spec series-name
                               (-> series-name colors-and-shapes :color)
                               (-> series-name colors-and-shapes :legend-shape))]})

(defn grouped-ds->median-iqr-95-series-and-legend [{::keys [colors-and-shapes grouped-data series-key] :as _d}]
  {::series      (into []
                       (mapcat (fn [[group-key ds]]
                                 (let [series-name (series-key group-key)]
                                   (ds->median-iqr-95-series
                                    ds
                                    (-> series-name colors-and-shapes :color)
                                    (-> series-name colors-and-shapes :shape)))))
                       (tc/groups->map grouped-data))
   ::legend-spec (into []
                       (map (fn [[group-key _]]
                              (let [series-name (series-key group-key)]
                                (legend-spec series-name
                                             (-> series-name colors-and-shapes :color)
                                             (-> series-name colors-and-shapes :legend-shape)))))
                       (into []
                             (sort-by
                              (fn [[group-key _]] (group-key series-key))
                              (tc/groups->map grouped-data))))})

(defn grouped-scenario-ds->median-iqr-95-series-and-legend [{:keys [colors-and-shapes grouped-data]}]
  {::series      (into []
                       (mapcat (fn [[{:keys [series-name]} ds]]
                                 (ds->median-iqr-95-series
                                  ds
                                  (-> series-name colors-and-shapes :color)
                                  (-> series-name colors-and-shapes :shape)
                                  [6.0 3.0])))
                       (tc/groups->map grouped-data))
   ::legend-spec (into []
                       (map (fn [[{:keys [series-name]} _]]
                              (legend-spec series-name
                                           (-> series-name colors-and-shapes :color)
                                           (-> series-name colors-and-shapes :legend-shape))))
                       (into []
                             (sort-by
                              (fn [[{:keys [series-name]} _]] series-name)
                              (tc/groups->map grouped-data))))})

;; FIXME: This pair should be a multimethod
(defn ds->line-and-line-legend [{:keys [series-name color ds x y dash] :as config}]
  {::series (line-series config)
   ::legend-spec [:line series-name
                  {:color color :stroke {:size 4 :dash (when dash dash)} :font "Open Sans"}]})

(defn ds->line-and-shape-legend [{:keys [series-name legend-shape color ds x y shape] :as config}]
  {::series (line-series config)
   ::legend-spec (legend-spec series-name color legend-shape)})

(defn grouped-ds->lines-and-legends [{:keys [ds series-key x-key y-key colors-and-shapes]}]
  (into {}
        (map (fn [m]
               (-> m
                   ((fn [[k ds]] {:series-name (series-key k)
                                  :ds ds}))
                   ((fn [{:keys [series-name] :as m}] (merge m (colors-and-shapes series-name))))
                   (assoc :x x-key :y y-key)
                   ((fn [m] (merge m (ds->line-and-shape-legend m))))
                   ((fn [m] [(:series-name m) m])))))
        (-> ds
            (tc/group-by [series-key])
            (tc/groups->map))))
