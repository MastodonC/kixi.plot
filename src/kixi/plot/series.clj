(ns kixi.plot.series
  (:require [hyperfiddle.rcf :as rcf]
            [kixi.plot.colors :as colors]
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

(rcf/tests

  ;; I get really intersting errors if I leave the color in
  "legend-spec"
  (-> (legend-spec "Foo" colors/mc-orange \^) ;; [:shape "Foo" {:color #vec4 [250.0, 129.0, 76.0, 255.0], :shape \^, :size 15, :stroke {:size 4.0}}]
      (update-in [2] dissoc :color))
  := [:shape "Foo" {:shape \^, :size 15, :stroke {:size 4.0}}]

  "line-series"
  (-> (line-series {:ds (tc/dataset {:calendar-year [0 1 2] :median [0 1 2]})
                    :x :calendar-year
                    :y :median
                    :shape \^}) ;; [:line [[0 0] [1 1] [2 2]] {:color #vec4 [31.0, 119.0, 180.0, 255.0], :point {:type \^, :size 15, :stroke {:size 4}}, :stroke {:size 4}}]
      (update-in [2] dissoc :color))
  := [:line [[0 0] [1 1] [2 2]] {:point {:type \^, :size 15, :stroke {:size 4}}, :stroke {:size 4}}]

  "ribbon-series"
  (-> (ribbon-series {:ds (tc/dataset {:calendar-year [2021 2022 2023]
                                       :q1 [0 1 2]
                                       :q3 [1 2 3]})
                      :x :calendar-year
                      :low-y :q1
                      :high-y :q3})
      (update-in [2] dissoc :color))
  ;; [:ci [[[2021 1] [2022 2] [2023 3]]
  ;;       [[2021 0] [2022 1] [2023 2]]]
  ;;  {:color #vec4 [31.0, 119.0, 180.0, 50.0]}]
  := [:ci [[[2021 1] [2022 2] [2023 3]] [[2021 0] [2022 1] [2023 2]]] {}]

  (into []
        (map (fn [s] (update s 2 dissoc :color)))
        (ds->median-iqr-95-series (tc/dataset {:calendar-year [2021 2022 2023]
                                               :low-95 [0 0 0]
                                               :q1 [0 1 2]
                                               :median [1 2 3]
                                               :q3 [2 3 4]
                                               :high-95 [3 4 5]})
                                  colors/orange
                                  \o))
  ;; [[:line [[2021 1] [2022 2] [2023 3]]
  ;;   {:color #vec4 [255.0, 127.0, 14.0, 255.0], :point {:type \o, :size 15, :stroke {:size 4}}, :stroke {:size 4}}]
  ;;  [:ci [[[2021 2] [2022 3] [2023 4]]
  ;;        [[2021 0] [2022 1] [2023 2]]]
  ;;   {:color #vec4 [255.0, 127.0, 14.0, 50.0]}]
  ;;  [:ci [[[2021 3] [2022 4] [2023 5]]
  ;;        [[2021 0] [2022 0] [2023 0]]]
  ;;   {:color #vec4 [255.0, 127.0, 14.0, 25.0]}]]
  := [[:line [[2021 1] [2022 2] [2023 3]] {:point {:type \o, :size 15, :stroke {:size 4}}, :stroke {:size 4}}]
      [:ci [[[2021 2] [2022 3] [2023 4]] [[2021 0] [2022 1] [2023 2]]] {}]
      [:ci [[[2021 3] [2022 4] [2023 5]] [[2021 0] [2022 0] [2023 0]]] {}]]

  )
