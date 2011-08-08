(ns talk
  (:require [goog.dom :as dom]))

(def svg (.documentElement js/document))

(defn linear [start end duration]
  (let [factor (/ (- end start) duration)]
    (fn [t] (+ start (* t factor)))))

(defn svg-point [x y]
  (let [point (.createSVGPoint svg ())]
    (set! (.x point) x)
    (set! (.y point) y)
    point))

(defn client-rect [elem]
  (let [bbox (.getBBox elem ())
        ctm (.getCTM elem ())
        tl (.matrixTransform (svg-point (.x bbox) (.y bbox)) ctm)
        br (.matrixTransform (svg-point (+ (.x bbox) (.width bbox))
                                        (+ (.y bbox) (.height bbox)))
                             ctm)]
    {:x (.x tl) :y (.y tl)
     :width (- (.x br) (.x tl)) :height (- (.y br) (.y tl))}))

(defn apply-world [w]
  (doseq [[k v] w]
    (if (= k ::view)
      (let [{:keys [x y width height]} v]
        (.setAttribute svg "viewBox" (str x "," y "," width "," height)))
      (let [elem (dom/getElement (name k))]
        (when (:opacity v)
          (set! (.opacity (.style elem))))))))

(def init
  [:view-title
   {:title {:opacity 0}}])

(def steps
  [:view-title {:once/title {:opacity 1}}
   :view-main {}])

(def world (atom nil))

(set! (.onload window)
  (fn []
    (reset! world {::view (client-rect (dom/getElement ":view-title"))})
    (apply-world @world)))
