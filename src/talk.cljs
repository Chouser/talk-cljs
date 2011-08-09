(ns talk
  (:require [goog.dom :as dom]
            ;[goog.fx :as fx]
            [goog.fx.Animation :as Animation]
            [goog.events :as events]
            [goog.events.KeyHandler :as KeyHandler]))

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

(declare computed-steps)

(def world (atom nil))

(def step (atom 0))

(def duration 1000)

(defn new-transition [start end]
  (if (number? start)
    (if (= start end)
      start
      (linear start end duration))
    (zipmap (keys start)
            (map new-transition (vals start) (vals end)))))

(defn compute-animation [obj t]
  (if (fn? obj)
    (obj t)
    (zipmap (keys obj) (map #(compute-animation % t) (vals obj)))))

(def transition (atom nil))

(defn alter-step [delta]
  (let [i (swap! step #(max 0 (min (dec (count computed-steps)) (+ % delta))))]
    (reset! transition
            (with-meta
              (new-transition @world (nth computed-steps i))
              {:start (js/Date.)}))
    (Animation/registerAnimation transition)))

(set! (.cycle transition)
  (fn []
    (let [trans @transition
          t (- (js/Date.) (:start (meta trans)))
          t (if (> t duration) duration t)]
      (reset! world (compute-animation trans t))
      (apply-world @world)
      (when (>= t duration)
        (Animation/unregisterAnimation transition)))))

(set! (.onload (js* "window"))
  (fn []
    (def computed-steps
      [{::view (client-rect (dom/getElement "view-title"))}
       {::view (client-rect (dom/getElement "view-main"))}])

    (events/listen
      (events/KeyHandler. svg true) KeyHandler/EventType.KEY
      (fn [e]
        (condp = (.keyCode e)
          events/KeyCodes.SPACE (alter-step 1)
          events/KeyCodes.RIGHT (alter-step 1)
          events/KeyCodes.LEFT  (alter-step -1)
          nil)))

    (reset! world (nth computed-steps 0))
    (apply-world @world)))

