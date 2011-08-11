(ns talk
  (:require #_[cljs.reader :as r]
            [goog.dom :as dom]
            [goog.fx.Animation :as Animation]
            [goog.events :as events]
            [goog.events.KeyHandler :as KeyHandler]))

; --- configuration ---

(def default-duration 1000)

(def init {:title {:opacity 0}})

(def steps
  [:view-title {:once/title {:opacity 1}}
   :view-main {}
   :view-read {}
   :view-eval {}])

; --- end configuration ---

(def svg (.documentElement js/document))

(defn parameterize [start end]
  (let [diff (- end start)]
    (fn [t] (+ start (* t diff)))))

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

(defn compute-steps [init steps]
  (loop [rtn [], default init, [[id css] & more-steps] (partition 2 steps)]
    (if-not id
      rtn
      (let [comp-step (-> (into default (zipmap (map (comp keyword name)
                                                     (keys css))
                                                (vals css)))
                          (assoc ::view (client-rect
                                          (dom/getElement (name id)))))
            new-default (into default
                              (remove #(= "once" (namespace (first %))) css))]
        (recur
          (conj rtn comp-step)
          new-default
          more-steps)))))

(defn new-transition [start end]
  (if (number? start)
    (if (= start end)
      start
      (parameterize start end))
    (zipmap (keys start)
            (map new-transition (vals start) (vals end)))))

(defn compute-animation [obj t]
  (cond
    (fn? obj) (obj t)
    (map? obj) (zipmap (keys obj) (map #(compute-animation % t) (vals obj)))
    :else obj))

(defn next-step [old-step steps delta]
  (max 0 (min (dec (count steps)) (+ old-step delta))))

; --- end pure functions ---

(defn apply-world [w]
  (doseq [[k v] w]
    (if (= k ::view)
      (let [{:keys [x y width height]} v]
        (.setAttribute svg "viewBox" (str x "," y "," width "," height)))
      (let [elem (dom/getElement (name k))]
        (when (:opacity v)
          (set! (.opacity (.style elem)) (:opacity v)))))))

(def computed-steps (atom nil))

(def world (atom nil))

(def step (atom 0))

(def transition (atom nil))

(defn alter-step [delta]
  (let [i (swap! step next-step @computed-steps delta)
        current-world @world
        target-world (nth @computed-steps i)]
    (when (not= current-world target-world)
      (set! (.location js/document) (str \# (pr-str {'slide i})))
      (reset! transition
              (with-meta
                (new-transition current-world target-world)
                {:start (js/Date.) :duration default-duration}))
      (Animation/registerAnimation transition))))

(set! (.cycle transition)
  (fn []
    (let [trans @transition
          {:keys [start duration]} (meta trans)
          t (min 1 (/ (- (js/Date.) start) (max duration 1)))]
      (reset! world (compute-animation trans t))
      (apply-world @world)
      (when (>= t 1)
        (Animation/unregisterAnimation transition)))))

(set! (.onload (js* "window"))
  (fn []
    ; We don't want this embedded in the SVG doc, when the adjustments to it are
    ; all in a separate .cljs file.
    #_(let [init (reduce conj {}
                       (for [desc (prim-seq
                                    (.getElementsByTagName svg "desc") 0)]
                         [(keyword (.getAttribute (.parentNode desc) "id"))
                          (r/read-string (.textContent desc))]))]
      (js/alert init))

    ; Hide view boxes
    (doseq [rect (prim-seq (.getElementsByTagName svg "rect") 0)]
      (when (re-find #"^view-" (.id rect))
        (set! (-> rect .style .visibility) "hidden")))

    ; Compute steps
    (reset! computed-steps (compute-steps init steps))

    (events/listen
      (events/KeyHandler. svg true) KeyHandler/EventType.KEY
      (fn [e]
        (condp = (.keyCode e)
          events/KeyCodes.SPACE (alter-step 1)
          events/KeyCodes.RIGHT (alter-step 1)
          events/KeyCodes.LEFT  (alter-step -1)
          nil)))

    (reset! world (nth @computed-steps 0))
    (apply-world @world)))

