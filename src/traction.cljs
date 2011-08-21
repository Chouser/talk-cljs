; traction -- presentations without slides
; Copyright 2011 (c) Chris Houser. All rights reserved.

(ns traction
  (:require [goog.net.XhrIo :as xhrio]
            [goog.dom :as dom]
            [goog.window :as gwin]
            [goog.fx.Animation :as Animation]
            [goog.events :as events]
            [goog.events.KeyHandler :as KeyHandler]))

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

(defn tags [elem tag-name]
      ; (dom/$$ tag-name nil elem) ?
  (prim-seq (.getElementsByTagName elem tag-name) 0))

(defn elem-style [sets]
  (reduce (fn [out elem]
            (if-let [duration (.getAttribute elem "duration")]
              (assoc out :duration (js/parseFloat duration))
              (let [id (.getAttribute elem "i")]
                (if (dom/getElement id)
                  (update-in out [id] assoc :opacity
                             (js/parseFloat (.getAttribute elem "opacity")))
                  (js/alert (str "Couldn't find elem " id ))))))
          {} sets))

(defn compute-steps [config-dom]
  (let [init (elem-style (-> config-dom (tags "init") first (tags "set")))
        steps (tags config-dom "step")]
    (loop [rtn [], default init, [step & more-steps] steps]
      (if-not step
        rtn
        (let [view-rect (if-let [view-id (.getAttribute step "view")]
                          (client-rect
                            (or (dom/getElement view-id)
                                (js/alert (str "Couldn't find view "view-id))))
                          (or (:view default)
                              (js/alert "First step requires a view attr")))
              comp-step (-> (into default (elem-style (tags step "set")))
                            (assoc :view view-rect))
              new-default (assoc (->> (tags step "set")
                                      (remove #(.getAttribute % "once"))
                                      elem-style
                                      (into default))
                                 :view view-rect)]
          (recur
            (conj rtn comp-step)
            new-default
            more-steps))))))

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

(defn limit-step [old-step steps f]
  (max 0 (min (dec (count steps)) (f old-step))))

; --- end pure functions ---

(defn apply-world [w]
  (doseq [[k v] w]
    (cond
      (= :view k)
        (let [{:keys [x y width height]} v]
          (.setAttribute svg "viewBox" (str x "," y "," width "," height)))
      (= :duration k)
        nil
      :else
        (let [elem (dom/getElement k)]
          (when (:opacity v)
            (set! (.opacity (.style elem)) (:opacity v)))))))

(def notes-window (atom nil))

(def computed-steps (atom nil))

(def world (atom nil))

(def step (atom 0))

(def transition (atom nil))

(defn alter-step [f]
  (let [i (swap! step limit-step @computed-steps f)
        current-world @world
        target-world (nth @computed-steps i)]
    (when (not= current-world target-world)
      (set! (.hash (.location @notes-window)) (str \# (str "step" i)))
      (set! (.hash (.location js/document)) (str \# (pr-str {'step i})))
      (reset! transition
              (with-meta
                (new-transition current-world target-world)
                {:start (js/Date.) :duration (:duration target-world)}))
      (Animation/registerAnimation transition))))

(defn set-step [i]
  (alter-step (constantly i)))

(defn open-notes [config-dom]
  (let [win (gwin/openBlank ""
                            (.strobj {"target" "traction-notes"
                                      "width" gwin/DEFAULT_POPUP_WIDTH
                                      "height" gwin/DEFAULT_POPUP_HEIGHT
                                      "resizable" true}))
        body (.body (.document win))
        notesdom (dom/getDomHelper body)]
    (reset! notes-window win)
    (.appendChild body
      (.createDom notesdom "style" (.strobj {"type" "text/css"})
        "body{ background: #000; color: #eee; }
        a { color: #88f; text-decoration: underline; cursor: pointer; }"))
    (doseq [[i step] (map-indexed vector (tags config-dom "step"))]
      (let [a (.createDom notesdom
                "a" (.strobj {"name" (str "step" i) "id" (str "step" i)})
                (str "Step " i ": " (.getAttribute step "view")))
            _ (events/listen a "click" ((fn [i] #(set-step i)) i))
            div (.createDom notesdom "div" nil a)]
        (.appendChild div (.cloneNode step true))
        (.appendChild body div)))
    (.focus win)
    (.focus body)))

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
    (xhrio/send "config.xml"
      (fn [x]
        (try
          (let [config (.documentElement (.getResponseXml (.target x) ()))]
            (reset! computed-steps (compute-steps config))
            (reset! world (nth @computed-steps 0))
            (apply-world @world)
            (open-notes config))
          (catch js/Error e
            (js/alert (.stack e))))))

    ; Hide view boxes
    (doseq [rect (prim-seq (.getElementsByTagName svg "rect") 0)]
      (when (re-find #"^view-" (.id rect))
        (set! (-> rect .style .visibility) "hidden")))

    (events/listen
      (events/KeyHandler. svg true) KeyHandler/EventType.KEY
      (fn [e]
        (condp = (.keyCode e)
          events/KeyCodes.SPACE (alter-step inc)
          events/KeyCodes.RIGHT (alter-step inc)
          events/KeyCodes.LEFT  (alter-step dec)
          nil)))))

