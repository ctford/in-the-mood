(ns jazz.standards.was-in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only [now play-buf load-sample freesound-path sample apply-at apply-by now at ctl square perc FREE adsr line:kr sin-osc definst hpf lpf clip2 env-gen]]
        jazz.instruments
        [quil.core :only
         [color clear smooth sketch ellipse frame-rate background
          width height stroke stroke-weight fill]]))

(def tonic (-> triad (root 7) (inversion 1)))
(def fourth (-> triad (root 3)))
(def fifth (-> triad (root 4)))

(def bar 8)

(def in-the-mood
  (let [bassline #(->> [0 2 4 5 4 7 5 4 2]
                       (phrase [1 1 1 1/2 1/2 1 1 1 1])
                       (cut bar)
                       (where :pitch (comp lower lower))
                       (where :pitch (from %)))
        hook (mapthen #(->> (phrase (repeat 12 1/2)
                            ;        (->> % vals sort cycle)
                            (repeat %)
                                    )
                            (then (phrase (repeat 5 1/2) (repeat nil)))
                            (cut bar)
                            )
                      [tonic tonic fourth tonic fifth tonic])
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (cut (* bar 6))
                  (all :part :beat))]
    (->>
      (mapthen bassline [0 0 3 0 4 0])
      (with hook)
      (with beat)
      (tempo (comp (scale [2/3 1/3]) (partial * 2)))
      (where :pitch (comp D major))
      (tempo (bpm 120)))))

(comment
  (jam (var in-the-mood) play-at)
  (play play-at in-the-mood)
  (play-at (now) (second in-the-mood))
  (def in-the-mood nil)
)

(defonce hits (atom []))
(defonce plinks (atom []))

(comment
  (do
    (reset! hits [])
    (reset! plinks []))

  (sketch
    :setup (fn [] (smooth) (background 200))
    :draw  (fn []
             (let [current (now)
                   hits-played (->> hits deref (filter #(< % current)))
                   plinks-played (->> plinks deref (filter #(< (:time %) current)))]
               (clear)
               (stroke-weight 5)
               (fill (color 135 4 55))
               (doseq [epoch (take-last 1 hits-played)]
                 (ellipse
                   (mod (/ epoch 30) 923)
                    (+ 100 (/ (- current epoch) 20)) 
                   50
                   50))
               (fill (color 234 123 28))
               (doseq [note plinks-played]
                 (ellipse
                   (mod (+ 350 (* 5 (- (:pitch note) 50))) 923)
                   (+ 50 (/ (- current (:time note)) 20))
                   10
                   (* 100 (:duration note))))))
    :size [1024 768])
  )

(defmethod play-note :beat [{epoch :time}]
  (do
    (swap! hits conj epoch)
    (hat)))

(defmethod play-note :default
  [{midi :pitch, seconds :duration :as note}]
  (do
    (swap! plinks conj note)
    (-> midi (piano :duration seconds))))
