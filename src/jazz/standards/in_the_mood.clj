(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only [play-buf load-sample freesound-path sample apply-at apply-by now at ctl square perc FREE adsr line:kr sin-osc definst hpf lpf clip2 env-gen]]
        jazz.instruments))

(def tonic (-> triad (root 7) (inversion 1)))
(def fourth (-> triad (root 3)))
(def fifth (-> triad (root 4)))

(def in-the-mood
  (let [bassline (->> [0]
                      (phrase [1]))
        hook #(->> (-> % list)
                   (phrase [8]))
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (take-while #(-> % :time (< 48)))
                  (all :part :beat))]
    (->>
      bassline
      (where :pitch (comp low B flat major))
      (tempo (bpm 90)))))

(comment
  (jam (var in-the-mood))
  (def in-the-mood nil)
)

(defmethod play-note :beat [_]
  (hat))

(defmethod play-note :default
  [{midi :pitch, seconds :duration}]
  (piano midi :duration seconds))
