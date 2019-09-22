(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.chord, leipzig.live jazz.instruments))

(def tonic (-> triad (root 7) (inversion 1)))
(def fourth (-> triad (root 3)))
(def fifth (-> triad (root 4)))
