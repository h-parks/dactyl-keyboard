(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [clojure.core.matrix.operators :refer [+ - / *]]))

(def ^:const LEFT 1)
(def ^:const RIGHT 2)
(def ^:const FAST_RENDER false)
(def ^:const RESTS_SEPERATE false)
(def ^:const STANDS_SEPERATE false)

(defn offset-case-place [offset block]
  (->> block
       (translate [0 0 1])
       (translate offset)))

(def additional-thumb-x-offset -1)  				;;additional offset for thumb cluster-0 by default

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def plate-thickness 2) ; was 4

(def keyswitch-height 13) ;; Was 14.1, then 14.25
(def keyswitch-width 17.2)
(def keyswitch-notch-width 15.5)
(def keyswitch-notch-height 1.1)

(def key-height 7.4) ; was 12.7, then 10.4
(def dsa-profile-key-height 7.4)
(def key-z (+ plate-thickness 6)) ; 3 is pressed, 7 is released

(def mount-width (+ keyswitch-width 1.4))
(def mount-height (+ keyswitch-height 4.8))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 2.4 plate-thickness)
                      (translate [0
                                  (+ (/ 2.8 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (union (->> (cube 1.5 (+ keyswitch-height 4.8) plate-thickness)
                              (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                          0
                                          (/ plate-thickness 2)]))
                         (->> (cube 1.5 (+ keyswitch-height 4.8) 1.1)
                              (translate [(+ (/ 1.5 2) (/ keyswitch-notch-width 2))
                                          0
                                          (- plate-thickness
                                             (/ keyswitch-notch-height 2))]))
                         )
        plate-half (union top-wall left-wall)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18)
(def sa-double-length 37.05)
(def sa-cap {1 (let [bl2 (/ sa-length 2)
                     m (/ dsa-profile-key-height 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 key-height])))]
                 (->> key-cap
                      (translate [0 0 key-z])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 key-height])))]
                 (->> key-cap
                      (translate [0 0 key-z])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 key-height])))]
                   (->> key-cap
                        (translate [0 0 (+ 6 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 6))
(def rows (range 0 5))

(def α (/ π 12))
(def β (/ π 36))
(def cap-top-height (+ plate-thickness key-height))
(def row-radius (+ (/ (/ (+ mount-height 1/2) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width 2) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))

(defn key-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-row-offset (cond
		                        (<= column 1) [0 -1.5 0] ;;was moved -4.5
                        (= column 2) [0 2.82 -4.5]
                        (>= column 4) [0 -6.3 5.64]
                        :else [0 0 0])
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-row-offset))]
    (->> placed-shape
         (rotate (/ π 12) [0 1 0])
         (translate [0 0 13]))))

(defn case-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset (if (= column 6)
                        [-7.25 -6.3 2.1]
                        [0 -3.35 4.9])
        row-offset (if (= row 0)
                     [0 -2.3 0]
                     [0 0 0])
        column-row-offset (if (and (= row 0) (= column 6))
                            [0 2.25 0]
                            [0 0 0])
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-row-offset)
                          (translate column-offset)
                          (translate row-offset))]
    (->> placed-shape
         (rotate (/ π 12) [0 1 0])
         (translate [0 0 14.5]))))

(defn bottom-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset (cond (< column 1.5) [-1 0 0]
                       :else [0 0 0])
        column-row-offset (if (not= column 6) [0 -4.35 4.8]
                        (if (not= row 4) [-7.25 -6.3 2.1]
                                         [-7.89 -6.3 3.6]))
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-row-offset)
                          (translate column-offset))]
    (->> placed-shape
         (rotate (/ π 12) [0 1 0])
         (translate [0 0 13]))))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (not= column 0)
                         (not= row 4))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (not= column 0)
                         (not= row 4))]
           (->> (sa-cap (if (and (= column 5) (not= row 4)) 1 1))
                (key-place column row)))))

(defn prism [w l h taper-1 taper-2]
  (let [t1 taper-1
        t2 taper-2
        wt (- w taper-1)
        lt (- l taper-2)]
    (polyhedron [[0 0 0]
                 [t1 t1 h]
                 [wt t1 h]
                 [w 0 0]
                 [0 l 0]
                 [t1 lt h]
                 [wt lt h]
                 [w l 0]]
                [[0 1 2] [2 3 0]
                 [3 2 6] [6 7 3]
                 [7 6 5] [5 4 7]
                 [4 5 1] [1 0 4]
                 [1 5 2] [2 5 6]
                 [4 0 3] [7 4 3]])))


;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness plate-thickness) ; was 3.5
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))
(def web-post2 (->> (cube post-size post-size 0.1)
                   (translate [0 0 0.05] )))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 1.85) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -1.85) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -1.85) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 1.85) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(def web-post-tr2 (translate [(- (/ mount-width 1.85) post-adj) (- (/ mount-height 2) post-adj) 0] web-post2))
(def web-post-tl2 (translate [(+ (/ mount-width -1.85) post-adj) (- (/ mount-height 2) post-adj) 0] web-post2))
(def web-post-bl2 (translate [(+ (/ mount-width -1.85) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post2))
(def web-post-br2 (translate [(- (/ mount-width 1.85) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post2))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column columns ;(drop-last columns)
                row rows
                :when (or (not= column 0)
                          (not= row 4))]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (drop-last rows)
                :when (or (not= column 0)
                          (not= row 3))]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column columns;(drop-last columns)
                row (drop-last rows)
                :when (or (not= column 0)
                          (not= row 3))]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(defn thumb-place [column row shape]
  (let [cap-top-height (+ plate-thickness key-height)
        α (/ π 12)
        row-radius (+ (/ (/ (+ mount-height 0.75) 2)
                         (Math/sin (/ α 2)))
                      cap-top-height)
        β (/ π 36)
        column-radius (+ (/ (/ (+ mount-width 1) 2)
                            (Math/sin (/ β 2)))
                         cap-top-height) ]
    (->> shape
         (translate [0 0 (- row-radius)])
         (rotate (* α row) [1 0 0])
         (translate [0 0 row-radius])
         (translate [0 0 (- column-radius)])
         (rotate (* column β) [0 1 0])
         (translate [0 0 column-radius])
         (translate [mount-width 0 0])
         (rotate (* π (- 1/4 3/16)) [0 0 1])
         (rotate (/ π 12) [1 1 0])
         (translate [(+ -52 additional-thumb-x-offset) -45 40]))))

(defn thumb-2x-column [shape]
  (union (thumb-place 0 -1/2 (rotate (/ π 2) [0 0 1] shape))
         (thumb-place 1 -1/2 (rotate (/ π 2) [0 0 1] shape))))

(defn thumb-2x+1-column [shape]
  (union #_(thumb-place 1 -1/2 (rotate (/ π 2) [0 0 1] shape)))
         (thumb-place 1 1 (rotate (/ π 1) [0 0 1] shape)))

(defn thumb-1x-column [shape]
  (union (thumb-place 2 -1 (rotate (/ π 1) [0 0 1] shape))
         (thumb-place 2 0 (rotate (/ π 1) [0 0 1] shape))
         (thumb-place 2 1 (rotate (/ π 1) [0 0 1] shape))))

(defn thumb-layout [shape]
  (union
   (thumb-2x-column shape)
   (thumb-2x+1-column shape)
   (thumb-1x-column shape)))

(def double-plates
  (let [plate-height (/ (- sa-double-length mount-height) 2)
        top-plate (->> (cube (+ 2.7 mount-height) plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))

        stabilizer-cutout (union (->> (cube 5.1 2.6 web-thickness)
                                      (translate [-6.9 13.75 (- plate-thickness (/ web-thickness 2.1))])
                                      (color [1 0 0 1/2]))
                                 (->> (cube 6.9 2.6 web-thickness)
                                      (translate [-6.9 13.75 (- plate-thickness (/ web-thickness 2.01) 1.4)])
                                      (color [1 0 0 1/2]))
(->> (cube 5.1 2.6 web-thickness)
                                      (translate [-6.9 -13.75 (- plate-thickness (/ web-thickness 2.1))])
                                      (color [1 0 0 1/2]))
                                 (->> (cube 6.9 2.6 web-thickness)
                                      (translate [-6.9 -13.75 (- plate-thickness (/ web-thickness 2.01) 1.4)])
                                      (color [1 0 0 1/2]))									  
									  )
        top-plate (difference top-plate stabilizer-cutout)
      right-side-plate (->> (cube 2.8 (* 3 plate-height ) web-thickness)
                      (translate [9 4 (- plate-thickness (/ web-thickness 2))]))
      left-side-plate (->> (cube 2.8 (* 3 plate-height ) web-thickness)
                      (translate [-9 4 (- plate-thickness (/ web-thickness 2))]))
					right-side-plate (difference right-side-plate stabilizer-cutout)
left-side-plate (difference left-side-plate stabilizer-cutout)					]

    (union top-plate
           (mirror [0 1 0] top-plate)
           right-side-plate
           left-side-plate)))

(def thumbcaps
  (union
   (thumb-place 0 -1/2 (sa-cap 2))
   (thumb-place 1 -1/2 (sa-cap 2))
   (thumb-place 1 1 (sa-cap 1))
   (thumb-1x-column (sa-cap 1))))

(def thumb-connectors
  (union
   (apply union
          (concat
           (for [column [2] row [1]]
             (triangle-hulls (thumb-place column row web-post-br)
                             (thumb-place column row web-post-tr)
                             (thumb-place (dec column) row web-post-bl)
                             (thumb-place (dec column) row web-post-tl)))
           (for [column [2] row [0 1]]
             (triangle-hulls
              (thumb-place column row web-post-bl)
              (thumb-place column row web-post-br)
              (thumb-place column (dec row) web-post-tl)
              (thumb-place column (dec row) web-post-tr)))))
   (let [plate-height (/ (- sa-double-length mount-height) 2)
         thumb-tl (->> web-post-tl
                       (translate [0 plate-height 0]))
         thumb-bl (->> web-post-bl
                       (translate [0 (- plate-height) 0]))
         thumb-tr (->> web-post-tr
                       (translate [0 plate-height 0]))
         thumb-br (->> web-post-br
                       (translate [0 (- plate-height) 0]))
		 thumb-tl2 (->> web-post-tl
                       (translate [-1 plate-height 0]))
         thumb-bl2 (->> web-post-bl
                       (translate [-1 (- plate-height) 0]))
         thumb-tr2 (->> web-post-tr
                       (translate [-1 plate-height 0]))
         thumb-br2 (->> web-post-br
                       (translate [-1 (- plate-height) 0]))			   
					   ]
     (union

      ;;Connecting the two doubles
      (triangle-hulls (thumb-place 0 -1/2 thumb-tl2)
                      (thumb-place 0 -1/2 thumb-bl2)
                      (thumb-place 1 -1/2 thumb-tr2)
                      (thumb-place 1 -1/2 thumb-br2))

      ;;Connecting the double to the one above it
      (triangle-hulls (thumb-place 1 -1/2 thumb-tr)
                      (thumb-place 1 -1/2 thumb-tl)
                      (thumb-place 1 1 web-post-br)
                      (thumb-place 1 1 web-post-bl))

      ;;Connecting the 4 with the double in the bottom left
      (triangle-hulls (thumb-place 1 1 web-post-bl)
                      (thumb-place 1 -1/2 thumb-tl)
                      (thumb-place 2 1 web-post-br)
                      (thumb-place 2 0 web-post-tr))

      ;;Connecting the two singles with the middle double
      (hull (thumb-place 1 -1/2 thumb-tl2)
            (thumb-place 1 -1/2 thumb-bl2)
            (thumb-place 2 0 web-post-br)
            (thumb-place 2 -1 web-post-tr))
      (hull (thumb-place 1 -1/2 thumb-tl2)
            (thumb-place 2 0 web-post-tr)
            (thumb-place 2 0 web-post-br))
      (hull (thumb-place 1 -1/2 thumb-bl2)
            (thumb-place 2 -1 web-post-tr)
            (thumb-place 2 -1 web-post-br))

      ;;Connecting the thumb to everything
      (triangle-hulls (thumb-place 0 -1/2 thumb-br)
                      (key-place 1 4 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (key-place 1 4 web-post-tl)
                      (key-place 1 3 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (key-place 0 3 web-post-br)
                      (key-place 0 3 web-post-bl)
                      (thumb-place 0 -1/2 thumb-tr)
                      (thumb-place 0 -1/2 thumb-tl)
                      (key-place 0 3 web-post-bl)
                      (thumb-place 1 -1/2 thumb-tr)
                      (thumb-place 1 1 web-post-br)
                      (key-place 0 3 web-post-bl)
                      (key-place 0 3 web-post-tl)
                      (thumb-place 1 1 web-post-br)
                      (thumb-place 1 1 web-post-tr))
      (hull (thumb-place 0 -1/2 web-post-tr)
            (thumb-place 0 -1/2 thumb-tr)
            (key-place 1 4 web-post-bl)
            (key-place 1 4 web-post-tl))))))

(def thumb
  (union
   thumb-connectors
   (thumb-layout single-plate)
   (thumb-place 0 -1/2 double-plates)
   (thumb-place 1 -1/2 double-plates)))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

;; In column units
(def right-wall-column (+ (last columns) 0.55))
(def left-wall-column (- (first columns) 1/2))
(def thumb-back-y 0.93)
(def thumb-case-z 3)
(def thumb-right-wall (- -1/2 0.05))
(def thumb-front-row (+ -1 0.07))
(def thumb-left-wall-column (+ 5/2 0.05))
(def back-y 0.02)

(defn range-inclusive [start end step]
  (concat (range start end step) [end]))

(def wall-step 0.2)
(def wall-sphere-n (if FAST_RENDER 10 20))

(defn wall-cube-at [coords]
  (->> (cube 3 3 3)
       (translate coords)))

(defn wall-thicc-cube-at [coords]
  (->> (cube 9 3 3)
       (translate coords)))

(defn wall-half-cube-at [coords]
  (->> (cube 3 3 1.5)
       (translate coords)))

(defn scale-to-range [start end x]
  (+ start (* (- end start) x)))

(defn wall-cube-bottom [front-to-back-scale]
  (wall-cube-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 5.0)
                    front-to-back-scale)
                   -6])) ; was -6, then 2

(defn wall-half-cube-bottom [front-to-back-scale]
  (wall-half-cube-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 5.0)
                    front-to-back-scale)
                   -6])) ; was -6, then 2

(defn wall-cube-thicc-bottom [front-to-back-scale]
  (wall-thicc-cube-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 5.0)
                    front-to-back-scale)
                   -6])) ; was -6, then 2

(defn wall-cube-top [front-to-back-scale]
  (wall-cube-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 3.5)
                    front-to-back-scale)
                   4])) ; case height

(def wall-cube-top-back (wall-cube-top 1))
(def wall-cube-bottom-back (wall-cube-bottom 1))
(def wall-cube-bottom-front (wall-cube-bottom 0))
(def wall-cube-top-front (wall-cube-top 0))


(def wall-cube-thicc-bottom-back (wall-cube-thicc-bottom 1))
(def wall-cube-thicc-bottom-front (wall-cube-thicc-bottom 0))

(def wall-half-cube-bottom-back (wall-half-cube-bottom 1))


(defn top-case-cover [place-fn sphere
                 x-start x-end
                 y-start y-end
                 step]
  (apply union
         (for [x (range-inclusive x-start (- x-end step) step)
               y (range-inclusive y-start (- y-end step) step)]
           (hull (place-fn x y sphere)
                 (place-fn (+ x step) y sphere)
                 (place-fn x (+ y step) sphere)
                 (place-fn (+ x step) (+ y step) sphere)))))

(def case-back-cutout
  (let [a (+ mount-width 8)]
      (->> (prism a a a 0 6)
           (rotate π [0 0 1])
           (rotate (/ π 15) [1 0 0])
           (translate [(/ a 2) (+ a 9.3) -15.25])
           (key-place 2 0))))

(def case-inside-cutout
  (let [a (+ mount-width 8)
        b (+ mount-width 3.8)
        c 6]
    (union
      (->> (prism b b c 2 2)
           (rotate π [1 0 0])
           (translate [(- (/ b 2)) 10.5 (+ c 4)])
           (key-place 2 0)))))

(def front-wall
  (let [step wall-step ;;0.1
        wall-step 0.05 ;;0.05
        place case-place
        top-cover (fn [x-start x-end y-start y-end]
                    (top-case-cover place wall-cube-top-front
                                    x-start x-end y-start y-end
                                    wall-step))]
    (union
     (apply union
            (for [x (range 2 5)]
              (union
               (hull (place (- x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                     (place (+ x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                     (key-place x 4 web-post-bl)
                     (key-place x 4 web-post-br))
               (hull (place (- x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                     (key-place x 4 web-post-bl)
                     (key-place (- x 1) 4 web-post-br)))))
     (hull (place right-wall-column 4 (translate [-1 0 1] wall-cube-bottom-front))
           (place (- right-wall-column 1) 4 (translate [0 0.7 1] wall-cube-bottom-front))
           (key-place 5 4 web-post-bl)
           (key-place 5 4 web-post-br)
           )
     (hull (place right-wall-column 4 (translate [-1 1 1] wall-cube-bottom-front))
           (place (- right-wall-column 1) 4 (translate [0 1 1] wall-cube-bottom-front))
           (key-place 6 4 web-post-bl)
           (key-place 5 4 web-post-br))
     (hull (place (+ 3 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
           (place (- right-wall-column 1) 4 (translate [0 0.6 1] wall-cube-bottom-front))
           (key-place 4 4 web-post-br)
           (key-place 5 4 web-post-bl))
     (hull (place 0.75 4 (translate [1 1.53 -0.805] wall-cube-bottom-front))
           (place 1.5 4 (translate [0 1 1] wall-cube-bottom-front))
           (key-place 1 4 (translate [0.001 0 0] web-post-bl))
           (key-place 1 4 (translate [0.001 0 0] web-post-br))))))

          ; It's not clear why the above translateions of 0.001 units are needed
          ; but they resolve an issue where the normals were invered.

(def back-wall
  (let [step wall-step
        wall-cube-top-backtep 0.05
        place case-place]
    (difference
      (union
        (union
           (hull (place left-wall-column 0 (translate [-3 -0.5 1] wall-cube-bottom-back))
                 (place (+ left-wall-column 1) 0  (translate [0 -0.5 1] wall-cube-bottom-back))
                 (key-place 0 0 web-post-tl)
                 (key-place 0 0 web-post-tr))

           (hull (place 5 0 (translate [0 -0.11 1] wall-cube-bottom-back))
                 (place right-wall-column 0 (translate [-1.1 -0.4 -0.1] wall-cube-bottom-back))
                 (key-place 5 0 web-post-tl)
                 (key-place 5 0 (translate [0.25 0 0.05] web-post-tr)))

           (apply union
                  (for [x (range 1 5)]
                    (union
                     (hull (place (- x 1/2) 0 (translate [0 -0.5 1] wall-cube-bottom-back))
                           (place (+ x 1/2) 0 (translate [0 -0.5 1] wall-cube-bottom-back))
                           (key-place x 0 web-post-tl)
                           (key-place x 0 web-post-tr))
                     (hull (place (- x 1/2) 0 (translate [0 -0.5 1] wall-cube-bottom-back))
                           (key-place x 0 web-post-tl)
                           (key-place (- x 1) 0 web-post-tr))))))

         (hull (place (- 5 1/2) 0 (translate [0 -0.44 1] wall-cube-bottom-back))
               (place 5 0 (translate [0 -0.11 1] wall-cube-bottom-back))               
               (key-place 4 0 web-post-tr)
               (key-place 5 0 web-post-tl)))
      (union case-back-cutout
             case-inside-cutout))))

(def right-wall
  (let [place case-place]
    (union
          (apply union
            (concat
             (for [x (range 0 5)]
               (union
                (hull (place right-wall-column x (translate [-1 -1 1] (wall-cube-bottom 1/2)))
                      (place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (place 6 x (translate[2 0 0]web-post-br2))
                      (place 6 x (translate[2 0 0]web-post-tr2)))))
             (for [x (range 0 4)]
               (union
                (hull (place right-wall-column x (translate [-1 -1 1] (wall-cube-bottom 1/2)))
                      (place right-wall-column (inc x) (translate [-1 -1 1] (wall-cube-bottom 1/2)))
                     (place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      (place right-wall-column (inc x) (translate [-1 -1 (- 2.45 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      (place 6 x (translate[2 0 0]web-post-br2))
                      (place 6 (inc x) (translate[2 0 0]web-post-tr2)))))
             [(union
               (hull (place right-wall-column 0 (translate [-1 -1 1] (wall-cube-bottom 1/2)))
                     (place right-wall-column 0 (translate [-1.1 -0.4 -0.1] (wall-cube-bottom 1)))
                     (key-place 5.5 -0.5 (translate [0 -1.25 3.4] (cube 0.1 0.1 0.1)))
                     (key-place 5.5 0 (translate [0 -1.25 3.4] (cube 0.1 0.1 0.1)))
                     (place 6 0 (translate [2 0 0]web-post-tr2)))
               (hull (place right-wall-column 4 (translate [-1 -1 1] (wall-cube-bottom 1/2)))
              						 ;(place right-wall-column 5 (translate [-1 10.45 -3.8] (cube 0.1 0.1 4)))
                     (place right-wall-column 4 (translate [-1.1 0 1] (wall-cube-bottom 0)))
                     (place right-wall-column 4 (translate [4 0 1] (wall-cube-bottom 0)))                     
                     (key-place 5.5 4 (translate [0 1.25 3] (cube 0.1 0.1 0.1)))
                     (key-place 5.5 4.5 (translate [0 -1.45 3.2] (cube 0.1 0.1 0.1)))
                     (place 6 4 (translate[2 -0.075 0]web-post-br2))))])))))

(def left-wall
  (let [place case-place]
    (union
     (hull (place left-wall-column 0 (translate [1 -0.9 1.25] wall-cube-bottom-back))
           (place left-wall-column 1 (translate [1 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-tl)
           (key-place 0 0 web-post-bl))
     (hull (place left-wall-column 1 (translate [1 0 1] wall-cube-bottom-back))
           (place left-wall-column 2 (translate [1 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-bl)
           (key-place 0 1 web-post-bl))
     (hull (place left-wall-column 2 (translate [1 0 1] wall-cube-bottom-back))
           (place left-wall-column 1.71 (translate [1 0 3.5] wall-cube-bottom-front))
           (key-place 0 1 web-post-bl)
           (key-place 0 2 web-post-bl))

     (hull (place left-wall-column 0 (translate [-2 -0.9 1.25] wall-cube-bottom-back))
           (place left-wall-column 1 (translate [-2 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-tl)
           (key-place 0 0 web-post-bl))
     (hull (place left-wall-column 1 (translate [-2 0 1] wall-cube-bottom-back))
           (place left-wall-column 2 (translate [-2 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-bl)
           (key-place 0 1 web-post-bl))
     (hull (place left-wall-column 2 (translate [-2 0 1] wall-cube-bottom-back))
           (place left-wall-column 1.71 (translate [-2 0 3.5] wall-cube-bottom-front))
           (key-place 0 1 web-post-bl)
           (key-place 0 2 web-post-bl))

     (hull (place left-wall-column 0 (translate [-3 -0.9 1.25] wall-cube-bottom-back))
           (place left-wall-column 1 (translate [-3 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-tl)
           (key-place 0 0 web-post-bl))
     (hull (place left-wall-column 1 (translate [-3 0 1] wall-cube-bottom-back))
           (place left-wall-column 2 (translate [-3 0 1] wall-cube-bottom-back))
           (key-place 0 0 web-post-bl)
           (key-place 0 1 web-post-bl))
     (hull (place left-wall-column 2 (translate [-3 0 1] wall-cube-bottom-back))
           (place left-wall-column 1.71 (translate [-3 0 3.5] wall-cube-bottom-front))
           (key-place 0 1 web-post-bl)
           (key-place 0 2 web-post-bl))

     )))

(def left-inside-wall
  (let [place case-place]
    (union
     (hull (place left-wall-column 1.71 (translate [1 0 3] wall-cube-bottom-front))
           (key-place 0 2 web-post-bl)
           (key-place 0 3 web-post-tl))
     (hull (place left-wall-column 1.71 (translate [2 0 3] wall-cube-bottom-front))
           (thumb-place 1 1 web-post-tr)
           (key-place 0 3 web-post-tl)
           (place left-wall-column 1.71 (translate [1 0 3] wall-cube-bottom-front))
           (thumb-place 1 1 web-post-tr)
           (thumb-place 1/2 thumb-back-y (translate [0 -1.7 thumb-case-z] wall-cube-bottom-back))
           (thumb-place 1/2 thumb-back-y (translate [1 -1.7 (- thumb-case-z 2.9)] wall-cube-bottom-back))))))

(def thumb-back-wall
  (let [step wall-step
        top-step 0.05
        back-y thumb-back-y]
    (union
     (hull
      (thumb-place 1/2 thumb-back-y (translate [0 -1.7 thumb-case-z] wall-cube-bottom-back))
      (thumb-place 1 1 web-post-tr)
      (thumb-place 3/2 thumb-back-y (translate [0 -1.7 thumb-case-z] wall-cube-bottom-back))
      (thumb-place 1 1 web-post-tl))
     (hull
      (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1.5 -0.59 (+ 1.225 thumb-case-z)] wall-cube-bottom-back))
      (thumb-place 3/2 thumb-back-y (translate [0 -1.7 thumb-case-z] wall-cube-bottom-back))
      (thumb-place 1 1 web-post-tl)
      (thumb-place 2 1 web-post-tl))
     (hull
      (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1.5 -0.59 thumb-case-z] wall-cube-bottom-back))
      (thumb-place 3/2 thumb-back-y (translate [0 -1.7 thumb-case-z] wall-cube-bottom-back))
      (thumb-place 1 1 web-post-tl)
      (thumb-place 2 1 web-post-tl))
     (hull
      (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1.5 -0.59 thumb-case-z] wall-cube-bottom-back))
      (thumb-place (+ 5/2 0.05) thumb-back-y (translate [-5.75 -0.58 thumb-case-z] wall-cube-bottom-back))
     ))))

(def thumb-left-wall
  (let [step wall-step
        place thumb-place
        wall (+ thumb-left-wall-column 0.001)]
    (union
     ;;top innermost left thumb wall
     (hull
      (translate [0 0 2.6] (thumb-place wall thumb-back-y (translate [1.2 -1.7 (+ -0.935 thumb-case-z)] wall-cube-bottom-back)))
      (translate [0 0 1.4] (thumb-place wall thumb-back-y (translate [1.2 -1.7 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.6] (thumb-place wall 0 (translate [1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [1.5 -3.2 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 4.5] (thumb-place 2 1 web-post-tl))
      (translate [0 0 2.5] (thumb-place 2 0 web-post-tl)))
     (hull
      (translate [0 0 2.6] (thumb-place wall 0 (translate [1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [-1.5 -3.2 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [-4.4 -3.2 thumb-case-z] wall-cube-bottom-back)))      
      (translate [0 0 2.5] (thumb-place 2 0 web-post-tl))
      (translate [0 0 4.5] (thumb-place 2 1 web-post-tl)))
     ;;middle innermost left thumb wall
     (hull
      (translate [0 -3.15 0.77] (thumb-place wall 0 (translate [1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.52] (thumb-place wall -1 (translate [1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.5]  (thumb-place 2 0 web-post-tl))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl)))
     (hull
      (translate [0 0.1255 1.2485](thumb-place wall 0 (translate [1.475 -3.25 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0.2 1.065](thumb-place wall -1 (translate [1.5 -3.25 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl))
      (translate [0 0 2.5](thumb-place 2 0 web-post-tl))
      )
     ;;bottom innermost left thumb wall
     (hull
      (translate [0 -3.15 1.515](thumb-place wall -1 (translate [1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.115](thumb-place wall (+ -1 0.07) (translate [1.5 3.6 (+ thumb-case-z 0.2)] wall-cube-bottom-front)))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl))
      (translate [0 0 4.5](thumb-place 2 -1 web-post-bl)))
     (hull
      (translate [0 2.015 0.825](thumb-place wall -1 (translate [0.9725 -6.27 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0.8375 0.675](thumb-place wall (+ -1 0.07) (translate [1 -0.25 (+ thumb-case-z 0.2)] wall-cube-bottom-front)))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl))
      (translate [0 0 4.5](thumb-place 2 -1 web-post-bl)))
     ;;top middle left thumb wall
     (hull
      (translate [0 0 2.6] (thumb-place wall thumb-back-y (translate [-1.8 -1.7 (+ -0.935 thumb-case-z)] wall-cube-bottom-back)))
      (translate [0 0 1.4] (thumb-place wall thumb-back-y (translate [-1.8 -1.7 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.6] (thumb-place wall 0 (translate [-1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [-1.5 -3.2 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 4.5] (thumb-place 2 1 web-post-tl))
      (translate [0 0 2.5] (thumb-place 2 1 web-post-bl))
      )
     ;;connector to middle innermost thumb wall
     ;(hull
      ;(translate [0 0 1.7] (thumb-place wall 0 (translate [-1.5 -3.2 thumb-case-z] wall-cube-bottom-back)))
      ;(translate [0 0 1.2] (thumb-place wall 0 (translate [-1.5 -3.2 thumb-case-z] wall-cube-bottom-back)))
      ;(translate [0 0 4.5] (thumb-place 2 0 web-post-tl))
      ;(translate [0 0 4.5] (thumb-place 2 1 web-post-bl)))
     ;;mid middle left thumb wall
     (hull
      (translate [0 -3.15 0.77](thumb-place wall 0 (translate [-1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.52](thumb-place wall -1 (translate [-1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.5] (thumb-place 2 -1 web-post-tl))
      (translate [0 0 2.5] (thumb-place 2 0 web-post-tl))
      )
     ;;connector to bottom innermost left thumb wall
     ;(hull
      ;(thumb-place wall -1 (translate [-1.5 0 thumb-case-z] wall-cube-bottom-back))
      ;(thumb-place 2 -1 web-post-tl)
      ;(thumb-place 2 0 web-post-bl))
     ;;bottom middle left thumb wall
     (hull
      (translate [0 -3.15 1.515](thumb-place wall -1 (translate [-1.5 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.115](thumb-place wall (+ -1 0.07) (translate [-1.5 3.6 (+ thumb-case-z 0.2)] wall-cube-bottom-front)))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl))
      (translate [0 0 4.5](thumb-place 2 -1 web-post-bl)))
     ;;top outermost left thumb wall 
     (hull
      (translate [0 0 2.6] (thumb-place wall thumb-back-y (translate [-4.8 -1.7 (+ -0.935 thumb-case-z)] wall-cube-bottom-back)))
      (translate [0 0 1.4] (thumb-place wall thumb-back-y (translate [-4.6 -1.7 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [-4.4 -3.2 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 1.2] (thumb-place wall 0 (translate [-4.4 -3.2 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 4.5] (thumb-place 2 1 web-post-tl))
      (translate [0 0 2.5] (thumb-place 2 1 web-post-bl)))
     ;;connector to middle innermost thumb wall
     ;(hull
      ;(thumb-place wall 0 (translate [-3.5 0 thumb-case-z] wall-cube-bottom-back))
      ;(thumb-place 2 0 web-post-tl)
      ;(thumb-place 2 1 web-post-bl))
     ;;middle outermost left thumb wall
     (hull
      (translate [0 -3.15 0.77](thumb-place wall 0 (translate [-3.75 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.52](thumb-place wall -1 (translate [-3.75 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 0 2.5] (thumb-place 2 0 web-post-tl))
      (translate [0 0 2.5] (thumb-place 2 -1 web-post-tl)))
     ;;connector to bottom innermost thumb wall
     ;(hull
      ;(thumb-place wall -1 (translate [-3.5 0 thumb-case-z] wall-cube-bottom-back))
      ;(thumb-place 2 -1 web-post-tl)
      ;(thumb-place 2 0 web-post-bl))
     ;;bottom outermost left thumb wall
     (hull
      (translate [0 -3.15 1.515](thumb-place wall -1 (translate [-3.675 0 thumb-case-z] wall-cube-bottom-back)))
      (translate [0 -3.15 1.115](thumb-place wall (+ -1 0.07) (translate [-4.45 3.6 (+ thumb-case-z 0.2)] wall-cube-bottom-front)))
      (translate [0 0 2.5](thumb-place 2 -1 web-post-tl))
      (translate [0 0 4.5](thumb-place 2 -1 web-post-bl)))
     ;(hull
      ;(translate [0 0 2.5] (thumb-place wall thumb-back-y (translate [-4 -1.7 thumb-case-z] wall-cube-bottom-back)))
      ;(translate [0 0 2.5] (thumb-place wall 0 (translate [-4 0 thumb-case-z] wall-cube-bottom-back)))
      ;(translate [0 0 4] (thumb-place 2 1 web-post-tl))
      ;(translate [0 0 4] (thumb-place 2 1 web-post-bl)))
     )))

(def thumb-front-wall
  (let [step wall-step
        wall-cube-top-front 0.05
        place thumb-place
        wall (- thumb-front-row 0.04)
        plate-height (/ (- sa-double-length mount-height) 2)
        thumb-tl (->> web-post-tl
                      (translate [0 plate-height 0]))
        thumb-bl (->> web-post-bl
                      (translate [0 (- plate-height) 0]))
        thumb-tr (->> web-post-tr
                      (translate [-0 plate-height 0]))
        thumb-br (->> web-post-br
                      (translate [-0 (- plate-height) 0]))]

    (union

     (hull (place (+ 5/2 0.05) wall (translate [1.5 1.43 (+ 0.77 thumb-case-z)] wall-cube-bottom-front))
           (place (+ 3/2 0.05) wall (translate [0 1.5 thumb-case-z] wall-cube-bottom-front))
           (place 2 -1 web-post-bl)
           (place 2 -1 web-post-br))

     (hull (place thumb-right-wall wall (translate [-1 1.5 thumb-case-z] wall-cube-bottom-front))
           (place (+ 1/2 0.05) wall (translate [0 1.5 thumb-case-z] wall-cube-bottom-front))
           (place 0 -1/2 thumb-bl)
           (place 0 -1/2 thumb-br))
     (hull (place (+ 1/2 0.05) wall (translate [0 1.5 thumb-case-z] wall-cube-bottom-front))
           (place (+ 3/2 0.05) wall (translate [0 1.5 thumb-case-z] wall-cube-bottom-front))
           (place 0 -1/2 thumb-bl)
           (place 1 -1/2 thumb-bl)
           (place 1 -1/2 thumb-br)
           (place 2 -1 web-post-br)))))

(def thumb-inside-wall
  (let [place thumb-place
        wall (- thumb-front-row 0.04)
        plate-height (/ (- sa-double-length mount-height) 2)
        thumb-bl (->> web-post-bl
                      (translate [0 (- plate-height) 0]))
        thumb-br (->> web-post-br
                      (translate [-0 (- plate-height) 0]))
        thumb-bottom (->> (cube 3 3 0.001)
                       (translate [13.6 -15 -8]))
        thumb-top (->> (cube 1 1 1)
                       (translate [13 -11.7 -5.4]))]
     (hull (place thumb-right-wall wall (translate [0 1.5 thumb-case-z] wall-cube-bottom-front))
           (key-place 1 4 web-post-bl)
           (place 0 -1/2 thumb-br)
           #_(case-place 0 4 thumb-top)
           (case-place 0 4 (translate [0 10 0] thumb-bottom)))))

(def new-case
    (union front-wall
           right-wall
           back-wall
           left-wall
           left-inside-wall
           thumb-back-wall
           thumb-left-wall
           thumb-inside-wall
           thumb-front-wall))

;;;;;;;;;;;;
;; Bottom ;;
;;;;;;;;;;;;


(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (/ height 2)])))

(defn bottom-hull [p]
  (hull p (bottom 1 p)))


(def bottom-key-guard (->> (cube (+ mount-width 1.5) mount-height web-thickness)
                           (translate [0 0 (+ (- (/ web-thickness 2)) -5)])))
(def bottom-front-key-guard (->> (cube (+ mount-width 1.5) (/ mount-height 2) web-thickness)
                                 (translate [0 (/ mount-height 4) (+ (- (/ web-thickness 2)) -5)])))

(defn stand-at [diameter placement]
  (let [bumper-radius (/ diameter 2)
       stand-diameter (+ diameter 2)
       stand-radius (/ stand-diameter 2)]
    (difference (->> (sphere stand-radius)
                     (translate [0 0 (+ (/ stand-radius -2) -4.5)])
                      placement
                      bottom-hull)
                (->> (cube stand-diameter stand-diameter stand-radius)
                     (translate [0 0 (/ stand-radius -2)])
                      placement)
                (->> (sphere bumper-radius)
                     (translate [0 0 (+ (/ stand-radius -2) -4.5)])
                      placement
                     (bottom 1.5)))))

(def bottom-plate
  (union
   (apply union
          (for [column columns
                row (drop-last rows) ;;
                :when (or (not= column 0)
                          (not= row 4))]
            (->> bottom-key-guard
                 (key-place column row))))
   (thumb-1x-column (rotate (/ π 1) [0 0 1] bottom-key-guard))
   (thumb-2x-column (rotate (/ π 2) [0 0 1] bottom-key-guard))
   (thumb-place 1 1 (rotate (/ π 1) [0 0 1] bottom-key-guard))
   (apply union
          (for [column columns
                row [(last rows)] ;;
                :when (or (not= column 0)
                          (not= row 4))]
            (->> bottom-front-key-guard
                 (key-place column row))))
   (let [shift #(translate [0 0 (+ (- web-thickness) -5)] %)
         thumb-ridge-height 1
         thumb-back-offset -1.28
         thumb-left-offset 1.13
         thumb-front-offset 0.56
         front-offset 0.63
         left-offset 0.9
         right-offset -0.73
         back-offset -0.85
         web-post-tl (shift web-post-tl)
         web-post-tr (shift web-post-tr)
         web-post-br (shift web-post-br)
         web-post-bl (shift web-post-bl)
         half-shift-correction #(translate [0 (/ mount-height 2) 0] %)
         half-post-br (half-shift-correction web-post-br)
         half-post-bl (half-shift-correction web-post-bl)
         row-connections (concat
                          (for [column (drop-last columns)
                                row (drop-last rows)
                                :when (or (not= column 0)
                                          (not= row 4))]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row web-post-bl)
                             (key-place column row web-post-br)))
                          (for [column (drop-last columns)
                                row [(last rows)]
                                :when (or (not= column 0)
                                          (not= row 4))]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row half-post-bl)
                             (key-place column row half-post-br))))
         column-connections (for [column columns
                                  row (drop-last rows)
                                  :when (or (not= column 0)
                                            (not= row 3))]
                              (triangle-hulls
                               (key-place column row web-post-bl)
                               (key-place column row web-post-br)
                               (key-place column (inc row) web-post-tl)
                               (key-place column (inc row) web-post-tr)))
         diagonal-connections (for [column (drop-last columns)
                                    row (drop-last rows)
                                    :when (or (not= column 0)
                                              (not= row 3))]
                                (triangle-hulls
                                 (key-place column row web-post-br)
                                 (key-place column (inc row) web-post-tr)
                                 (key-place (inc column) row web-post-bl)
                                 (key-place (inc column) (inc row) web-post-tl)))
         main-keys-bottom (concat row-connections
                                  column-connections
                                  diagonal-connections)
         front-wall (concat
                     (for [x (range 2 5)]
                       (union
                        (hull (bottom-place (- x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                              (bottom-place (+ x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place x 4 half-post-br))
                        (hull (bottom-place (- x 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place (- x 1) 4 half-post-br))))
                     [(hull (bottom-place right-wall-column 4 (translate [-1.5 1 1] wall-cube-bottom-front))
                            (bottom-place (- right-wall-column 1) 4 (translate [-1.5 1 1] wall-cube-bottom-front))
                            (key-place 5 4 half-post-bl)
                            (key-place 5 4 half-post-br))
                      (hull (bottom-place (+ 4 1/2) 4 (translate [0 1 1] wall-cube-bottom-front))
                            (bottom-place (- right-wall-column 1) 4 (translate [0 1 1] wall-cube-bottom-front))
                            (key-place 4 4 half-post-br)
                            (key-place 5 4 half-post-bl))
                      ])
         right-wall (concat
                     (for [x (range 0 4)]
                       (hull (bottom-place right-wall-column x (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (key-place 5 x web-post-br)
                             (key-place 5 x web-post-tr)))
                     (for [x (range 0 4)]
                       (hull (bottom-place right-wall-column x (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (bottom-place right-wall-column (inc x) (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (key-place 5 x web-post-br)
                             (key-place 5 (inc x) web-post-tr)))
;;this is bottom-mount code
              (for [x (range 0 1)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                						(bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (bottom-place 6 x (translate [2.1 0.85 1.205] web-post-br2))
                      (bottom-place 6 x (translate [2.1 0.75 1.205] web-post-tr2)))))
             (for [x (range 1 2)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                						(bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (bottom-place 6 x (translate [2 0 1.295] web-post-br2))
                      (bottom-place 6 x (translate [2 0.25 1.295] web-post-tr2)))))
             (for [x (range 2 3)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                						(bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (bottom-place 6 x (translate [2 0 1.295] web-post-br2))
                      (bottom-place 6 x (translate [2 0 1.295] web-post-tr2)))))
             (for [x (range 3 4)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                						(bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (bottom-place 6 x (translate [2.2 -0.25 1.335] web-post-br2))
                      (bottom-place 6 x (translate [2 0 1.305] web-post-tr2)))))

             (for [x (range 4 5)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                						(bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))

                      (bottom-place 6 x (translate [2.3 0.05 -0.2] web-post-br2))
                      (bottom-place 6 x (translate [2.3 0 -0.2] web-post-tr2)))))

             (for [x (range 0 1)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                     ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      ;(place right-wall-column (inc x) (translate [-1 -1 (- 2.45 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      (bottom-place 6 x (translate [2.1 0.85 1.205] web-post-br2))
                      (bottom-place 6 (inc x) (translate [2 0.35 1.295] web-post-tr2)))))
             (for [x (range 1 3)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                     ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      ;(place right-wall-column (inc x) (translate [-1 -1 (- 2.45 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      (bottom-place 6 x (translate [2 0 1.305] web-post-br2))
                      (bottom-place 6 (inc x) (translate [2 0 1.305] web-post-tr2)))))

             (for [x (range 3 4)]
               (union
                (hull (bottom-place right-wall-column x (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column x (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                      (bottom-place right-wall-column (inc x) (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                     ;(place right-wall-column x (translate [-1 -1 (- 3.05 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      ;(place right-wall-column (inc x) (translate [-1 -1 (- 2.45 (* x 0.55))] (cube 0.1 0.1 0.1)))
                      (bottom-place 6 x (translate [2.2 -0.25 1.335] web-post-br2))
                      (bottom-place 6 (inc x) (translate [2.3 0 -0.2] web-post-tr2)))))



  [(union
               (hull (bottom-place right-wall-column 0 (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
                     (bottom-place right-wall-column 0 (translate [-1 -0.8 -1] (wall-cube-bottom 1)))
                     (bottom-place right-wall-column 0 (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                     ;(bottom-place right-wall-column 0 (translate [4 -0.8 -1] (wall-cube-bottom 1)))
                     ;(key-place 5.5 -0.5 (translate [0 -1.25 3.4] (cube 0.1 0.1 0.1)))
                     ;(key-place 5.5 0 (translate [0 -1.25 3.4] (cube 0.1 0.1 0.1)))
                     (bottom-place 6 0 (translate [2.1 0.85 1.205] web-post-tr2)))
               (hull (bottom-place right-wall-column 4 (translate [-1 -1 -1] (wall-cube-bottom 1/2)))
               						(bottom-place right-wall-column 4 (translate [4 -1 -1] (wall-cube-bottom 1/2)))
                     (bottom-place right-wall-column 5 (translate [-1 10.45 -3.8] (cube 0.1 0.1 4)))
                     (bottom-place right-wall-column 4 (translate [-1 0.5 1] (wall-cube-bottom 0)))
                     (bottom-place right-wall-column 4 (translate [4.1 0.115 1] (wall-cube-bottom 0)))
                     ;(bottom-place right-wall-column 4 (translate [4 0 -1] (wall-cube-bottom 0)))
                     ;(key-place 5.5 4 (translate [0 1.25 3] (cube 0.1 0.1 0.1)))
                     ;(key-place 5.5 4.5 (translate [0 1.45 3.2] (cube 0.1 0.1 0.1)))
                     (bottom-place 6 4 (translate [2.3 0 -0.2] web-post-br2))))]

;;this is the end of the bottom-mount code


                     [(union
                       (hull (bottom-place right-wall-column 0 (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (bottom-place right-wall-column 0.017 (translate [-1 -1 1.15] (wall-cube-bottom 1)))
                             (key-place 5 0 web-post-tr))
                       (hull (bottom-place right-wall-column 4 (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (bottom-place right-wall-column 4 (translate [-1 1 1] (wall-cube-bottom 0)))
                             (key-place 5 4 half-post-br))
                       (hull (bottom-place right-wall-column 4 (translate [-1 0 1] (wall-cube-bottom 1/2)))
                             (key-place 5 4 half-post-br)
                             (key-place 5 4 web-post-tr)))])
         back-wall (concat
                    (for [x (range 1 6)]
                      (union
                       (hull
                             (do (bottom-place (- x 1/2) 0 (translate [0 back-offset 1] wall-cube-bottom-back)))
                              (if (= x 5)
                               (do (bottom-place (+ x 1/2) 0 (translate [0 back-offset 1] wall-cube-bottom-back)))
                               (do (bottom-place (+ x 1/2) 0 (translate [0 back-offset 1] wall-cube-bottom-back))))
                             (key-place x 0 web-post-tl)
                             (key-place x 0 web-post-tr))
                       (hull (bottom-place (- x 1/2) 0 (translate [0 back-offset 1] wall-cube-bottom-back))
                             (key-place x 0 web-post-tl)
                             (key-place (- x 1) 0 web-post-tr))))
                    [(hull (bottom-place left-wall-column 0 (translate [left-offset back-offset 1.2] wall-cube-bottom-back))
                           (bottom-place (+ left-wall-column 1) 0  (translate [0 back-offset 1.2] wall-cube-bottom-back))
                           (key-place 0 0 web-post-tl)
                           (key-place 0 0 web-post-tr))]

                    )
         left-wall (let [place bottom-place]
               [
                (hull (place left-wall-column 0 (translate [left-offset back-offset 1.2] wall-cube-bottom-back))
                      (place left-wall-column 1 (translate [left-offset 1.5 1/2] wall-cube-bottom-back))
                      (key-place 0 0 web-post-tl)
                      (key-place 0 0 web-post-bl))
                (hull (place left-wall-column 1 (translate [left-offset 0 1/2] wall-cube-bottom-back))
                      (place left-wall-column 2 (translate [left-offset 1.5 0.7] wall-cube-bottom-back))
                      (key-place 0 0 web-post-bl)
                      (key-place 0 1 web-post-bl))
                (hull (place left-wall-column 2 (translate [left-offset 0 0.7] wall-cube-bottom-back))
                      (place left-wall-column 1.6666  (translate [left-offset 0 3] wall-cube-bottom-front))
                      (place left-wall-column 1.6666  (translate [left-offset 0 1] wall-cube-bottom-front))
                      (key-place 0 1 web-post-bl)
                      (key-place 0 2 web-post-bl))
                (hull (place left-wall-column 1.6666  (translate [left-offset 0 1] wall-cube-bottom-front))
                      (key-place 0 2 web-post-bl)
                      (key-place 0 3 web-post-tl))

                ])
         thumbs [(hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1/2 web-post-tl)
                       (thumb-place 1 -1/2 web-post-tr)
                       (thumb-place 1 -1/2 web-post-br))
                 (hull (thumb-place 1 -1/2 web-post-tr)
                       (thumb-place 1 -1/2 web-post-tl)
                       (thumb-place 1 1 web-post-bl)
                       (thumb-place 1 1 web-post-br))
                 (hull (thumb-place 2 -1 web-post-tr)
                       (thumb-place 2 -1 web-post-tl)
                       (thumb-place 2 0 web-post-bl)
                       (thumb-place 2 0 web-post-br))
                 (hull (thumb-place 2 0 web-post-tr)
                       (thumb-place 2 0 web-post-tl)
                       (thumb-place 2 1 web-post-bl)
                       (thumb-place 2 1 web-post-br))
                 (triangle-hulls (thumb-place 2 1 web-post-tr)
                                 (thumb-place 1 1 web-post-tl)
                                 (thumb-place 2 1 web-post-br)
                                 (thumb-place 1 1 web-post-bl)
                                 (thumb-place 2 0 web-post-tr)
                                 (thumb-place 1 -1/2 web-post-tl)
                                 (thumb-place 2 0 web-post-br)
                                 (thumb-place 1 -1/2 web-post-bl)
                                 (thumb-place 2 -1 web-post-tr)
                                 (thumb-place 2 -1 web-post-br))
                 (hull (thumb-place 2 -1 web-post-br)
                       (thumb-place 1 -1/2 web-post-bl)
                       (thumb-place 1 -1 web-post-bl))
                 (hull (thumb-place 1 -1/2 web-post-bl)
                       (thumb-place 1 -1 web-post-bl)
                       (thumb-place 1 -1/2 web-post-br)
                       (thumb-place 1 -1 web-post-br))
                 (hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1 web-post-bl)
                       (thumb-place 0 -1/2 web-post-br)
                       (thumb-place 0 -1 web-post-br))
                 (hull (thumb-place 0 -1/2 web-post-bl)
                       (thumb-place 0 -1 web-post-bl)
                       (thumb-place 1 -1/2 web-post-br)
                       (thumb-place 1 -1 web-post-br))]
         thumb-back-wall [
                          (hull
                           (thumb-place 1/2 thumb-back-y (translate [-0.2 (+ -0.445 thumb-back-offset) thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 1 1 web-post-tr)
                           (thumb-place 3/2 thumb-back-y (translate [-0.2 (+ -0.41 thumb-back-offset) thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 1 1 web-post-tl))
                          (hull
                           (thumb-place (+ 5/2 0.05) thumb-back-y (translate [thumb-left-offset thumb-back-offset thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 3/2 thumb-back-y (translate [-0.41 (+ -0.445 thumb-back-offset) thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 1 1 web-post-tl)
                           (thumb-place 2 1 web-post-tl))
                          (hull
                           (thumb-place 1 1 web-post-tr)
                           (key-place 0 3 web-post-tl)
                           (thumb-place 1 1 web-post-br)
                           (key-place 0 3 web-post-bl)
                           (thumb-place 1/2 thumb-back-y (translate [-0.2 (+ thumb-back-offset -0.405) (+ -1.6 thumb-ridge-height)] wall-half-cube-bottom-back))
                           (thumb-place 1/2 thumb-back-y (translate [-3.2 (+ thumb-back-offset -0.385) (+ -1.6  thumb-ridge-height)] wall-half-cube-bottom-back))
                           (bottom-place left-wall-column 1.6666 (translate [left-offset 0 (- thumb-ridge-height 0.2)] wall-cube-bottom-front))
                           (key-place 0 3 web-post-tl)
                           (thumb-place 1 1 web-post-tr)

                           )
                          ]
         thumb-left-wall [(hull
                           (thumb-place thumb-left-wall-column thumb-back-y (translate [thumb-left-offset thumb-back-offset thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column 0 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column thumb-back-y (translate [(+ -5.1 thumb-left-offset) thumb-back-offset thumb-ridge-height] wall-cube-bottom-back))
                           ;(thumb-place thumb-left-wall-column thumb-back-y (translate [(+ 1.68 thumb-left-offset) 23.625 (+ -5.625 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column 0 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 2 1 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column 0 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column -1 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column 0 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column -1 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column -1 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [thumb-left-offset 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [thumb-left-offset 0.55 thumb-ridge-height] wall-cube-bottom-front))
                           (thumb-place thumb-left-wall-column -1 (translate [(+ -5.1 thumb-left-offset) 0 thumb-ridge-height] wall-cube-bottom-back))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [(+ -5.1 thumb-left-offset) 0.55 thumb-ridge-height] wall-cube-bottom-front))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 -1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [(+ 1.78 thumb-left-offset) -4.45 (+ -4.7 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [(+ 3.455 thumb-left-offset) -4.45 (+ -7.5 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [(+ 3.5 thumb-left-offset) -4.45 (+ -4.7 thumb-ridge-height)] web-post-bl2))

                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 2.38 thumb-left-offset) -2.55 (+ -4.7 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 3.455 thumb-left-offset) -0.44 (+ -8.365 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 3.5 thumb-left-offset) -2.55 (+ -4.7 thumb-ridge-height)] web-post-bl2))
                          )
                          (hull
                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 2.38 thumb-left-offset) -2.55 (+ -4.7 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 3.455 thumb-left-offset) -0.44 (+ -8.365 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 0 0.07) (translate [(+ 3.5 thumb-left-offset) -2.55 (+ -4.7 thumb-ridge-height)] web-post-bl2))

                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 2.58 thumb-left-offset) -5.25 (+ -3.91 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 3.455 thumb-left-offset) -8.45 (+ -6.5 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 3.5 thumb-left-offset) -5.25 (+ -3.91 thumb-ridge-height)] web-post-bl2))
                          )
                          (hull
                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 2.58 thumb-left-offset) -5.25 (+ -3.91 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 3.455 thumb-left-offset) -8.45 (+ -6.5 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.07) (translate [(+ 3.5 thumb-left-offset) -5.25 (+ -3.91 thumb-ridge-height)] web-post-bl2))

                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 2.58 thumb-left-offset) -6.55 (+ -4.035 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 3.455 thumb-left-offset) -4.7 (+ -7.79 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 3.5 thumb-left-offset) -6.55 (+ -4.035 thumb-ridge-height)] web-post-bl2))

                          )
                          (hull
                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 2.58 thumb-left-offset) -6.55 (+ -4.035 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 3.455 thumb-left-offset) -4.7 (+ -7.79 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 1 0.257) (translate [(+ 3.5 thumb-left-offset) -6.55 (+ -4.035 thumb-ridge-height)] web-post-bl2))

                           (thumb-place thumb-left-wall-column (+ 2 0.057) (translate [(+ 1.68 thumb-left-offset) -1.5 (+ -5.16 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 2 0.057) (translate [(+ 3.455 thumb-left-offset) -2.45 (+ -8 thumb-ridge-height)] web-post-bl2))
                           (thumb-place thumb-left-wall-column (+ 2 0.057) (translate [(+ 3.5 thumb-left-offset) -2.1 (+ -5.06 thumb-ridge-height)] web-post-bl2))

                          
                          )
                          ]



         thumb-front-wall [(hull (thumb-place (+ 5/2 0.05) thumb-front-row (translate [thumb-left-offset thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place 2 -1 web-post-bl)
                                 (thumb-place 2 -1 web-post-br))
                           (hull (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place 0 -1 web-post-bl)
                                 (thumb-place 1 -1 web-post-bl)
                                 (thumb-place 1 -1 web-post-br)
                                 (thumb-place 2 -1 web-post-br))
                           (hull (thumb-place thumb-right-wall thumb-front-row (translate [-1.12 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                                 (thumb-place 0 -1 web-post-bl)
                                 (thumb-place 0 -1 web-post-br))]
         thumb-inside [(triangle-hulls
                        (thumb-place 1 1 web-post-tr)
                        (key-place 0 3 web-post-tl)
                        (thumb-place 1 1 web-post-br)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 1 -1/2 web-post-tr)
                        (thumb-place 0 -1/2 web-post-tl)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 0 3 web-post-br)
                        (key-place 1 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 1 4 web-post-tl)
                        (key-place 1 4 half-post-bl))

                       (hull
                        (thumb-place 0 -1/2 web-post-tr)
                        (thumb-place 0 -1/2 web-post-br)
                        (key-place 1 4 half-post-bl))

                       (hull
                        (key-place 1 4 half-post-bl)
                        (key-place 1 4 half-post-br)
                        (bottom-place (- 2 1/2) 4 (translate [0 front-offset 1] wall-cube-bottom-front))
                        (bottom-place 0.8 4 (translate [0 (+ front-offset 0.65) 0] wall-cube-bottom-front)))

                       (hull
                        (thumb-place 0 -1 web-post-br)
                        (thumb-place 0 -1/2 web-post-br)
                        (thumb-place thumb-right-wall thumb-front-row (translate [-1.12 thumb-front-offset thumb-ridge-height] wall-cube-bottom-front))
                        (key-place 1 4 (translate [2.5 -2 -0.8] web-post-bl))
                        (key-place 1 4 (translate [1.23 -4 0] web-post-bl))
                        (key-place 1 4 half-post-bl))]]

     (apply union
            (concat
             main-keys-bottom
             front-wall
             right-wall
             back-wall
             left-wall
             thumbs
             thumb-back-wall
             thumb-left-wall
             thumb-front-wall
             thumb-inside
             )))))

(defn stands-at [diameter]
  (union
    [(stand-at diameter #(key-place 0.06 1 %))
     (stand-at diameter #(thumb-place 1 -1/2 %))
     (stand-at diameter #(key-place 5 0 %))
     (stand-at diameter #(key-place 5 3 %))]))

(defn stands-alignment [side]
  (let
    [hole (->> (cylinder 2 15)
               (translate [0 0 -8])
               (with-fn wall-sphere-n))]
    (union [(if (= side RIGHT)
              (translate [0 0 -7] (key-place 0.06 1 hole))
              (key-place 0.06 1 hole))
           (thumb-place 1 -1/2 hole)
           (key-place 5 0 hole)
           (key-place 5 3 hole)])))

(defn stands-diff [shape]
  (let [-tolerance (if STANDS_SEPERATE (- 0.2) 0)
       diff (union
              bottom-plate
              (hull shape))]
   (union (translate [0 -tolerance 0] diff)
          (translate [-tolerance 0 0] diff))))

(def stands (stands-at 9.8)) ; 3/8 = 9.6 1/2 = 12.7


;; Defining Screw Holes

(def screw-hole (->> (cylinder 1.5 16)
                     (translate [0 2 -6])
                     (with-fn wall-sphere-n)))

(def screw-holes
  (union
   (key-place (+ 5 7/10) 1/2 screw-hole)
   (key-place (+ 5 7/10) (+ 3 1/2) screw-hole)
   (thumb-place 2.645 -1.15 screw-hole)
   (thumb-place 2.645 0.975 screw-hole))
  )


;; Defining Screw Mounting Posts (screw-tab attaches to plate. case-tab attaches to bottom case. screw-tab sits flush on top of case-tab.)

(def screw-mount (->> (cylinder 2.5 2)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def screw-cube (->> (cube 2 3.4 plate-thickness)
                     (translate [2 2 0])
                     ))


(def screw-cube2 (->> (cube 2.75 3.4 plate-thickness)
                     (translate [2.75 2 0])
                     ))

(def screw-tab (difference (hull screw-mount screw-cube) screw-hole))

(def screw-tab2 (difference (hull screw-mount screw-cube2) screw-hole))

(def case-mount (->> (cylinder 2.5 6.45)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def case-cube (->> (cube 2 3.4 6.45)
                     (translate [2 2 0])
                     ))

(def case-hole (->> (cylinder 1.5 6.45)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def case-tab (difference (hull case-mount case-cube) case-hole))

;; the top-right-hand mounting post needs to be taller to align with the bottom case.

(def case-mount-tr (->> (cylinder 2.5 7.95)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def case-cube-tr (->> (cube 2 3.4 7.95)
                     (translate [2 2 0])
                     ))

(def case-hole-tr (->> (cylinder 1.5 7.95)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def case-tab-tr (difference (hull case-mount-tr case-cube-tr) case-hole-tr))

;; the bottom-right-hand mounting post needs to be shorter to align with the bottom case. 

(def case-mount-br (->> (cylinder 2.5 6.05)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def case-cube-br (->> (cube 2 3.4 6.05)
                     (translate [2 2 0])
                     ))

(def case-hole-br (->> (cylinder 1.5 6.05)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def case-tab-br (difference (hull case-mount-br case-cube-br) case-hole-br))

;; setting up top case <--> plate tolerances

(def tol-screw-mount (->> (cylinder 2.6 2.1)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def tol-screw-cube (->> (cube 2.85 3.4 plate-thickness)
                     (translate [2.75 2 0])
                     ))

(def tol-case-mount (->> (cylinder 2.6 6.5)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def tol-case-cube (->> (cube 2.1 3.4 6.5)
                     (translate [2.1 2 0])
                     ))

(def tol-case-hole (->> (cylinder 1.5 6.45)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def tol-case-mount-tr (->> (cylinder 2.6 8)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def tol-case-cube-tr (->> (cube 2.1 3.4 7.95)
                     (translate [2.1 2 0])
                     ))

(def tol-case-hole-tr (->> (cylinder 1.5 7.95)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def tol-case-mount-br (->> (cylinder 2.6 6.1)
                    (translate [0 2 0])
                    (with-fn wall-sphere-n)))

(def tol-case-cube-br (->> (cube 2.1 3.4 6.1)
                     (translate [2.1 2 0])
                     ))

(def tol-case-hole-br (->> (cylinder 1.5 6.05)
                     (translate [0 2 -2])
                     (with-fn wall-sphere-n)))

(def tol-case-tab (difference (hull tol-case-mount tol-case-cube) tol-case-hole))

(def tol-case-tab-tr (difference (hull tol-case-mount-tr tol-case-cube-tr) tol-case-hole-tr))

(def tol-case-tab-br (difference (hull tol-case-mount-br tol-case-cube-br) tol-case-hole-br))

(def tol-screw-tab (difference (hull tol-screw-mount tol-screw-cube) screw-hole))



;; Placing the plate screw-tabs

(def screw-tabs (union
																				(thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	-0.63] screw-tab)))
																				(thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0 -0.27] screw-tab)))
																				(key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 -0.29] screw-tab)))
																				(key-place 5.65 -0.4425 (rotate -3.255 [0.9 0.015 0] (translate [0 0 -0.305] (mirror [1 0 0] screw-tab))))
																				(key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 -0.6775] (mirror [1 0 0] screw-tab))))
																				)
)

(def screw-tabs2 (union
																				(thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	-0.63] screw-tab2)))
																				(thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0 -0.27] screw-tab2)))
																				(key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 -0.29] screw-tab2)))
																				(key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 -0.305] (mirror [1 0 0] screw-tab2))))
																				(key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 -0.6775] (mirror [1 0 0] screw-tab2))))
																				)
)

;; Placing the bottom case mounting posts

(def screw-joints (union
																				  (thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	3.6] case-tab)))
																				  (thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0	3.96] case-tab)))					
                      (key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 3.94] case-tab)))
																				  (key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 4.68] (mirror [1 0 0] case-tab-tr))))
																				  (key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 3.3525] (mirror [1 0 0] case-tab-br))))															  
																		)
)

;; Attempt to create an alternate thumb-left-wall on the bottom case. Didn't work.

;(def screw-wall (difference
;																				  (hull 
;                           (thumb-place 2.65 1.441 (rotate -3.256 [1 0 0] (translate [0 0	3.96] case-tab)))
;																				       (thumb-place 2.65 0.741 (rotate -3.256 [1 0 0] (translate [0 0	3.96] case-tab)))
;																				  )
;																	(thumb-place 2.65 1.441 (rotate -3.256 [1 0 0] (translate [0 0	3.98] case-tab)))														  
;																		)
;)

;; This is a cutout to prevent any of the cases from filling into the plate-to-bottom-case screw holes.

(def hull-joints (union
																				  (hull 
																				  (thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	3.6] case-tab)))
																		  		(thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	-0.63] screw-tab)))
																		  		)
																				  (hull																				  
																				  (thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0	3.96] case-tab)))					
  																				(thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0 -0.27] screw-tab)))
  																				)
															  			  (hull                      
                      (key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 2.94] case-tab)))
  																				(key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 -1.29] screw-tab)))
  																				)																				  
																				  (hull
																				  (key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 4.68] (mirror [1 0 0] case-tab-tr))))
																			  	(key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 -0.305] (mirror [1 0 0] screw-tab))))
																			  	)																				  
																				  (hull
																				  (key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 3.3525] (mirror [1 0 0] case-tab-br))))
																				  (key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 -0.6775] (mirror [1 0 0] screw-tab))))
																				  )															  
																		)
)


(def tol-hull-joints (union
																				  (hull 
																				  (thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	3.6] tol-case-tab)))
																		  		(thumb-place 2.65 -1.24 (rotate 3.208 [1 0 0] (translate [0 0	-0.63] tol-screw-tab)))
																		  		)
																				  (hull																				  
																				  (thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0	3.96] tol-case-tab)))					
  																				(thumb-place 2.65 1.431 (rotate -3.256 [1 0 0] (translate [0 0 -0.27] tol-screw-tab)))
  																				)
															  			  (hull                      
                      (key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 2.94] tol-case-tab)))
  																				(key-place -0.65 -0.4325 (rotate -3.258 [0.9 0.015 0] (translate [0 0 -1.29] tol-screw-tab)))
  																				)																				  
																				  (hull
																				  (key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 4.68] (mirror [1 0 0] tol-case-tab-tr))))
																			  	(key-place 5.65 -0.44 (rotate -3.255 [0.9 0.015 0] (translate [0 0 -0.305] (mirror [1 0 0] tol-screw-tab))))
																			  	)																				  
																				  (hull
																				  (key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 3.3525] (mirror [1 0 0] tol-case-tab-br))))
																				  (key-place 5.645 4.2392 (rotate -3.075 [1.1 0.015 0] (translate [0 0 -0.6775] (mirror [1 0 0] tol-screw-tab))))
																				  )															  
																		)
)

(defn circuit-cover [width length height]
  (let [cover-sphere-radius 2
        cover-sphere (->> (sphere cover-sphere-radius)
                          (with-fn 2))
        cover-sphere-z (+ (- height) (- cover-sphere-radius))
        cover-sphere-x (+ (/ width 2) cover-sphere-radius)
        cover-sphere-y (+ (/ length 2) (+ cover-sphere-radius))
        cover-sphere-tl (->> cover-sphere
                             (translate [(- cover-sphere-x) (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-tr (->> cover-sphere
                             (translate [cover-sphere-x (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-br (->> cover-sphere
                             (translate [cover-sphere-x cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-bl (->> cover-sphere
                             (translate [(- cover-sphere-x) cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))

        lower-to-bottom #(translate [0 0 (+ (- cover-sphere-radius) -5.5)] %)
        bl (->> cover-sphere lower-to-bottom (key-place 0 1/2))
        br (->> cover-sphere lower-to-bottom (key-place 1 1/2))
        tl (->> cover-sphere lower-to-bottom (key-place 0 5/2))
        tr (->> cover-sphere lower-to-bottom (key-place 1 5/2))

        mlb (->> cover-sphere
                 (translate [(- cover-sphere-x) 0 (+ (- height) -1)])
                 (key-place 1/2 3/2))
        mrb (->> cover-sphere
                 (translate [cover-sphere-x 0 (+ (- height) -1)])
                 (key-place 1/2 3/2))

        mlt (->> cover-sphere
                 (translate [(+ (- cover-sphere-x) -4) 0 -6])
                 (key-place 1/2 3/2))
        mrt (->> cover-sphere
                 (translate [(+ cover-sphere-x 4) 0 -6])
                 (key-place 1/2 3/2))]
    (union
      (hull cover-sphere-bl cover-sphere-br cover-sphere-tl cover-sphere-tr)
      (hull cover-sphere-br cover-sphere-bl bl br)
      (hull cover-sphere-tr cover-sphere-tl tl tr)
      (hull cover-sphere-tl tl mlb mlt)
      (hull cover-sphere-bl bl mlb mlt)
      (hull cover-sphere-tr tr mrb mrt)
      (hull cover-sphere-br br mrb mrt))))

(def io-exp-width 21)
(def io-exp-height 12)
(def io-exp-length 35)

(def teensy-width 21)
(def teensy-height 12)
(def teensy-length 35)

(def io-exp-cover (circuit-cover io-exp-width io-exp-length io-exp-height))
(def teensy-cover (circuit-cover teensy-width teensy-length teensy-height))

(def trrs-diameter 6.6)
(def trrs-radius (/ trrs-diameter 2))
(def trrs-hole-depth 10)

(def trrs-hole (->> (union (cylinder trrs-radius trrs-hole-depth)
                           (->> (cube trrs-diameter (+ trrs-radius 5) trrs-hole-depth)
                                (translate [0 (/ (+ trrs-radius 5) 2) 0])))
                    (rotate (/ π 2) [1 0 0])
                    (translate [0 (+ (/ mount-height 2) 4) (- trrs-radius)])
                    (with-fn 50)))

(def trrs-hole-just-circle
  (->> (cylinder trrs-radius trrs-hole-depth)
       (rotate (/ π 2) [1 0 0])
       (translate [0 (+ (/ mount-height 2) 4) (- trrs-radius)])
       (with-fn 50)
       (key-place 1/2 0)))

(def trrs-box-hole (->> (cube 14 14 7 )
                        (translate [0 1 -2])))


(def trrs-cutout
  (->> (union trrs-hole
              trrs-box-hole)
       (key-place 1/2 0)))

(def teensy-pcb-thickness 1.6)
(def teensy-offset-height 5)

(def teensy-pcb (->> (cube 18 30.5 teensy-pcb-thickness)
                     (translate [0 0 (+ (/ teensy-pcb-thickness -2) (- teensy-offset-height))])
                     (key-place 1/2 3/2)
                     (color [1 0 0])))

(def teensy-support
  (difference
   (union
    (->> (cube 5 5 6)
         (translate [0 0 -2])
         (key-place 1/2 3/2)
         (color [0 1 0]))
    (hull (->> (cube 3.675 6 7)
               (translate [0 0 -2])
               (key-place 1/2 2)
               (color [0 0 1]))
          (->> (cube 3.675 5 (+ teensy-pcb-thickness 5))
               (translate [0 (/ 30.5 -2) (+ (- teensy-offset-height)
                                            #_(/ (+ teensy-pcb-thickness 5) -2)
                                            )])
               (key-place 1/2 3/2)
               (color [0 0 1]))

          ))
   teensy-pcb
   (->> (cube 18 31 (+ teensy-pcb-thickness 1))
        (translate [0 1.5 (+ (/ teensy-pcb-thickness -2) (- teensy-offset-height) -1.5)])
        (key-place 1/2 3/2)
        (color [1 0 0]))
   ))

(def usb-cutout
  (let [hole-height 6.2
        side-radius (/ hole-height 2)
        hole-width 10.75
        side-cylinder (->> (cylinder side-radius teensy-length)
                           (with-fn 20)
                           (translate [(/ (- hole-width hole-height) 2) 0 0]))]
    (->> (hull side-cylinder
               (mirror [-1 0 0] side-cylinder))
         (rotate (/ π 2) [1 0 0])
         (translate [0 (/ teensy-length 2) (- side-radius)])
         (translate [0 0 (- 1)])
         (translate [0 0 (- teensy-offset-height)])
         (key-place 1/2 3/2))))


;;;;;;;;;;;;;;;;
;; Tolerances ;;
;;;;;;;;;;;;;;;;

(def tolerance 0.2)

(def case-tolerance
  (let [place offset-case-place
        t tolerance
        -t (- tolerance)
        th (/ t 2)
        -th (/ -t 2)
        -tq (/ -t 4)
        t2 (* t 2)
        -t2 (* -t 2)]
    (union
      (place [0 0 0] case-inside-cutout)
      (place [0 0 0] new-case)
      (if-not FAST_RENDER
        (union
          (place [0 0 -t] new-case)
          (place [t2 -t -tq] front-wall)
          (place [t t -th] front-wall)
          (place [0 -t -th] front-wall)
          (place [-t 0 -t] right-wall)
          (place [t 0 -t] right-wall)
          (place [0 th -th] back-wall)
          (place [0 -th -th] back-wall)
          ;(place [-45 50 54.3] (rotate (/ π 5.5) [1 0 0] (cube 10 10 10)))
          ;(place [80 49 29.83] (rotate (/ π 5.5) [1 0 0] (cube 10 10 10)))
          (place [-t -t -t] left-wall)
          (place [t 0 -t2] left-wall)
          (place [-t 0 0] left-inside-wall)
          (place [-t 0 -t] left-inside-wall)
          (place [-t -t -t] left-inside-wall)
          (place [0 -t -t2] thumb-back-wall)
          (place [-t t -th] thumb-back-wall)
          (place [0 t -t] thumb-back-wall)
          (place [t 0 (* -t 1.5)] thumb-left-wall)
          (place [-t 0 -t] thumb-left-wall)
          (place [t t -t] thumb-inside-wall)
          (place [-t 0 -t] thumb-inside-wall)
          (place [-t t -t] thumb-front-wall)
          (place [t 0 -t] thumb-front-wall)
          (place [0 (* 2 -t) -t] thumb-front-wall))))))


;;;;;;;;;;;;;;;;
;; Palm Rests ;;
;;;;;;;;;;;;;;;;

(defn bezier-conic [p0 p1 p2 steps]
  (let [step1 (/ (- p1 p0) steps)
        step2 (/ (- p2 p1) steps)]
    (for [i (range steps)]
      (let [point1 (+ p0 (* step1 i))
            point2 (+ p1 (* step2 i))
            point3 (+ p0 (* step1 (+ i 1)))
            point4 (+ p1 (* step2 (+ i 1)))
            bpoint1 (+ point1 (* (- point2 point1) (/ i steps)))
            bpoint2 (+ point3 (* (- point4 point3) (/ (+ i 1) steps)))]
        (polygon [bpoint1 bpoint2 p1])))))

(defn bezier-cone [d h curve steps & {:keys [curve2] :or {curve2 (/ h 2)}}]
  (let [p0 [(/ d 2) 0]
        p1 [(+ curve (/ d 4)) curve2]
        p2 [0 h]]
  (cond
     (< (nth p1 0) (/ d 4)) ; concave
       (do (->> (union (polygon [[0 0] p0 p1 p2 [0 h]])
                       (bezier-conic p0 p1 p2 steps))
                (extrude-rotate {:fn steps})))
     (> (nth p1 0) (/ d 4)) ; convex
       (do (->> (difference (polygon [[0 0] p0 p1 p2 [0 h]])
                            (bezier-conic p0 p1 p2 steps))
                (extrude-rotate {:fn steps}))))))

(defn front-palm-rest-rotate [shape]
  (->> shape
    (rotate (/ π 1) [1 0 0])
    (rotate (/ π 2) [0 0 1])
    (rotate (/ π 4) [-1 0 0])
    (rotate (/ π 6.8) [0 1 0])))

(def palm-rest
  (let [p0 [15 0]
        p1 [25 14]
        p2 [7 30]
        stand-diameter 9.6
        rest-sphere-n (if FAST_RENDER 20 170)
        profile-sphere-n (* rest-sphere-n 2)
        floor (->> (cube 300 300 50)
                   (translate [0 0 -25]))

        profile-cyl (->> (cylinder 200 50)
                         (with-fn profile-sphere-n))

        rest-place #(->> % (rotate (/ π 40) [0 1 1])
                           (rotate (/ π 11) [1 0 0])
                           (rotate (/ π 12) [0 1 0])
                           (translate [22 -100 7]))

        front-profile (->> (difference profile-cyl
                                       (scale [1.4 0.81 1.1] profile-cyl))
                           (scale [1 1.4 1])
                           (translate [0 -225 55])
                           (rotate (/ π 3.2) [-1 -0.2 -0.2]))

        bottom-profile (->> (cylinder 100 200)
                            (with-fn profile-sphere-n)
                            (rotate (/ π 2.3) [0 1 0])
                            (translate [0 0 -65])
                            (scale [1 1.1 1]))

        base-shape (->> (bezier-cone 80 100 43 rest-sphere-n :curve2 43)
                        (rotate (/ π 2) [-1 0 0])
                        (translate [0 -10 0])
                        (scale [1.1 1 1]))

        base-rest-shape (difference
                          (rest-place
                           (difference base-shape
                                       front-profile
                                       bottom-profile
                                       (scale [0.93 0.93 0.93] base-shape)))
                         floor)

        shape-profile (->> (project base-rest-shape)
                           (extrude-linear {:height 20}))

        side-profile (->> (difference (scale [1.1 1.1 1] shape-profile)
                                      (translate [3 -12 0]
                                        (scale [0.97 0.97 1.2] shape-profile)))
                          (rotate (/ π 26) [0 1 0])
                          (translate [-2 9 50]))

        rest-shape (difference base-rest-shape
                               side-profile)

        inner-rest #(intersection
                      % (intersection
                        (rest-place base-shape)
                        (->> (project rest-shape)
                             (extrude-linear {:height 100})
                             (translate [0 0 (/ 100 2)]))))

        stand-place #(translate [24 -60 0] %)

        front-rect (->> (prism 15 9 50 5 -2)
                        front-palm-rest-rotate
                        (translate [15.5 15 35])
                        stand-place)

        front-rect-diff (->> (cube 100 30 30)
                             (rotate (/ π 4) [1 0 0])
                             (rotate (/ π 20) [0 0 -1])
                             (translate [0 -37 31]))

        back-neg-rect (->> (prism 20.3 8 43 3 1)
                           (rotate (/ π 3.5) [1 0 0])
                           (translate [-10.15 -35 -5])
                           stand-place)

        back-neg-rect-diff (->> (cube 30 30 30)
                                (rotate (/ π 6) [-1 0 0])
                                (translate [-7.3 -80.6 28])
                                stand-place)

        back-pos-rect-1 (->> (prism 12 10 60 3 4)
                             (rotate (/ π 2) [0 0 1])
                             (rotate (/ π 9) [-1 0 0])
                             (translate [5 -56 11.8])
                             stand-place
                             inner-rest)

        back-pos-rect-2 (->> (prism 15 5 31 3 -2)
                             (rotate (/ π 6.8) [-1 0 0])
                             (translate [-7.5 -66 23.5])
                             stand-place)

        inner-support (->> (bezier-cone 90 12 -10 rest-sphere-n)
                           (rotate (/ π 1.01) [-1 0 0])
                           (rotate (/ π 20) [0 1 0])
                           (translate [3 -57 54])
                           stand-place
                           inner-rest)

        bottom-rect (->> (cube 30 20 7)
                         (rotate (/ π 2) [0 0 1])
                         (translate [0 -19.3 1])
                         stand-place)
        stands (difference
                 (union front-rect
                        (translate [48 0 0] (mirror [-1 0 0] front-rect))
                        bottom-rect
                        inner-support
                        back-neg-rect
                        back-pos-rect-1
                        back-pos-rect-2)
               back-neg-rect-diff
               bottom-plate
               (if RESTS_SEPERATE
                 (do case-tolerance
                     (translate [0 (- tolerance) 0] bottom-plate)))
               front-rect-diff
               floor)]
    (union stands
           rest-shape)))

(def rest-alignment
  (let [shape (->> (cylinder 2 20)
                   front-palm-rest-rotate
                   (translate [13.5 0 0])
                   (with-fn 20))]

    (translate [24 -51 18]
      (union
        (translate [-0.8 -1.75 -1.75] shape)
        (mirror [-1 0 0] shape)))))

;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(def floor
  (->> (cube 1000 1000 10)
       (translate [0 0 -5])))

(def dactyl-stands-left
  (mirror [-1 0 0]
    (difference stands
                (if STANDS_SEPERATE (stands-alignment LEFT)
                   (stands-diff io-exp-cover)))))

(def dactyl-stands-right
  (difference stands
              (stands-diff teensy-cover)
              (if STANDS_SEPERATE
                ((stands-alignment RIGHT)
                 (stands-diff (key-place 0 1 (cube 10 10 10)))
                 (stands-diff (key-place 0 1 (translate [0 0 -5] (cube 15 15 15) )))))))

(def dactyl-rest-left
  (mirror [-1 0 0]
    (difference palm-rest
                (if RESTS_SEPERATE rest-alignment))))

(def dactyl-rest-right
  (difference palm-rest
                (if RESTS_SEPERATE rest-alignment)))

(def dactyl-keycaps-left
  (mirror [-1 0 0]
      (union thumbcaps caps)))

(def dactyl-keycaps-right
      (union thumbcaps caps))

(def dactyl-bottom-right
  (union
    (if-not RESTS_SEPERATE dactyl-rest-right)
    (if-not STANDS_SEPERATE dactyl-stands-right)
    (difference
      (union teensy-cover
      							screw-joints
      							;screw-tabs2
      							(difference (difference bottom-plate hull-joints)
                         case-tolerance
                         (hull teensy-cover)
                          (if RESTS_SEPERATE rest-alignment)
                         teensy-cover
                         trrs-cutout
                         screw-holes
                         floor))
      (if STANDS_SEPERATE (stands-alignment RIGHT))
      usb-cutout)))

(def dactyl-bottom-left
  (union
    (if-not RESTS_SEPERATE dactyl-rest-left)
    (if-not STANDS_SEPERATE dactyl-stands-left)
    (mirror [-1 0 0]
      (difference
        (union io-exp-cover
        							screw-joints
              (difference (difference bottom-plate hull-joints)
                          case-tolerance
                          (hull io-exp-cover)
                          (if RESTS_SEPERATE rest-alignment)
                          io-exp-cover
                          trrs-cutout
                          screw-holes
                          floor))
        (if STANDS_SEPERATE (stands-alignment LEFT))))))

(def dactyl-top-right
  ;(offset-case-place [0 0 0]
    (union
      (difference
        (union ;key-holes
               ;connectors
               ;thumb
               ;screw-tabs2
               (difference new-case (union tol-hull-joints screw-tabs2 screw-joints))
               ;teensy-support
			   ;dactyl-keycaps-right
			   )
      trrs-hole-just-circle
      screw-holes
      ;screw-tabs
      )))
;)

(def dactyl-top-right-plate
  ;(offset-case-place [0 0 0]
    (union
      (difference
        (union key-holes
               connectors
               thumb
               screw-tabs2
               ;(translate [0 0 10] (difference new-case (union tol-hull-joints screw-tabs2 screw-joints screw-holes)))
               teensy-support
			   ;dactyl-keycaps-right
			   )
      trrs-hole-just-circle
      screw-holes
      ;screw-tabs
      )))
;)

(def dactyl-top-left
  (mirror [-1 0 0]
      (union
        (difference
          (union ;key-holes
                 ;connectors
                 ;thumb
                 ;screw-tabs2
                 (difference new-case tol-hull-joints)
          ;dactyl-keycaps-right
				 )
          trrs-hole-just-circle
          screw-holes
          ;screw-tabs
          ))))

(def dactyl-top-left-plate
  (mirror [-1 0 0]
    ;(offset-case-place [0 0 0]
      (union
        (difference
          (union key-holes
                 connectors
                 thumb
                 screw-tabs2
                 ;(difference new-case tol-hull-joints)
          ;dactyl-keycaps-right
				 )
          trrs-hole-just-circle
          screw-holes
          ;screw-tabs
          ))))


;;;;;;;;;;;;;
;; Outputs ;;
;;;;;;;;;;;;;

(spit "things/dactyl-top-right.scad"
      (write-scad dactyl-top-right))

(spit "things/dactyl-top-right-plate.scad"
      (write-scad dactyl-top-right-plate))

(spit "things/dactyl-bottom-right.scad"
      (write-scad dactyl-bottom-right))

(spit "things/dactyl-top-left.scad"
      (write-scad dactyl-top-left))

(spit "things/dactyl-top-left-plate.scad"
      (write-scad dactyl-top-left-plate))

(spit "things/dactyl-bottom-left.scad"
      (write-scad dactyl-bottom-left))

;(spit "things/dactyl-keycaps-left.scad"
    ;  (write-scad dactyl-keycaps-left))

;(spit "things/dactyl-keycaps-right.scad"
   ;   (write-scad dactyl-keycaps-right))

;(spit "things/dactyl-combined-left.scad"
      ;(write-scad (union dactyl-bottom-left dactyl-top-left)))
	  
(spit "things/dactyl-combined-right.scad"
      (write-scad (rotate 90 [0 0 1] (union dactyl-bottom-right dactyl-top-right dactyl-top-right-plate ))))

;(spit "things/switch-hole.scad"
      ;(write-scad single-plate))

;(spit "things/thumb-switch-hole.scad"
	;					(write-scad double-plates))

;(spit "things/key-plate.scad"
 ;     (write-scad (union connectors key-holes)))

;(spit "things/key-holes.scad"
 ;     (write-scad key-holes))

;(spit "things/top-plate.scad"
 ;     (write-scad (union connectors key-holes thumb screw-tabs)))

(spit "things/bottom-mount.scad"
      (write-scad (difference
      												(union 
								                (difference new-case tol-hull-joints)
      																			(difference bottom-plate hull-joints) 
      																			screw-joints 
                  )
                  trrs-hole-just-circle
                  screw-holes
      												)))

;(spit "things/bottom-mount-left.scad"
      
      ;(write-scad (mirror [-1 0 0]
       ;           (difference
      		;										(union ;(difference new-case hull-joints)
      			;										      connectors
      				;															key-holes 
      					;														thumb 
      						;													screw-tabs 
      							;												(difference bottom-plate hull-joints) 
      								;											screw-joints 
               ;   )
                ;  trrs-hole-just-circle
      											;	)
      											;	)
      ;)
;)


(spit "things/screw-tab.scad"
      (write-scad screw-tab2))

(spit "things/top-case.scad"
      (write-scad 
      (difference
      (union
      (difference new-case tol-hull-joints)
      connectors
      key-holes
      screw-tabs2
      screw-joints
      thumb
      ;(difference bottom-plate hull-joints)
      )
      screw-holes)
      ))

;(spit "things/case.scad"
 ;     (write-scad (difference new-case (union hull-joints screw-tabs screw-joints))))

;(spit "things/left-thumb-wall.scad"
      ;(write-scad (difference thumb-left-wall screw-holes)))


(if RESTS_SEPERATE
  (do
    (spit "things/dactyl-rest-left.scad"
          (write-scad dactyl-rest-left))

    (spit "things/dactyl-rest-right.scad"
          (write-scad dactyl-rest-right))))


(if STANDS_SEPERATE
  (do
   (spit "things/dactyl-stands-left.scad"
         (write-scad dactyl-stands-left))

   (spit "things/dactyl-stands-right.scad"
         (write-scad dactyl-stands-right))))
