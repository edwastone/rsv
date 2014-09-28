(ns rsv.parser
  (:require
   [net.cgrand.enlive-html :as html]
   [clojure.math.numeric-tower :as math]
   [clojure.java.jdbc :as sql]
   [clojurewerkz.urly.core :as urly]))
(use 'rsv.util)

(def max-pair-distance 5);;max no of html child per semantic table row; used for generating candidate parsers

(defn myconcat [S s] (hash [S s]))
(defn tree2paths
  ([T] (tree2paths T []))
  ([T path]
  (if (or (some #{(T :tag)} '(:script :title :a)) (string? (T :content)))
    [{:id (myconcat "" (apply str (T :content))) :path path}] ;;base case
    (loop [i 0 text "" res [] children (T :content)]
      (if (empty? children) (conj res {:id text :path path})
      (let [child (first children) tag (T :tag)]
        (if (string? child)
          (recur (inc i) (myconcat text child) res (rest children))
          (let [child-res (tree2paths child (conj path (:tag child) i)) child-text (:id (last child-res))]
        	(recur (inc i) (myconcat text child-text) (into res child-res) (rest children))
          )
        )
       )
      )
    )
  ))
)

(defn keep-unique [items]
  (let [freqs (frequencies (map :id items))
        ids (set (map first (filter #(= 1 (second %)) freqs)))]
   (filterv (comp ids :id) items)
    )
  )

;(keep-unique [{:id 1 :path [0]} {:id 2 :path [1]} {:id 1 :path [2]}])


(defn restv [v]
  (if (empty? v) [] (subvec v 1)))
(defn head-n-tail [path]
  (let [n (count path)]
    (map list
         (drop-last path )
         (reverse (take (inc n) (iterate pop path)))
         (iterate restv (restv path)))
    ))
;(head-n-tail [1 2 3])

(defn enumerate-headtail [items]
  (mapcat (comp head-n-tail every-second :path) items))

(defn group-headtail [items]
  (map #(update-in % [1] (partial map first)) (group-by rest items)))


(defn enumerate-pairs [L]
  (let [L (sort L)]
    (mapcat #(map list L (drop % L)) (range 1 max-pair-distance)))
  )
(defn enumerate-pattern [items]
  (mapcat
   (fn [[[head tail] L]]
     (map
      (fn [[i j]] {:head head :tail tail :delta (- j i) :p i :mod (mod i (- j i))})
      (enumerate-pairs L)))
   items))


(defn score-child-pattern [patterns s1]
  (let [freqs (frequencies (map #(dissoc % :p) patterns))]
    (map (fn [pattern cnt]
           (assoc pattern :score
             (/ cnt
                (count (:content (rsv.util/traverse s1 (:head pattern)))))))
         (keys freqs) (vals freqs))))

(defn median [L]
  (if (empty? L) nil
    (nth (sort L) (quot (dec (count L)) 2))))

(defn enumerate-roots [L1]
  (set
    (mapcat
     #(let [n (count %)]
        (if (zero? n) []
          (rest (take (inc n) (iterate pop %)))
          ))
      L1)))

(defn score-mod-pattern [patterns]
  (let [groups (group-by #(dissoc % :score :tail) patterns)]
    (map (fn [pattern insts score roots]
           (assoc pattern
             :children (sort-by :tail (filterv #(and (not (roots (:path %))) (>= (:score %) score)) insts))
             :score score))
         (keys groups)
         (vals groups)
         (map #(median (map :score %)) (vals groups))
         (map #(enumerate-roots (map :tail %)) (vals groups))
         ))
  )

(defn score-delta-pattern [patterns]
  (let [groups (group-by #(dissoc % :score :mod :children) patterns)]
     (map #(assoc %1 :children (apply vector %2) :score %3 )
         (keys groups) (vals groups) (map #(reduce + (map :score %)) (vals groups)))
    )
  )

(defn score-table [patterns]
  (let [groups (group-by #(dissoc % :score :delta :children) patterns)]
     (map #(assoc %1 :children [%2] :score (:score %2) )
         (keys groups) (map #(apply max-key :score %) (vals groups)))
    )
  )

(defn score-page [patterns]
  (apply max-key :score patterns)
  )

(defn sum-score-child [patterns]
  (map
   (fn [[k v]] (assoc k :score (reduce + (map :score v))))
   (group-by #(select-keys % [:head :tail :delta :mod]) patterns))
  )

(defn pattern-pair-match? [pattern-pair pattern]
  (and (apply = (map #(select-keys % [:head :tail]) [pattern-pair pattern]))
       (zero? (mod (:delta pattern-pair) (:delta pattern)))
       (= (:mod pattern) (mod (:p pattern-pair) (:delta pattern)))
  ))

(defn check-pair [L];;admissible if there are exactly 2 paths differing each other at exactly 1 node
  (let [L (map (comp rsv.util/every-second :path) L)]
  (if (not= 2 (count L)) false
    (and (apply = (map count L))
         (= (dec (count (first L))) (get (frequencies (apply map - L)) 0))
    ))))

(defn pathpair2pattern [path-pair]
  (let [[p1 p2] (map (comp rsv.util/every-second :path) path-pair)
        D (first (filter (partial apply not=) (map list (map list p1 (range)) (map list p2 (range)))))
        i (-> D second second)
        delta (- (-> D first first) (-> D second first))
        head (subvec p1 0 i)
        tail (subvec p1 (inc i))
        ]
    {:head head :tail tail :delta delta :p (-> D first first)}
    ))

(defn get-parsers[s1 s2]
  (let
    [nodes1 (-> s1 tree2paths keep-unique)
     nodes2 (-> s2 tree2paths keep-unique)
     path-pairs (filter check-pair (vals (group-by :id (concat nodes1 nodes2))))
     pattern-pairs (map pathpair2pattern path-pairs)
     pattern1 (-> nodes1 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s1))
     pattern2 (-> nodes2 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s2))
     patterns (sum-score-child (concat pattern1 pattern2))
     validated-patterns (concat patterns (sum-score-child (mapcat #(filter (partial pattern-pair-match? %) patterns) pattern-pairs)))
     ]
    (-> validated-patterns score-mod-pattern score-delta-pattern score-table score-page)
  ))

;; (def url "https://news.ycombinator.com/active")
;; (def s1 (fetch-url url))
;; (def s2 (fetch-url "https://news.ycombinator.com/newest"))
;; (get-parsers s1 s2)
;; (let
;;     [nodes1 (-> s1 tree2paths keep-unique)
;;      nodes2 (-> s2 tree2paths keep-unique)
;;      path-pairs (filter check-pair (vals (group-by :id (concat nodes1 nodes2))))
;;      pattern-pairs (map pathpair2pattern path-pairs)
;;      pattern1 (-> nodes1 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s1))
;;      pattern2 (-> nodes2 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s2))
;;      ;patterns (sum-score-child (concat pattern1 pattern2))
;;      ;validated-patterns (sum-score-child (mapcat #(filter (partial pattern-pair-match? %) patterns) pattern-pairs))
;;      ]
;;   (doall (concat pattern2))
;;   )
;; ;(try
;;   (-> s2 tree2paths
;;       keep-unique enumerate-headtail group-headtail enumerate-pattern
;;       (score-child-pattern s2)
;;       sum-score-child
;;       score-mod-pattern score-delta-pattern score-table score-page
;;   )
;(catch Exception e (.getMessage e)))
;;(defn keep-root [L1]
;;   (let [S1 (set L1)]
;; 	(filter #(not (some S1 (rest (take (count %) (iterate pop %))))) L1))
;;   )

