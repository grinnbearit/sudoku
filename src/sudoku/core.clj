(ns sudoku.core
  (:require [clojure.set :as set]))


(defn problem->constraints
  "Converts a 9x9 sudoku grid into a set of constraints
  given by [rows, columns, grids, blanks]"
  [problem]
  (letfn [(reducer [[rows columns squares blanks] [r c digit]]
            (if (nil? digit)
              [rows columns squares (conj blanks [r c])]
              [(update rows r conj digit)
               (update columns c conj digit)
               (update-in squares [(quot r 3) (quot c 3)] conj digit)
               blanks]))]

    (->> (for [row (range 9)
               column (range 9)]
           [row column (get-in problem [row column])])
         (reduce reducer [(vec (repeat 9 #{}))
                          (vec (repeat 9 #{}))
                          (vec (repeat 3 (vec (repeat 3 #{}))))
                          []]))))


(defn solve
  "Either returns a list of blanks to be filled or Nil if no solution
  can be found given the constraints"
  [rows columns squares blanks]
  (if (empty? blanks)
    ()
    (let [[r c] (first blanks)]
      (->> (for [option (set/difference (set (range 1 10))
                                        (rows r)
                                        (columns c)
                                        (get-in squares [(quot r 3) (quot c 3)]))
                 :let [solution (solve (update rows r conj option)
                                       (update columns c conj option)
                                       (update-in squares [(quot r 3) (quot c 3)] conj option)
                                       (rest blanks))]
                 :when solution]
             (conj solution [r c option]))
           first))))


(defn fill-blanks
  "Given a sudoku problem and a solution returns a filled sudoku grid"
  [problem solution]
  (letfn [(reducer [acc [r c digit]]
            (assoc-in acc [r c] digit))]

    (reduce reducer problem solution)))


(defn sudoku
  "Given a sudoku problem grid, returns a filled solution
  or nil if none exists"
  [problem]
  (let [[rows columns squares blanks] (problem->constraints problem)]
    (when-let [solution (solve rows columns squares blanks)]
      (fill-blanks problem solution))))
