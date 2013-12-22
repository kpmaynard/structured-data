(ns structured-data)

(defn do-a-thing [x]

  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (+ (get v 0) (get v 2)))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x _ y] v]
     (if (or (nil? x) (nil? y)) nil (+ x y))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  ;:-
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1)))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1)))
  )

(defn square? [rectangle]
   (== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (and (>= x3 x1) (<= x3 x2)) (and (>= y3 y1) (<= y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [bottom-left top-right] inner]
      (and (contains-point? outer bottom-left) (contains-point? outer top-right))))


(defn title-length [book]
  (count (get book :title) )
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
 (let [new-authors (conj (get book :authors) new-author)]
   (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]

  (map count collection)
  )

(defn second-elements [collection]
  (map #(get % 1) collection)
  )

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]

  ;montonic increasing OR monotomic decreasing
  ;monotonic increasing

  (or
  (and (apply <= a-seq))
    (and (apply >= a-seq)))
  )

(defn stars [n]

  (apply str (repeat n \* ))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [ new-authors (set (get book :authors))]
     (assoc book :authors new-authors))
   )

(defn has-author? [book author]

  (contains? (get book :authors ) author)

  )

(defn authors [books]
  (apply clojure.set/union (map :authors  books))
  )

(defn all-author-names [books]
  ; version 1
  ; (set (apply concat (mapv #(mapv :name (:authors %)) books)))
  ;version 2
  (set (map :name (authors books)))

  )

(defn make-date [authors]
   (let [{death-year :death-year birth-year :birth-year} authors]
    (cond (contains? authors :death-year) (str " " \( birth-year " - " death-year \))
          (contains? authors :birth-year) (str " " \( birth-year  " - "\))
          :else ""
    )))


(defn author->string [author]
  (str (:name author) (make-date author))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str  (:title book) ", written by " (authors->string (:authors book)) ".")
  )

(defn count-books [books]
  (let [num-books (count books)]
  ( cond (> num-books 1) (str num-books " books. ")
         (== 1 num-books) (str num-books " book. ")
          :else "No books."
    )
  ))

(defn books->string [books]

  (let [num-books (count books)]

    (str (count-books books) (apply str (interpose " " (map book->string books))) )
  ))

(defn books-by-author [author books]

  (mapv :title (filterv #(has-author? % author) books))

  )

(defn author-by-name [name authors]
   (first (filter #(= (:name %) name) authors))
  )

(defn living-authors [authors]

   (filter #(alive? %) authors)
  )

(defn has-a-living-author? [book]
  (not ( empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  ;:-
  (filter #(has-a-living-author? %) books)
  )

; %________%
