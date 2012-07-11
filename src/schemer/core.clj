(ns schemer.core)

(defn atom?
  [tester]
  (or (string? tester) (number? tester)))

(defn lat?
  [group]
  (cond
    (empty? group) true
    (atom? (first group)) (lat? (rest group))
    :else false))

(lat? '("foo" foo 1 2 3)) ; true
(lat? '("foo" ()  1 2 3)) ; false

(defn member?
  [a group]
  (cond
    (or (not (lat? group)) (empty? group)) false
    (= a (first group)) true
    :else (recur a (rest group))))

(member? "fo" '("foo" "bar" 12 "baz")) ; false
(member? "foo" '("foo" "bar" 12 "baz")) ; true
(member? "foo" "foo") ; false

(defn rember
  [a group]
  (cond
    (not (lat? group)) group
    (empty? group) '()
    (= (first group) a) (rest group)
    :else (cons (first group) (rember a (rest group)))))

(rember "foo" '("foo" "google" "test" "foo")) ; ("google" "test" "foo")
(rember "fo" '("foo" "google" "test" "foo")) ; ("foo" "google" "test" "foo")

(defn rember-all
  [a group]
  (cond
    (not (lat? group)) group
    (empty? group) '()
    (= (first group) a) (rember a (rest group))
    :else (cons (first group) (rember a (rest group)))))

(rember-all "foo" '("foo" "google" "test" "foo")) ; ("google" "test" "foo")
(rember-all "fo" '("foo" "google" "test" "foo")) ; ("foo" "google" "test" "foo")

(defn insert-after
  [a b group]
  (cond
    (empty? group) '()
    (or (not (list? group)) (not (member? b group))) group
    (= (first group) b) (conj (rest group) a b)
    :else (cons (first group) (insert-after a b (rest group)))))

(insert-after "f" "b" "test") ; "test"
(insert-after "f" "b" '("f" "foo" "bar")) ; ("f" "foo" "bar")
(insert-after "f" "b" '("a" "f" "b" "j" "bar")) ; ("a" "f" "b" "f" "j" "bar")
(insert-after "f" "a" '("a" "f" "b" "j" "bar")) ; ("a" "f" "f" "b" "j" "bar")

(defn insert-before
  [a b group]
  (cond
    (empty? group) '()
    (or (not (list? group)) (not (member? b group))) group
    (= (first group) b) (conj (rest group) b a)
    :else (cons (first group) (insert-after a b (rest group)))))

(insert-before "f" "b" "test") ; "test"
(insert-before "f" "b" '("f" "foo" "bar")) ; ("f" "foo" "bar")
(insert-before "f" "b" '("b" "a" "f" "j" "bar")) ; ("f" "b" "a" "f" "j" "bar")

(defn insert-after-all
  [a b group]
  (cond
    (empty? group) '()
    (or (not (list? group)) (not (member? b group))) group
    (= (first group) b) (conj (insert-after a b (rest group)) a b)
    :else (cons (first group) (insert-after a b (rest group)))))

(insert-after-all "f" "b" '("a" "f" "b" "j" "bar")) ; ("a" "f" "b" "f" "j" "bar")
(insert-after-all "b" "a" '("a" "f" "b" "a" "j" "bar")) ; ("a" "b" "f" "b" "a" "b" "j" "bar")

(defn insert-before-all
  [a b group]
  (cond
    (empty? group) '()
    (or (not (list? group)) (not (member? b group))) group
    (= (first group) b) (conj (insert-before a b (rest group)) b a)
    :else (cons (first group) (insert-before a b (rest group)))))

(insert-before-all "f" "b" '("a" "f" "b" "j" "bar")) ; ("a" "f" "b" "f" "j" "bar")
(insert-before-all "b" "a" '("a" "f" "b" "a" "j" "bar")) ; ("b" "a" "f" "b" "a" "b" "j" "bar")

(defn substitute
  [a b group]
  (cond
    (empty? group) '()
    (or (not (list? group))
        (not (member? b group))) group
    (= (first group) b) (cons a (substitute a b (rest group)))
    :else (cons (first group) (substitute a b (rest group)))))

(substitute "f" "b" '("a" "f" "b" "j" "bar")) ; ("a" "f" "f" "j" "bar")
(substitute "b" "a" '("a" "f" "b" "a" "j" "bar")) ; ("b" "f" "b" "b" "j" "bar")

(defn rev
  [n]
  (* -1 n))

(declare subtract)

(defn add
  [n m & additional]
  (cond
    (neg? m) (apply subtract n (rev m) (map (fn [x] (rev x)) additional))
    (zero? m) (if-not (empty? additional)
                (recur n (first additional) (rest additional))
                n)
    :else (recur (inc n) (dec m) additional)))

(defn subtract
  [n m & additional]
  (cond
    (neg? m) (apply add n (rev m) (map (fn [x] (rev x)) additional))
    (zero? m) (if-not (empty? additional)
                (recur n (first additional) (rest additional))
                n)
    :else (recur (dec n) (dec m) additional)))

(add 5 200) ; 205
(add 5 -200 200) ; 5
(add 5 200 200 20) ; 5
(add -5 -4) ; -9
(subtract 4 20 4) ; -20
(subtract 20 4 4 4 4 4) ; 0
(subtract 4 -2) ; 6
(subtract -4 -2) ; -2
(subtract -4 -2 -20 -20) ; 38
