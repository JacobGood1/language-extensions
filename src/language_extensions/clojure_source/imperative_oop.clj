(ns clojure-source.imperative-oop
  (:import [internal_mutation_capsules LongHolder RatioHolder DoubleHolder BooleanHolder])
  (:require [clojure.walk]
            [clojure.core.match]
            [clojure-source.macro-helpers]))

(defn- make-map-destructure
  [map obj-name]
  (let [true-map (eval map)]
    (apply hash-map
           (-> (for [[a b] true-map] [(symbol (str obj-name "-" (.substring (str a) 1))) a])
               flatten
               reverse
               (conj :as map)
               reverse))))

(defn- determine-what-is-an-object
  [args]
  (loop [[arg & args] args
         destructured []]
    (if-not (nil? arg)
      (if (try (eval arg)
               (catch Exception e nil))
        (if (not (map? (eval arg)))
          (recur args (conj destructured arg))
          (recur args (conj destructured (make-map-destructure arg arg))))
        (recur args (conj destructured arg)))
      destructured)))



(defmacro def-method
  [name args & code]
  (let [morph-args (determine-what-is-an-object args)]
    `(defn ~name
       [~@morph-args]
       ~@code)))


(clojure.core/defn- make-mutable-args
  [args]
  (for [[arg value] (partition 2 args) :let [val (type value)]]
    (cond (or (map? value)
              (set? value)
              (vector? value))                      [arg `(transient ~value)]
          (= val clojure.lang.Cons)        [arg `(transient (vec ~value))]
          (= val java.lang.Long)           [arg `(new LongHolder ~value)]
          (= val clojure.lang.Ratio)       [arg `(new RatioHolder ~value)]
          (= val java.lang.Double)         [arg `(new DoubleHolder ~value)]
          (= val java.lang.Boolean)        [arg `(new BooleanHolder ~value)]
          :else                            [arg value])))

(clojure.core/defn- turn-args-into-proper-args-for-let
  [args]
  (loop [[[a b] :as list] args
         new-list []]
    (if (seq list)
      (recur (rest list)
             (conj new-list a b))
      new-list)))

(defn- set-get-and-transient-marks-for-args
  [args]
  (->> (for [[arg [value name]] args]
         (cond
           (re-find #"internal_mutation_capsules.*" (str name)) {arg (str "marked-get-" arg)}
           (= value 'clojure.core/transient) {arg (str "marked-transient-" arg)}
           :else {arg arg}))
       (map seq)
       flatten
       (apply hash-map)))

(defn- pop-value-out-of-containers
  [args code]
  (let [get-trans-keys (set-get-and-transient-marks-for-args args)]
    (loop [[v & vrest :as all] (for [[x _] args] x)
           times (count all)
           new-structure code]
      (if-not (zero? times)
        (recur vrest
               (dec times)
               (clojure.walk/prewalk (fn [x] (if (= x v)
                                               (get get-trans-keys x)
                                               x))
                                     new-structure))
        new-structure))))

(defn- cleave-mark
  [string]
  (-> (re-find #"(marked-get-|marked-transient-)(.*)" string)
      last
      symbol))


(defn- transient-conj!
  [transient [_ _ & values]]
  `(loop [total# 0]
     (when-not (= total# (count ~(vec values)))
       (conj! ~transient (nth ~(vec values) total#))
       (recur (inc total#)))))

(defn- mark?
  [string-mark]
  (-> (re-find #"(marked-get-|marked-transient-)(.*)" string-mark)
      empty?
      not))

(defn- transient-mark?
  [string-mark]
  (-> (re-find #"(marked-transient-)(.*)" string-mark)
      empty?
      not))

(defn- cleave-marks
  [code]
  (map #(if (string? %)
         (if (mark? %)
           (cleave-mark %)
           %)
         %)
       code))

(defn- any-marks-in-list?
  [code]
  (empty? (filter mark? code)))

(clojure.core/defn- findop
  [code]
  (clojure.walk/prewalk (fn [x] (if (list? x)
                                  (let [[a b c] x]
                                    (clojure.core.match/match [a]
                                                              ['+=] `(.setPlusValue ~(cleave-mark b) ~c)
                                                              ['-=] `(.setMinusValue ~(cleave-mark b) ~c)
                                                              ['*=] `(.setTimesValue ~(cleave-mark b) ~c)
                                                              ['|=] `(.setDividesValue ~(cleave-mark b) ~c)
                                                              ['++] `(.setPlusValue ~(cleave-mark b) 1)
                                                              ['--] `(.setMinusValue ~(cleave-mark b) 1)
                                                              ['conj] (transient-conj! (cleave-mark b) x)
                                                              ['set] `(.setValue ~(cleave-mark b) ~c)
                                                              :else (clojure.core.match/match [x]
                                                                                              [(['assoc  (mark :guard string?) & rest] :seq)] `(~(symbol (str a "!")) ~(cleave-mark b) ~@rest)
                                                                                              [(['pop    (mark :guard string?) & rest] :seq)] `(~(symbol (str a "!")) ~(cleave-mark b) ~@rest)
                                                                                              [(['dissoc (mark :guard string?) & rest] :seq)] `(~(symbol (str a "!")) ~(cleave-mark b) ~@rest)
                                                                                              [(['disj   (mark :guard string?) & rest] :seq)] `(~(symbol (str a "!")) ~(cleave-mark b) ~@rest)
                                                                                              :else       x)))
                                  x))
                        code))

(defn- get-vals-from-singles
  [code]
  (for [x code]
    (if (string? x)
      (if (transient-mark? x)
        `(persistent! ~(cleave-mark x))
        x)
      x)))

(defn- last-pass-clear-marks
  [code]
  (clojure.walk/prewalk (fn [x] (if (string? x)
                                  (cond
                                    (.contains x "marked-get-") `(.getValue ~(cleave-mark x))
                                    (.contains x "marked-transient-") (cleave-mark x)
                                    :else x)
                                  x))
                        code))

(defn vars
  [args code]
  (let [args (make-mutable-args args)
        code (-> (pop-value-out-of-containers args code)
                 findop
                 get-vals-from-singles
                 last-pass-clear-marks)]
    `(let ~(turn-args-into-proper-args-for-let args)
       ~@code)))

(defmacro for-loop
  [[name val loop-cond inc-value] & code]
  (let [[_ _ inc-symbol amount] (re-find #"([^\+\-]+)(\+\=|\+\+|\-\-|\-\=)([0-9]+)?"
                                         (apply str (str inc-value)))
        inc-it? (if-not amount
                  true
                  false)]
    `(vars [~name ~val]
          (while ~loop-cond
            ~@code
            ~(if inc-it?
               (read-string (str `(~(symbol inc-symbol) ~name)))
               (read-string (str `(~(symbol inc-symbol) ~name ~(symbol amount)))))))))

(defn universal-setter-math
  [obj-name operation code]
  (concat '(do)
          (for [[a b] (partition 2 (clojure-source.macro-helpers/format-helper-infix code))]
            `(set! ~(clojure-source.macro-helpers/obj-format obj-name a)
                   (~operation ~(clojure-source.macro-helpers/obj-format obj-name a) ~b)))
          (list obj-name)))


(defmacro uset!
  [obj-name & code]
  (concat '(do)
          (for [[[key] [code]] (->> (partition-by keyword? code)
                                    (partition 2))]
            `(set! ~(clojure-source.macro-helpers/obj-format obj-name key) ~code))
          (list obj-name)))

(defn- uget-setup
  [obj-name fields-to-get]
  (if (> (count fields-to-get) 1)
    (mapv #(clojure-source.macro-helpers/obj-format obj-name %) fields-to-get)
    (clojure-source.macro-helpers/obj-format obj-name (first fields-to-get))))

(defmacro uget
  "takes an object and keys that correspond to that objects fields
  gets all the fields and returns as a vector, if one value given it will only
  return one field without the vector"
  [obj-name & fields-to-get]
  {:pre [(not (empty? fields-to-get))]}
  (uget-setup obj-name fields-to-get))


(defn type-hinted-def
  [name vals]
    (let [str-type (last (re-find #"(new |)([^\(\)]+)" (str vals)))
          type (if (= \. (last str-type))
                 (->> (butlast str-type)
                      (apply str)
                      symbol)
                 (symbol str-type))
          type (vary-meta name assoc :tag type)]
      `(def ~type ~vals)))