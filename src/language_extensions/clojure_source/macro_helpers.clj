(ns clojure-source.macro-helpers)

(defn attach-type-information-to-symbol
  "Takes a symbol, determines its type,
   then attaches that type as a type hint to the symbol itself,
   this will avoid reflection!"
  [sym]
  (vary-meta sym assoc :tag (->> (eval sym)
                                 type
                                 str
                                 (drop-while #(not= % \space))
                                 rest
                                 (apply str)
                                 symbol)))

(defn obj-format
  "Takes a keyword and an object and turns it into a java call
  ex. :x = .x
  After doing so it makes tha call on the object with a type hint
  OBJECT MUST BE BOUND TO A SYMBOL, CANNOT WORK WITH OBJECTS CREATED ON THE FLY"
  [obj keyw]
  (let [type (attach-type-information-to-symbol obj)]
    (->> (re-find #"[A-Za-z]+" (str keyw))
         (str ".")
         symbol
         (list type)
         reverse)))


(clojure.core/defn format-helper-infix
  "Takes code in the form of '(:key code & :keys codes) and wraps the right side with the i macro
  returning a vector
  ex: [:key (i code)]"
  [code]
  (loop [[[a] b & rest :as code] (partition-by keyword? code)
         updated-code []]
    (if (seq code)
      (recur rest (conj updated-code a (cons 'i b)))
      updated-code)))

(clojure.core/defn apply-ops-to-map-value
  "Takes a map's symbol an operator ex: + and code"
  [map-sym op code]
  (loop [[a b & rest :as code] (format-helper-infix code)
         updated-code []]
    (if (seq code)
      (recur rest (conj updated-code a (list op (list a map-sym) b)))
      updated-code)))

(clojure.core/defn apply-assoc-to-code
  [obj sym code]
  (-> (apply-ops-to-map-value obj sym code)
      (->> (cons obj)
           (cons 'assoc))))
