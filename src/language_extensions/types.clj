(ns language-extensions.types)

(defn type=?
  [x val]
  (= (type x) val))

(defn boolean?
  [val]
  (= (type val) java.lang.Boolean))

(defn atom?
  [val]
  (= (type val) clojure.lang.Atom))

(defn ref?
  [val]
  (= (type val) clojure.lang.Ref))

(defn agent?
  [val]
  (= (type val) clojure.lang.Agent))


(defn lazy-seq?
  [x]
  (type=? x clojure.lang.LazySeq))
