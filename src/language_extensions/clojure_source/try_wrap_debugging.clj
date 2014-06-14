(ns clojure-source.try-wrap-debugging
  (:require [clojure.core.match])
  (:use [clojure-source.types]))

(def can-i-print? (atom true))

(defn slow-println
  [& args]
      (when @can-i-print?
        (reset! can-i-print? false)
        (apply println args)))

(clojure.core/defn- wrap-try-format
  [name x]
  (read-string (str "(try "
                    x
                    "(catch Exception e (language-extensions.try-wrap-debugging/slow-println \"\nError occured at:\""
                    name
                    "\"\nThe error was:\" (.getMessage e))))")))

;create more of these if needed



;new area of coding

;add more words as you see fit!

(clojure.core/defn unusable-word?
  [word]
  (-> (filter (fn [w] (= w word)) '[fn* proxy loop* loop doto recur set! -> ->> i])
      empty?
      not))

(clojure.core/defn pop-tries
  [code]
  (clojure.walk/postwalk (fn [code]
                           (if (list? code)
                             (if (= 'try (first code))
                               (second code)
                               code)
                             code))
                         code))

(defn sift-code
  [code]
  (clojure.walk/postwalk (fn [x] (if (list? x)
                                   (if (unusable-word? (first x))
                                     (pop-tries x)
                                     x)
                                   x))
                         code))

(defn variable-arity?
  [[x]]
  (vector? (first x)))



(defn sanatize-macro-expansion
  [code]
  (clojure.walk/postwalk (fn [x]
                           (if (isa? x clojure.lang.ISeq)
                             (apply list x)
                             x))
                         code))


(clojure.core/defn wrap-tries [name code]
  (let [transfer-code (comp read-string str)
        debug (fn [x] (println (type x)) x)
        wrap-it-with-tries (fn [code] (-> (clojure.walk/postwalk (fn [x]
                                                                   (if (list? x)
                                                                     (wrap-try-format name x)
                                                                     x))
                                                                 code)
                                          (->> (into '())
                                               reverse)))]
    (-> (clojure.walk/macroexpand-all code)
        sanatize-macro-expansion
        (->> (map (fn [x] (wrap-it-with-tries (vec x))))
             sift-code))))




