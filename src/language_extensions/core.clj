(ns language-extensions.core
  (:require [clojure.walk]
            [clojure.core.match]
            [clojure.pprint]
            [clojure.reflect]
            [language-extensions.try-wrap-debugging]
            [language-extensions.types]
            [language-extensions.imperative-oop]
            [language-extensions.infix]))

;; by David Edgar Liebke http://incanter.org
;; March 11, 2009

;; Copyright (c) David Edgar Liebke, 2009. All rights reserved. The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

;; CHANGE LOG
;; March 11, 2009: First version

(comment (if (find-ns 'com.badlogic.gdx.math)
           (import '(com.badlogic.gdx.math MathUtils))))

(defmacro match
  [& code]
  `(clojure.core.match/match ~@code))

(defmacro def-method
  ([name args code]
   `(language-extensions.imperative-oop/def-method ~name ~args ~code))
  ([name doc args code]
   `(language-extensions.imperative-oop/def-method ~name ~doc ~args ~code)))




(clojure.core/defn- make-args-proper
  [args]
  (clojure.walk/prewalk (fn [x] (if-not (or (= (type x) clojure.lang.Cons)
                                            (= (type x) clojure.lang.ArraySeq))
                                  (if (re-find #"\." (str x))
                                    (-> (if (re-find #"\/\/" (str x))
                                          "/"
                                          (take-while #(not= \/ %) (-> (str x)
                                                                       reverse)))
                                        reverse
                                        (->> (apply str))
                                        symbol)
                                    x)
                                  x))
                        args))



(defmacro i
  "Make sure everything in i is written as a normal math expression!"
  [& equation]
  ;(let [corrected (make-args-proper equation)]
    (language-extensions.infix/infix-to-prefix equation))


;allow thread safe imperative constructs!

(defmacro vars
  [args & code]
  `(language-extensions.imperative-oop/vars ~args ~@code))


(clojure.core/defn- format-helper
  [code]
  (loop [[[a] b & rest :as code] (partition-by keyword? code)
         updated-code []]
    (if (seq code)
      (recur rest (conj updated-code a (cons 'i b)))
      updated-code)))

(clojure.core/defn- apply-obj-syms
  [obj sym code]
  (loop [[a b & rest :as code] (format-helper code)
         updated-code []]
    (if (seq code)
      (recur rest (conj updated-code a (list sym (list a obj) b)))
      updated-code)))

(clojure.core/defn- apply-assoc-to-code
  [obj sym code]
  (-> (apply-obj-syms obj sym code)
      (->> (cons obj)
           (cons 'assoc))))

(defmacro +=
  [object & code]
  (let [code (apply-assoc-to-code object '+ code)]
    code))

(defmacro -=
  [object & code]
  (let [code (apply-assoc-to-code object '- code)]
    code))

(defmacro *=
  [object & code]
  (let [code (apply-assoc-to-code object '* code)]
    code))

(defmacro |=
  [object & code]
  (let [code (apply-assoc-to-code object (symbol "clojure.core//") code)]
    code))

(clojure.core/defn get-methods
  [obj]
  (clojure.pprint/pprint
    (for [{return-type :return-type name :name} ((clojure.reflect/reflect obj) :members) :when return-type]
      name)))

(clojure.core/defn get-methods-extra
  [obj]
  (clojure.pprint/pprint
    (for [{return-type :return-type name :name parameter-types :parameter-types {:keys [static]} :flags} ((clojure.reflect/reflect obj) :members) :when return-type]
      {:name name :args parameter-types :return-type return-type :public-or-static (if static "static" "public")})))




(clojure.core/defn- format-code
  [[first-code & rest-code]]
  (match [first-code (vec rest-code)]
         [(first-code :guard string?) [(args :guard vector?) & rest]] {:args args :doc first-code :code rest}
         [(first-code :guard string?) [(args :guard list?) & rest]]   {:args nil :doc first-code :code (cons args rest)}
         [(first-code :guard list?) _]                                {:args nil :doc "nil" :code (cons first-code rest-code)}
         [(first-code :guard vector?) _]                              {:args first-code :doc "nil" :code rest-code}))



(defmacro defn-try
  [name & code]
  (let [{:keys [args doc code]} (format-code code)
        code (if args
               (language-extensions.try-wrap-debugging/wrap-tries name (list (cons args code)))
               (language-extensions.try-wrap-debugging/wrap-tries name code))]
    (reset! language-extensions.try-wrap-debugging/can-i-print? true)
    `(clojure.core/defn ~name ~doc ~@code)))

(defmacro for-loop
  [[a b c d] & code]
   `(language-extensions.imperative-oop/for-loop [~a ~b ~c ~d] ~@code))



(defn universal-setter
  [obj-name operation code]
  (let [type (->> (eval obj-name)
                  type
                  str
                  (drop-while #(not= % \space))
                  rest
                  (apply str)
                  symbol)
        type-hinted (vary-meta obj-name assoc :tag type)]
    (conj (for [[a b] (partition 2 (format-helper code))
                :let [obj-format (->> (re-find #"[A-Za-z]+" (str a))
                                      (str ".")
                                      symbol
                                      (list type-hinted)
                                      reverse)]]
            `(set! ~obj-format (~operation ~obj-format ~b)))
          'do)))

(defmacro +=!
  [obj-name & code]
  (universal-setter obj-name '+ code))
(defmacro -=!
  [obj-name & code]
  (universal-setter obj-name '- code))
(defmacro *=!
  [obj-name & code]
  (universal-setter obj-name '* code))
(defmacro |=!
  [obj-name & code]
  (universal-setter obj-name '/ code))


