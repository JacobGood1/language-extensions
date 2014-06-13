 (ns language-extensions.infix)

;; operator precedence for formula macro
(def +precedence-table+ (atom {}))

;; symbol translation for symbols in formula (only supports binary operators)
(def +translation-table+ (atom {}))

(def +highest-precedence+ (atom 0))

(clojure.core/defn defop
  "Define operators for formula macro"
  ([op prec & [trans]]
   (swap! +precedence-table+ assoc op prec)
   (when-not (nil? trans)
     (swap! +translation-table+ assoc op trans))
   (reset! +highest-precedence+ (reduce max (map val @+precedence-table+)))))



(defmacro div
  [args]
  `(/ ~@args))

;; == operators ==
(defop '|| 10 'or)
(defop '&& 20 'and)
(defop '== 30 '=)
(defop '!= 30 'not=)
(defop '< 40)
(defop '> 40)
(defop '<= 40)
(defop '>= 40)
(defop 'mod 90 'rem)
(defop '- 60 '-)
(defop 'clojure.core/- 60 '-)
(defop '+ 60 '+)
;(defop 'clojure.core/+ 60 '+)
(defop '/ 80 '/)
(defop (symbol "clojure.core//") 80 '/)
(defop '* 80 '*)
(defop 'clojure.core/* 80 '*)
(defop '** 100 'Math/pow)
(defop 'math-infix.core/** 100 'Math/pow)


(clojure.core/defn- operator?
  "Check if is valid operator"
  ([sym]
   (not (nil? (get @+precedence-table+ sym)))))

(clojure.core/defn- find-lowest-precedence
  "find the operator with lowest precedence; search from left to right"
  ([col]
   ;; loop through terms in the coluence
   (loop [idx 0
          col col
          lowest-idx nil
          lowest-prec @+highest-precedence+]
     ;; nothing left to process
     (if (empty? col)
       ;; return lowest found
       lowest-idx
       ;; otherwise check if current term is lower
       (let [prec (get @+precedence-table+ (first col))]
         ;; is of lower or equal precedence
         (if (and prec (<= prec lowest-prec))
           (recur (inc idx) (rest col)
                  idx prec)
           ;; is of high precedence therefore skip for now
           (recur (inc idx) (rest col)
                  lowest-idx lowest-prec)))))))

(clojure.core/defn- translate-op
  "Translation of symbol => symbol for binary op allows for
user defined operators"
  ([op]
   (get @+translation-table+ op op)))

(clojure.core/defn infix-to-prefix
  "Convert from infix notation to prefix notation"
  ([col]
   (cond
     ;; handle term only
     (not (seq? col)) col
     ;; handle sequence containing one term (i.e. handle parens)
     (= (count col) 1) (infix-to-prefix (first col))
     ;; handle all other cases
     true (let [lowest (find-lowest-precedence col)]
            (if (nil? lowest) ;; nothing to split
              col
                              ;; (a b c) bind a to hd, c to tl, and b to op
              (let [[hd [op & tl]] (split-at lowest col)]
                ;; recurse
                (list (translate-op op)
                      (infix-to-prefix hd)
                      (infix-to-prefix tl))))))))
