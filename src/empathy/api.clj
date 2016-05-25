(ns empathy.api
  (:require [clojure.string :refer [join]]
            [clojure.spec :as s]
            [io.aviso.ansi :as a]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]
            [fipp.engine :refer [pprint-document]]))

(defn chain-phrases [phrases]
  (case (count phrases)
    1 (first phrases)
    2 (join " or " phrases)
    (apply str (join ", " (butlast phrases)) ", or " (last phrases))))

(defmulti classify-error
  (fn [pred x]
    pred))

(defmethod classify-error 'keyword?
  [pred x]
  {:type :type-error
   :expected (.getName clojure.lang.Keyword)
   :actual (.getName (.getClass x))
   :summary "a keyword"})

(defmethod classify-error 'integer?
  [pred x]
  {:type :type-error
   :expected (.getName java.lang.Integer)
   :actual (.getName (.getClass x))
   :summary "an integer"})

(defmethod classify-error 'string?
  [pred x]
  {:type :type-error
   :expected (.getName java.lang.String)
   :actual (.getName (.getClass x))
   :summary "a string"})

(defmulti primary-error-msg
  (fn [error-map x]
    (:type error-map)))

(defmethod primary-error-msg :type-error
  [error-map x]
  (format "Value %s must be of type %s (found %s)"
          (pr-str x)
          (:expected error-map)
          (:actual error-map)))

(defmethod primary-error-msg :conditional-violation
  [error-map x]
  (format "Value %s failed a conditional check. It wasn't %s."
          (pr-str x)
          (:summary error-map)))

(defmethod primary-error-msg :missing-required-key
  [error-map x]
  (format "Key %s is required. It was missing." (:missing-key error-map)))

(defn format-error [msg]
  (a/magenta (str "^-- " msg)))

(defn error-message [errors val]
  (if (= (count errors) 1)
    (let [err (first errors)
          et (classify-error (:pred err) val)]
      (format-error (primary-error-msg et val)))
    (->> errors
         (map :pred)
         (map (fn [x] (classify-error x val)))
         (map :summary)
         (chain-phrases)
         ((fn [x] (str "^-- " (pr-str val) " wasn't " x ".")))
         (a/magenta))))

(defn ppd [doc]
  (pprint-document doc {:width 10}))

(defmulti pretty-form
  (fn [x path target err-msg error?]
    (type x)))

(defn maybe-error [x path target err-msg error?]
  (if (= path target)
    [:group
     :span (pretty-form x path target err-msg true)
     :line err-msg]
    (pretty-form x path target err-msg error?)))

(defn maybe-error-kv [k v path target err-msg error?]
  (let [kpath (conj path k)]
    (cond (= kpath target)
          [:span (pretty-form k kpath target err-msg true)
           :line err-msg
           :break
           :span (pretty-form v kpath target err-msg error?)]

          :else
          [:span (pretty-form k kpath target err-msg error?)
           " "
           :span (pretty-form v kpath target err-msg error?)])))

(defmethod pretty-form :default
  [x path target err-msg error?]
  (if error?
    (a/bold-red (pr-str x))
    (pr-str x)))

(defmethod pretty-form clojure.lang.PersistentArrayMap
  [x path target err-msg error?]
  [:group
   "{"
   :break
   (reduce-kv
    (fn [result k v]
      (-> result
          (into (maybe-error-kv k v path target err-msg error?))
          (into [:break])))
    [:nest 4]
    x)
   "}"])

(defn pretty-coll-form [open close x path target err-msg error?]
  [:group
   open
   (reduce
    (fn [result [k i]]
      (into result [:line (maybe-error k (conj path i) target err-msg error?)]))
    [:nest 4]
    (map vector x (range)))
   :break
   close])

(defmethod pretty-form clojure.lang.PersistentVector
  [x path target err-msg error?]
  (pretty-coll-form "[" "]" x path target err-msg error?))

(defmethod pretty-form clojure.lang.PersistentList
  [x path target err-msg error?]
  (pretty-coll-form "(" ")" x path target err-msg error?))

(defmethod pretty-form clojure.lang.LazySeq
  [x path target err-msg error?]
  (pretty-coll-form "[" "]" x path target err-msg error?))

(defmethod pretty-form clojure.lang.PersistentHashSet
  [x path target err-msg error?]
  [:group
   "#{"
   (reduce
    (fn [result k]
      (into result [:line (maybe-error k (conj path k) target err-msg error?)]))
    [:nest 4]
    x)
   :break
   "}"])

(defn pretty-error! [spec x]
  (let [problems (:clojure.spec/problems
                  (s/explain-data spec x))
        grouped (group-by :in (vals problems))
        target (first (keys grouped))
        errors (get grouped target)
        {:keys [val in]} (first errors)]
    (if (= in [])
      (let [es (first (mapcat :pred errors))]
        (fipp x)
        (if (= 'contains? (first es))
          (println
           (format-error
            (primary-error-msg
             {:type :missing-required-key
              :missing-key (last es)}
             val)))
          (do (println "Failed to produce a good exception. clojure.spec data:")
              (fipp problems))))
      (let [err-msg (error-message errors val)]
        (ppd (pretty-form x [] in err-msg false))))))
