(ns af.ex
  (:require
   [af.fect :as af]
   [clojure.edn :as edn]))

(defn failure-message [data input output actual]
  (str "Failure in "   (or (:is data) (:as data))
       " with mock inputs " (pr-str input)
       " when expecting "    (pr-str output)
       " but actually got "     (pr-str actual)))

(def mocker
  (af/fect
   {:as :mock
    :void :mock
    :join #(do {:mocks (vec (concat (:mock %1) (:mock %2)))})
    :af (fn [{:as env :keys [mock]}]
          (when mock
            (let [this-af (af/fect (-> env (dissoc :mock) (assoc :as (:is env))))
                  failures (->> mock
                                (partition 2)
                                (mapv (fn [[in out]]
                                        (assert (coll? in))
                                        (let [result (apply this-af in)]
                                          (when (and result (not= result out))
                                            (failure-message env in out result)))))
                                (filter (complement nil?)))]
              (when (seq failures)
                (->> failures (mapv (fn [er] (throw (ex-info (str er) {})))))))))}))

(defn strings->ints [& string-ints]
  (->> string-ints
       (map str)
       (mapv edn/read-string)))

(def +s
  (af/fect
   {:as ::+s :with mocker
    :op +
    :ef (fn [{:keys [args]}]
          {:args (apply strings->ints args)})
    :mock [[1 "2" 3 4 "5" 6] 21]}))

(+s "1" 2)
;=> 3

(defn vecs->ints [& s]
  (->> s
       (reduce (fn [acc arg]
                 (if (vector? arg)
                   (into acc arg)
                   (conj acc arg)))
               [])))

(def +sv
  (+s
   {:as ::+sv
    :ef (fn [{:keys [args]}]
          {:args (apply vecs->ints args)})
    :mock [[1 [2]] 3]}))

(+sv "1" [2] 3 [4 5])
;=> 15
