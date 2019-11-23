(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as html]))

(defn extractor
  [fn-map]
  (let [f (->> fn-map
               (map (fn [[k v]] (fn [m] [k (v m)])))
               (apply juxt))]
    (fn [m]
      (->> m
           (f)
           (into {})))))

((extractor {:test (comp second :a)}) {:a [1 2]})

((comp second :a) {:a [1 2]})

(defn inner-texts
  [x]
  (cond
    (string? x) x
    (= :br (:tag x)) "\n"
    (map? x) (inner-texts (:content x))
    (seq? x) (map inner-texts x)))

(def inner-text (comp (partial apply str) flatten inner-texts))

(defn empty-nil [x] (if (= x "") nil x))

;; https://stackoverflow.com/questions/10062967/clojures-equivalent-to-pythons-encodehex-and-decodehex/15627016#15627016
(defn hexify "Convert byte sequence to hex string" [coll]
  (let [hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]]
    (letfn [(hexify-byte [b]
              (let [v (bit-and b 0xFF)]
                [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
      (apply str (mapcat hexify-byte coll)))))

(def digest (java.security.MessageDigest/getInstance "SHA-256"))
(def b64-decoder (java.util.Base64/getDecoder))

(defn extract-attachments
  [segments]
  (->> segments
       (mapcat :content)                ; uls
       (mapcat :content)                ; lis
       (mapcat :content)                ; actual attachments
       (map (fn [node]
              (let [{:keys [tag]} node
                    embed (cond-> node
                            (= tag :img) (-> :attrs :src)
                            (= tag :a) (-> :attrs :href))
                    [_ ext data] (re-find #"data:[^/]+/([^;]+);base64,(.+)" embed)
                    buf (.decode b64-decoder data)
                    hash (hexify (.digest digest buf))]
                {:ext ext :data buf :hash hash})))))

(def attachment-path "./attachments/")

(defn save-attachment
  "Returns path"
  [{:keys [ext hash data] :as _attachment}]
  (let [path (str attachment-path hash "." ext)
        file (io/file path)]
    (io/make-parents file)
    (with-open [w (io/output-stream file)]
      (.write w data))
    path))

(defn format-note
  [{:keys [title date content attachments]}]
  (format "** TODO %s\nDate: %s\n%s\n\n%s"
          title
          date
          (string/join "\n" (map #(format "[[%s]]" %) attachments))
          content))

(defn -main
  [& args]

  (->> "./Takeout/Keep/"
       io/file
       file-seq
       (filter #(not (.isDirectory %)))
       (filter #(string/ends-with? (.getName %) "html"))

       ;; (map #(do (prn %) %))

       (map html/html-resource)
       ;; (map #(html/select % [:head :title]))
       (map (extractor {:title #(inner-text (html/select % [:head :title]))
                        ;; :title #(empty-nil (inner-text (html/select % [:div.title])))
                        :date #(string/trim (inner-text (html/select % [:.note :.heading])))
                        :content #(string/trim (inner-text (html/select % [:.content])))
                        :attachments #(let [as (html/select % [:.attachments])]
                                        (if (= as '()) nil (extract-attachments as)))}))

       (map #(update % :attachments (fn [as] (map save-attachment as))))

       (map format-note)
       (run! println))
  ;;
  )
