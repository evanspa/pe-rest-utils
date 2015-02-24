(ns pe-rest-utils.core
  (:require [liberator.representation :refer [render-map-generic ring-response]]
            [pe-rest-utils.meta :as meta]
            [pe-core-utils.core :as ucore]
            [datomic.api :refer [q db] :as d]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.string :refer [split]]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [environ.core :refer [env]])
  (:import [java.net URL]))

(declare write-res)
(declare put-or-post-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn assoc-err-mask [ring-resp-map ctx ctx-keyword hdr-error-mask]
  (assoc-in ring-resp-map
            [:response :headers hdr-error-mask]
            (str (ctx-keyword ctx))))

(defn content-type
  "Returns a content type string of the form: type/subtype-vXXX;charset=XXX"
  ([mt-type mt-subtype version format-ind charset-name]
   (format "%s/%s-v%s+%s;charset=%s" mt-type mt-subtype version format-ind
           charset-name))
  ([mt-type mt-subtype version]
   (format "%s/%s-v%s" mt-type mt-subtype version)))

(defn make-abs-link-href
  [base-url abs-path]
  (format "%s%s" base-url abs-path))

(defn make-abs-link
  [version rel mt-subtype base-url abs-path]
  [rel {:href (make-abs-link-href base-url abs-path)
        :type (content-type meta/mt-type mt-subtype version)}])

(defn make-abs-entity-link-href
  [base-url entity-uri entid]
  (format "%s%s/%s" base-url entity-uri entid))

(defn make-abs-entity-link
  [version rel mt-subtype base-url entity-uri entid]
  [rel {:href (make-abs-entity-link-href base-url entity-uri entid)
        :type (content-type meta/mt-type mt-subtype version)}])

(defn link-href
  [[_ {href :href}]]
  href)

(defn assoc-link
  [m [rel link-m]]
  (assoc m rel link-m))

(defn enumerate-media-types
  "Returns a seq of HTTP-formated media type strings from the
   'supported-media-types' var."
  [mts]
  (for [type (keys mts)
        subtype (keys (get-in mts [type :subtypes]))
        version (keys (get-in mts [type :subtypes subtype :versions]))
        format-ind (get-in mts [type :subtypes subtype :versions version
                                :format-inds])]
    (format "%s/%s-v%s+%s" type subtype version format-ind)))

(defn parse-media-type
  "Parses a stylized media type returning a map of 5 entries:
     :type
     :subtype
     :bare-mediatype (concatenation of type and subtype with '/' in-between)
     :version
     :format-ind
     :charset"
  [media-type]
  (let [[_ type subtype _ version _ format-ind _ charset-name]
        (first
         (re-seq
          #"(\w*)/([\w.]*)(-v(\d.\d.\d))?(\+(\w*))?(;charset=([\w-]*))?"
          media-type))]
    {:type type :subtype subtype :bare-mediatype (format "%s/%s" type subtype)
     :version version :format-ind format-ind :charset charset-name}))

(defn is-known-content-type?
  "Returns a vector of 2 elements; the first indicates if the content-type
   of the entity of the request is known by this REST API, and the second
   element is the parsed character set of the entity string representation.
   If the content-type is not supported, then the second element will be
   nil."
  [ctx mts charsets]
  (let [parsed-ct (parse-media-type
                   (get-in ctx [:request :headers "content-type"]))
        charset-name (:charset parsed-ct)]
    [(not (nil? (get-in mts [(:type parsed-ct) :subtypes (:subtype parsed-ct)
                             :versions (:version parsed-ct)
                             :format-inds (:format-ind parsed-ct)])))
     {:charset (get charsets charset-name)}]))

(defn merge-links
  [links-map representation]
  (merge representation links-map))

(defn media-type
  [mt-type mt-subtype version format-ind]
  (str mt-type "/" mt-subtype "-v" version "+" format-ind))

(defn parse-auth-header
  [authorization scheme scheme-param-name]
  (when authorization
    (let [auth-str-tokens (split authorization #" ")]
      (when (= (count auth-str-tokens) 2)
        (let [auth-scheme (nth auth-str-tokens 0)]
          (when (= auth-scheme scheme)
            (let [auth-scheme-param-val (nth auth-str-tokens 1)
                  auth-scheme-param-val-str-tokens (split auth-scheme-param-val #"=")]
              (when (= (count auth-scheme-param-val-str-tokens) 2)
                (let [auth-scheme-param-name (nth auth-scheme-param-val-str-tokens 0)]
                  (when (= auth-scheme-param-name scheme-param-name)
                    (let [auth-scheme-param-value (nth auth-scheme-param-val-str-tokens 1)
                          auth-scheme-param-value (if (or (= (nth auth-scheme-param-value 0) (char 34))  ; (char 34) is double-quote char
                                                          (= (nth auth-scheme-param-value 0) (char 39))) ; (char 39) is single-quote char
                                                    (.substring auth-scheme-param-value 1 (dec (count auth-scheme-param-value)))
                                                    auth-scheme-param-value)]
                      [auth-scheme auth-scheme-param-name auth-scheme-param-value])))))))))))

(defn last-modified
  [conn entity-entid known-entity-attr]
  (ffirst (q '[:find ?tx-time
               :in $ ?e ?a
               :where
               [$ ?e ?a _ ?tx]
               [$ ?tx :db/txInstant ?tx-time]]
             (db conn)
             entity-entid
             known-entity-attr)))

(defn known-content-type-predicate [supported-media-types]
  (fn [ctx]
    (let [{{method :request-method} :request} ctx]
      (if (or (= method :put)
              (= method :post))
        (is-known-content-type?
         ctx
         supported-media-types
         meta/char-sets)
        true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource Serializers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti read-res
  (fn [pct content charset]
    {:format-ind (:format-ind pct)}))

(defmulti write-res
  "dispatch on format indicator"
  (fn [res format-ind charset] format-ind))

(defmethod write-res "edn"
  [res _ charset]
  (let [^java.nio.charset.Charset charset charset]
    (io/input-stream (.getBytes (pr-str res) charset))))

(defmethod write-res "json"
  [res _ charset]
  (let [^java.nio.charset.Charset charset charset
        res (ucore/instants->rfc7231str-dates res)
        ^java.lang.String json-str (json/write-str res :key-fn #(ucore/keyword->jsonkey %) :escape-slash false)]
    (io/input-stream (.getBytes json-str charset))))

(defmethod read-res
  {:format-ind "edn"}
  [_ content charset]
  (edn/read-string (slurp content :encoding (.name charset))))

(defmethod read-res
  {:format-ind "json"}
  [_ content charset]
  (let [content-str (slurp content :encoding (.name charset))]
    (-> (json/read-str content-str)
        (ucore/rfc7231str-dates->instants))))

(defn put-or-post-invoker
  [ctx
   method
   conn
   partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url          ; e.g., https://api.example.com:4040
   entity-uri-prefix ; e.g., /fp/
   entity-uri        ; e.g., /fp/users/191491
   embedded-resources-fn
   links-fn
   entids
   validator-fn
   any-issues-bit
   body-data-in-transform-fn
   body-data-out-transform-fn
   existing-entity-fns
   &
   more]
  (let [saveentity-entity-already-exists-bit (nth more 0)
        save-new-entity-txnmap-fn (nth more 1)
        save-entity-txnmap-fn (nth more 2)
        hdr-establish-session (nth more 3)
        make-session-fn (nth more 4)
        post-as-do-fn (nth more 5)
        apptxn-usecase (nth more 6)
        apptxnlog-proc-started-usecase-event (nth more 7)
        apptxnlog-proc-done-success-usecase-event (nth more 8)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 9)
        known-entity-attr (nth more 10)
        record-apptxn-async-fn (nth more 11)
        apptxnlog-txn-fn (nth more 12)
        {{:keys [media-type lang charset]} :representation} ctx
        accept-charset-name charset
        accept-lang lang
        accept-mt media-type
        parsed-accept-mt (parse-media-type accept-mt)
        accept-format-ind (:format-ind parsed-accept-mt)
        content-type (get-in ctx [:request :headers "content-type"])
        content-lang (get-in ctx [:request :headers "content-language"])
        parsed-content-type (parse-media-type content-type)
        content-type-charset-name (:charset parsed-content-type)
        version (:version parsed-content-type)
        body (get-in ctx [:request :body])
        accept-charset (get meta/char-sets accept-charset-name)
        content-type-charset (get meta/char-sets content-type-charset-name)
        body-data (read-res parsed-content-type body content-type-charset)
        body-data (keywordize-keys body-data)]
    (put-or-post-t version
                   body-data
                   accept-format-ind
                   accept-charset
                   accept-lang
                   ctx
                   method
                   conn
                   partition
                   hdr-apptxn-id
                   hdr-useragent-device-make
                   hdr-useragent-device-os
                   hdr-useragent-device-os-version
                   base-url
                   entity-uri-prefix
                   entity-uri
                   embedded-resources-fn
                   links-fn
                   entids
                   validator-fn
                   any-issues-bit
                   body-data-in-transform-fn
                   body-data-out-transform-fn
                   existing-entity-fns
                   saveentity-entity-already-exists-bit
                   save-new-entity-txnmap-fn
                   save-entity-txnmap-fn
                   hdr-establish-session
                   make-session-fn
                   post-as-do-fn
                   apptxn-usecase
                   apptxnlog-proc-started-usecase-event
                   apptxnlog-proc-done-success-usecase-event
                   apptxnlog-proc-done-err-occurred-usecase-event
                   known-entity-attr
                   record-apptxn-async-fn
                   apptxnlog-txn-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn put-or-post-t
  [version
   body-data
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   method
   conn
   partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   entids
   &
   more]
  (let [validator-fn (nth more 0)
        any-issues-bit (nth more 1)
        body-data-in-transform-fn (nth more 2)
        body-data-out-transform-fn (nth more 3)
        existing-entity-fns (nth more 4)
        saveentity-entity-already-exists-bit (nth more 5)
        save-new-entity-txnmap-fn (nth more 6)
        save-entity-txnmap-fn (nth more 7)
        hdr-establish-session (nth more 8)
        make-session-fn (nth more 9)
        post-as-do-fn (nth more 10)
        apptxn-usecase (nth more 11)
        apptxnlog-proc-started-usecase-event (nth more 12)
        apptxnlog-proc-done-success-usecase-event (nth more 13)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 14)
        known-entity-attr (nth more 15)
        apptxn-async-logger-fn (nth more 16)
        make-apptxn-fn (nth more 17)
        validation-mask (if validator-fn (validator-fn version body-data) 0)
        apptxn-maker (partial make-apptxn-fn
                              version
                              ctx
                              conn
                              partition
                              hdr-apptxn-id
                              hdr-useragent-device-make
                              hdr-useragent-device-os
                              hdr-useragent-device-os-version
                              apptxn-usecase)
        async-apptxnlogger (partial apptxn-async-logger-fn
                                    version
                                    ctx
                                    conn
                                    partition
                                    hdr-apptxn-id
                                    hdr-useragent-device-make
                                    hdr-useragent-device-os
                                    hdr-useragent-device-os-version
                                    apptxn-usecase)]
    (try
      (when apptxn-usecase (async-apptxnlogger apptxnlog-proc-started-usecase-event))
      (if (and any-issues-bit (pos? (bit-and validation-mask any-issues-bit)))
        {:unprocessable-entity true
         :error-mask validation-mask}
        (let [transformed-body-data (body-data-in-transform-fn version body-data)
              merge-links-fn (fn [saved-entity saved-entity-entid]
                               (if links-fn
                                 (assoc saved-entity
                                        :_links
                                        (links-fn version
                                                  base-url
                                                  entity-uri-prefix
                                                  entity-uri
                                                  saved-entity-entid))
                                 saved-entity))
              merge-embedded-fn (fn [saved-entity saved-entity-entid]
                                  (if embedded-resources-fn
                                    (assoc saved-entity
                                           :_embedded
                                           (embedded-resources-fn version
                                                                  base-url
                                                                  entity-uri-prefix
                                                                  entity-uri
                                                                  conn
                                                                  accept-format-ind
                                                                  saved-entity-entid))
                                    saved-entity))]
          (letfn [(post-as-create-t [f]
                    (let [does-already-exist (when existing-entity-fns
                                               (reduce (fn [does-already-exist
                                                            [name-extraction-fn
                                                             get-entities-by-name-fn]]
                                                         (or does-already-exist
                                                             (let [name (name-extraction-fn version transformed-body-data)
                                                                   args (flatten (conj []
                                                                                       version
                                                                                       conn
                                                                                       entids
                                                                                       name))
                                                                   entities (apply get-entities-by-name-fn args)]
                                                               (> (count entities) 0))))
                                                       false
                                                       existing-entity-fns))]
                      (if does-already-exist
                        {:entity-already-exists true
                         :error-mask (bit-or saveentity-entity-already-exists-bit
                                             any-issues-bit)}
                        (let [save-new-entity-txnmap-fn-args (flatten (conj []
                                                                            version
                                                                            conn
                                                                            partition
                                                                            entids
                                                                            transformed-body-data))
                              entity-txn-or-txnmap (apply save-new-entity-txnmap-fn save-new-entity-txnmap-fn-args)
                              newentity-tempid (if (vector? entity-txn-or-txnmap)
                                                 (:db/id (first entity-txn-or-txnmap))
                                                 (:db/id entity-txn-or-txnmap))
                              {{{est-session? hdr-establish-session} :headers} :request} ctx
                              [token newauthtoken-txnmap] (when est-session?
                                                            (make-session-fn version
                                                                             partition
                                                                             newentity-tempid
                                                                             nil))
                              apptxnlog-txn (when apptxn-usecase
                                              (apptxn-maker apptxnlog-proc-done-success-usecase-event))
                              txn-maker-fn (fn [apptxnlog-log]
                                             (remove nil?
                                                     (if apptxnlog-log
                                                       (if (vector? entity-txn-or-txnmap)
                                                         (conj (concat apptxnlog-txn entity-txn-or-txnmap) newauthtoken-txnmap)
                                                         (conj apptxnlog-txn entity-txn-or-txnmap newauthtoken-txnmap))
                                                       (if (vector? entity-txn-or-txnmap)
                                                         (conj entity-txn-or-txnmap newauthtoken-txnmap)
                                                         (conj [] entity-txn-or-txnmap newauthtoken-txnmap)))))]
                          (merge (if est-session?
                                   {:auth-token token}
                                   (when (:auth-token ctx)
                                     {:auth-token (:auth-token ctx)}))
                                 (f txn-maker-fn apptxnlog-txn entity-txn-or-txnmap))))))
                  (post-as-create []
                    (post-as-create-t
                     (fn [txn-maker-fn apptxnlog-txn entity-txn-or-txnmap]
                       (let [newentity-tempid (if (vector? entity-txn-or-txnmap)
                                                (:db/id (first entity-txn-or-txnmap))
                                                (:db/id entity-txn-or-txnmap))
                             tx @(d/transact conn (txn-maker-fn apptxnlog-txn))
                             saved-entity-entid (d/resolve-tempid (d/db conn) (:tempids tx) newentity-tempid)
                             entity-txn-time (last-modified conn saved-entity-entid known-entity-attr)
                             entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                             body-data (body-data-out-transform-fn version body-data)
                             saved-entity (merge-links-fn body-data saved-entity-entid)
                             saved-entity (merge-embedded-fn saved-entity saved-entity-entid)]
                         (merge {:status 201
                                 :location (make-abs-entity-link-href base-url entity-uri saved-entity-entid)
                                 :last-modified entity-txn-time-str
                                 :entity (write-res saved-entity accept-format-ind accept-charset)})))))
                  (post-as-create-async []
                    (post-as-create-t
                     (fn [txn-maker-fn apptxnlog-txn entity-txn-or-txnmap]
                       (d/transact conn (txn-maker-fn apptxnlog-txn))
                       {:status 202})))
                  (post-as-do []
                    (let [resp (post-as-do-fn version
                                              conn
                                              partition
                                              base-url
                                              entity-uri-prefix
                                              entity-uri
                                              body-data
                                              async-apptxnlogger
                                              merge-embedded-fn
                                              merge-links-fn)]
                      (merge resp
                             (when-let [body-data (:do-entity resp)]
                               {:entity (write-res (body-data-out-transform-fn version body-data)
                                                   accept-format-ind
                                                   accept-charset)})
                             (when (:auth-token ctx)
                               {:auth-token (:auth-token ctx)}))))
                  (put []
                    (let [save-entity-txnmap-fn-args (flatten (conj []
                                                                    version
                                                                    conn
                                                                    partition
                                                                    entids
                                                                    transformed-body-data))
                          entity-txnmap (apply save-entity-txnmap-fn save-entity-txnmap-fn-args)
                          apptxnlog-txn (when apptxn-usecase
                                          (apptxn-maker apptxnlog-proc-done-success-usecase-event))
                          tx @(d/transact conn (if apptxnlog-txn
                                                 (conj apptxnlog-txn entity-txnmap)
                                                 (conj [] entity-txnmap)))
                          entity-txn-time (last-modified conn (last entids) known-entity-attr)
                          entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                          body-data (body-data-out-transform-fn body-data)
                          saved-entity (merge-links-fn body-data (last entids))
                          saved-entity (merge-embedded-fn saved-entity (last entids))]
                      (merge {:status 200
                              :location entity-uri
                              :last-modified entity-txn-time-str
                              :entity (write-res saved-entity accept-format-ind accept-charset)}
                             (when (:auth-token ctx)
                               {:auth-token (:auth-token ctx)}))))]
            (cond
              (= method :post-as-create) (post-as-create)
              (= method :post-as-create-async) (post-as-create-async)
              (= method :post-as-do) (post-as-do)
              (= method :put) (put)))))
      (catch Exception e
        (log/error e "Exception caught")
        (when apptxn-usecase (async-apptxnlogger apptxnlog-proc-done-err-occurred-usecase-event
                                           nil
                                           (ucore/throwable->str e)))
        {:err e}))))

(defn handle-resp
  [ctx
   hdr-auth-token
   hdr-error-mask]
  (cond
    (:err ctx) (ring-response {:status 500})
    (:unprocessable-entity ctx) (-> (ring-response {:status 422})
                                    (assoc-err-mask ctx :error-mask hdr-error-mask))
    (:entity-already-exists ctx) (-> (ring-response {:status 403})
                                     (assoc-err-mask ctx :error-mask hdr-error-mask))
    :else (ring-response
           (merge
            {}
            (when-let [status (:status ctx)]
              {:status status})
            {:headers
             (merge
              {}
              (when-let [location (:location ctx)]
                {"location" location})
              (when-let [auth-token (:auth-token ctx)]
                {hdr-auth-token auth-token})
              (when (:last-modified ctx)
                {"last-modified" (:last-modified ctx)}))}
            (when (:entity ctx)
              {:body (:entity ctx)})))))
