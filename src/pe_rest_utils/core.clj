(ns pe-rest-utils.core
  (:require [liberator.representation :refer [render-map-generic ring-response]]
            [pe-rest-utils.meta :as meta]
            [pe-core-utils.core :as ucore]
            [datomic.api :refer [q db] :as d]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]
            [clojure.string :refer [split]]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [environ.core :refer [env]])
  (:import [java.net URL]))

(declare write-res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn assoc-err-mask [ring-resp-map ctx ctx-keyword]
  (assoc-in ring-resp-map
            [:response :headers meta/hdr-error-mask]
            (str (ctx-keyword ctx))))

(defn base-url->url [base-url absolute-path]
  (format "%s%s" base-url absolute-path))

(defn content-type
  "Returns a content type string of the form: type/subtype-vXXX;charset=XXX"
  ([mt-type mt-subtype version format-ind charset-name]
   (format "%s/%s-v%s+%s;charset=%s" mt-type mt-subtype version format-ind
           charset-name))
  ([mt-type mt-subtype version]
   (format "%s/%s-v%s" mt-type mt-subtype version)))

(defn link
  [rel base-url path mt-subtype version]
  [rel {:href (base-url->url base-url path)
        :type (content-type meta/mt-type mt-subtype version)}])

(defn v001-link
  [rel base-url path mt-subtype]
  (link rel base-url path mt-subtype "0.0.1"))

(defn entity-v001-sublink
  [base-url parent-pathcomp parent-entid rel pathcomp mt-subtype]
  (v001-link rel
             base-url
             (format "%s/%s/%s" parent-pathcomp parent-entid pathcomp)
             mt-subtype))

(defn entity-v001-subsublink
  [base-url parent-pathcomp parent-entid entity-entid rel pathcomp mt-subtype]
  (v001-link rel
             base-url
             (format "%s/%s/%s/%s" parent-pathcomp parent-entid pathcomp entity-entid)
             mt-subtype))

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
    {:type type
     :subtype subtype
     :bare-mediatype (format "%s/%s" type subtype)
     :version version
     :format-ind format-ind
     :charset charset-name}))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn handle-post!
  [ctx
   conn
   partition
   embedded-resources-fn
   links-fn
   base-url
   parent-entids
   entity-entid
   apptxn-usecase
   apptxnlog-usecase-event-proc-started
   post!-fn
   record-apptxn-async-fn
   scheme
   scheme-param-name]
  (when apptxn-usecase
    (record-apptxn-async-fn ctx
                            conn
                            partition
                            apptxn-usecase
                            apptxnlog-usecase-event-proc-started))
  (let [{{:keys [media-type lang charset]} :representation} ctx
        accept-charset-name charset
        accept-lang lang
        accept-mt media-type
        parsed-accept-mt (parse-media-type accept-mt)
        accept-format-ind (:format-ind parsed-accept-mt)
        content-type (get-in ctx [:request :headers "content-type"])
        content-lang (get-in ctx [:request :headers "content-language"])
        parsed-content-type (parse-media-type content-type)
        content-type-charset-name (:charset parsed-content-type)]
    (let [[_ _ auth-token] (parse-auth-header (get-in ctx [:request :headers "authorization"])
                                              scheme
                                              scheme-param-name)]

      (post!-fn accept-format-ind
                accept-charset-name
                accept-lang
                content-type-charset-name
                content-lang
                parsed-content-type
                ctx
                conn
                partition
                embedded-resources-fn
                links-fn
                base-url
                auth-token
                parent-entids
                entity-entid))))

(defn handle-put!
  [ctx
   conn
   partition
   embedded-resources-fn
   links-fn
   base-url
   parent-entids
   entity-entid
   apptxn-usecase
   apptxnlog-usecase-event-proc-started
   put!-fn
   record-apptxn-async-fn
   scheme
   scheme-param-name]
  (when apptxn-usecase
    (record-apptxn-async-fn ctx
                            conn
                            partition
                            apptxn-usecase
                            apptxnlog-usecase-event-proc-started))
  (let [{{:keys [media-type lang charset]} :representation} ctx
        accept-charset-name charset
        accept-lang lang
        accept-mt media-type
        parsed-accept-mt (parse-media-type accept-mt)
        accept-format-ind (:format-ind parsed-accept-mt)
        content-type (get-in ctx [:request :headers "content-type"])
        content-lang (get-in ctx [:request :headers "content-language"])
        parsed-content-type (parse-media-type content-type)
        content-type-charset-name (:charset parsed-content-type)]
    (let [[_ _ auth-token] (parse-auth-header (get-in ctx [:request :headers "authorization"])
                                              scheme
                                              scheme-param-name)]
      (put!-fn accept-format-ind
               accept-charset-name
               accept-lang
               content-type-charset-name
               content-lang
               parsed-content-type
               ctx
               conn
               partition
               embedded-resources-fn
               links-fn
               base-url
               auth-token
               parent-entids
               entity-entid))))

(defn post!-t
  [version
   body-data
   accept-format-ind
   accept-charset
   accept-lang
   parsed-content-type
   ctx
   conn
   partition
   embedded-resources-fn
   links-fn
   base-url
   auth-token
   parent-entids
   entity-entid
   validator-fn
   any-issues-bit
   body-data-in-transform-fn
   body-data-out-transform-fn
   name-extractor-fn
   &
   more]
  (let [existing-entities-by-name-fn (nth more 0)
        saveentity-entity-already-exists-bit (nth more 0)
        save-new-entity-txnmap-fn (nth more 2)
        apptxn-usecase (nth more 3)
        apptxnlog-proc-done-success-usecase-event (nth more 4)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 5)
        known-entity-attr (nth more 6)
        location-fn (nth more 7)
        record-apptxn-async-fn (nth more 8)
        apptxnlog-txn-fn (nth more 9)
        validation-mask (if validator-fn (validator-fn body-data) 0)
        apptxnlogger (partial record-apptxn-async-fn ctx conn partition apptxn-usecase)]
    (try
      (if (and any-issues-bit (pos? (bit-and validation-mask any-issues-bit)))
        {:unprocessable-entity true
         :error-mask validation-mask}
        (let [transformed-body-data (body-data-in-transform-fn body-data)
              name (when name-extractor-fn (name-extractor-fn transformed-body-data))]
          (let [existing-entities (when existing-entities-by-name-fn (existing-entities-by-name-fn conn entity-entid name))
                content-type-partial #(content-type meta/mt-type % version)]
            (if (> (count existing-entities) 0)
              {:entity-already-exists true
               :error-mask (bit-or saveentity-entity-already-exists-bit
                                      any-issues-bit)}
              (let [entity-txn-or-txnmap (save-new-entity-txnmap-fn conn partition entity-entid transformed-body-data)]
                (if (vector? entity-txn-or-txnmap)
                  (let [entity-txn entity-txn-or-txnmap]  ; admin or similar case (e.g., saving app-txn set)
                    @(d/transact conn entity-txn)
                    {})
                  (let [entity-txnmap entity-txn-or-txnmap
                        newentity-tempid (:db/id entity-txn-or-txnmap) ; usual case
                        apptxnlog-txn (when apptxn-usecase
                                        (apptxnlog-txn-fn ctx
                                                          conn
                                                          partition
                                                          apptxn-usecase
                                                          apptxnlog-proc-done-success-usecase-event))
                        tx @(d/transact conn (if apptxnlog-txn
                                               (conj apptxnlog-txn entity-txnmap)
                                               (conj [] entity-txnmap)))
                        saved-entity-entid (d/resolve-tempid (d/db conn) (:tempids tx) newentity-tempid)
                        entity-txn-time (last-modified conn saved-entity-entid known-entity-attr)
                        entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                        body-data (body-data-out-transform-fn body-data)
                        links-fn-args (conj base-url
                                            version
                                            parent-entids
                                            entity-entid
                                            saved-entity-entid)
                        saved-entity (if links-fn
                                       (assoc body-data
                                              :_links
                                              (apply links-fn links-fn-args))
                                       body-data)
                        saved-entity (if embedded-resources-fn
                                       (let [embedded-resources-fn-args (conj conn
                                                                              version
                                                                              accept-format-ind
                                                                              parent-entids
                                                                              entity-entid
                                                                              saved-entity-entid)]
                                         (assoc saved-entity
                                                :_embedded
                                                (apply embedded-resources-fn embedded-resources-fn-args)))
                                       saved-entity)]
                    (merge (when location-fn {:location (link-href (apply location-fn links-fn-args))})
                           {:last-modified entity-txn-time-str
                            :saved-entity (write-res saved-entity accept-format-ind accept-charset)
                            :auth-token auth-token}))))))))
      (catch Exception e
        (log/error e "Exception caught")
        (apptxnlogger apptxnlog-proc-done-err-occurred-usecase-event
                      nil
                      (ucore/throwable->str e))
        {:err e}))))

(defn put-t
  [version
   body-data
   accept-format-ind
   accept-charset
   accept-lang
   parsed-content-type
   ctx
   conn
   partition
   embedded-resources-fn
   links-fn
   base-url
   auth-token
   parent-entids
   entity-entid
   validator-fn
   any-issues-bit
   body-data-in-transform-fn
   body-data-out-transform-fn
   name-extractor-fn
   &
   more]
  (let [save-entity-txnmap-fn (nth more 0)
        apptxn-usecase (nth more 1)
        apptxnlog-proc-done-success-usecase-event (nth more 2)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 3)
        known-entity-attr (nth more 4)
        assoc-links-fn (nth more 5)
        location-fn (nth more 6)
        record-apptxn-async-fn (nth more 7)
        apptxnlog-txn-fn (nth more 8)
        validation-mask (if validator-fn (validator-fn body-data) 0)
        apptxnlogger (partial record-apptxn-async-fn ctx conn partition apptxn-usecase)]
    (try
      (if (pos? (bit-and validation-mask any-issues-bit))
        {:unprocessable-entity true
         :error-mask validation-mask}
        (let [transformed-body-data (body-data-in-transform-fn body-data)
              name (when name-extractor-fn (name-extractor-fn transformed-body-data))]
          (let [content-type-partial #(content-type meta/mt-type % version)]
            (let [entity-txnmap (save-entity-txnmap-fn entity-entid entity-entid transformed-body-data)
                  apptxnlog-txn (apptxnlog-txn-fn ctx
                                                  conn
                                                  partition
                                                  apptxn-usecase
                                                  apptxnlog-proc-done-success-usecase-event)
                  tx @(d/transact conn (conj apptxnlog-txn entity-txnmap))
                  entity-txn-time (last-modified conn entity-entid known-entity-attr)
                  entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                  body-data (body-data-out-transform-fn body-data)
                  links-fn-args (conj base-url version parent-entids entity-entid)
                  saved-entity (if links-fn
                                 (assoc body-data
                                        :_links
                                        (apply links-fn links-fn-args))
                                 body-data)
                  saved-entity (if embedded-resources-fn
                                 (let [embedded-resources-fn-args (conj conn
                                                                        version
                                                                        accept-format-ind
                                                                        parent-entids
                                                                        entity-entid)]
                                   (assoc saved-entity
                                          :_embedded
                                          (apply embedded-resources-fn embedded-resources-fn-args)))
                                 saved-entity)]
              (merge (when location-fn {:location (link-href (apply location-fn links-fn-args))})
                     {:last-modified entity-txn-time-str
                      :saved-entity (write-res saved-entity accept-format-ind accept-charset)
                      :auth-token auth-token})))))
      (catch Exception e
        (log/error e "Exception caught")
        (apptxnlogger apptxnlog-proc-done-err-occurred-usecase-event
                      nil
                      (ucore/throwable->str e))
        {:err e}))))

(defn handle-resp
  [ctx]
  (cond
    (:err ctx) (ring-response {:status 500})
    (:unprocessable-entity ctx) (-> (ring-response {:status 422})
                                    (assoc-err-mask ctx :error-mask))
    (:entity-already-exists ctx) (-> (ring-response {:status 403})
                                     (assoc-err-mask ctx :error-mask))
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
                {meta/hdr-auth-token auth-token})
              (when (:last-modified ctx)
                {"last-modified" (:last-modified ctx)})
              (when (:auth-token ctx)
                {meta/hdr-auth-token (:auth-token ctx)}))}
            (when (:saved-entity ctx)
              {:body (:saved-entity ctx)})))))
