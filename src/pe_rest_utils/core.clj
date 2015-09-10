(ns pe-rest-utils.core
  "A set of functions to simplify the development of (hypermedia) REST services
  on top of Liberator."
  (:require [liberator.representation :refer [render-map-generic ring-response]]
            [pe-rest-utils.meta :as meta]
            [pe-core-utils.core :as ucore]
            [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clojure.java.jdbc :as j]
            [pe-jdbc-utils.core :as jcore]
            [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.string :refer [split]]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [environ.core :refer [env]])
  (:import [java.net URL]))

(def ^:dynamic *retry-after* nil)

(declare write-res)
(declare any-t-if-not-busy)
(declare put-or-post-t)
(declare get-t)
(declare delete-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn assoc-err-mask
  "Associates hdr-error-mask as a response header in the Ring response map,
  ring-resp-map.  The value used is entry found in ctx keyed on ctx-keyword."
  [ring-resp-map ctx ctx-keyword hdr-error-mask]
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
  "Returns an absolute URL string from base-url and abs-path."
  [base-url abs-path]
  (format "%s%s" base-url abs-path))

(defn make-abs-link
  "Returns a vector where the first element is rel, and the second element is
map with keys :href and :type.  The value at :href is the absolute URL
constructed from base-url and abs-path, and :type is a media type string
constructed from pe-rest-utils.meta/mt-type and mt-subtype."
  [version rel mt-subtype base-url abs-path]
  [rel {:href (make-abs-link-href base-url abs-path)
        :type (content-type meta/mt-type mt-subtype version)}])

(defn link-href
  "Returns the absolute URL string part of a link vector (as returned by
  make-abs-link)."
  [[_ {href :href}]]
  href)

(defn assoc-link
  "Associates the link vector (as returned by make-abs-link) with m keyed on the
  relation of the link vector (its first element)."
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

(defn media-type
  "Returns a media type string constructed from the given arguments."
  [mt-type mt-subtype version format-ind]
  (str mt-type "/" mt-subtype "-v" version "+" format-ind))

(defn parse-auth-header
  "Parses an HTTP 'Authorization' header returning a vector containing the
  parsed authorization scheme , scheme parameter name and parameter value."
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

(defn known-content-type-predicate
  "Returns a function suitable to be used for the :known-content-type? slot of a
  Liberator resource."
  [supported-media-types]
  (fn [ctx]
    (let [{{method :request-method} :request} ctx]
      (if (or (= method :put)
              (= method :post))
        (is-known-content-type?
         ctx
         supported-media-types
         meta/char-sets)
        true))))

(defn entity-id-from-uri
  [uri-str]
  (Long/parseLong (.substring uri-str (inc (.lastIndexOf uri-str "/")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource Serializers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti read-res
  "Returns an object by parsing content based on pct (parsed content type) and
  charset."
  (fn [pct content charset]
    {:format-ind (:format-ind pct)}))

(defmulti write-res
  "Returns a string reprsentation of res based on format-ind (format indicator)
  and charset."
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
  "Convenience function for handling HTTP PUT or POST methods.  Within the
  context of POST, 3 'types' of POST are supported: (1) POST-as-create, (2)
  POST-as-create Async, (3) POST-as-do.  The 'POST-as-create' types are your
  typically resource-creation types.  The former would return a 201 in a success
  scenario, the latter would return a 202.  The 3rd type, POST-as-do, is for
  those non-creation use cases that don't reasonably fit well within a different
  HTTP method.  You might use 'POST-as-do' for performing a login action."
  [ctx
   method
   db-spec
   base-url          ; e.g., https://api.example.com:4040
   entity-uri-prefix ; e.g., /fp/
   entity-uri        ; e.g., /fp/users/191491
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   validator-fn
   any-issues-bit
   body-data-in-transform-fn
   body-data-out-transform-fn
   next-entity-id-fn
   save-new-entity-fn
   save-entity-fn
   hdr-establish-session
   &
   more]
  (let [make-session-fn (nth more 0)
        post-as-do-fn (nth more 1)
        if-unmodified-since-hdr (nth more 2)
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
                   db-spec
                   base-url
                   entity-uri-prefix
                   entity-uri
                   embedded-resources-fn
                   links-fn
                   entids
                   plaintext-auth-token
                   validator-fn
                   any-issues-bit
                   body-data-in-transform-fn
                   body-data-out-transform-fn
                   next-entity-id-fn
                   save-new-entity-fn
                   save-entity-fn
                   hdr-establish-session
                   make-session-fn
                   post-as-do-fn
                   if-unmodified-since-hdr)))

(defn delete-invoker
  "Convenience function for processing HTTP DELETE calls."
  [ctx
   db-spec
   base-url          ; e.g., https://api.example.com:4040
   entity-uri-prefix ; e.g., /fp/
   entity-uri        ; e.g., /fp/users/191491
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   body-data-out-transform-fn
   delete-entity-fn
   delete-reason-hdr
   if-unmodified-since-hdr]
  (let [{{:keys [media-type lang charset]} :representation} ctx
        accept-charset-name charset
        accept-lang lang
        accept-mt media-type
        parsed-accept-mt (parse-media-type accept-mt)
        version (:version parsed-accept-mt)
        accept-format-ind (:format-ind parsed-accept-mt)
        accept-charset (get meta/char-sets accept-charset-name)]
    (delete-t version
              accept-format-ind
              accept-charset
              accept-lang
              ctx
              db-spec
              base-url
              entity-uri-prefix
              entity-uri
              embedded-resources-fn
              links-fn
              entids
              plaintext-auth-token
              body-data-out-transform-fn
              delete-entity-fn
              delete-reason-hdr
              if-unmodified-since-hdr)))

(defn delete-t
  "Convenience function for handling HTTP DELETE calls."
  [version
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   db-spec
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   body-data-out-transform-fn
   delete-entity-fn
   delete-reason-hdr
   if-unmodified-since-hdr]
  (any-t-if-not-busy
   #(try
      (let [merge-links-fn (fn [saved-entity saved-entity-entid]
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
                                                                db-spec
                                                                accept-format-ind
                                                                saved-entity-entid))
                                  saved-entity))]
        (j/with-db-transaction [conn db-spec]
          (let [if-unmodified-since-epoch-str (get-in ctx [:request :headers if-unmodified-since-hdr])
                delete-reason-str (get-in ctx [:request :headers delete-reason-hdr])
                delete-reason (when delete-reason-str (Long/parseLong delete-reason-str))
                if-unmodified-since-val (when if-unmodified-since-epoch-str (c/from-long (Long/parseLong if-unmodified-since-epoch-str)))
                delete-entity-fn-args (flatten (conj []
                                                     version
                                                     conn
                                                     entids
                                                     delete-reason
                                                     plaintext-auth-token
                                                     if-unmodified-since-val))]
            (letfn [(write-resp-entity [entity]
                      (let [body-data-out-transform-fn-args (flatten (conj []
                                                                           version
                                                                           conn
                                                                           entids
                                                                           base-url
                                                                           entity-uri-prefix
                                                                           entity-uri
                                                                           entity))
                            transformed-saved-entity (apply body-data-out-transform-fn body-data-out-transform-fn-args)
                            transformed-saved-entity (merge-links-fn transformed-saved-entity
                                                                     (last entids))]
                        (-> transformed-saved-entity
                            (merge-embedded-fn (last entids))
                            (write-res accept-format-ind accept-charset))))]
              (try
                (apply delete-entity-fn delete-entity-fn-args)
                (merge {:status 204}
                       (when (:auth-token ctx)
                         {:auth-token (:auth-token ctx)}))
                (catch clojure.lang.ExceptionInfo e
                  (let [cause (-> e ex-data :cause)
                        latest-entity (-> e ex-data :latest-entity)]
                    (merge {cause cause}
                           (when latest-entity
                             {:latest-entity (write-resp-entity latest-entity)})))))))))
      (catch Exception e
        (log/error e "Exception caught")
        {:err e}))
   (fn [] {})))

(defn get-invoker
  "Convenience function for handling HTTP GET requests."
  [ctx
   db-spec
   base-url          ; e.g., https://api.example.com:4040
   entity-uri-prefix ; e.g., /fp/
   entity-uri        ; e.g., /fp/users/191491
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   body-data-out-transform-fn
   fetch-fn
   if-modified-since-hdr
   updated-at-keyword
   resp-gen-fn]
  (let [{{:keys [media-type lang charset]} :representation} ctx
        accept-charset-name charset
        accept-lang lang
        accept-mt media-type
        parsed-accept-mt (parse-media-type accept-mt)
        version (:version parsed-accept-mt)
        accept-format-ind (:format-ind parsed-accept-mt)
        accept-charset (get meta/char-sets accept-charset-name)]
    (get-t version
           accept-format-ind
           accept-charset
           accept-lang
           ctx
           db-spec
           base-url
           entity-uri-prefix
           entity-uri
           embedded-resources-fn
           links-fn
           entids
           plaintext-auth-token
           body-data-out-transform-fn
           fetch-fn
           if-modified-since-hdr
           updated-at-keyword
           resp-gen-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn put-or-post-t
  "Convenience function for handling HTTP PUT or POST methods.  Within the
  context of POST, 3 'types' of POST are supported: (1) POST-as-create, (2)
  POST-as-create Async, (3) POST-as-do.  The 'POST-as-create' types are your
  typically resource-creation types.  The former would return a 201 in a success
  scenario, the latter would return a 202.  The 3rd type, POST-as-do, is for
  those non-creation use cases that don't reasonably fit well within a different
  HTTP method.  You might use 'POST-as-do' for performing a login action."
  [version
   body-data
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   method
   db-spec
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   validator-fn
   any-issues-bit
   body-data-in-transform-fn
   body-data-out-transform-fn
   &
   more]
  (let [next-entity-id-fn (nth more 0)
        save-new-entity-fn (nth more 1)
        save-entity-fn (nth more 2)
        hdr-establish-session (nth more 3)
        make-session-fn (nth more 4)
        post-as-do-fn (nth more 5)
        if-unmodified-since-hdr (nth more 6)
        validation-mask (if validator-fn (validator-fn version body-data) 0)]
    (any-t-if-not-busy
     #(try
        (if (and any-issues-bit (pos? (bit-and validation-mask any-issues-bit)))
          {:unprocessable-entity true
           :error-mask validation-mask}
          (let [body-data-in-transform-fn-args (flatten (conj []
                                                              version
                                                              entids
                                                              body-data))
                transformed-body-data (apply body-data-in-transform-fn body-data-in-transform-fn-args)
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
                                                                    db-spec
                                                                    accept-format-ind
                                                                    saved-entity-entid))
                                      saved-entity))]
            (letfn [(post-as-create []
                      (j/with-db-transaction [conn db-spec]
                        (let [new-entity-id (next-entity-id-fn version conn)
                              save-new-entity-fn-args (flatten (conj []
                                                                     version
                                                                     conn
                                                                     entids
                                                                     plaintext-auth-token
                                                                     new-entity-id
                                                                     transformed-body-data))]
                          (try
                            (let [[_ newly-saved-entity] (apply save-new-entity-fn save-new-entity-fn-args)]
                              (let [{{{est-session? hdr-establish-session} :headers} :request} ctx
                                    body-data-out-transform-fn-args (flatten (conj []
                                                                                   version
                                                                                   conn
                                                                                   entids
                                                                                   base-url
                                                                                   entity-uri-prefix
                                                                                   entity-uri
                                                                                   new-entity-id
                                                                                   newly-saved-entity))
                                    transformed-newly-saved-entity (apply body-data-out-transform-fn body-data-out-transform-fn-args)
                                    transformed-newly-saved-entity (merge-links-fn transformed-newly-saved-entity
                                                                                   new-entity-id)
                                    transformed-newly-saved-entity (merge-embedded-fn transformed-newly-saved-entity
                                                                                      new-entity-id)]
                                (-> {:status 201
                                     :location (make-abs-link-href base-url
                                                                   (str entity-uri
                                                                        "/"
                                                                        new-entity-id))
                                     :entity (write-res transformed-newly-saved-entity
                                                        accept-format-ind
                                                        accept-charset)}
                                    (merge
                                     (if est-session?
                                       (let [plaintext-token (make-session-fn version conn new-entity-id)]
                                         {:auth-token plaintext-token})
                                       (when (:auth-token ctx)
                                         {:auth-token (:auth-token ctx)}))))))
                            (catch IllegalArgumentException e
                              (let [msg-mask (Long/parseLong (.getMessage e))]
                                {:unprocessable-entity true
                                 :error-mask msg-mask}))
                            (catch clojure.lang.ExceptionInfo e
                              {(-> e ex-data :cause) true})))))
                    (post-as-do []
                      (j/with-db-transaction [conn db-spec]
                        (try
                          (let [post-as-do-fn-args (flatten (conj []
                                                                  version
                                                                  conn
                                                                  entids
                                                                  base-url
                                                                  entity-uri-prefix
                                                                  entity-uri
                                                                  plaintext-auth-token
                                                                  transformed-body-data
                                                                  merge-embedded-fn
                                                                  merge-links-fn))
                                resp (apply post-as-do-fn post-as-do-fn-args)]
                            (merge resp
                                   (when-let [body-data (:do-entity resp)]
                                     (let [body-data-out-transform-fn-args (flatten (conj []
                                                                                          version
                                                                                          conn
                                                                                          entids
                                                                                          base-url
                                                                                          entity-uri-prefix
                                                                                          entity-uri
                                                                                          body-data))
                                           transformed-body-data-out (apply body-data-out-transform-fn body-data-out-transform-fn-args)]
                                       {:entity (write-res transformed-body-data-out
                                                           accept-format-ind
                                                           accept-charset)}))
                                   (when (:auth-token ctx)
                                     {:auth-token (:auth-token ctx)})))
                          (catch IllegalArgumentException e
                            (let [msg-mask (Long/parseLong (.getMessage e))]
                              {:unprocessable-entity true
                               :error-mask msg-mask}))
                          (catch clojure.lang.ExceptionInfo e
                            {(-> e ex-data :cause) true}))))
                    (put []
                      (j/with-db-transaction [conn db-spec]
                        (let [if-unmodified-since-epoch-str (get-in ctx [:request :headers if-unmodified-since-hdr])
                              if-unmodified-since-val (when if-unmodified-since-epoch-str (c/from-long (Long/parseLong if-unmodified-since-epoch-str)))
                              save-entity-fn-args (flatten (conj []
                                                                 version
                                                                 conn
                                                                 entids
                                                                 plaintext-auth-token
                                                                 transformed-body-data
                                                                 if-unmodified-since-val))]
                          (letfn [(write-resp-entity [entity]
                                    (let [body-data-out-transform-fn-args (flatten (conj []
                                                                                         version
                                                                                         conn
                                                                                         entids
                                                                                         base-url
                                                                                         entity-uri-prefix
                                                                                         entity-uri
                                                                                         entity))
                                          transformed-saved-entity (apply body-data-out-transform-fn body-data-out-transform-fn-args)
                                          transformed-saved-entity (merge-links-fn transformed-saved-entity
                                                                                   (last entids))]
                                      (-> transformed-saved-entity
                                          (merge-embedded-fn (last entids))
                                          (write-res accept-format-ind accept-charset))))]
                            (try
                              (let [[_ saved-entity] (apply save-entity-fn save-entity-fn-args)]
                                (merge {:status 200
                                        :location entity-uri
                                        :entity (write-resp-entity saved-entity)}
                                       (when (:auth-token ctx)
                                         {:auth-token (:auth-token ctx)})))
                              (catch IllegalArgumentException e
                                (let [msg-mask (Long/parseLong (.getMessage e))]
                                  {:unprocessable-entity true
                                   :error-mask msg-mask}))
                              (catch clojure.lang.ExceptionInfo e
                                (let [cause (-> e ex-data :cause)
                                      latest-entity (-> e ex-data :latest-entity)]
                                  (merge {cause cause}
                                         (when latest-entity
                                           {:latest-entity (write-resp-entity latest-entity)})))))))))]
              (cond
                (= method :post-as-create) (post-as-create)
                (= method :post-as-do) (post-as-do)
                (= method :put) (put)))))
        (catch Exception e
          (log/error e "Exception caught")
          {:err e}))
     (fn [] {}))))

(defn get-t
  "Convenience function for handling HTTP GET requests."
  [version
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   db-spec
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   entids
   plaintext-auth-token
   body-data-out-transform-fn
   fetch-fn
   if-modified-since-hdr
   updated-at-keyword
   resp-gen-fn]
  (any-t-if-not-busy
   #(try
      (let [merge-links-fn (fn [fetched-entity fetched-entity-entid]
                             (if links-fn
                               (assoc fetched-entity
                                      :_links
                                      (links-fn version
                                                base-url
                                                entity-uri-prefix
                                                entity-uri
                                                fetched-entity-entid))
                               fetched-entity))
            merge-embedded-fn (fn [fetched-entity fetched-entity-entid modified-since]
                                (if embedded-resources-fn
                                  (assoc fetched-entity
                                         :_embedded
                                         (embedded-resources-fn version
                                                                base-url
                                                                entity-uri-prefix
                                                                entity-uri
                                                                db-spec
                                                                accept-format-ind
                                                                fetched-entity-entid
                                                                modified-since))
                                  fetched-entity))]
        (let [if-modified-since-epoch-str (get-in ctx [:request :headers if-modified-since-hdr])
              if-modified-since-val (when if-modified-since-epoch-str (c/from-long (Long/parseLong if-modified-since-epoch-str)))
              fetch-entity-fn-args (flatten (conj []
                                                  version
                                                  db-spec
                                                  entids
                                                  plaintext-auth-token
                                                  if-modified-since-val))]
          (letfn [(write-resp-entity [entity]
                    (let [body-data-out-transform-fn-args (flatten (conj []
                                                                         version
                                                                         db-spec
                                                                         entids
                                                                         base-url
                                                                         entity-uri-prefix
                                                                         entity-uri
                                                                         entity))
                          transformed-fetched-entity (apply body-data-out-transform-fn body-data-out-transform-fn-args)
                          transformed-fetched-entity (merge-links-fn transformed-fetched-entity
                                                                     (last entids))]
                      (-> transformed-fetched-entity
                          (merge-embedded-fn (last entids) if-modified-since-val)
                          (write-res accept-format-ind accept-charset))))]
            (let [loaded-entity-result (apply fetch-fn fetch-entity-fn-args)]
              (resp-gen-fn
               (if (not (nil? loaded-entity-result))
                 (let [[_ loaded-entity] loaded-entity-result
                       updated-at (get loaded-entity updated-at-keyword)]
                   (merge
                    {:location entity-uri}
                    (when (:auth-token ctx)
                      {:auth-token (:auth-token ctx)})
                    (if (or (nil? if-modified-since-val)
                            (t/after? updated-at if-modified-since-val))
                      {:status 200 :entity (write-resp-entity loaded-entity)}
                      {:status 304})))
                 {:status 404}))))))
      (catch Exception e
        (log/error e "Exception caught")
        {:err e}))
   #(resp-gen-fn {})))

(defn any-t-if-not-busy
  [t-f busy-f]
  (if (nil? *retry-after*)
    (t-f)
    (busy-f)))

(defn handle-resp
  "Returns a Ring response based on the content of the Liberator context.
  hdr-auth-token and hdr-error-mark are the names of the authentication token
  and error mask response headers."
  ([ctx hdr-auth-token hdr-error-mask]
   (handle-resp ctx hdr-auth-token hdr-error-mask nil))
  ([ctx hdr-auth-token hdr-error-mask login-failed-reason-hdr]
   (cond
     *retry-after* (ring-response
                    {:status 503
                     :headers {"retry-after" (ucore/instant->rfc7231str (c/to-date *retry-after*))}})
     (:err ctx) (ring-response {:status 500})
     (:became-unauthenticated ctx) (ring-response {:status 401})
     (:unmodified-since-check-failed ctx) (ring-response {:status 409
                                                          :body (:latest-entity ctx)})
     (:entity-not-found ctx) (ring-response {:status 404})
     (:unprocessable-entity ctx) (-> (ring-response {:status 422})
                                     (assoc-err-mask ctx :error-mask hdr-error-mask))
     :else (ring-response
            (merge {}
                   (when-let [status (:status ctx)] {:status status})
                   {:headers (merge {}
                                    (when-let [location (:location ctx)]
                                      {"location" location})
                                    (when-let [auth-token (:auth-token ctx)]
                                      {hdr-auth-token auth-token})
                                    (when-let [login-failed-reason (:login-failed-reason ctx)]
                                      {login-failed-reason-hdr login-failed-reason}))}
                   (when (:entity ctx) {:body (:entity ctx)}))))))
