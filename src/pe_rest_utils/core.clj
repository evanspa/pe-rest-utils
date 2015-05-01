(ns pe-rest-utils.core
  "A set of functions to simplify the development of (hypermedia) REST services
  on top of Liberator."
  (:require [liberator.representation :refer [render-map-generic ring-response]]
            [pe-rest-utils.meta :as meta]
            [pe-core-utils.core :as ucore]
            [pe-datomic-utils.core :as ducore]
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
(declare get-t)

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
  HTTP method.  You might use 'POST-as-do' for performing a login action.

  The parameters are as follows:

  ctx - Liberator context.
  method - :post-as-create, :post-as-create-async, :post-as-do or :put
  conn - Datomic connection.
  partition - Primary application Datomic partition.
  apptxn-partition - Application transaction logging Datomic partition.
  hdr-apptxn-id - Application transaction ID header name.
  hdr-useragent-device-make - User-agent device make header name.
  hdr-useragent-device-os - User-agent device operating system header name.
  hdr-useragent-device-os-version - User-agent device operating system version header name.
  base-url - The base URL used by the REST API.
  entity-uri-prefix - The entity URI prefix used by the REST API
  entity-uri - The URI of the request entity.
  embedded-resources-fn - Function whose return value is used for the value of
  the '_embedded' slot of the returned resource object.
  links-fn - Function whose return value is used for the value the '_links' slot
  of the returned resource object.
  entids - The set of entity IDs that appear in the request URI.
  validator-fn - Predicate function to validate the contents of the request body.
  any-issues-bit - Bit used to indicate if there are any issues with the request.
  body-data-in-transform-fn - Function used to transform the request body before starting processing.
  body-data-out-transform-fn - Function used to transform the response before returning it.

  The 'more' arguments are as follows:

  existing-entity-fns (more[0]) - Function used to test for the existence of the request
  entity in the case of POST-as-create requests.  If it evaluates to true, an
  error HTTP response is returned.
  saveentity-entity-already-exists-bit (more[1]) - The bit to use to indicate if the
  request entity already exists (in the case of POST-as-create).
  save-new-entity-txnmap-fn (more[2]) - Function to create a new entity (in the case of POST-as-create).
  save-entity-txnmap-fn (more[3]) - Function to save an existing entity (in the case of PUT).
  hdr-establish-session (more[4]) - Name of header indicating if a session should be establish.
  make-session-fn (more[5]) - Function to create a new session.
  post-as-do-fn (more[6]) - Function to process the request in the case of POST-as-do.
  apptxn-usecase (more[7]) - Application transaction use case name for this request.
  apptxnlog-proc-started-usecase-event (more[8]) - Specific application transaction use
  case 'processing started' event type.
  apptxnlog-proc-done-success-usecase-event (more[9]) - Specific application transaction
  use case 'processing done, successful' event type.
  apptxnlog-proc-done-err-occurred-usecase-event - (more[10]) Specific application
  transaction use case 'processing done, error ocurred' event type.
  known-entity-attr (more[11]) -  Known Datomic attribute of the request entity.
  apptxn-async-logger-fn (more[12]) - Function for asynchronously logging application transactions.
  make-apptxn-fn (more[13]) - Function for creating Datomic transactions for persisting
  application transaction log data."
  [ctx
   method
   conn
   partition
   apptxn-partition
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
   &
   more]
  (let [existing-entity-fns (nth more 0)
        saveentity-entity-already-exists-bit (nth more 1)
        save-new-entity-txnmap-fn (nth more 2)
        save-entity-txnmap-fn (nth more 3)
        hdr-establish-session (nth more 4)
        make-session-fn (nth more 5)
        post-as-do-fn (nth more 6)
        apptxn-usecase (nth more 7)
        apptxnlog-proc-started-usecase-event (nth more 8)
        apptxnlog-proc-done-success-usecase-event (nth more 9)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 10)
        known-entity-attr (nth more 11)
        apptxn-async-logger-fn (nth more 12)
        make-apptxn-fn (nth more 13)
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
                   apptxn-partition
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
                   apptxn-async-logger-fn
                   make-apptxn-fn)))

(defn get-invoker
  "Convenience function for handling HTTP GET requests.

  The parameters are as follows:

  ctx - Liberator context.
  conn - Datomic connection.
  apptxn-partition - Application transaction logging Datomic partition.
  hdr-apptxn-id - Application transaction ID header name.
  hdr-useragent-device-make - User-agent device make header name.
  hdr-useragent-device-os - User-agent device operating system header name.
  hdr-useragent-device-os-version - User-agent device operating system version header name.
  base-url - The base URL used by the REST API.
  entity-uri-prefix - The entity URI prefix used by the REST API
  entity-uri - The URI of the request entity.
  embedded-resources-fn - Function whose return value is used for the value of
  the '_embedded' slot of the returned resource object.
  links-fn - Function whose return value is used for the value the '_links' slot
  of the returned resource object.

  The 'more' arguments are as follows:

  entids (more[0]) - The set of entity IDs that appear in the request URI.
  any-issues-bit (more[1]) - Bit used to indicate if there are any issues with the request.
  body-data-out-transform-fn (more[2]) - Function used to transform the response before returning it.
  fetch-fn (more[3]) - The function performing the actual fetching of the entity from the database.
  apptxn-usecase (more[4]) - Application transaction use case name for this request.
  apptxnlog-proc-started-usecase-event (more[5]) - Specific application transaction use
  case 'processing started' event type.
  apptxnlog-proc-done-success-usecase-event (more[6]) - Specific application transaction
  use case 'processing done, successful' event type.
  apptxnlog-proc-done-err-occurred-usecase-event - (more[7]) Specific application
  transaction use case 'processing done, error ocurred' event type.
  apptxn-async-logger-fn (more[8]) - Function for asynchronously logging application transactions.
  make-apptxn-fn (more[9]) - Function for creating Datomic transactions for persisting
  application transaction log data."
  [ctx
   conn
   apptxn-partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url          ; e.g., https://api.example.com:4040
   entity-uri-prefix ; e.g., /fp/
   entity-uri        ; e.g., /fp/users/191491
   embedded-resources-fn
   links-fn
   &
   more]
  (let [entids (nth more 0)
        any-issues-bit (nth more 1)
        body-data-out-transform-fn (nth more 2)
        fetch-fn (nth more 3)
        apptxn-usecase (nth more 4)
        apptxnlog-proc-started-usecase-event (nth more 5)
        apptxnlog-proc-done-success-usecase-event (nth more 6)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 7)
        apptxn-async-logger-fn (nth more 8)
        make-apptxn-fn (nth more 9)
        {{:keys [media-type lang charset]} :representation} ctx
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
           conn
           apptxn-partition
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
           any-issues-bit
           body-data-out-transform-fn
           fetch-fn
           apptxn-usecase
           apptxnlog-proc-started-usecase-event
           apptxnlog-proc-done-success-usecase-event
           apptxnlog-proc-done-err-occurred-usecase-event
           apptxn-async-logger-fn
           make-apptxn-fn)))

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
  HTTP method.  You might use 'POST-as-do' for performing a login action.

  The parameters are as follows:

  version - The media type version indicator.
  body-data - The request body data.
  accept-format-ind - The accept format indicator (i.e., the format the
  response should be in).
  accept-charset - The accept character set (i.e., the character set to encode
  the textual-response).
  accept-lang - The accept language (i.e., the language to use for
  human-consumable text-content in the response).
  ctx - Liberator context.
  method - :post-as-create, :post-as-create-async, :post-as-do or :put
  conn - Datomic connection.
  partition - Primary application Datomic partition.
  apptxn-partition - Application transaction logging Datomic partition.
  hdr-apptxn-id - Application transaction ID header name.
  hdr-useragent-device-make - User-agent device make header name.
  hdr-useragent-device-os - User-agent device operating system header name.
  hdr-useragent-device-os-version - User-agent device operating system version header name.
  base-url - The base URL used by the REST API.
  entity-uri-prefix - The entity URI prefix used by the REST API
  entity-uri - The URI of the request entity.
  embedded-resources-fn - Function whose return value is used for the value of
  the '_embedded' slot of the returned resource object.
  links-fn - Function whose return value is used for the value the '_links' slot
  of the returned resource object.

  The 'more' arguments are as follows:

  entids (more[0]) - The set of entity IDs that appear in the request URI.
  validator-fn (more[1]) - Predicate function to validate the contents of the request body.
  any-issues-bit (more[2]) - Bit used to indicate if there are any issues with the request.
  body-data-in-transform-fn (more[3]) - Function used to transform the request body before starting processing.
  body-data-out-transform-fn (more[4]) - Function used to transform the response before returning it.
  existing-entity-fns (more[5]) - Function used to test for the existence of the request
  entity in the case of POST-as-create requests.  If it evaluates to true, an
  error HTTP response is returned.
  saveentity-entity-already-exists-bit (more[6]) - The bit to use to indicate if the
  request entity already exists (in the case of POST-as-create).
  save-new-entity-txnmap-fn (more[7]) - Function to create a new entity (in the case of POST-as-create).
  save-entity-txnmap-fn (more[8]) - Function to save an existing entity (in the case of PUT).
  hdr-establish-session (more[9]) - Name of header indicating if a session should be establish.
  make-session-fn (more[10]) - Function to create a new session.
  post-as-do-fn (more[11]) - Function to process the request in the case of POST-as-do.
  apptxn-usecase (more[12]) - Application transaction use case name for this request.
  apptxnlog-proc-started-usecase-event (more[13]) - Specific application transaction use
  case 'processing started' event type.
  apptxnlog-proc-done-success-usecase-event (more[14]) - Specific application transaction
  use case 'processing done, successful' event type.
  apptxnlog-proc-done-err-occurred-usecase-event - (more[15]) Specific application
  transaction use case 'processing done, error ocurred' event type.
  known-entity-attr (more[16]) -  Known Datomic attribute of the request entity.
  apptxn-async-logger-fn (more[17]) - Function for asynchronously logging application transactions.
  make-apptxn-fn (more[18]) - Function for creating Datomic transactions for persisting
  application transaction log data."
  [version
   body-data
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   method
   conn
   partition
   apptxn-partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   &
   more]
  (let [entids (nth more 0)
        validator-fn (nth more 1)
        any-issues-bit (nth more 2)
        body-data-in-transform-fn (nth more 3)
        body-data-out-transform-fn (nth more 4)
        existing-entity-fns (nth more 5)
        saveentity-entity-already-exists-bit (nth more 6)
        save-new-entity-txnmap-fn (nth more 7)
        save-entity-txnmap-fn (nth more 8)
        hdr-establish-session (nth more 9)
        make-session-fn (nth more 10)
        post-as-do-fn (nth more 11)
        apptxn-usecase (nth more 12)
        apptxnlog-proc-started-usecase-event (nth more 13)
        apptxnlog-proc-done-success-usecase-event (nth more 14)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 15)
        known-entity-attr (nth more 16)
        apptxn-async-logger-fn (nth more 17)
        make-apptxn-fn (nth more 18)
        validation-mask (if validator-fn (validator-fn version body-data) 0)
        apptxn-maker (partial make-apptxn-fn
                              version
                              ctx
                              conn
                              apptxn-partition
                              hdr-apptxn-id
                              hdr-useragent-device-make
                              hdr-useragent-device-os
                              hdr-useragent-device-os-version
                              apptxn-usecase)
        async-apptxnlogger (partial apptxn-async-logger-fn
                                    version
                                    ctx
                                    conn
                                    apptxn-partition
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
                                                             (when-let [name (name-extraction-fn version transformed-body-data)]
                                                               (let [args (flatten (conj []
                                                                                         version
                                                                                         conn
                                                                                         entids
                                                                                         name))
                                                                     entities (apply get-entities-by-name-fn args)]
                                                                 (> (count entities) 0)))))
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
                             entity-txn-time (ducore/txn-time conn saved-entity-entid known-entity-attr)
                             entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                             body-data (body-data-out-transform-fn version body-data)
                             saved-entity (merge-links-fn body-data saved-entity-entid)
                             saved-entity (merge-embedded-fn saved-entity saved-entity-entid)]
                         (merge {:status 201
                                 :location (make-abs-link-href base-url
                                                               (str entity-uri
                                                                    "/"
                                                                    saved-entity-entid));(make-abs-entity-link-href base-url entity-uri saved-entity-entid)
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
                          entity-txn-time (ducore/txn-time conn (last entids) known-entity-attr)
                          entity-txn-time-str (ucore/instant->rfc7231str entity-txn-time)
                          body-data (body-data-out-transform-fn version body-data)
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

(defn get-t
  "Convenience function for handling HTTP GET requests.

  The parameters are as follows:

  version - The media type version indicator.
  accept-format-ind - The accept format indicator (i.e., the format the
  response should be in).
  accept-charset - The accept character set (i.e., the character set to encode
  the textual-response).
  accept-lang - The accept language (i.e., the language to use for
  human-consumable text-content in the response).
  ctx - Liberator context.
  conn - Datomic connection.
  apptxn-partition - Application transaction logging Datomic partition.
  hdr-apptxn-id - Application transaction ID header name.
  hdr-useragent-device-make - User-agent device make header name.
  hdr-useragent-device-os - User-agent device operating system header name.
  hdr-useragent-device-os-version - User-agent device operating system version header name.
  base-url - The base URL used by the REST API.
  entity-uri-prefix - The entity URI prefix used by the REST API
  entity-uri - The URI of the request entity.
  embedded-resources-fn - Function whose return value is used for the value of
  the '_embedded' slot of the returned resource object.
  links-fn - Function whose return value is used for the value the '_links' slot
  of the returned resource object.

  The 'more' arguments are as follows:

  entids (more[0]) - The set of entity IDs that appear in the request URI.
  any-issues-bit (more[1]) - Bit used to indicate if there are any issues with the request.
  body-data-out-transform-fn (more[2]) - Function used to transform the response before returning it.
  fetch-fn (more[3]) - The function performing the actual fetching of the entity from the database.
  apptxn-usecase (more[4]) - Application transaction use case name for this request.
  apptxnlog-proc-started-usecase-event (more[5]) - Specific application transaction use
  case 'processing started' event type.
  apptxnlog-proc-done-success-usecase-event (more[6]) - Specific application transaction
  use case 'processing done, successful' event type.
  apptxnlog-proc-done-err-occurred-usecase-event - (more[7]) Specific application
  transaction use case 'processing done, error ocurred' event type.
  apptxn-async-logger-fn (more[8]) - Function for asynchronously logging application transactions.
  make-apptxn-fn (more[9]) - Function for creating Datomic transactions for persisting
  application transaction log data."
  [version
   accept-format-ind
   accept-charset
   accept-lang
   ctx
   conn
   apptxn-partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url
   entity-uri-prefix
   entity-uri
   embedded-resources-fn
   links-fn
   &
   more]
  (let [entids (nth more 0)
        any-issues-bit (nth more 1)
        body-data-out-transform-fn (nth more 2)
        fetch-fn (nth more 3)
        apptxn-usecase (nth more 4)
        apptxnlog-proc-started-usecase-event (nth more 5)
        apptxnlog-proc-done-success-usecase-event (nth more 6)
        apptxnlog-proc-done-err-occurred-usecase-event (nth more 7)
        apptxn-async-logger-fn (nth more 8)
        make-apptxn-fn (nth more 9)
        apptxn-maker (partial make-apptxn-fn
                              version
                              ctx
                              conn
                              apptxn-partition
                              hdr-apptxn-id
                              hdr-useragent-device-make
                              hdr-useragent-device-os
                              hdr-useragent-device-os-version
                              apptxn-usecase)
        async-apptxnlogger (partial apptxn-async-logger-fn
                                    version
                                    ctx
                                    conn
                                    apptxn-partition
                                    hdr-apptxn-id
                                    hdr-useragent-device-make
                                    hdr-useragent-device-os
                                    hdr-useragent-device-os-version
                                    apptxn-usecase)]
    (try
      (when apptxn-usecase (async-apptxnlogger apptxnlog-proc-started-usecase-event))
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
            merge-embedded-fn (fn [fetched-entity fetched-entity-entid]
                                (if embedded-resources-fn
                                  (assoc fetched-entity
                                         :_embedded
                                         (embedded-resources-fn version
                                                                base-url
                                                                entity-uri-prefix
                                                                entity-uri
                                                                conn
                                                                accept-format-ind
                                                                fetched-entity-entid))
                                  fetched-entity))]
        (let [if-modified-since-str (get-in ctx [:request :headers "if-modified-since"])
              if-unmodified-since-str (get-in ctx [:request :headers "if-unmodified-since"])
              if-modified-since-inst (when if-modified-since-str
                                       (ucore/rfc7231str->instant if-modified-since-str))
              if-unmodified-since-inst (when if-unmodified-since-str
                                         (ucore/rfc7231str->instant if-unmodified-since-str))
              resp (fetch-fn version
                             conn
                             accept-format-ind
                             entids
                             if-modified-since-inst
                             if-unmodified-since-inst
                             base-url
                             entity-uri-prefix
                             entity-uri
                             async-apptxnlogger
                             merge-embedded-fn
                             merge-links-fn)]
          (when apptxn-usecase (async-apptxnlogger apptxnlog-proc-done-success-usecase-event))
          (merge resp
                 (when-let [body-data (:fetched-entity resp)]
                   {:entity (write-res (body-data-out-transform-fn version body-data)
                                       accept-format-ind
                                       accept-charset)})
                 (when (:auth-token ctx)
                   {:auth-token (:auth-token ctx)}))))
      (catch Exception e
        (log/error e "Exception caught")
        (when apptxn-usecase (async-apptxnlogger apptxnlog-proc-done-err-occurred-usecase-event
                                                 nil
                                                 (ucore/throwable->str e)))
        {:err e}))))

(defn handle-resp
  "Returns a Ring response based on the content of the Liberator context.
  hdr-auth-token and hdr-error-mark are the names of the authentication token
  and error mask response headers."
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
           (merge {}
                  (when-let [status (:status ctx)] {:status status})
                  {:headers (merge {}
                                   (when-let [location (:location ctx)]
                                     {"location" location})
                                   (when-let [auth-token (:auth-token ctx)]
                                     {hdr-auth-token auth-token})
                                   (when (:last-modified ctx)
                                     {"last-modified" (:last-modified ctx)}))}
                  (when (:entity ctx) {:body (:entity ctx)})))))
