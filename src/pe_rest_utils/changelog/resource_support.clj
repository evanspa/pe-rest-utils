(ns pe-rest-utils.changelog.resource-support
  "Core components for exposing the server-side processing of the
  PEAppTransaction Logging Framework as a REST API."
  (:require [datomic.api :refer [q db] :as d]
            [clj-time.core :as t]
            [liberator.core :refer [defresource]]
            [pe-rest-utils.changelog.meta :as meta]
            [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
            [pe-datomic-utils.core :as ducore]
            [pe-rest-utils.core :as rucore]
            [pe-rest-utils.macros :refer [defmulti-by-version]]
            [pe-rest-utils.meta :as rumeta]))

(declare body-data-out-transform-fn)
(declare save-new-entity-fn)
(declare apptxn-async-logger)
(declare make-apptxn)
(declare fetch-changelog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn handle-changelog-get
  "Liberator handler function for fetching a changelog."
  [ctx
   conn
   apptxn-partition
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   base-url
   entity-uri-prefix
   entity-uri
   entid
   ent-reqd-attrs-and-vals
   body-data-out-transform-fn
   apptxn-usecase
   apptxnlog-proc-started-usecase-event
   apptxnlog-proc-done-success-usecase-event
   apptxnlog-proc-done-err-occurred-usecase-event
   apptxn-async-logger-fn
   make-apptxn-fn]
  (rucore/get-invoker ctx
                      conn
                      apptxn-partition
                      hdr-apptxn-id
                      hdr-useragent-device-make
                      hdr-useragent-device-os
                      hdr-useragent-device-os-version
                      base-url
                      entity-uri-prefix
                      entity-uri
                      nil
                      nil
                      [entid]
                      nil
                      body-data-out-transform-fn
                      (fn [version
                           conn
                           accept-format-ind
                           entids ; will ignore
                           if-modified-since-inst
                           if-unmodified-since-inst ; will ignore
                           base-url
                           entity-uri-prefix
                           entity-uri
                           async-apptxnlogger
                           merge-embedded-fn ; will ignore
                           merge-links-fn]   ; will ignore
                        (fetch-changelog version
                                               conn
                                               accept-format-ind
                                               ent-reqd-attrs-and-vals
                                               if-modified-since-inst
                                               base-url
                                               entity-uri-prefix
                                               entity-uri
                                               async-apptxnlogger))
                      apptxn-usecase
                      apptxnlog-proc-started-usecase-event
                      apptxnlog-proc-done-success-usecase-event
                      apptxnlog-proc-done-err-occurred-usecase-event
                      apptxn-async-logger-fn
                      make-apptxn-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetch-changelog function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version fetch-changelog meta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version body-data-out-transform-fn meta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defresource changelog-res
  [conn
   apptxn-partition
   mt-subtype-prefix
   hdr-auth-token
   hdr-error-mask
   base-url
   entity-uri-prefix
   hdr-apptxn-id
   hdr-useragent-device-make
   hdr-useragent-device-os
   hdr-useragent-device-os-version
   authorized-fn
   entid
   ent-reqd-attr-entids
   apptxn-usecase
   apptxnlog-proc-started-usecase-event
   apptxnlog-proc-done-success-usecase-event
   apptxnlog-proc-done-err-occurred-usecase-event
   &
   more]
  :available-media-types (rucore/enumerate-media-types (meta/supported-media-types mt-subtype-prefix))
  :available-charsets rumeta/supported-char-sets
  :available-languages rumeta/supported-languages
  :allowed-methods [:get]
  :authorized? authorized-fn
  :modified-since? (fn [ctx] true)
  :handle-ok (fn [ctx] (handle-changelog-get ctx
                                             conn
                                             apptxn-partition
                                             hdr-apptxn-id
                                             hdr-useragent-device-make
                                             hdr-useragent-device-os
                                             hdr-useragent-device-os-version
                                             base-url
                                             entity-uri-prefix
                                             (:uri (:request ctx))
                                             entid
                                             ent-reqd-attr-entids
                                             body-data-out-transform-fn
                                             apptxn-usecase
                                             apptxnlog-proc-started-usecase-event
                                             apptxnlog-proc-done-success-usecase-event
                                             apptxnlog-proc-done-err-occurred-usecase-event
                                             (nth more 0)    ;apptxn-async-logger-fn
                                             (nth more 1)))) ;make-apptxn-fn
