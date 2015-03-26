(ns pe-rest-utils.changelog.resource-support
  "Core components for exposing the server-side processing of the
  PEAppTransaction Logging Framework as a REST API."
  (:require [datomic.api :refer [q db] :as d]
            [clj-time.core :as t]
            [liberator.core :refer [defresource]]
            [pe-rest-utils.changelog.meta :as meta]
            [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as rucore]
            [pe-rest-utils.macros :refer [defmulti-by-version]]
            [pe-rest-utils.meta :as rumeta]))

(declare body-data-in-transform-fn)
(declare body-data-out-transform-fn)
(declare save-new-entity-fn)
(declare apptxn-async-logger)
(declare make-apptxn)

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
   user-entid
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
                      [user-entid]
                      nil
                      body-data-out-transform-fn
                      (fn [version
                           conn
                           base-url
                           entity-uri-prefix
                           entity-uri
                           async-apptxnlogger
                           _
                           __]
                        (let [as-of-inst (get-in )]))
                      apptxn-usecase
                      apptxnlog-proc-started-usecase-event
                      apptxnlog-proc-done-success-usecase-event
                      apptxnlog-proc-done-err-occurred-usecase-event
                      apptxn-async-logger-fn
                      make-apptxn-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defresource changelog-res [conn
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
                            user-entid
                            body-data-out-transform-fn
                            apptxn-usecase
                            apptxnlog-proc-started-usecase-event
                            apptxnlog-proc-done-success-usecase-event
                            apptxnlog-proc-done-err-occurred-usecase-event
                            apptxn-async-logger-fn
                            make-apptxn-fn]
  :available-media-types (rucore/enumerate-media-types (meta/supported-media-types mt-subtype-prefix))
  :available-charsets rumeta/supported-char-sets
  :available-languages rumeta/supported-languages
  :allowed-methods [:get]
  :authorized? authorized-fn
  :known-content-type? (rucore/known-content-type-predicate (meta/supported-media-types mt-subtype-prefix))
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
                                             nil)))
