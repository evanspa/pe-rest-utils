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
(defn handle-changelog-post!
  "liberator handler function of post-as-create calls for persisting sets of
  application transactions."
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
   user-entid]
  (rucore/put-or-post-invoker ctx
                              :post-as-create-async
                              conn
                              apptxn-partition
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
                              nil
                              body-data-in-transform-fn
                              body-data-out-transform-fn
                              nil
                              nil
                              save-new-entity-fn
                              nil
                              nil
                              nil
                              nil
                              nil
                              nil
                              nil
                              nil
                              nil
                              apptxn-async-logger
                              make-apptxn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version body-data-in-transform-fn meta/v001)
(defmulti-by-version body-data-out-transform-fn meta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save-entity functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version save-new-entity-fn meta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sync and async transaction log writing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version apptxn-async-logger meta/v001)
(defmulti-by-version make-apptxn meta/v001)

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
                            authorized-fn]
  :available-media-types (rucore/enumerate-media-types (meta/supported-media-types mt-subtype-prefix))
  :available-charsets rumeta/supported-char-sets
  :available-languages rumeta/supported-languages
  :allowed-methods [:get]
  :authorized? authorized-fn
  :known-content-type? (rucore/known-content-type-predicate (meta/supported-media-types mt-subtype-prefix))
  :post! (fn [ctx] (handle-changelog-post! ctx
                                           conn
                                           apptxn-partition
                                           hdr-apptxn-id
                                           hdr-useragent-device-make
                                           hdr-useragent-device-os
                                           hdr-useragent-device-os-version
                                           base-url
                                           entity-uri-prefix
                                           (:uri (:request ctx))
                                           nil))
  :handle-created (fn [ctx] (rucore/handle-resp ctx
                                                hdr-auth-token
                                                hdr-error-mask)))
