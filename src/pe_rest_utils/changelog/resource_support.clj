(ns pe-rest-utils.changelog.resource-support
  "Core components for exposing the server-side processing of the
  PEAppTransaction Logging Framework as a REST API."
  (:require [clj-time.core :as t]
            [liberator.core :refer [defresource]]
            [pe-rest-utils.changelog.meta :as clmeta]
            [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
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
   db-spec
   base-url
   entity-uri-prefix
   entity-uri
   entid
   ent-reqd-attrs-and-vals
   body-data-out-transform-fn
   hdr-auth-token
   hdr-error-mask]
  (rucore/get-invoker ctx
                      db-spec
                      base-url
                      entity-uri-prefix
                      entity-uri
                      nil
                      nil
                      [entid]
                      nil
                      body-data-out-transform-fn
                      (fn [version
                           ctx
                           db-spec
                           accept-format-ind
                           entids ; will ignore
                           if-modified-since-inst ; will ignore
                           if-unmodified-since-inst ; will ignore
                           base-url
                           entity-uri-prefix
                           entity-uri
                           merge-embedded-fn ; will ignore
                           merge-links-fn]   ; will ignore
                        (fetch-changelog version
                                         ctx
                                         db-spec
                                         accept-format-ind
                                         ent-reqd-attrs-and-vals
                                         base-url
                                         entity-uri-prefix
                                         entity-uri))
                      hdr-auth-token
                      hdr-error-mask))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetch-changelog function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version fetch-changelog clmeta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti-by-version body-data-out-transform-fn clmeta/v001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defresource changelog-res
  [db-spec
   mt-subtype-prefix
   hdr-auth-token
   hdr-error-mask
   base-url
   entity-uri-prefix
   authorized-fn
   entid
   ent-reqd-attr-entids]
  :available-media-types (rucore/enumerate-media-types (clmeta/supported-media-types mt-subtype-prefix))
  :available-charsets rumeta/supported-char-sets
  :available-languages rumeta/supported-languages
  :allowed-methods [:get]
  :authorized? authorized-fn
  :handle-ok (fn [ctx] (handle-changelog-get ctx
                                             db-spec
                                             base-url
                                             entity-uri-prefix
                                             (:uri (:request ctx))
                                             entid
                                             ent-reqd-attr-entids
                                             body-data-out-transform-fn
                                             hdr-auth-token
                                             hdr-error-mask)))
