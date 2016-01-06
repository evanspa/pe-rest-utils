(ns pe-rest-utils.changelog.version.resource-support-v001
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clojure.tools.logging :as log]
            [pe-core-utils.core :as ucore]
            [pe-jdbc-utils.core :as jcore]
            [pe-rest-utils.core :as core]
            [pe-rest-utils.changelog.meta :as clmeta]
            [pe-rest-utils.changelog.resource-support :refer [load-changelog-fn
                                                              body-data-out-transform-fn]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod body-data-out-transform-fn clmeta/v001
  [version
   db-spec
   user-id
   base-url
   entity-uri-prefix
   entity-uri
   changelog]
  (-> changelog
      (ucore/transform-map-val :changelog/updated-at #(c/to-long %))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 load-changelog-fn function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod load-changelog-fn clmeta/v001
  [ctx
   version
   db-spec
   user-id
   plaintext-auth-token
   modified-since
   tables-and-updated-at-cols]
  [nil {:changelog/updated-at
        (jcore/most-recent-modified-at-overall db-spec
                                               modified-since
                                               tables-and-updated-at-cols)}])
