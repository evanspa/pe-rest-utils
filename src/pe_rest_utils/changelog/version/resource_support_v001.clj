(ns pe-rest-utils.changelog.version.resource-support-v001
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as core]
            [pe-rest-utils.changelog.meta :as clmeta]
            [pe-rest-utils.changelog.resource-support :refer [fetch-changelog
                                                              body-data-out-transform-fn]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod body-data-out-transform-fn clmeta/v001
  [version
   db-spec
   _ ; there is no 'entid' associated with a changelog
   changelog]
  (identity changelog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 fetch-changelog function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod fetch-changelog clmeta/v001
  [version
   ctx
   db-spec
   accept-format-ind
   ent-reqd-attrs-and-vals
   base-url
   entity-uri-prefix
   entity-uri]
  (letfn [(u-transform-fn [ent mt-fn loc-fn assoc-links-fn keys-to-dissoc]
            {:location (loc-fn (:db/id ent))
             :media-type (mt-fn version accept-format-ind)
             :last-modified (:last-modified ent)
             :payload (apply dissoc
                             (-> (dissoc ent :last-modified)
                                 (dissoc :db/id)
                                 (assoc :_links (if assoc-links-fn
                                                  (assoc-links-fn version base-url entity-uri-prefix entity-uri (:db/id ent))
                                                  {})))
                             keys-to-dissoc)})
          (d-transform-fn [ent mt-fn loc-fn]
            {:location (loc-fn (:db/id ent))
             :media-type (mt-fn version accept-format-ind)})]
    (let [if-modified-since-str (get-in ctx [:request :query-params clmeta/since-query-param])
          if-modified-since (c/from-long (Long. if-modified-since-str))]
      ; TODO
      )))
