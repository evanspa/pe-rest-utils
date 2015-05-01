(ns pe-rest-utils.changelog.version.resource-support-v001
  (:require [datomic.api :refer [q db] :as d]
            [clj-time.core :as t]
            [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as core]
            [pe-datomic-utils.core :as ducore]
            [pe-rest-utils.changelog.meta :as clmeta]
            [pe-rest-utils.changelog.resource-support :refer [fetch-changelog-since
                                                              body-data-out-transform-fn]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 body-data transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod body-data-out-transform-fn clmeta/v001
  [version body-data]
  (identity body-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.0.1 fetch-changelog-since function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod fetch-changelog-since clmeta/v001
  [version
   conn
   accept-format-ind
   ent-reqd-attrs-and-vals ;[[:fpuser/email "p@p.com" "application/vnd.fp" assoc-links-fn] [:fpvehicle/user 1920391]]
   if-modified-since-inst
   base-url
   entity-uri-prefix
   entity-uri
   async-apptxnlogger]
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
    (reduce (fn [ocl ent-reqd-attrs-and-val-parts]
              (if (keyword? (first ent-reqd-attrs-and-val-parts))
                (let [[reqd-attr reqd-attr-val mt-fn loc-fn assoc-links-fn keys-to-dissoc] ent-reqd-attrs-and-val-parts
                      cl (ducore/change-log-since conn
                                                  if-modified-since-inst
                                                  reqd-attr
                                                  reqd-attr-val
                                                  #(u-transform-fn % mt-fn loc-fn assoc-links-fn keys-to-dissoc)
                                                  #(d-transform-fn % mt-fn loc-fn))]
                  (merge-with concat ocl cl))
                (let [[entid mt-fn loc-fn assoc-links-fn keys-to-dissoc] ent-reqd-attrs-and-val-parts]
                  (if (ducore/is-entity-updated-since conn if-modified-since-inst entid)
                    (let [db (d/db conn)
                          since-db (d/since db if-modified-since-inst)]
                      (merge-with concat
                                  ocl
                                  {:updates [(u-transform-fn (into {:db/id entid} (d/entity since-db entid))
                                                             mt-fn
                                                             loc-fn
                                                             assoc-links-fn
                                                             keys-to-dissoc)]}))
                    (if (ducore/is-entity-deleted-since conn if-modified-since-inst entid)
                      (merge-with concat
                                  ocl
                                  {:deletions [(d-transform-fn {:db/id entid} loc-fn)]})
                      ocl)))))
            {}
            ent-reqd-attrs-and-vals)))
