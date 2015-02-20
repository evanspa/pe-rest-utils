(ns pe-rest-utils.macros
  (:require [pe-rest-utils.meta :as meta]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as core]))

(defmacro defprocessor-impl-post
  [name
   proctemplate-fn
   mt-subtype
   version]
  `(defmethod ~name
     {:type meta/mt-type
      :subtype ~mt-subtype
      :version ~version}
     [accept-format-ind#
      accept-charset-name#
      accept-lang#
      content-type-charset-name#
      content-lang#
      parsed-content-type#
      ctx#
      conn#
      partition#
      embedded-resources-fn#
      links-fn#
      base-url#
      auth-token#
      parent-entids#
      entity-entid#]
     (let [body# (get-in ctx# [:request :body])
           accept-charset# (get meta/char-sets accept-charset-name#)
           content-type-charset# (get meta/char-sets content-type-charset-name#)
           body-data# (core/read-res parsed-content-type# body# content-type-charset#)
           body-data# (keywordize-keys body-data#)]
       (~proctemplate-fn ~version
                         body-data#
                         accept-format-ind#
                         accept-charset#
                         accept-lang#
                         parsed-content-type#
                         ctx#
                         conn#
                         partition#
                         embedded-resources-fn#
                         links-fn#
                         base-url#
                         auth-token#
                         parent-entids#
                         entity-entid#))))

(defmacro defprocessor-impl-put
  [name
   sub-entid
   proctemplate-fn
   mt-subtype
   version]
  `(defmethod ~name
     {:type meta/mt-type
      :subtype ~mt-subtype
      :version ~version}
     [accept-format-ind#
      accept-charset-name#
      accept-lang#
      content-type-charset-name#
      content-lang#
      parsed-content-type#
      ctx#
      conn#
      partition#
      embedded-resources-fn#
      links-fn#
      base-url#
      auth-token#
      parent-entids#
      entity-entid#
      sub-entid#]
     (let [body-stream# (get-in ctx# [:request :body])
           accept-charset# (get meta/char-sets accept-charset-name#)
           content-type-charset# (get meta/char-sets content-type-charset-name#)
           body-data# (core/read-res parsed-content-type# body-stream# content-type-charset#)
           body-data-transformed-keys# (keywordize-keys body-data#)]
       (~proctemplate-fn ~version
                         body-data-transformed-keys#
                         accept-format-ind#
                         accept-charset#
                         accept-lang#
                         parsed-content-type#
                         ctx#
                         conn#
                         partition#
                         embedded-resources-fn#
                         links-fn#
                         base-url#
                         auth-token#
                         parent-entids#
                         entity-entid#
                         ~sub-entid))))

(defmacro defproctemplate-post [name
                                validator-fn
                                any-issues-bit
                                body-data-in-transform-fn
                                body-data-out-transform-fn
                                name-extractor-fn
                                existing-entities-by-name-fn
                                saveentity-entity-already-exists-bit
                                save-new-entity-txnmap-fn
                                apptxn-usecase
                                apptxnlog-proc-done-success-usecase-event
                                apptxnlog-proc-done-err-occurred-usecase-event
                                known-entity-attr
                                location-fn
                                record-apptxn-async-fn
                                apptxnlog-txn-fn]
  `(defn ~name
     [version#
      body-data#
      accept-format-ind#
      accept-charset#
      accept-lang#
      parsed-content-type#
      ctx#
      conn#
      partition#
      embedded-resources-fn#
      links-fn#
      base-url#
      auth-token#
      parent-entids#
      entity-entid#]
     (core/post!-t version#
                   body-data#
                   accept-format-ind#
                   accept-charset#
                   accept-lang#
                   parsed-content-type#
                   ctx#
                   conn#
                   partition#
                   embedded-resources-fn#
                   links-fn#
                   base-url#
                   auth-token#
                   parent-entids#
                   entity-entid#
                   ~validator-fn
                   ~any-issues-bit
                   ~body-data-in-transform-fn
                   ~body-data-out-transform-fn
                   ~name-extractor-fn
                   ~existing-entities-by-name-fn
                   ~saveentity-entity-already-exists-bit
                   ~save-new-entity-txnmap-fn
                   ~apptxn-usecase
                   ~apptxnlog-proc-done-success-usecase-event
                   ~apptxnlog-proc-done-err-occurred-usecase-event
                   ~known-entity-attr
                   ~location-fn
                   ~record-apptxn-async-fn
                   ~apptxnlog-txn-fn)))

(defmacro defproctemplate-put [name
                               validator-fn
                               any-issues-bit
                               body-data-in-transform-fn
                               body-data-out-transform-fn
                               name-extractor-fn
                               save-entity-txnmap-fn
                               apptxn-usecase
                               apptxnlog-proc-done-success-usecase-event
                               apptxnlog-proc-done-err-occurred-usecase-event
                               known-entity-attr
                               location-fn]
  `(defn ~name
     [version#
      body-data#
      accept-format-ind#
      accept-charset#
      accept-lang#
      parsed-content-type#
      ctx#
      conn#
      partition#
      embedded-resources-fn#
      links-fn#
      base-url#
      auth-token#
      parent-entids#
      entity-entid#]
     (core/put-t version#
                 body-data#
                 accept-format-ind#
                 accept-charset#
                 accept-lang#
                 parsed-content-type#
                 ctx#
                 conn#
                 partition#
                 embedded-resources-fn#
                 links-fn#
                 base-url#
                 auth-token#
                 parent-entids#
                 entity-entid#
                 ~validator-fn
                 ~any-issues-bit
                 ~body-data-in-transform-fn
                 ~body-data-out-transform-fn
                 ~name-extractor-fn
                 ~save-entity-txnmap-fn
                 ~apptxn-usecase
                 ~apptxnlog-proc-done-success-usecase-event
                 ~apptxnlog-proc-done-err-occurred-usecase-event
                 ~known-entity-attr
                 ~location-fn)))

(defmacro defprocessor-post [name]
  `(defmulti ~name
     (fn [accept-format-ind#
          accept-charset-name#
          accept-lang#
          content-type-charset-name#
          content-lang#
          parsed-content-type#
          ctx#
          conn#
          partition#
          embedded-resources-fn#
          links-fn#
          base-url#
          auth-token#
          parent-entids#
          entity-entid#]
       {:type (:type parsed-content-type#)
        :subtype (:subtype parsed-content-type#)
        :version (:version parsed-content-type#)})))

(defmacro defprocessor-put [name entid]
  `(defmulti ~name
     (fn [accept-format-ind#
          accept-charset-name#
          accept-lang#
          content-type-charset-name#
          content-lang#
          parsed-content-type#
          ctx#
          conn#
          partition#
          embedded-resources-fn#
          links-fn#
          base-url#
          auth-token#
          parent-entids#
          entity-entid#
          ~entid]
       {:type (:type parsed-content-type#)
        :subtype (:subtype parsed-content-type#)
        :version (:version parsed-content-type#)})))
