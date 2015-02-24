(ns pe-rest-utils.macros
  (:require [pe-rest-utils.meta :as meta]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as core]))

(defmacro defmulti-by-version [name default]
  `(defmulti ~name
     (fn [version#
          &
          more#]
       version#)
     :default ~default))
