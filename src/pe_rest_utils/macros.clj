(ns pe-rest-utils.macros
  "A set of macros simplifying the definitions need to create a REST API on top
  of Liberator."
  (:require [pe-rest-utils.meta :as meta]
            [clojure.walk :refer [keywordize-keys]]
            [pe-rest-utils.core :as core]))

(defmacro defmulti-by-version
  "Creates a multi-method definition wherein the dispatch function receives a
  'version' string value as its first parameter (and any number of parameters
  after that).  The version parameter is simply returned and thus the basis for
  dispatch to concrete methods."
  [name default]
  `(defmulti ~name
     (fn [version#
          &
          more#]
       version#)
     :default ~default))
