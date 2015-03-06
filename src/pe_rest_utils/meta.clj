(ns pe-rest-utils.meta
  "Contains a set of definitions typically used by a REST API.")

(def char-sets
  "The set of character sets (java.nio.charset.Charset
  instances) installed on this platform."
  (java.nio.charset.Charset/availableCharsets))

(def char-sets-names
  "The character set names installed on this platorm."
  (keys char-sets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Media type vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def mt-type
  "The 'type' part of this REST API's media type."
  "application")

(def supported-char-sets
  "The supported character set names."
  char-sets-names)

(def supported-languages
  "The set of supported languages for this REST API."
  #{"en-US"})
