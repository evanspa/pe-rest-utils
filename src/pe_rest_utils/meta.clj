(ns pe-rest-utils.meta)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom headers and cookies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def hdr-auth-token "pe-rest-auth-token")
;(def hdr-error-mask "pe-rest-error-mask")

;(def hdr-apptxn-id "peapptxn-id")

;(def hdr-useragent-device-make "pe-rest-useragent-device-make")
;(def hdr-useragent-device-os "pe-rest-useragent-device-os")
;(def hdr-useragent-device-os-version "pe-rest-useragent-device-os-version")

(def supported-char-sets
  "The supported character set names."
  char-sets-names)

(def supported-languages
  "The set of supported languages for this REST API."
  #{"en-US"})
