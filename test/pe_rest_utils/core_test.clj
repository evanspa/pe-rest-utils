(ns pe-rest-utils.core-test
  (:require [pe-rest-utils.core :as core]
            [clojure.test :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-parse-media-type
  (testing "Different permutations of inputs"
    (let [p (core/parse-media-type "text/html")]
      (is (= "text" (:type p)))
      (is (= "html" (:subtype p)))
      (is (= "text/html" (:bare-mediatype p)))
      (is (nil? (:charset p)))
      (is (nil? (:version p)))
      (is (nil? (:format-ind p))))
    (let [p (core/parse-media-type
             "application/vnd.test.com-v6.2.1+xml;charset=UTF-8")]
      (is (= "application" (:type p)))
      (is (= "vnd.test.com" (:subtype p)))
      (is (= "application/vnd.test.com" (:bare-mediatype p)))
      (is (= "6.2.1" (:version p)))
      (is (= "xml" (:format-ind p)))
      (is (= "UTF-8" (:charset p))))
    (let [p (core/parse-media-type
             "application/vnd.test.com+xml")]
      (is (= "application" (:type p)))
      (is (= "vnd.test.com" (:subtype p)))
      (is (= "application/vnd.test.com" (:bare-mediatype p)))
      (is (nil? (:version p)))
      (is (= "xml" (:format-ind p)))
      (is (nil? (:charset p))))))
