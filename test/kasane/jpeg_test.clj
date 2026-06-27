(ns kasane.jpeg-test
  "JPEG metadata reader vs a real Pillow-encoded JPEG. Pixels are not decoded
   (DCT deferred, ADR-2606272300) — we verify marker-walk geometry."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [kasane.jpeg :as jpeg]
            [kasane.normalize :as norm]))

(defn- rd [p] (mapv #(bit-and (int %) 0xff)
                    (with-open [in (io/input-stream (io/resource p))] (.readAllBytes in))))

(deftest jpeg-metadata
  (let [p (jpeg/parse (rd "kasane/fixtures/sample.jpg"))]
    (testing "dimensions from SOF marker"
      (is (= 48 (:width p)))
      (is (= 32 (:height p)))
      (is (= 3 (:components p)))
      (is (false? (:progressive? p))))
    (testing "marker walk reached the scan"
      (is (some? (:scan-start p)))
      (is (contains? (set (:markers p)) 0xDA)))))             ; SOS seen

(deftest jpeg-normalize
  (let [doc (norm/->doc :jpeg (jpeg/parse (rd "kasane/fixtures/sample.jpg")))]
    (is (= :jpeg (:kasane/format doc)))
    (is (= {:width 48 :height 32 :unit :px :dpi 72} (:kasane/canvas doc)))
    (let [n (first (:kasane/nodes doc))]
      (is (= :raster (:node/kind n)))
      (is (= :jpeg (get-in n [:raster/blob :fmt])))            ; opaque, not decoded
      (is (nil? (get-in n [:raster/blob :cid]))))))