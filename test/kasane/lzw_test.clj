(ns kasane.lzw-test
  "LZW validated against REAL files. Fixtures in resources/kasane/fixtures were
   encoded by libtiff/Pillow with ground-truth pixels recorded in expected*.edn
   (re-read via Pillow). The library is dependency-free; Pillow is only the
   fixture oracle.

   Both are verified bit-exact: TIFF (MSB, early-change) and GIF (LSB,
   non-early) — including a 96x40 image that crosses the 9→10→11→12-bit
   code-width boundaries, and an interlaced GIF (4-pass de-ordering)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [kasane.tiff :as tiff]
            [kasane.gif :as gif]))

(defn- rd [p] (mapv #(bit-and (int %) 0xff)
                    (with-open [in (io/input-stream (io/resource p))] (.readAllBytes in))))
(defn- expected [p] (edn/read-string (slurp (io/resource p))))

(deftest tiff-lzw-pixels
  (testing "small 8x4 grayscale LZW vs libtiff ground truth"
    (is (= (get-in (expected "kasane/fixtures/expected.edn") [:tiff-gray :samples])
           (tiff/pixels (rd "kasane/fixtures/lzw_gray.tif")))))
  (testing "96x40 LZW crossing 9→10→11→12-bit code-width boundaries (bit-exact)"
    (let [exp (get-in (expected "kasane/fixtures/expected_big.edn") [:tiff :samples])]
      (is (= 3840 (count exp)))
      (is (= exp (tiff/pixels (rd "kasane/fixtures/lzw_big.tif")))))))

(deftest gif-lzw-pixels
  (testing "small 4x2 indexed LZW vs Pillow ground truth"
    (is (= (get-in (expected "kasane/fixtures/expected.edn") [:gif-idx :indices])
           (gif/first-frame-indices (rd "kasane/fixtures/lzw_idx.gif")))))
  (testing "96x40 interlaced indexed LZW (bit-exact, 4-pass de-ordering)"
    (let [exp (get-in (expected "kasane/fixtures/expected_big.edn") [:gif :indices])]
      (is (= 3840 (count exp)))
      (is (= exp (gif/first-frame-indices (rd "kasane/fixtures/lzw_big.gif")))))))
