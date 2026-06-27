(ns kasane.png-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [kasane.png :as png]
            [kasane.normalize :as norm]))

(def grammar (edn/read-string (slurp (io/resource "kasane/grammar/png.edn"))))

(defn- u32 [n] [(bit-and (bit-shift-right n 24) 0xff) (bit-and (bit-shift-right n 16) 0xff)
                (bit-and (bit-shift-right n 8) 0xff) (bit-and n 0xff)])
(defn- ascii [s] (mapv int s))
(defn- chunk [type data] (vec (concat (u32 (count data)) (ascii type) data (u32 0)))) ; crc=0 (not validated)

(defn- zlib [bytes]
  (let [d (java.util.zip.Deflater.) out (java.io.ByteArrayOutputStream.) buf (byte-array 8192)]
    (.setInput d (byte-array (map unchecked-byte bytes))) (.finish d)
    (while (not (.finished d)) (let [k (.deflate d buf)] (.write out buf 0 k)))
    (.end d) (mapv #(bit-and (int %) 0xff) (.toByteArray out))))

(defn- make-png
  "2x2 RGB8 image. Row0: red, green. Row1: blue, white. Each row is filter-0
   prefixed (None)."
  []
  (let [ihdr (vec (concat (u32 2) (u32 2) [8 2 0 0 0]))      ; w h depth color(rgb) comp filt interlace
        scan (vec (concat [0] [255 0 0  0 255 0]             ; row0 filter None + red,green
                          [0] [0 0 255  255 255 255]))       ; row1 filter None + blue,white
        idat (zlib scan)]
    (vec (concat png/signature
                 (chunk "IHDR" ihdr)
                 (chunk "IDAT" idat)
                 (chunk "IEND" [])))))

(deftest png-decode
  (let [p (png/parse grammar (make-png))]
    (testing "IHDR"
      (is (= 2 (get-in p [:ihdr :width])))
      (is (= 2 (get-in p [:ihdr :height])))
      (is (= 8 (get-in p [:ihdr :bit-depth])))
      (is (= :rgb (get-in p [:ihdr :color-type]))))
    (testing "unfiltered pixels (filter bytes removed)"
      (is (= [255 0 0  0 255 0  0 0 255  255 255 255] (:pixels p))))))

(deftest png-filters
  ;; same image but row1 uses Up filter (2): stored = actual - above.
  (let [ihdr (vec (concat (u32 2) (u32 2) [8 2 0 0 0]))
        row0-actual [255 0 0  0 255 0]
        row1-actual [10 20 30  40 50 60]
        row1-stored (mapv (fn [a b] (bit-and (- a b) 0xff)) row1-actual row0-actual)
        scan (vec (concat [0] row0-actual [2] row1-stored))
        bytes (vec (concat png/signature
                           ((fn [t d] (vec (concat (u32 (count d)) (mapv int t) d (u32 0)))) "IHDR" ihdr)
                           ((fn [t d] (vec (concat (u32 (count d)) (mapv int t) d (u32 0)))) "IDAT" (zlib scan))
                           ((fn [t d] (vec (concat (u32 (count d)) (mapv int t) d (u32 0)))) "IEND" [])))
        p (png/parse grammar bytes)]
    (is (= (vec (concat row0-actual row1-actual)) (:pixels p)))))

(deftest png-normalize
  (let [doc (norm/->doc :png (png/parse grammar (make-png)))]
    (is (= :png (:kasane/format doc)))
    (is (= {:width 2 :height 2 :unit :px :dpi 72 :color-mode :rgb :depth 8} (:kasane/canvas doc)))
    (let [n (first (:kasane/nodes doc))]
      (is (= :raster (:node/kind n)))
      (is (= [0 0 2 2] (:node/bbox n)))
      (is (nil? (get-in n [:raster/blob :cid]))))))         ; pixels not inlined
