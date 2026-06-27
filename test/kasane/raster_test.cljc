(ns kasane.raster-test
  "BMP / TIFF / GIF structural decode + normalize."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            #?(:clj [clojure.java.io :as io])
            [kasane.decode :as d]
            [kasane.tiff :as tiff]
            [kasane.gif :as gif]
            [kasane.normalize :as norm]))

(defn- res [p] (edn/read-string #?(:clj (slurp (io/resource p)) :cljs (throw (ex-info "n/a" {})))))
(def bmp-grammar (res "kasane/grammar/bmp.edn"))
(def gif-grammar (res "kasane/grammar/gif.edn"))

;; little-endian builders
(defn- u16le [n] [(bit-and n 0xff) (bit-and (bit-shift-right n 8) 0xff)])
(defn- u32le [n] [(bit-and n 0xff) (bit-and (bit-shift-right n 8) 0xff)
                  (bit-and (bit-shift-right n 16) 0xff) (bit-and (bit-shift-right n 24) 0xff)])
(defn- u16be [n] [(bit-and (bit-shift-right n 8) 0xff) (bit-and n 0xff)])
(defn- u32be [n] [(bit-and (bit-shift-right n 24) 0xff) (bit-and (bit-shift-right n 16) 0xff)
                  (bit-and (bit-shift-right n 8) 0xff) (bit-and n 0xff)])
(defn- ascii [s] (mapv int s))

(deftest bmp-decode
  (let [bytes (vec (concat (ascii "BM") (u32le 70) (u32le 0) (u32le 54)   ; file hdr
                           (u32le 40) (u32le 8) (u32le 6) (u16le 1) (u16le 24) ; DIB: w=8 h=6 bpp=24
                           (u32le 0) (u32le 16) (u32le 2835) (u32le 2835) (u32le 0) (u32le 0)))
        doc   (norm/->doc :bmp (d/decode bmp-grammar bytes))]
    (is (= :bmp (:kasane/format doc)))
    (is (= {:width 8 :height 6 :unit :px :dpi 72 :depth 24 :compression :rgb} (:kasane/canvas doc)))
    (is (= [0 0 8 6] (:node/bbox (first (:kasane/nodes doc)))))))

(deftest tiff-decode
  (testing "little-endian IFD0 with SHORT/LONG values"
    (let [entry (fn [tag typ cnt val] (vec (concat (u16le tag) (u16le typ) (u32le cnt)
                                                   (if (= typ 3) (concat (u16le val) [0 0]) (u32le val)))))
          ifd   (vec (concat (u16le 4)                  ; 4 entries
                             (entry 256 4 1 640)         ; width LONG
                             (entry 257 4 1 480)         ; height LONG
                             (entry 258 3 1 8)           ; bits-per-sample SHORT
                             (entry 259 3 1 5)           ; compression SHORT = LZW
                             (u32le 0)))                 ; next IFD = 0
          bytes (vec (concat (ascii "II") (u16le 42) (u32le 8) ifd))  ; header: IFD at offset 8
          doc   (norm/->doc :tiff (tiff/parse bytes))]
      (is (= :tiff (:kasane/format doc)))
      (is (= {:width 640 :height 480 :unit :px :dpi 72 :depth 8 :byte-order :little} (:kasane/canvas doc)))
      (is (= :lzw (get-in doc [:kasane/meta :compression]))))))

(deftest tiff-big-endian
  (let [entry (fn [tag typ cnt val] (vec (concat (u16be tag) (u16be typ) (u32be cnt)
                                                 (if (= typ 3) (concat (u16be val) [0 0]) (u32be val)))))
        ifd   (vec (concat (u16be 2) (entry 256 3 1 100) (entry 257 3 1 50) (u32be 0)))
        bytes (vec (concat (ascii "MM") (u16be 42) (u32be 8) ifd))
        p     (tiff/parse bytes)]
    (is (= :big (:byte-order p)))
    (is (= 100 (:width p)))
    (is (= 50 (:height p)))))

(deftest gif-decode
  (let [bytes (vec (concat (ascii "GIF89a") (u16le 320) (u16le 240) [0xF7 0 0]
                           [0x2C] (repeat 9 0)        ; one image separator (frame 1)
                           [0x2C] (repeat 9 0)))      ; another (frame 2)
        doc   (norm/->doc :gif (gif/parse gif-grammar bytes))]
    (is (= :gif (:kasane/format doc)))
    (is (= {:width 320 :height 240 :unit :px :dpi 72} (:kasane/canvas doc)))
    (is (= 2 (get-in doc [:kasane/meta :frames])))
    (is (= 2 (count (:kasane/nodes doc))))))
