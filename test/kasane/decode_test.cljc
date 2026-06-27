(ns kasane.decode-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [kasane.decode :as d]
            [kasane.normalize :as norm]
            #?(:clj [clojure.java.io :as io])))

;; ---- tiny big-endian byte builders (pure) -------------------------------
(defn- u16 [n] [(bit-and (bit-shift-right n 8) 0xff) (bit-and n 0xff)])
(defn- u32 [n] [(bit-and (bit-shift-right n 24) 0xff)
                (bit-and (bit-shift-right n 16) 0xff)
                (bit-and (bit-shift-right n 8) 0xff)
                (bit-and n 0xff)])
(defn- i32 [n] (u32 (bit-and n 0xffffffff)))
(defn- ascii [s] (mapv int s))

(def psd-grammar
  (edn/read-string
   #?(:clj (slurp (io/resource "kasane/grammar/psd.edn"))
      :cljs (throw (ex-info "load grammar via build" {})))))

(defn- synthetic-psd
  "Build a minimal valid .psd: RGB 8-bit 4x3, one normal layer at (1,2)-(3,5)
   with 3 channels, opacity 200."
  []
  (let [layer (vec (concat (i32 2) (i32 1) (i32 5) (i32 3)   ; top left bottom right
                           (u16 3)                            ; nchan
                           (u16 0) (u32 0)                    ; chan 0
                           (u16 1) (u32 0)                    ; chan 1
                           (u16 2) (u32 0)                    ; chan 2
                           (ascii "8BIM") (ascii "norm")      ; blendsig blend
                           [200 0 0 0]                        ; opacity clip flags filler
                           (u32 0)))                          ; extra length 0
        layer-info (vec (concat (u16 1)                       ; layer count = 1
                                layer))
        ;; layer-and-mask block = layerinfo-len(u32) + layer-info bytes
        lam-body (vec (concat (u32 (count layer-info)) layer-info))
        header (vec (concat (ascii "8BPS") (u16 1) [0 0 0 0 0 0]
                            (u16 3)        ; channels
                            (u32 3)        ; height
                            (u32 4)        ; width
                            (u16 8)        ; depth
                            (u16 3)))      ; mode 3 = RGB
        ]
    (vec (concat header
                 (u32 0)                  ; color mode data length 0
                 (u32 0)                  ; image resources length 0
                 (u32 (count lam-body))   ; layer-and-mask length
                 lam-body
                 (u16 0)))))              ; image data: compression = raw

(deftest psd-header+layers
  (let [raw (d/decode psd-grammar (synthetic-psd))]
    (testing "header"
      (is (= "8BPS" (:sig raw)))
      (is (= 4 (:width raw)))
      (is (= 3 (:height raw)))
      (is (= 8 (:depth raw)))
      (is (= :rgb (:mode raw)))
      (is (= 3 (:chans raw))))
    (testing "layers"
      (let [layers (get-in raw [:limask :layers])]
        (is (= 1 (count layers)))
        (let [ly (first layers)]
          (is (= [1 2 3 5] [(:left ly) (:top ly) (:right ly) (:bottom ly)]))
          (is (= 3 (:nchan ly)))
          (is (= "norm" (:blend ly)))
          (is (= 200 (:opacity ly))))))))

(deftest psd-normalize
  (let [doc (norm/->doc :psd (d/decode psd-grammar (synthetic-psd)))]
    (is (= :psd (:kasane/format doc)))
    (is (= {:width 4 :height 3 :unit :px :dpi 72 :color-mode :rgb :depth 8}
           (:kasane/canvas doc)))
    (let [n (first (:kasane/nodes doc))]
      (is (= :raster (:node/kind n)))
      (is (= :normal (:node/blend n)))
      (is (= [1 2 2 3] (:node/bbox n)))                ; [left top w h]
      (is (< 0.78 (:node/opacity n) 0.79)))))          ; 200/255
