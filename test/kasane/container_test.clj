(ns kasane.container-test
  "ISOBMFF (AVIF/HEIC) box metadata + PDF image XObject extraction."
  (:require [clojure.test :refer [deftest is testing]]
            [kasane.isobmff :as iso]
            [kasane.normalize :as norm]
            [kasane.cos :as cos]))

;; ---- byte builders ----
(defn- u32 [n] [(bit-and (bit-shift-right n 24) 0xff) (bit-and (bit-shift-right n 16) 0xff)
                (bit-and (bit-shift-right n 8) 0xff) (bit-and n 0xff)])
(defn- ascii [s] (mapv int s))
(defn- box [type payload] (vec (concat (u32 (+ 8 (count payload))) (ascii type) payload)))

;; ---- ISOBMFF (minimal AVIF: ftyp + meta>iprp>ipco>ispe) ----
(defn- make-avif [w h]
  (let [ispe (box "ispe" (vec (concat (u32 0) (u32 w) (u32 h))))        ; version/flags + w + h
        ipco (box "ipco" ispe)
        iprp (box "iprp" ipco)
        meta (box "meta" (vec (concat (u32 0) iprp)))                   ; meta is a fullbox
        ftyp (box "ftyp" (vec (concat (ascii "avif") (u32 0) (ascii "avif"))))]
    (vec (concat ftyp meta))))

(deftest isobmff-avif
  (let [p (iso/parse (make-avif 320 240))]
    (is (= "avif" (:brand p)))
    (is (= 320 (:width p)))
    (is (= 240 (:height p)))
    (is (contains? (set (:boxes p)) "ispe")))
  (testing "normalize"
    (let [doc (norm/->doc :avif (iso/parse (make-avif 64 48)))]
      (is (= :avif (:kasane/format doc)))
      (is (= {:width 64 :height 48 :unit :px :dpi 72} (:kasane/canvas doc)))
      (is (= :avif (get-in doc [:kasane/nodes 0 :raster/blob :fmt]))))))   ; pixels opaque

;; ---- PDF image XObject (FlateDecode) ----
(defn- zlib [^bytes in]
  (let [d (java.util.zip.Deflater.) out (java.io.ByteArrayOutputStream.) buf (byte-array 4096)]
    (.setInput d in) (.finish d)
    (while (not (.finished d)) (let [k (.deflate d buf)] (.write out buf 0 k)))
    (.end d) (mapv #(bit-and (int %) 0xff) (.toByteArray out))))

(deftest pdf-image-xobject
  (let [samples [10 20 30 40]                                            ; 2x2 gray, 8bpc
        comp    (zlib (byte-array (map unchecked-byte samples)))
        ascii*  (fn [s] (mapv #(bit-and (int %) 0xff) (.getBytes ^String s "ISO-8859-1")))
        head (ascii* (str "%PDF-1.5\n"
                          "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n"
                          "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >> endobj\n"
                          "3 0 obj << /Type /Page /Parent 2 0 R /Resources << /XObject << /Im0 4 0 R >> >> >> endobj\n"
                          "4 0 obj << /Type /XObject /Subtype /Image /Width 2 /Height 2 /BitsPerComponent 8 "
                          "/ColorSpace /DeviceGray /Filter /FlateDecode /Length " (count comp) " >> stream\n"))
        tail (ascii* "\nendstream endobj\ntrailer << /Root 1 0 R >>\n%%EOF\n")
        bytes (vec (concat head comp tail))
        parsed (cos/parse bytes)
        page (first (cos/pages parsed))
        imgs (cos/page-images (:objects parsed) page)]
    (is (= 1 (count imgs)))
    (let [img (first imgs)]
      (is (= "Im0" (:name img)))
      (is (= [2 2] [(:w img) (:h img)]))
      (let [dec (cos/decode-image (:objects parsed) img)]
        (is (= :raw (:fmt dec)))
        (is (= samples (:samples dec)))))))                              ; FlateDecode → original samples
