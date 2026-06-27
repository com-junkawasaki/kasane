(ns kasane.cos-test
  (:require [clojure.test :refer [deftest is testing]]
            [kasane.cos :as cos]
            [kasane.normalize :as norm]))

(defn- ->ubytes [^String s] (mapv #(bit-and (int %) 0xff) (.getBytes s "ISO-8859-1")))

(def uncompressed-pdf
  (str "%PDF-1.4\n"
       "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n"
       "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 100] >> endobj\n"
       "3 0 obj << /Type /Page /Parent 2 0 R /Contents 4 0 R >> endobj\n"
       "4 0 obj << >> stream\n"
       "BT /F1 12 Tf 10 50 Td (Hello kasane) Tj 0 -14 Td (layer two) Tj ET\n"
       "endstream endobj\n"
       "trailer << /Root 1 0 R >>\n"
       "%%EOF\n"))

(deftest pdf-uncompressed
  (let [parsed (cos/parse (->ubytes uncompressed-pdf))
        pgs    (cos/pages parsed)]
    (testing "structure"
      (is (= :Catalog (:Type (:root parsed))))
      (is (= 1 (count pgs)))
      (is (= [0 0 200 100] (:MediaBox (first pgs)))))
    (testing "text extraction (between BT/ET)"
      (is (= ["Hello kasane" "layer two"] (cos/page-text (:objects parsed) (first pgs)))))))

(deftest pdf-normalize
  (let [parsed (cos/parse (->ubytes uncompressed-pdf))
        doc    (norm/pdf->doc parsed cos/pages cos/page-text)]
    (is (= :pdf (:kasane/format doc)))
    (is (= {:width 200 :height 100 :unit :pt :dpi 72} (:kasane/canvas doc)))
    (let [n (first (:kasane/nodes doc))]
      (is (= :page (:node/kind n)))
      (is (= [0 0 200 100] (:node/bbox n)))
      (is (= "Hello kasane" (-> n :text/runs first :text))))))

(deftest ai-normalize
  ;; modern .ai = PDF; ai->doc reuses the COS path, pages become artboards.
  (let [parsed (cos/parse (->ubytes uncompressed-pdf))
        doc    (norm/ai->doc parsed cos/pages cos/page-text)]
    (is (= :ai (:kasane/format doc)))
    (let [n (first (:kasane/nodes doc))]
      (is (= :artboard (:node/kind n)))
      (is (= 0 (:ai.artboard/index n)))
      (is (nil? (:pdf.page/index n)))
      (is (= "Hello kasane" (-> n :text/runs first :text))))))

;; FlateDecode content stream — proves the pure inflate path through COS.
(defn- zlib [^bytes in]
  (let [d (java.util.zip.Deflater.) out (java.io.ByteArrayOutputStream.) buf (byte-array 8192)]
    (.setInput d in) (.finish d)
    (while (not (.finished d)) (let [k (.deflate d buf)] (.write out buf 0 k)))
    (.end d) (.toByteArray out)))

(deftest pdf-flate
  (let [content   "BT (Flate works) Tj ET"
        comp      (zlib (.getBytes content "ISO-8859-1"))
        comp-u    (mapv #(bit-and (int %) 0xff) comp)
        head (->ubytes (str "%PDF-1.5\n"
                            "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n"
                            "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 300 300] >> endobj\n"
                            "3 0 obj << /Type /Page /Parent 2 0 R /Contents 4 0 R >> endobj\n"
                            "4 0 obj << /Filter /FlateDecode /Length " (count comp-u) " >> stream\n"))
        tail (->ubytes "\nendstream endobj\ntrailer << /Root 1 0 R >>\n%%EOF\n")
        bytes (vec (concat head comp-u tail))
        parsed (cos/parse bytes)
        pg (first (cos/pages parsed))]
    (is (= ["Flate works"] (cos/page-text (:objects parsed) pg)))))
