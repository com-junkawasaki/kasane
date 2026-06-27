(ns kasane.gif
  "GIF decode: header/LSD via the grammar engine, then a byte scan for image
   separators (0x2C) to count frames. R0 = dims + frame count + version;
   LZW pixel decode is deferred (ADR-2606272100)."
  (:require [kasane.decode :as d]))

(defn parse
  "Parse GIF `data` with `grammar` (gif.edn). Returns
   {:width :height :frames :version :global-color-table?}."
  [grammar data]
  (let [bv  (vec data)
        hdr (d/decode grammar bv)]
    (when-not (= "GIF8" (subs (:magic hdr) 0 4))
      (throw (ex-info "gif: bad signature" {:magic (:magic hdr)})))
    {:version              (:magic hdr)
     :width                (:width hdr)
     :height               (:height hdr)
     :global-color-table?  (bit-test (:flags hdr) 7)
     :frames               (count (filter #(= % 0x2C) bv))}))   ; 0x2C = Image Separator
