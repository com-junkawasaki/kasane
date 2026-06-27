(ns kasane.tiff
  "TIFF decode — the offset-based sibling of the PDF hybrid path. TIFF's IFD
   lives at a file offset and entry values may themselves be offsets, which
   doesn't fit the linear grammar engine, so this is a small hand parser
   (byte order auto-detected). R0 extracts IFD0 metadata (dims/bps/compression/
   photometric); pixel strips are left as blob pointers. PSD/DNG/camera-RAW
   share this container. See ADR-2606272100."
  (:require [kasane.bytes :as b]))

(def ^:private tag-names
  {256 :image-width 257 :image-height 258 :bits-per-sample 259 :compression
   262 :photometric 273 :strip-offsets 277 :samples-per-pixel 278 :rows-per-strip
   279 :strip-byte-counts 282 :x-resolution 283 :y-resolution 296 :resolution-unit})

(def ^:private compressions
  {1 :none 2 :ccitt-rle 3 :ccitt-g3 4 :ccitt-g4 5 :lzw 6 :jpeg-old 7 :jpeg
   8 :deflate 32773 :packbits 32946 :deflate-old})

(defn parse
  "Parse TIFF `data` → {:byte-order :width :height :bits-per-sample
   :compression :photometric :entries}."
  [data]
  (let [bv   (vec data)
        big? (= [0x4D 0x4D] (subvec bv 0 2))              ; "MM"=big, "II"=little
        u16  (fn [o] (b/uint! (b/cursor (subvec bv o (+ o 2))) 2 big?))
        u32  (fn [o] (b/uint! (b/cursor (subvec bv o (+ o 4))) 4 big?))
        ifd  (u32 4)
        n    (u16 ifd)
        entries (mapv (fn [i]
                        (let [eo   (+ ifd 2 (* i 12))
                              tag  (u16 eo)
                              typ  (u16 (+ eo 2))
                              cnt  (u32 (+ eo 4))
                              val  (if (= typ 3) (u16 (+ eo 8)) (u32 (+ eo 8)))] ; SHORT vs LONG
                          {:tag (tag-names tag tag) :type typ :count cnt :value val}))
                      (range n))
        byname (into {} (map (juxt :tag :value) entries))]
    {:byte-order      (if big? :big :little)
     :width           (:image-width byname)
     :height          (:image-height byname)
     :bits-per-sample (:bits-per-sample byname)
     :compression     (compressions (:compression byname) (:compression byname))
     :photometric     (:photometric byname)
     :entries         entries}))
