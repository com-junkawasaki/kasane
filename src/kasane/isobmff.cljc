(ns kasane.isobmff
  "ISO Base Media File Format (MP4/HEIF) box reader. AVIF/HEIC wrap AV1/HEVC
   intra frames in a 'meta' box tree; this walks the boxes and extracts the
   brand (ftyp) and image dimensions (ispe). Pixel decode (AV1/HEVC) is out of
   scope — carried as an opaque blob, like JPEG2000. Pure cljc. See
   ADR-2606280010 (DCT/coded-image deferral family)."
  (:require [kasane.bytes :as b]))

(defn- u32 [bv o] (b/uint! (b/cursor (subvec bv o (+ o 4))) 4 true))
(defn- u64 [bv o] (b/uint! (b/cursor (subvec bv o (+ o 8))) 8 true))
(defn- fourcc [bv o] (b/bytes->ascii (subvec bv o (+ o 4))))

(def ^:private containers
  #{"meta" "iprp" "ipco" "moov" "trak" "mdia" "minf" "stbl" "iref" "dinf" "mdat?"})

(defn- walk [bv start end acc]
  (loop [p start acc acc]
    (if (> (+ p 8) end)
      acc
      (let [sz   (u32 bv p)
            typ  (fourcc bv (+ p 4))
            boxend (cond (= sz 0) end
                         (= sz 1) (+ p (u64 bv (+ p 8)))
                         :else    (+ p sz))
            body (+ p 8)
            acc  (conj acc {:type typ :start p :end (min boxend end) :body body})]
        (if (and (contains? containers typ) (<= boxend end) (> boxend p))
          (recur boxend (walk bv (if (= typ "meta") (+ body 4) body) (min boxend end) acc))
          (recur (if (> boxend p) boxend (+ p 8)) acc))))))

(defn parse
  "Parse ISOBMFF `data` → {:brand :width :height :boxes}."
  [data]
  (let [bv  (vec data)
        all (walk bv 0 (count bv) [])
        bt  (group-by :type all)
        ftyp (first (bt "ftyp"))
        ispe (first (bt "ispe"))]
    {:brand  (when ftyp (fourcc bv (:body ftyp)))
     :width  (when ispe (u32 bv (+ (:body ispe) 4)))          ; after 4-byte version/flags
     :height (when ispe (u32 bv (+ (:body ispe) 8)))
     :boxes  (mapv :type all)}))
