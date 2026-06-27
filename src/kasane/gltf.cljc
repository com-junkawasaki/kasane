(ns kasane.gltf
  "glTF 2.0 decode — .gltf (JSON text) or .glb (binary container). GLB is a
   tiny chunked wrapper (magic 'glTF', a JSON chunk + optional BIN chunk), so
   the JSON document reuses kasane.json and the binary buffer is left as a
   blob. Demonstrates the JSON+binary family. See ADR-2606272100."
  (:require [kasane.json :as json]
            [kasane.bytes :as b]))

(def ^:private glb-magic  0x46546C67)                          ; "glTF"
(def ^:private chunk-json 0x4E4F534A)                          ; "JSON"
(def ^:private chunk-bin  0x004E4942)                          ; "BIN\0"

(defn- u32le [bv o] (+ (nth bv o) (* 256 (nth bv (+ o 1)))
                       (* 65536 (nth bv (+ o 2))) (* 16777216 (nth bv (+ o 3)))))

(defn- parse-glb [bv]
  (loop [o 12 doc nil bin nil]
    (if (>= (+ o 8) (count bv))
      {:json doc :bin bin :binary? true}
      (let [clen  (u32le bv o)
            ctype (u32le bv (+ o 4))
            start (+ o 8)
            cdata (subvec bv start (+ start clen))]
        (cond
          (= ctype chunk-json) (recur (+ start clen) (json/parse (b/bytes->ascii cdata)) bin)
          (= ctype chunk-bin)  (recur (+ start clen) doc cdata)
          :else                (recur (+ start clen) doc bin))))))

(defn parse
  "Parse glTF `data` (auto-detects .glb binary vs .gltf JSON text)."
  [data]
  (let [bv (vec data)]
    (if (and (>= (count bv) 4) (= (u32le bv 0) glb-magic))
      (parse-glb bv)
      {:json (json/parse (b/bytes->ascii bv)) :bin nil :binary? false})))
