(ns kasane.bytes
  "Portable read cursor over a sequence of unsigned byte values (0-255).
   Pure cljc — no host interop, so the same code runs on JVM, cljs and
   (the EDN-subset of) kotoba-clj → WASM.")

(defn cursor
  "Create a read cursor over `data` (anything seqable into 0-255 ints)."
  [data]
  (let [v (vec data)]
    {:data v :len (count v) :pos (atom 0)}))

(defn pos  [c] @(:pos c))
(defn len  [c] (:len c))
(defn eof? [c] (>= @(:pos c) (:len c)))
(defn seek! [c p] (reset! (:pos c) p) c)
(defn skip! [c n] (swap! (:pos c) + n) c)

(defn u8! [c]
  (let [p @(:pos c)]
    (when (>= p (:len c)) (throw (ex-info "kasane.bytes EOF" {:pos p :len (:len c)})))
    (reset! (:pos c) (inc p))
    (nth (:data c) p)))

(defn read-bytes!
  "Read `n` raw bytes, returning a vector of unsigned ints."
  [c n]
  (let [p @(:pos c) end (+ p n)]
    (when (> end (:len c)) (throw (ex-info "kasane.bytes EOF read-bytes" {:pos p :n n :len (:len c)})))
    (reset! (:pos c) end)
    (subvec (:data c) p end)))

(defn uint!
  "Read an `n`-byte unsigned integer. `big?` selects byte order."
  [c n big?]
  (let [bs (read-bytes! c n)
        bs (if big? bs (reverse bs))]
    (reduce (fn [acc b] (+ (* acc 256) b)) 0 bs)))

(defn sint!
  "Read an `n`-byte two's-complement signed integer."
  [c n big?]
  (let [u    (uint! c n big?)
        bits (* 8 n)
        half (bit-shift-left 1 (dec bits))]
    (if (>= u half) (- u (bit-shift-left 1 bits)) u)))

(defn bytes->ascii
  "Interpret a seq of unsigned bytes as an ASCII/Latin-1 string."
  [bs]
  (apply str (map char bs)))
