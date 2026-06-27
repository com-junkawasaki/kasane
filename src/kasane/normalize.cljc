(ns kasane.normalize
  "Map a raw decoded format tree (kasane.decode output) onto the common
   :kasane/doc model — the cross-format layered tree from ADR-2606272100.
   Pure cljc.")

(def ^:private psd-blend->kw
  {"norm" :normal "mul " :multiply "scrn" :screen "over" :overlay
   "dark" :darken "lite" :lighten "diff" :difference "lum " :luminosity
   "hue " :hue "sat " :saturation "colr" :color "add " :linear-dodge})

(defn- psd-layer->node [idx ly]
  (let [{:keys [top left bottom right blend opacity flags]} ly]
    {:node/id      (str "L" idx)
     :node/kind    :raster
     :node/visible? (not (bit-test (or flags 0) 1))        ; bit1 set = hidden
     :node/opacity (/ (double (or opacity 255)) 255.0)
     :node/blend   (get psd-blend->kw blend :normal)
     :node/bbox    [left top (- right left) (- bottom top)]
     :node/channels (count (:chans ly))}))

(defn psd->doc
  "Raw PSD decode tree → :kasane/doc."
  [raw]
  {:kasane/format :psd
   :kasane/canvas {:width      (:width raw)
                   :height     (:height raw)
                   :unit       :px
                   :dpi        72
                   :color-mode (:mode raw)
                   :depth      (:depth raw)}
   :kasane/nodes  (vec (map-indexed psd-layer->node
                                    (get-in raw [:limask :layers] [])))
   :kasane/meta   {:version  (:ver raw)
                   :channels (:chans raw)}})

(defn- nnum [x] (if (number? x) x 0))

(defn pdf->doc
  "Parsed PDF (kasane.cos/parse output) → :kasane/doc. Pages become :page
   nodes carrying MediaBox bbox and extracted text runs."
  [parsed pages-fn text-fn]
  (let [objs  (:objects parsed)
        pgs   (pages-fn parsed)
        mb    (mapv nnum (get (first pgs) :MediaBox [0 0 0 0]))
        [x0 y0 x1 y1] mb]
    {:kasane/format :pdf
     :kasane/canvas {:width (- x1 x0) :height (- y1 y0) :unit :pt :dpi 72}
     :kasane/nodes  (vec (map-indexed
                          (fn [i pg]
                            (let [m (mapv nnum (get pg :MediaBox [0 0 0 0]))]
                              {:node/id        (str "P" i)
                               :node/kind      :page
                               :pdf.page/index i
                               :node/bbox      [(m 0) (m 1) (- (m 2) (m 0)) (- (m 3) (m 1))]
                               :text/runs      (mapv (fn [t] {:text t}) (text-fn objs pg))}))
                          pgs))
     :kasane/meta   {:pages (count pgs)}}))

(defn png->doc
  "Parsed PNG (kasane.png/parse output) → :kasane/doc. A single :raster node;
   pixels are NOT inlined — :raster/blob is a pointer (cid filled when the
   sample buffer is offloaded to B2/DataLad, per CLAUDE.md / ADR-2606272100)."
  [parsed]
  (let [{:keys [width height color-type bit-depth]} (:ihdr parsed)]
    {:kasane/format :png
     :kasane/canvas {:width width :height height :unit :px :dpi 72
                     :color-mode color-type :depth bit-depth}
     :kasane/nodes  [{:node/id      "raster"
                      :node/kind    :raster
                      :node/visible? true
                      :node/opacity 1.0
                      :node/blend   :normal
                      :node/bbox    [0 0 width height]
                      :raster/blob  {:cid nil :w width :h height :fmt :raw}}]
     :kasane/meta   {:chunks (mapv :type (:chunks parsed))}}))

(defn ->doc
  "Dispatch raw decode tree → :kasane/doc by detected format."
  [format raw]
  (case format
    :psd (psd->doc raw)
    :png (png->doc raw)
    (throw (ex-info "kasane.normalize: unsupported format (use psd->doc/pdf->doc directly)" {:format format}))))
