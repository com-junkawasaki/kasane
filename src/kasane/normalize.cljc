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

(defn ->doc
  "Dispatch raw decode tree → :kasane/doc by detected format."
  [format raw]
  (case format
    :psd (psd->doc raw)
    (throw (ex-info "kasane.normalize: unsupported format" {:format format}))))
