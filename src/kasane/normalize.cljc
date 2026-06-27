(ns kasane.normalize
  "Map a raw decoded format tree (kasane.decode output) onto the common
   :kasane/doc model — the cross-format layered tree from ADR-2606272100.
   Pure cljc."
  (:require [clojure.string :as str]
            [kasane.json :as json]
            [kasane.svg :as svg]))

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

(defn- raster-node [id w h extra]
  (merge {:node/id id :node/kind :raster :node/visible? true :node/opacity 1.0
          :node/blend :normal :node/bbox [0 0 w h]
          :raster/blob {:cid nil :w w :h h :fmt :raw}}
         extra))

(defn bmp->doc
  "Raw BMP decode tree → :kasane/doc (pixels left as blob pointer)."
  [raw]
  (let [w (:width raw) h (abs (:height raw))]
    {:kasane/format :bmp
     :kasane/canvas {:width w :height h :unit :px :dpi 72 :depth (:bpp raw)
                     :compression (:compression raw)}
     :kasane/nodes  [(raster-node "raster" w h {})]
     :kasane/meta   {:dib-size (:dib-size raw) :data-offset (:data-offset raw)}}))

(defn tiff->doc
  "Parsed TIFF (kasane.tiff/parse) → :kasane/doc."
  [parsed]
  (let [w (:width parsed) h (:height parsed)]
    {:kasane/format :tiff
     :kasane/canvas {:width w :height h :unit :px :dpi 72
                     :depth (:bits-per-sample parsed) :byte-order (:byte-order parsed)}
     :kasane/nodes  [(raster-node "raster" w h {})]
     :kasane/meta   {:compression (:compression parsed) :photometric (:photometric parsed)}}))

(defn gif->doc
  "Parsed GIF (kasane.gif/parse) → :kasane/doc (one :raster node per frame)."
  [parsed]
  (let [w (:width parsed) h (:height parsed)]
    {:kasane/format :gif
     :kasane/canvas {:width w :height h :unit :px :dpi 72}
     :kasane/nodes  (vec (for [i (range (:frames parsed))] (raster-node (str "F" i) w h {})))
     :kasane/meta   {:frames (:frames parsed) :version (:version parsed)}}))

(defn ai->doc
  "Parsed AI (modern .ai = PDF; kasane.cos/parse) → :kasane/doc. Pages become
   :artboard nodes."
  [parsed pages-fn text-fn]
  (-> (pdf->doc parsed pages-fn text-fn)
      (assoc :kasane/format :ai)
      (update :kasane/nodes
              (fn [nodes]
                (mapv (fn [n] (-> n
                                  (assoc :node/kind :artboard
                                         :ai.artboard/index (:pdf.page/index n))
                                  (dissoc :pdf.page/index)))
                      nodes)))))

(defn- bytes->str [bs] (apply str (map char bs)))

(def ^:private sketch-kind
  {"artboard" :artboard "symbolMaster" :artboard "group" :group
   "rectangle" :vector "oval" :vector "shapePath" :vector "polygon" :vector
   "star" :vector "triangle" :vector "shapeGroup" :group
   "text" :text "bitmap" :raster "symbolInstance" :smart-object "slice" :group})

(defn- sketch-node [layer]
  (let [f (:frame layer)]
    (cond-> {:node/kind (get sketch-kind (:_class layer) :group)
             :node/name (:name layer)}
      f                  (assoc :node/bbox [(:x f) (:y f) (:width f) (:height f)])
      (seq (:layers layer)) (assoc :node/children (mapv sketch-node (:layers layer))))))

(defn- walk-artboards [node]
  (cond
    (map? node)        (concat (when (#{"artboard" "symbolMaster"} (:_class node)) [node])
                               (mapcat walk-artboards (:layers node)))
    (sequential? node) (mapcat walk-artboards node)
    :else nil))

(defn sketch->doc
  "Sketch ZIP entries (kasane.zip/parse) → :kasane/doc. Parses pages/*.json
   (kasane.json) into a nested artboard→layer node tree (kind + frame→bbox)."
  [entries]
  (let [names (set (map :name entries))
        pages (filter #(re-find #"^pages/.*\.json$" (:name %)) entries)
        arts  (mapcat #(walk-artboards (json/parse (bytes->str (:bytes %)))) pages)]
    {:kasane/format :sketch
     :kasane/canvas {:unit :px}
     :kasane/nodes  (vec (map-indexed (fn [i a] (assoc (sketch-node a) :node/id (str "AB" i))) arts))
     :kasane/meta   {:has-document (contains? names "document.json")
                     :entries (count entries) :pages (count pages) :artboards (count arts)}}))

(defn- xml-texts
  "Extract text between <tag …>…</tag> elements (namespace-prefixed local name,
   e.g. \"w:t\"). Portable across clj/cljs ([\\s\\S] instead of dotall flag)."
  [xml tag]
  (mapv second (re-seq (re-pattern (str "<" tag "[^>]*>([\\s\\S]*?)</" tag ">")) xml)))

(defn- pptx-shapes
  "Extract <p:sp> shapes from a slide XML: position/size (EMU) + text."
  [xml]
  (for [[_ sp] (re-seq #"<p:sp>([\s\S]*?)</p:sp>" xml)]
    (let [off (re-find #"<a:off\s+x=\"(-?\d+)\"\s+y=\"(-?\d+)\"" sp)
          ext (re-find #"<a:ext\s+cx=\"(\d+)\"\s+cy=\"(\d+)\"" sp)
          txt (xml-texts sp "a:t")]
      {:bbox (when (and off ext)
               (mapv #(#?(:clj Long/parseLong :cljs js/parseInt) %)
                     [(nth off 1) (nth off 2) (nth ext 1) (nth ext 2)]))
       :text txt})))

(defn ooxml->doc
  "OOXML ZIP entries → :kasane/doc. Detects docx/xlsx/pptx and extracts text
   (Word w:t / Excel shared-strings t) or shapes with geometry (PowerPoint:
   <p:sp> a:off/a:ext in EMU + a:t text)."
  [entries]
  (let [names (set (map :name entries))
        pref? (fn [p] (some #(str/starts-with? % p) names))
        fmt   (cond (pref? "word/") :docx (pref? "ppt/") :pptx (pref? "xl/") :xlsx :else :ooxml)
        part  (fn [n] (some #(when (= (:name %) n) (bytes->str (:bytes %))) entries))
        parts-re (fn [re] (filter #(re-find re (:name %)) entries))]
    (if (= fmt :pptx)
      (let [shapes (vec (mapcat #(pptx-shapes (bytes->str (:bytes %)))
                                (parts-re #"^ppt/slides/slide\d+\.xml$")))]
        {:kasane/format :pptx
         :kasane/canvas {:unit :emu}
         :kasane/nodes  (vec (map-indexed
                              (fn [i s] (cond-> {:node/id (str "SP" i) :node/kind :group}
                                          (:bbox s)        (assoc :node/bbox (:bbox s))
                                          (seq (:text s))  (assoc :node/kind :text
                                                                  :text/runs (mapv (fn [t] {:text t}) (:text s)))))
                              shapes))
         :kasane/meta   {:entries (count entries) :shapes (count shapes)}})
      (let [texts (case fmt
                    :docx (xml-texts (or (part "word/document.xml") "") "w:t")
                    :xlsx (xml-texts (or (part "xl/sharedStrings.xml") "") "t")
                    [])]
        {:kasane/format fmt
         :kasane/canvas {:unit :pt}
         :kasane/nodes  (vec (map-indexed (fn [i t] {:node/id (str "T" i) :node/kind :text
                                                     :text/runs [{:text t}]}) texts))
         :kasane/meta   {:entries (count entries) :text-runs (count texts)}}))))

(defn ttf->doc
  "Parsed SFNT font (kasane.ttf/parse) → :kasane/doc. A font is modelled as a
   metadata document (no canvas); one node per … nothing yet (glyphs deferred)."
  [parsed]
  {:kasane/format :ttf
   :kasane/canvas {:unit :font-unit :units-per-em (:units-per-em parsed)}
   :kasane/nodes  []
   :kasane/meta   {:family (:family parsed) :glyphs (:num-glyphs parsed)
                   :tables (:tables parsed) :sfnt-version (:sfnt-version parsed)}})

(defn gltf->doc
  "Parsed glTF (kasane.gltf/parse) → :kasane/doc. glTF nodes → :mesh/:group
   nodes (name + transform matrix). 3D, so :kasane/canvas has no 2D size."
  [parsed]
  (let [g (:json parsed)]
    {:kasane/format :gltf
     :kasane/canvas {:unit :scene}
     :kasane/nodes  (vec (map-indexed
                          (fn [i n] (cond-> {:node/id (str "N" i)
                                            :node/kind (if (:mesh n) :mesh :group)}
                                      (:name n)   (assoc :node/name (:name n))
                                      (:matrix n) (assoc :node/transform (:matrix n))))
                          (:nodes g)))
     :kasane/meta   {:meshes (count (:meshes g)) :scenes (count (:scenes g))
                     :version (get-in g [:asset :version])}}))

(def ^:private svg-kind {:text :text :image :raster})

(defn svg->doc
  "SVG string → :kasane/doc. Shape elements become vector/text/raster nodes."
  [svg-str]
  (let [root (svg/root-attrs svg-str)
        els  (svg/elements svg-str)]
    {:kasane/format :svg
     :kasane/canvas {:unit :px
                     :width  (svg/parse-len (:width root))
                     :height (svg/parse-len (:height root))}
     :kasane/nodes  (vec (map-indexed
                          (fn [i {:keys [tag attrs]}]
                            {:node/id   (str "S" i)
                             :node/kind (get svg-kind tag :vector)
                             :svg/tag   tag
                             :svg/attrs attrs})
                          els))
     :kasane/meta   {:elements (count els)}}))

(defn jpeg->doc
  "Parsed JPEG (kasane.jpeg/parse) → :kasane/doc. Pixels are NOT decoded (DCT
   deferred, ADR-2606272300) — the raster node carries a blob pointer and the
   :jpeg format codec tag so the entropy-coded scan stays opaque."
  [parsed]
  (let [w (:width parsed) h (:height parsed)]
    {:kasane/format :jpeg
     :kasane/canvas {:width w :height h :unit :px :dpi 72}
     :kasane/nodes  [(raster-node "raster" w h {:raster/blob {:cid nil :w w :h h :fmt :jpeg}})]
     :kasane/meta   {:components (:components parsed) :progressive? (:progressive? parsed)}}))

(defn ->doc
  "Dispatch raw decode tree → :kasane/doc by detected format."
  [format raw]
  (case format
    :psd  (psd->doc raw)
    :png  (png->doc raw)
    :jpeg (jpeg->doc raw)
    :bmp  (bmp->doc raw)
    :tiff (tiff->doc raw)
    :gif  (gif->doc raw)
    :ttf  (ttf->doc raw)
    :gltf (gltf->doc raw)
    :svg  (svg->doc raw)
    (throw (ex-info "kasane.normalize: unsupported format (use pdf->doc/ai->doc directly)" {:format format}))))
