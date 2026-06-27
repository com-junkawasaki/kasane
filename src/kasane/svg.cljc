(ns kasane.svg
  "SVG (XML) reader → shape elements with attributes. SVG is the vector lingua
   franca and the projection target shared with svgraph/drawingml-svg; this is
   the read side. R0 extracts top-level shape elements (rect/circle/ellipse/
   line/polygon/polyline/path/text/image) and their attributes. See
   ADR-2606272100."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn attrs
  "Parse name=\"value\" attribute pairs from an element's attribute string."
  [s]
  (into {} (map (fn [[_ k v]] [(keyword k) v])
                (re-seq #"([\w:.-]+)\s*=\s*\"([^\"]*)\"" s))))

(defn parse-len
  "Parse a leading numeric SVG length (drops units like px/pt/%)."
  [s]
  (when s
    (when-let [m (re-find #"-?[0-9]*\.?[0-9]+" s)]
      (edn/read-string (if (str/starts-with? m ".") (str "0" m) m)))))

(def ^:private shape-kind
  {"text" :text "image" :raster})                              ; everything else → :vector

(defn elements
  "Return the shape elements of an SVG string as {:tag :attrs}."
  [svg]
  (mapv (fn [[_ tag a]] {:tag (keyword tag) :attrs (attrs a)})
        (re-seq #"<(rect|circle|ellipse|line|path|text|polygon|polyline|image)\b([^>]*?)/?>" svg)))

(defn root-attrs [svg] (attrs (or (second (re-find #"<svg\b([^>]*)>" svg)) "")))
