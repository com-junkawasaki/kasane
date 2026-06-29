# kasane（重ね）

[![CI](https://github.com/kotoba-lang/kasane/actions/workflows/ci.yml/badge.svg)](https://github.com/kotoba-lang/kasane/actions/workflows/ci.yml)

Adobe(AI/PSD/PDF) を **外部依存ゼロの純 cljc + EDN データ駆動文法**で扱うライブラリ。
ImageMagick / psd-tools / MuPDF / pdfium に依存しない。binary 文法を EDN で宣言し、
小さな純 cljc エンジンで解析する（Kaitai Struct を EDN データ + Clojure 解釈で再構成）。

SSoT: `90-docs/adr/2606272100-adobe-edn-kasane.md`（superproject 側）

## 構成（全 cljc・コアは外部依存ゼロ）

| ns | 役割 | WASM(kotoba-clj) |
|---|---|---|
| `kasane.bytes` | byte cursor + read primitive（u8/u16/u32/i*・endian・固定長） | ○ |
| `kasane.decode` | **EDN 文法を解釈する純エンジン** `(decode grammar bytes)→EDN` | ○ |
| `kasane.codec` | PackBits + コーデック dispatch | ○ |
| `kasane.codec.inflate` | **純 cljc DEFLATE/zlib**（PSD-ZIP と PDF-Flate を 1 本で） | ○ |
| `kasane.normalize` | raw → 共通 `:kasane/doc` モデル | ○ |
| `kasane.schema` | **malli = 共通モデルの SSoT**（検証/生成。WASM 経路外） | ✕ |
| `resources/kasane/grammar/*.edn` | **フォーマット文法（データ）** — 追加はコードでなく EDN | data |

## 使い方

```clojure
(require '[clojure.edn :as edn] '[clojure.java.io :as io]
         '[kasane.decode :as d] '[kasane.normalize :as norm])

(let [g   (edn/read-string (slurp (io/resource "kasane/grammar/psd.edn")))
      raw (d/decode g (slurp-bytes "design.psd"))]   ; bytes = seq of 0-255
  (norm/->doc :psd raw))
;; => #:kasane{:format :psd
;;             :canvas {:width 4 :height 3 :unit :px :color-mode :rgb ...}
;;             :nodes [#:node{:kind :raster :blend :normal :bbox [1 2 2 3] ...}]}
```

EDN 文法は宣言的データ:

```clojure
{:meta  {:endian :big :root :psd-file}
 :enums {:color-mode {3 :rgb 4 :cmyk ...}}
 :types {:psd-file [{:id :sig :type :magic :value "8BPS"}
                    {:id :width :type :u32} ...]}}
```

## テスト

```bash
bb test            # 純 cljc スイート（外部依存なし・速い）
clojure -X:test    # JVM test-runner
```

`inflate_test` のみ、fixture 生成に JVM の `java.util.zip.Deflater` を使う
（**ライブラリ本体は java.util.zip を一切使わない** — 検証用の既知圧縮入力を作るだけ）。

## R0 スコープ / 既知の限界（ADR-2606272100）

- **PSD**: ヘッダ・color-mode/image-resource blob・レイヤレコードまで。channel image data は
  layer-and-mask の length で skip。
- **PDF**: 未実装（`kasane.cos` で COS/xref/trailer/object-stream を別途。`.ai` は相乗り）。
- raster ピクセル実体は EDN/git にインラインせず B2+DataLad の CID 参照（CLAUDE.md 規律）。
- DCTDecode(JPEG)/JPXDecode は復号せず opaque blob で通す（純 cljc JPEG は別 ADR）。
- WASM 化は kotoba-clj の EDN-subset 成熟に追随（`decode`/`inflate` から段階適用）。

## manifest 連結（未了 / 要 remote）

west project への登録は remote リポジトリ作成が前提のため未実施。手順:
`manifest/repos.edn` へ `com-junkawasaki/kasane` を反映 → `bb scripts/gen-west-manifest.bb`。
