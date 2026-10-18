# sxmml 入門と目的別手順

この手順は Gauche 0.9.15 で sxmml を使い始める人向けです。最初の取得と変換は [README](../README.md#最初の変換) を参照してください。以下は導入済みの `sxlc` が使えるシェルで実行します。入力ファイルは作業用ディレクトリに保存してください。

## Scheme で音符列を作る

次を phrase.sxl に保存します。

```scheme
(define notes (map (lambda (name) (string-append name "8"))
                   '("c" "d" "e" "f")))
(mml G notes "r8")
```

```console
sxlc -m pmd phrase.sxl
iconv -f CP932 -t UTF-8 phrase.mml
```

phrase.mml が生成され、G トラックは合計 60 tick と表示されます。notes は Scheme の文字列リストで、mml は各要素を順に MML として扱います。使用できる音符と音長は [MML コマンドマニュアル](sxmml-mml-manual.adoc)を参照してください。

## リズムを加える

次を beat.sxl に保存します。

```scheme
(mml G "l8 c d e f")
(rhythm L "V10 vb8 @b8 r8")
```

```console
sxlc -m pmd beat.sxl
iconv -f CP932 -t UTF-8 beat.mml
```

G と L のトラックがそれぞれ 48 tick と 24 tick と表示されます。L は PMD モードで登録されるリズム用トラックです。@b8 はバスドラムを指定し、r8 は休符です。楽器別音量や反復などは [リズム MML](sxmml-mml-manual.adoc#rhythm-mml)を参照してください。

## 登録する MML を加工する

次を repeat.sxl に保存します。

```scheme
(mml-repeat G 2
  ((mml G "c8d8")))
```

```console
sxlc -m pmd -O 0 repeat.sxl
iconv -f CP932 -t UTF-8 repeat.mml
```

生成物には G の反復記法が含まれ、合計 48 tick と表示されます。一部のトラックだけに脱出位置を置く例や、反復後に音を加えて遅れを調整する例は、[公開機能の説明](sxmml-reference-manual.adoc#mml-repeat)を参照してください。

## 最適化水準を比較する

同じ phrase.sxl を水準別に変換できます。生成先は同じ phrase.mml なので、比較する場合は各回で別名へコピーします。

```console
sxlc -m pmd -O 0 phrase.sxl
cp phrase.mml phrase-O0.mml
sxlc -m pmd -O 1 phrase.sxl
cp phrase.mml phrase-O1.mml
sxlc -m pmd -O 2 phrase.sxl
cp phrase.mml phrase-O2.mml
```

-O 0 は通常の最適化なし、-O 1 は第 1 段階、-O 2 は第 2 段階まで、-O 3 はさらに命令列を反復で圧縮し、-O 4 は小さい既存のリピートを展開してから圧縮します（時間がかかります）。最適化はコンパイル後のオブジェクト数を減らす処理で、生成テキストの長さを評価基準にはしません。この例では生成内容を比較できます。削減効果を比較するには [オブジェクト数の比較](verification.md#r-03-オブジェクト数の削減)を使います。通常の最適化を避けるには -O 0 を指定してください。各水準の動作は [sxlc マニュアル](sxlc.1.md)を参照してください。

## 出力とエラーを確認する

PMD 向け生成物は CP932 なので、UTF-8 の端末では iconv で読めます。-m を省く場合はソース内で sxmml-mode を指定します。指定が双方にない場合はエラーです。FM トラック A～F で @0 を使う場合は、先に対応する音色を定義してください。使用可能な定義と引数は [MML 記述者向けリファレンス](sxmml-reference-manual.adoc)を参照してください。

生成先を変えるには `sxlc` の `-o` を使います。ソース内の `filename` 指定は無視されます。CLI とソースの両方でモードを指定すると、CLI 側の指定が有効になります。詳しくは[既知の制約](behavior-spec.md#要求との対応と既知の差)を参照してください。
