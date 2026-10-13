# 検証方針と手順

この文書は sxmml の変更時に実施する検証手順と期待結果を示す。検証基準は Gauche 0.9.15。[要求仕様](requirements.md)、[外部動作仕様](behavior-spec.md)、[開発手順](developer-guide.md)と併せて使う。

## 検証の層

| 層 | 確認方法 | 確認する内容 |
| --- | --- | --- |
| binding | `./configure`、`make check`、`test.log` | 9モジュールの binding 検査と、`-c` の後処理へ空白・`;` 入り生成名を一引数で渡す検査、CLI とソースのモード指定の優先順位の検査。他の変換結果は別に確認する |
| 解析・中間オブジェクト | `sxmml-compile-mml`、`sxmml-compile-rhythm` と公開マクロの評価 | オブジェクト列・トラック文脈・合計 tick |
| CLI・生成物 | インストール済み `sxlc` で入力を変換 | 終了値、表示、生成先、CP932 の内容、後処理の引数 |
| 意味保持 | 最適化前後の列を解釈して比較 | 下記 Q-01 の時間配置・音高・状態変更 |
| 文書例 | README・入門・両マニュアルの例を実行 | 各例の期待結果 |

`make check` の成功時は終了値0、`test.log` の各検査が `ok`、末尾が `passed.` となる。変換例と配置の手順は[開発手順](developer-guide.md#隔離したコピーでの確認)と[配布手順](release-guide.md)を使う。

## 要求と検証の対応

| 要求 | 確認する入力と結果 |
| --- | --- |
| R-01 柔軟な MML 記述 | README の Scheme 式・文字列リスト、通常・リズム MML。正の tick 数とゼロ埋めの受理、0 の拒否 |
| R-02 MML オブジェクト操作 | [公開マクロ](sxmml-reference-manual.adoc)の反復・echo・delay の例。三本文反復は第三節の本文を反復後に置く |
| R-03 最適化の自動化 | 下記の独立プロセスで水準別にオブジェクト数を比較 |
| R-04 生成物 | PMD4 は `pmd`、PMD3 は `pmd3`。ヘッダー・改行・CP932・生成先・後処理の一引数化 |
| Q-01 音楽的意味の保持 | 下記の方法で最適化前後の解釈結果を比較 |

## R-03 オブジェクト数の削減

最適化の効果は、コンパイル後のオブジェクトリストに連結されたオブジェクト数で評価する。バックエンドの最終文字列長を削減効果の判定に使わない。同一入力を水準ごとに独立した Gauche プロセスで評価し、`sxmml-optimize-tree` が返す平坦なリストの要素数を `length` で数える。`:optimize #f` は無操作オブジェクトを除く平坦化のみを行い、通常の最適化を行わない比較基準となる。登録時の `compiled-data` の外側のリスト長は、入れ子を含み得るため比較値に使わない。

README の導入手順でインストールしたモジュールが読み込めるシェルで、次を実行する。`#f`、`#t`、`:full` は CLI の `-O 0`、`-O 1`、`-O 2` に対応する。

```console
gosh -e '(use sxmml)' -e '(define-track G)' -e '(mml G "v8v8c8r8r8")' -e '(print (length (sxmml-optimize-tree (quote G) :optimize #f)))'
gosh -e '(use sxmml)' -e '(define-track G)' -e '(mml G "v8v8c8r8r8")' -e '(print (length (sxmml-optimize-tree (quote G) :optimize #t)))'
gosh -e '(use sxmml)' -e '(define-track G)' -e '(mml G "v8v8c8r8r8")' -e '(print (length (sxmml-optimize-tree (quote G) :optimize :full)))'
```

この例では順に `5`、`4`、`3` となる。第 1 段階で重複音量指定を整理し、第 2 段階で隣接する休符を結合する。八つの休符だけの入力 `r8r8r8r8r8r8r8r8` に置き換えると `8`、`8`、`1` となる。各入力・水準で削減前後の数と対象の操作を記録する。

最適化処理は [`sxmml/optimizer.scm`](../sxmml/optimizer.scm)、`length` は [Gauche 0.9.15 Users’ Reference, 6.6.5 List accessors and modifiers](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照する。

## Q-01 音楽的な意味の保持

Q-01 は、最適化前後で、sxmml がコンパイルした後のオブジェクトリストを解釈した結果の意味が等しいことを求める。バックエンドの先にいるドライバーがどう発音するかは対象外である。`-O` 別の文字列、オブジェクト列そのもの、ファイル長やオブジェクト数の一致を意味保持の判定には使わない。

例えば、八つの `r8` と一つの `r1`、`v117 v*2` と `v119` は、表記の違いだけで不一致と判定しない。PMD 向けの生成行 `l%12o4cr` と `o4c%12r%12` も同じ考え方の例である。生成行は sxmml の入力構文ではない。

比較を行う場合は、同じ Scheme 入力を**独立した Gauche プロセス**で `-O 0`、`-O 1`、`-O 2` 相当の条件で評価し、環境・トラック文脈・最適化された列を各回で採取する。最適化処理はオブジェクトの値を更新する箇所があるため、同じオブジェクト列に水準を順番に適用して比較しない。入力、モード、解像度、トラック、Gauche バージョンを固定し、比較対象と対象外を結果に残す。現行の分岐は [`sxlc.in`](../sxlc.in) の `-O` 設定と [`sxmml/optimizer.scm`](../sxmml/optimizer.scm) の `sxmml-optimize-tree` を参照する。

まず反復・ループを含まない入力に限定し、各オブジェクトリストをトラックの文脈に沿って解釈する。音符の音高・開始 tick・長さ、休符を含む時間配置、音量の相対指定と絶対指定などの状態変更を、同じ意味なら同じ系列になるよう正規化する。最適化前後で系列と合計 tick が一致するかを比較する。反復、テンポ、ゲート、LFO、エンベロープなど、解釈方法が未確定の要素を含む入力は、単純な列比較で「一致」と判定しない。対象外または未判定として記録し、必要な解釈の根拠を調べる。

`test.scm` には、上記の解釈結果を比較する比較器と境界ケースの動作テストがない。上記の比較を行うときは、扱う要素を明示し、比較結果を入力・条件とともに記録する。

## 文書例と境界値の確認

[README の導入手順](../README.md#最初の変換)で配置した `sxlc` とモジュールを使う。各入力を別のファイルに保存し、変換前に同名の出力が存在しない状態で、終了値・生成内容・合計 tick を確認する。

| 入力・操作 | 期待結果 |
| --- | --- |
| `(mml G "c8r8")` を `-m pmd` で変換 | 終了値0、G は24 tick、`G l%12o4cr`、ヘッダー `PMDver4.8` |
| README の `phrase.sxl` | 終了値0、G は60 tick |
| 同じ入力を `-m pmd3` で変換 | ヘッダー `PMDver3.3`、末尾 `0x1A` |
| 通常音符・休符・リズム発音・リズム休符・ポルタメントで `%01`、`%00012` | 1 tick と12 tick。ゼロ埋めを除いた入力と同じ結果 |
| `8&%01`、`%01&%00012` | 合計13 tick |
| `%0`、`%00`、`8&%00` | 正の整数が必要というエラー、終了値70、新規出力なし |
| `define-track (G H)` の後に無引数の `sxmml-track-clear!` | トラック名の一覧が `()` |
| `(define-track ())` | トラック名が必要というエラー |
| `filename` に空白入り生成名を指定して `-c` で引数を表示 | 実際の生成先が一引数で渡る。`Translate` 表示は入力名由来のまま |

`-w` と後処理の組み合わせは、`wslpath` のある環境で Windows 形式のパスと引数個数を確認する。生成物を実際の Windows ツールへ渡す確認は、パス変換の確認とは別に行う。

## 未実装・残る検証

* 公開機能の境界入力は、binding 検査に加えて上記の動作検証を行う。動作テストの対応状況は [issue #9](https://github.com/sayzbrdg/sxmml/issues/9)を参照する。
* `mml-echo` の複数トラック形、`Translate` 表示の現在の動作と再現手順は、それぞれ [issue #6](https://github.com/sayzbrdg/sxmml/issues/6)・[issue #4](https://github.com/sayzbrdg/sxmml/issues/4)を参照する。
* 標準配置と man の導入、外部ツールとの接続は各環境で確認する。

## 関連資料

[`test.scm`](../test.scm)、[`Makefile.in`](../Makefile.in)、[`sxlc.in`](../sxlc.in)、[`sxmml/optimizer.scm`](../sxmml/optimizer.scm)、[Gauche 0.9.15 Users’ Reference](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)。
