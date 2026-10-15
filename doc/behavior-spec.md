# sxmml 外部動作仕様

この文書は sxmml が外部から受け付ける入力と、生成物・表示・エラーを定める。[要求仕様](requirements.md)の R-01～R-04 との対応は末尾に示す。動作環境は Gauche 0.9.15 である。コマンド例は [README の導入手順](../README.md#最初の変換)でビルド・インストールした `sxlc` を使う。例の保存先ディレクトリは事前に作成する。

## 入力と実行順

`sxlc` は少なくとも一つの入力ファイル名を必要とする。渡された最初のファイルを Gauche の `load` で読み込み、ソース中の Scheme 式を評価する。入力ファイルの拡張子は制限しない。複数のファイル名を渡した場合、変換するのは最初の一つである。ソースは MML の文字列だけでなく、Scheme 式と `mml` / `rhythm`、定義・設定マクロを含められる。

処理順は、オプションを読む → `-m` があればモードを選ぶ → 最初の入力ファイルを読み込む → 出力形式の設定を確認する → `-O` の水準と出力先を設定する → 生成する → 出力名・処理モード・最適化水準を表示する → トラックの tick 数を表示する → `-c` があれば後処理を起動する、である。入力ファイルの読み込み中にエラーが起きれば、出力形式の確認より先にそのエラーが出る。`-m pmd` がなくても、ソース中の `(sxmml-mode pmd)` で PMD モードを選べる。両方を指定すると `-m` の指定が有効になり、ソース中の `sxmml-mode` は無視される。このとき標準エラー出力に `warning: sxmml-mode in source is ignored; mode is already set to モード名` を表示する。

PMD4 向けには `pmd`、PMD3 向けには `pmd3` を指定する。ソース内でモードを選ぶ場合、`sxlc` は起動時に利用可能なバックエンドを読み込む。各モードの出力差は「PMD 向け生成物」に示す。

## CLI

| オプション | 現行動作 |
| --- | --- |
| `-m MODE` / `--mode MODE` | 読み込み前に指定名のモード手続きを呼ぶ。未指定ならソース側でモード選択が必要 |
| `-O N` / `--optimize N` | 整数を受け、既定値は 1。0 以下は通常の最適化なし、1 は第 1 段階、2 以上は第 1・第 2 段階。0 以下でも無操作の中間データは除かれる |
| `-o FILE` / `--output FILE` | 生成物の出力先を指定する。相対パスは現在のディレクトリを基準に絶対パスにする。省略時は入力ファイル名の拡張子を `.mml` に替えたパス。ソースの読み込み後に設定するため、ソースから出力先を変更することはできない |
| `-c COMMAND` / `--command COMMAND` | 生成後、指定コマンドの後ろに出力名を付けて起動する。`COMMAND` の引用・エスケープは指定する側が行う。`COMMAND` だけを引数に分割し、sxmml が追加する出力パスは分割せず末尾の一引数として渡す。空白や `;` を含む名前も一引数になる。実行結果を Shift_JIS として読み取り、後処理の異常終了は無視する。例えば `-c false` を指定しても `sxlc` は終了値 0 となる |
| `-w` / `--wsl` | `-c` と併用すると `wslpath -aw` で後処理に渡すパスを変換する。外部コマンド `wslpath` が必要 |
| `-l` / `--list` | 実行環境で見つかる `sxmml.backend.*` モジュールを列挙する。表示件数は環境に依存する |
| `-h` / `--help` | 使用法を表示して終了値 0 |
| `-v` / `--version` | パッケージのバージョンを表示して終了値 0 |

`sxlc` は生成後に出力名・処理モード・指定した最適化水準を表示し、続けて tick 数が正のトラックについて合計 tick 数を表示する。表示する出力名は実際の生成先であり、`-c` に渡す名前と一致する。生成中にエラーが起きた場合、これらは表示されない。`L` でループを指定したトラックでは loop の tick 数も表示する。例えば `(mml G "c8L d8")` は `Track G: total: 24 / loop: 12` と表示する。この数値は sxmml が算出した値で、外部ドライバーによる演奏時間を示さない。

## MML ソースに記述できる内容

`mml TRACK EXPR ...` は、Scheme 式を評価し、得られた値を MML 文字列に組み立てて通常 MML として登録する。`rhythm TRACK EXPR ...` は同様にリズム MML として登録する。両者とも単一の登録済みトラック名、または登録済みトラック名のリストを指定できる。式の値がリストなら要素を順に文字列化できる。入力がトラックの構文に合わなければ変換は失敗する。ソース中の定義・設定として `define-track`、`sxmml-resolution`、`define-metadata`、`define-lfo`、`define-envelope`、`define-opn-voice`、`sxmml-mode` がある。定義形式と PMD 向け生成物への反映は[MML 記述者向けリファレンス](sxmml-reference-manual.adoc)を参照する。

通常 MML では、`c d e f g a b` の音符、`r` の休符、`/` のポルタメント、音長・音高の `l o > < q &`、音量・定位の `v v* ) ( p`、移調・テンポ等の `K K$ K* t T D`、制御・定義参照の `L [ : ] @ E M Mt Mf w P` を使える。音符と休符には分母指定、`%` による tick 指定、付点、`&` による長さの加算がある。解像度の既定値は 24、音長の既定の分母は 4、オクターブの初期値は 4。`(mml G "c8r8")` は既定の解像度で合計 24 tick となる。個々のコマンドの効果は対象トラックと生成モードにも依存する。

リズム MML では `@` で `b s t c h i` から一つ以上の楽器を選んで発音し、`!` で指定楽器を止める。波括弧付きの楽器指定も受ける。`r V v p ) ( l L [ : ]` も使える。`V` は全体の音量指定、`v`・`p`・`)`・`(` は楽器別の指定である。`(rhythm L "@b8r8")` は合計 24 tick となり、PMD 向けファイルにリズム行を生成する。

通常 MML とリズム MML の `%` 指定は、正の整数を十進数字で受け付ける。先頭の 0 は許され、`%01` は 1 tick、`%00012` は 12 tick となる。音符・休符・リズム発音・ポルタメントと `&` による長さの加算で同じ制約が適用される。`%0`、`%00` など値が 0 の指定、符号付き数、小数、数字の省略はエラーになる。0 tick・分母 0・不正な楽器指定の失敗例は「失敗条件と境界値」に示す。

MML オブジェクトを加工する公開マクロとして `mml-repeat`、`mml-echo`、`mml-delay` がある。`mml-echo` が受けるトラックは単一名である。`mml-delay` は音符の後の休符を利用して直前の音を再発音する。`mml-repeat` の三本文形式では第三節の本文を反復後に置く。複数トラック形では第三節の先頭で脱出対象トラックを指定する。オブジェクトの slot の直接変更では関連する文脈値が更新されない場合がある。直接編集する場合は、関連する文脈値も利用者が更新する必要がある。

## PMD 向け生成物

PMD 向けの生成では、`-o` で指定した出力先、指定がなければ入力ファイル名の拡張子を `.mml` に替えた名前を生成先に使う。`filename` メタデータは生成先の指定に使わず、指定されていれば標準エラー出力に `warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file` を表示して無視する。生成物は CP932 で書かれ、行末は CRLF。先頭に元の入力ファイル名と `PMDver4.8` または `PMDver3.3` のモード行を含む。設定されたメタデータ、音色定義、データのあるトラックの行が続く。音色定義は PMD の音色番号の順に並ぶ。同じ条件で変換した生成物はバイト単位で一致する（要求仕様の Q-03）。PMD3 形式は末尾に `0x1A` を付ける。日本語のタイトルも CP932 で出力する。

モードごとの出力を比較するには、まず `mkdir -p /tmp/sxmml-examples/pmd-output` を実行し、`/tmp/sxmml-examples/pmd-output/modes.sxl` に `(mml G "c8")` を保存する。次のコマンドは同じ `modes.mml` を上書きするため、各回の出力を別名に保存する。

| コマンド | `Processing Mode` | 生成物のモード行 | 末尾 |
| --- | --- | --- | --- |
| `sxlc -m pmd /tmp/sxmml-examples/pmd-output/modes.sxl` | `PMD` | `;; mode: PMDver4.8` | CRLF |
| `sxlc -m pmd3 /tmp/sxmml-examples/pmd-output/modes.sxl` | `PMD3` | `;; mode: PMDver3.3` | `0x1A` |

`/tmp/sxmml-examples/pmd-output/source-mode.sxl` に `(sxmml-mode pmd3)` と `(mml G "c8")` を保存し、`sxlc -m pmd /tmp/sxmml-examples/pmd-output/source-mode.sxl` を実行すると、ソース中の指定は無視されて `Processing Mode: PMD` と表示され、生成物のモード行は `;; mode: PMDver4.8` となる。`-m` を省くと、ソース中の指定により `Processing Mode: PMD3`、`;; mode: PMDver3.3` となる。

`-O` の水準によって生成行は変わる。例えば、`-O 0` の `G l%12v8v8o4crr`、`-O 1` の `G l%12v8o4crr`、`-O 2` の `G l%12v8o4cr%24` となる入力がある。通常の最適化を避けるには `-O 0` を指定する。品質上の制約は [issue #8](https://github.com/sayzbrdg/sxmml/issues/8)を参照する。

空白を含む `space name.sxl` を変換して `-c "printf '%s\\n'"` を指定すると、生成先 `space name.mml` は一引数として渡る。`semi;name.sxl` の生成先 `semi;name.mml` も同様に一引数として渡る。`-w` の有無にかかわらず、`shell-tokenize-string` で分割するのは `COMMAND` だけであり、生成名は分割しない。

出力先の指定は、`/tmp/sxmml-examples/pmd-output/output-name.sxl` に以下を保存し、`sxlc -o /tmp/sxmml-examples/pmd-output/melody.mml -c 'printf %s' /tmp/sxmml-examples/pmd-output/output-name.sxl` で確認できる。`melody.mml` が生成され、`Translate` 行、`Post process` 行、後処理のいずれにもその生成先が使われる。`-o` を省くと `output-name.mml` が生成される。

```scheme
(sxmml-mode pmd)
(mml G "c8")
```

## 失敗条件と境界値

入力ファイル名がない場合は終了値 1、Gauche の未処理例外は終了値 70 となる。

表中の Scheme 入力を試す場合は、それぞれ 1 行の `/tmp/sxmml-examples/invalid-input.sxl` に保存し、特記した条件以外は `sxlc -m pmd /tmp/sxmml-examples/invalid-input.sxl` を実行する。「モードを指定しない空ソース」は空ファイル、「未登録トラック」は `(mml G "c8r8")` だけを記したファイルを `-m` なしで読み込む。「入力ファイル名なし」は `sxlc` を実行する。

| 条件と入力 | 結果 |
| --- | --- |
| 入力ファイル名なし | `no source file specified` と使用法を表示し、終了値 1 |
| モードを指定しない空ソース | `backend is not specified by option or source file`、終了値 70 |
| 未登録トラックへ `(mml G "c8r8")` を評価 | `no defined track: G`、終了値 70。モード未指定の判定より先に失敗 |
| `(sxmml-resolution 0)` | 正の整数が必要というエラー、終了値 70 |
| `(mml G "c0")` | 0 での除算エラー、終了値 70 |
| `(rhythm L "@b0")` | 0 での除算エラー、終了値 70 |
| `(mml G "c8?")` | 未登録の `?` コマンドに関するエラー、終了値 70 |
| `(rhythm L "@x8")` | `sxmml syntax error, near "x8" on L track`、終了値 70 |
| `(mml A "@0c8")` で未定義の FM 音色 `@0` を使う | 生成中に `no instrument define`、終了値 70 |
| 通常トラック G で `(mml G "c%0")` | `ticks must be a positive integer:  0`、終了値 70。新規出力なし |
| `(rhythm L "@b%0")` | `ticks must be a positive integer:  0`、終了値 70。新規出力なし |
| `(rhythm L "r%0")` | `ticks must be a positive integer:  0`、終了値 70。新規出力なし |
| `(mml G "c8")` を `-o /tmp/sxmml-examples/no-such-dir/out.mml` を付けて変換 | `mkstemp failed: No such file or directory`、終了値 70 |
| `(mml G "c8")` を `-o /tmp/sxmml-examples` を付けて変換（出力先が既存のディレクトリ） | 一時ファイルを出力先へ改名できず `renaming` で始まるエラー、終了値 70 |

値が 0 の tick 指定は、先頭に 0 を追加しても同じエラーになる。`8&%00` など加算する部分が 0 の入力も拒否する。一方、`c%01` と `c%00012` はそれぞれ 1 tick と 12 tick で成功する。失敗の種類によっては、生成を開始した後でエラーになる。`@0` の例では生成中にエラーとなり、`Translate` 行は表示されない。

上表の失敗はいずれも、ソースの読み込み、生成、出力先への書き込みのどこで起きても、出力先の既存ファイルを変更せず、途中までの生成物や一時ファイルを残さない（要求仕様の Q-02）。生成物は出力先と同じディレクトリの一時ファイルに書き、書き終えてから出力先へ改名する。変換に失敗した場合は、出力先に残っているファイルは以前の変換の生成物であるため、後処理へ渡さず、原因を修正して再実行する。

## 使用例

次の内容を `/tmp/sxmml-examples/melody-and-rhythm.sxl` に保存する。`/tmp/sxmml-examples` は事前に作成する。

```scheme
(sxmml-mode pmd)
(define phrase (list "c8" "r8"))
(mml G phrase)
(rhythm L "@b8r8")
```

```console
sxlc /tmp/sxmml-examples/melody-and-rhythm.sxl
# => Processing Mode: PMD
# => Optimize level: 1
# => Track G: total: 24
# => Track L: total: 24
```

生成先は `/tmp/sxmml-examples/melody-and-rhythm.mml`。この例では `G l%12o4cr` を含む PMD 向けファイルができる。入力のコードは Gauche によって評価されるため、実行環境の権限で動作する。

日本語タイトルの例として、`/tmp/sxmml-examples/japanese-title.sxl` に `(define-metadata title "曲")` と `(mml G "c8")` を保存し、`sxlc -m pmd /tmp/sxmml-examples/japanese-title.sxl` を実行する。生成ファイル `japanese-title.mml` は CP932 として復号でき、`#title       曲` を含む。

## 要求との対応と既知の差

| 要求 | この仕様の該当箇所 | 現行動作と要求との差 |
| --- | --- | --- |
| R-01 柔軟な MML 記述 | 入力と実行順、MML ソース、失敗条件、使用例 | Scheme 式から MML を組み立てられるが、全ての定義マクロが式を値に受けるわけではない。tick 指定はゼロ埋めした正の整数も受け付け、0 は拒否する |
| R-02 MML オブジェクト操作 | MML ソース、[公開マクロの範囲](sxmml-reference-manual.adoc) | 複数トラックのエコーはトラックごとに `mml-echo` を呼ぶ。[issue #6](https://github.com/sayzbrdg/sxmml/issues/6) |
| R-03 自動最適化 | CLI、PMD 向け生成物、[オブジェクト数の比較](verification.md#r-03-オブジェクト数の削減) | 最適化水準によって削減処理が異なる。音楽的な意味の判定は要求仕様の Q-01 に従う |
| R-04 生成物 | 入力と実行順、PMD 向け生成物、使用例 | 現在は PMD 向け MML を生成する |

各差の現行内容・影響・対応状況は [既知の課題（GitHub の issue）](https://github.com/sayzbrdg/sxmml/issues)にまとめる。

変更時の確認手順は[検証方針](verification.md)を参照する。
