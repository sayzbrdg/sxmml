# sxmml 外部動作仕様

この文書は sxmml が外部から受け付ける入力と、生成物・表示・エラーを定める。[要求仕様](requirements.md)の R-01～R-04 との対応は末尾に示す。動作環境は Gauche 0.9.15 である。コマンド例は [README の導入手順](../README.md#最初の変換)でビルド・インストールした `sxlc` を使う。例の保存先ディレクトリは事前に作成する。

## 定義マクロの名前の規則

音色・LFO・エンベロープの定義名はsymbolまたは数値とし、マクロ展開時にsymbolへそろえる。名前は空でなく、MMLの名前指定に使えない空白・波括弧を含まないものに限る。数値名には整数以外も使え、大文字・小文字は保持する。

種類・メタデータキー・トラック名はsymbolとして受け取り、利用者マクロから来るidentifierも扱う。種類・トラック名は検査してquoteし、キーは元のsymbolを小文字化してquoteする。登録時にidentifierを持ち回らない。複数トラック形式も各名前を検査し、数値の特例やMML定義名の文字制限は適用しない。

違反は位置情報を取得できるフォーム構文エラーとして通知する。登録手続きの直接利用へ新しい名前制限を加えず、値の式のスコープと評価、バックエンドの種類検査は維持する。

## トラック名の列挙順

公開手続き `sxmml-track-list` は、現在登録されているトラック名をリストで返す。定義順・名前順を含め、戻り値の順序は保証しない。順序が処理結果に影響する利用側は、必要な順序に並べ替える。

この列挙順とは別に、生成物には要求仕様Q-03の再現性を要求する。バックエンドは、ハッシュ表の反復順や `sxmml-track-list` の列挙順に依存せず、出力形式に従う決まった処理順を定める。

## 入力と実行順

`sxlc` は入力ファイル名を一つだけ受け付ける。そのファイルを Gauche の `load` で読み込み、ソース中の Scheme 式を評価する。入力ファイルの拡張子は制限しない。オプションは入力ファイル名より前に書く。入力ファイル名より後ろに引数があると、ソースを読み込む前に `unexpected argument after the source file: 引数` と使用法を表示し、終了値 2 で終了する。後ろの引数が `-` で始まる場合は、`options must be placed before the source file` も表示する。`-` で始まる入力ファイル名は `--` の後に書く。ソースは MML の文字列だけでなく、Scheme 式と `mml` / `rhythm`、定義・設定マクロを含められる。

処理順は、オプションを読む → `-m` があればモードを選ぶ → 入力ファイルを読み込む → 出力形式の設定を確認する → `-O` の水準と出力先を設定する → 生成する → 出力名・処理モード・最適化水準を表示する → トラックの tick 数を表示する → `-c` があれば後処理を起動する、である。入力ファイルの読み込み中にエラーが起きれば、出力形式の確認より先にそのエラーが出る。`-m pmd` がなくても、ソース中の `(sxmml-mode pmd)` で PMD モードを選べる。両方を指定すると `-m` の指定が有効になり、ソース中の `sxmml-mode` は無視される。このとき標準エラー出力に `warning: /home/user/song.sxl:1: sxmml-mode in source is ignored; mode is already set to モード名` のように、その `sxmml-mode` の位置を示す警告を表示する。無視する `sxmml-mode` のモード名と引数は評価しないので、未定義のモード名が書かれていても変換は失敗しない。

PMD4 向けには `pmd`、PMD3 向けには `pmd3` を指定する。実験的なモードとして、PMD のバイナリを直接生成する `pmd-binary`・`pmd4-binary`（PMD4 向け）と `pmd3-binary`（PMD3 向け）がある。ソース内でモードを選ぶ場合、`sxlc` は起動時に利用可能なバックエンドを読み込む。各モードの出力差は「PMD 向け生成物」と「PMD バイナリ生成物（実験的）」に示す。

## CLI

| オプション | 現行動作 |
| --- | --- |
| `-m MODE` / `--mode MODE` | 読み込み前に指定名のモード手続きを呼ぶ。未指定ならソース側でモード選択が必要 |
| `-O N` / `--optimize N` | 整数を受け、既定値は 1。0 以下は通常の最適化なし、1 は第 1 段階、2 は第 1・第 2 段階、3 はさらに既存のリピートをひとまとめにして命令列を反復（`[ ]`・`[ : ]`）で圧縮し、4 以上は展開後の長さが 256 以下の既存のリピートを展開し、3 より広い範囲で圧縮する。3 以上は列の長さに応じて時間がかかり、4 以上は 3 よりさらにかかる。0 以下でも無操作の中間データは除かれる |
| `-o FILE` / `--output FILE` | 生成物の出力先を指定する。相対パスは現在のディレクトリを基準に絶対パスにする。省略時は入力ファイル名の拡張子を、選んだモードの拡張子（PMD 向けは `.mml`、PMD バイナリは `.m`）に替えたパス。ソースの読み込み後に設定するため、ソースから出力先を変更することはできない。出力先が入力ファイルと同じファイル（書き方の違いやシンボリックリンク経由を含む）になる場合は、ソースを上書きせず `output file is the same as the source file: パス` と表示して終了値 2 で終了する。`-o` の指定はソースを読み込む前に、既定の出力先はソースの読み込み後・生成の前に検査する |
| `-c COMMAND` / `--command COMMAND` | 生成後、指定コマンドの後ろに出力名を付けて起動する。`COMMAND` の引用・エスケープは指定する側が行う。`COMMAND` だけを引数に分割し、sxmml が追加する出力パスは分割せず末尾の一引数として渡す。空白や `;` を含む名前も一引数になる。`-w` を指定しない場合、後処理の標準出力と標準エラー出力は変換せずに `sxlc` の標準出力と標準エラー出力へそのまま出る。`COMMAND` の字句の誤り（閉じていない引用符など）は、変換の前に使用法の誤りとして検出する。後処理が 0 以外の終了値やシグナルで終了した場合は、標準エラー出力に `post process failed: exit status N` または `post process killed by signal N` と表示し、終了値 3 で終了する。変換は成功しているため生成物は残る。例えば `-c false` を指定すると `sxlc` は終了値 3 となる |
| `-w` / `--wsl` | WSL から Windows のツールを後処理に使うための指定。`-c` と併用すると `wslpath -aw` で後処理に渡すパスを変換し、後処理の標準出力と標準エラー出力を CP932 として読み取って UTF-8 に変換し、`sxlc` の標準出力に表示する。CP932 として復号できないバイト列は置換文字に置き換え、失敗にはしない。外部コマンド `wslpath` が必要。`wslpath` が失敗した場合は `post process failed: wslpath:` で始まる通知を表示し、終了値 3 で終了する |
| `-l` / `--list` | 実行環境で見つかる `sxmml.backend.*` モジュールを列挙する。表示件数は環境に依存する |
| `-h` / `--help` | 使用法を表示して終了値 0 |
| `-v` / `--version` | パッケージのバージョンを表示して終了値 0 |

`sxlc` は生成後に出力名・処理モード・指定した最適化水準を表示し、続けて tick 数が正のトラックについて合計 tick 数を表示する。表示する出力名は実際の生成先であり、`-c` に渡す名前と一致する。生成中にエラーが起きた場合、これらは表示されない。`L` でループを指定したトラックでは loop の tick 数も表示する。例えば `(mml G "c8L d8")` は `Track G: total: 24 / loop: 12` と表示する。この数値は sxmml が算出した値で、外部ドライバーによる演奏時間を示さない。

## MML ソースに記述できる内容

`mml TRACK EXPR ...` は、Scheme 式を評価し、得られた値を MML 文字列に組み立てて通常 MML として登録する。`rhythm TRACK EXPR ...` は同様にリズム MML として登録する。両者とも単一の登録済みトラック名、または登録済みトラック名のリストを指定できる。式の値がリストなら要素を順に文字列化できる。入力がトラックの構文に合わなければ変換は失敗する。ソース中の定義・設定として `define-track`、`sxmml-resolution`、`define-metadata`、`define-lfo`、`define-envelope`、`define-opn-voice`、`sxmml-mode` がある。`define-metadata` の値は式として評価する。括弧の中に文字列だけを並べる旧書式 `(define-metadata memo ("a" "b"))` は、文字列のリストとして受け付け、標準エラー出力に `warning: /home/user/song.sxl:12: (define-metadata memo ("..." ...)) is deprecated; use (list "..." ...) or '("..." ...)` のように位置を示す警告を表示する。警告は変換の成否に影響しない。定義形式と PMD 向け生成物への反映は[MML 記述者向けリファレンス](sxmml-reference-manual.adoc)を参照する。

通常 MML では、`c d e f g a b` の音符、`r` の休符、`/` のポルタメント、音長・音高の `l o > < q &`、音量・定位の `v v* ) ( p`、移調・テンポ等の `K K$ K* t T D`、制御・定義参照の `L [ : ] @ E M Mt Mf w P` を使える。音符と休符には分母指定、`%` による tick 指定、付点、`&` による長さの加算がある。解像度の既定値は 24、音長の既定の分母は 4、オクターブの初期値は 4。`(mml G "c8r8")` は既定の解像度で合計 24 tick となる。個々のコマンドの効果は対象トラックと生成モードにも依存する。

リズム MML では `@` で `b s t c h i` から一つ以上の楽器を選んで発音し、`!` で指定楽器を止める。波括弧付きの楽器指定も受ける。`r V v p ) ( l L [ : ]` も使える。`V` は全体の音量指定、`v`・`p`・`)`・`(` は楽器別の指定である。`(rhythm L "@b8r8")` は合計 24 tick となり、PMD 向けファイルにリズム行を生成する。

通常 MML とリズム MML の `%` 指定は、正の整数を十進数字で受け付ける。先頭の 0 は許され、`%01` は 1 tick、`%00012` は 12 tick となる。音符・休符・リズム発音・ポルタメントと `&` による長さの加算で同じ制約が適用される。`%0`、`%00` など値が 0 の指定、符号付き数、小数、数字の省略はエラーになる。0 tick・分母 0・不正な楽器指定の失敗例は「失敗条件と境界値」に示す。

MML オブジェクトを加工する公開マクロとして `mml-repeat`、`mml-echo`、`mml-delay` がある。`mml-echo` が受けるトラックは単一名である。`mml-delay` は音符の後の休符を利用して直前の音を再発音する。`mml-repeat` の三本文形式では第三節の本文を反復後に置く。複数トラック形では第三節の先頭で脱出対象トラックを指定する。`mml-repeat` の回数と、`mml-echo`・`mml-delay` の括弧の中の値は式として評価し、変換時に検査する。オブジェクトの slot の直接変更では関連する文脈値が更新されない場合がある。直接編集する場合は、関連する文脈値も利用者が更新する必要がある。

通常・リズム MML の反復回数は正の正確な整数とし、十進の数字列で書く。波括弧で囲んだ数字列や、ゼロ埋めした正の回数も受理する。`[ab]1` は `ab`、`[a:b]1` は `a` と同じ意味であり、`-O 0` では反復を保持する。回数0は、脱出（`:`）の有無や出力モードによらず、構文解釈時に変換の失敗とする。反復の後の既定音長・オクターブ・`K$` の移調値は、脱出のある反復では `:` の位置、脱出のない反復では本文末尾の設定を引き継ぐ。

通常・リズム共通で `L` は1トラックに1個までとし、同じトラックへの複数回の登録にまたがる2個目も拒否する。別トラックでは独立して使用できる。pmd-source・pmd-binaryは、直接渡されたIRにも複数のループ開始があれば生成を失敗させる。

### FM音色の共通登録

`define-opn-voice`と`sxmml-opn-voice-set!`は、使用・未使用にかかわらず全定義を登録時に検査する。すべて正確な整数に限り、範囲は次のとおり。

| 項目 | 受理範囲 |
| --- | --- |
| AL・FB | 0～7 |
| AR・DR・SR | 0～31 |
| RR・SL・ML | 0～15 |
| TL | 0～127 |
| KS | 0～3 |
| DT | −3～7 |
| DT2 | 0～3 |
| SEG | 0～15 |
| AMS | 0～1 |

DT=−1・−2・−3は5・6・7へ正規化し、既存の0～7は4も含め維持する。形の補完と全項目の検査が成功してから音色表を更新する。診断は `invalid voice @V1 operator(2) AR: 32 (expected an integer from 0 to 31)` のように音色名・演算子番号・項目名・指定値・必要な条件を示す。形のエラーと未知のScheme式のエラーの扱いは維持する。

手作りIRをバックエンドへ渡すことは禁止しないため、共通登録の検査で出力境界の検査を代用しない。pmd-sourceの既存検査を維持し、pmd-binaryでも出力する使用FM音色の12項目を検査する。両バックエンドともDTは0～7に限り、負数を正規化せず拒否する。出力しないDT2・SEGと未使用の手作り音色には、新しい出力時検査を加えない。パラメータの組合せ検査や手作りIR全般の保証は含めない。

### トラック名とpartの対応

トラック名とバックエンドのpartは独立した名前とし、管理用手続きが一対一対応を維持する。`define-track`と`sxmml-track-set!`はpart省略時にトラック名を使い、明示partは指定どおり使う。他のトラックが使用中のpartへの登録は値エラーとする。同名再定義は自身を重複確認から除外し、新しいcontextに置き換える。以前の設定・MML・partを引き継がず、空いている別partへの再定義では旧partを解放する。

`rename-track`と`sxmml-track-rename!`は同じcontextを保持して登録名とcontextのtrackを変更する。part・設定・IR・コンパイル状態を保持し、旧名を除去する。変更元がなければ参照エラー、変更先が登録済みなら同名への変更も含め値エラーとする。マクロの名前は評価せずsymbol／identifierを受け、手続きはsymbolを受ける。形式・名前の型に違反するマクロはフォーム構文エラーとする。取得できるフォーム位置は既知の実行時エラーにも添える。

検査に失敗した登録・名前変更手続きは既存の登録・context・IRを変更しない。複数トラック形式の`:part`指定は、登録前に `invalid define-track form: :part requires a single track` として拒否する。それ以外は従来の逐次登録を維持し、後続の失敗によって先行する成功分を取り消さない。削除・全消去後はpartを再利用できる。

contextや登録表の直接編集は引き続き可能で、検索時の重複拒否は追加しない。利用者自身が一対一対応を崩した状態では、候補選択と、それに依存する生成物の再現性を保証しない。既存の値・形式検査は維持する。

## PMD 向け生成物

PMD 向けの生成では、`-o` で指定した出力先、指定がなければ入力ファイル名の拡張子を `.mml` に替えた名前を生成先に使う。`filename` メタデータは生成先の指定に使わず、指定されていれば標準エラー出力に `warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file` を表示して無視する。生成物は CP932 で書かれ、行末は CRLF。先頭に元の入力ファイル名と `PMDver4.8` または `PMDver3.3` のモード行を含む。設定されたメタデータ、音色定義、データのあるトラックの行が続く。音色定義は、最適化レベルにかかわらず、生成する FM 音色指定から参照されるものだけを PMD の音色番号の順に出力する。実際の発音に寄与するかは追加解析しない。未使用の定義を省いても番号を詰め直さない。音色番号は、定義した全ての FM 音色に、音色の名前の辞書順（文字コードの順）で 0 から振る。音色が 256 を超える場合は、`too many voices: 257 (max 256)` のように音色の数と上限を表示して、変換の失敗とする。PMD が使うメタデータ（`title`・`composer`・`arranger`・`pcmfile`・`ppzfile`・`ppsfile`・`memo`）の値は、書くときに検査する。`memo` は文字列 1 つか文字列のリスト、それ以外は文字列に限り、それ以外の値は `invalid metadata value: title is 1 (expected a string)` のようにキーと値を表示して変換の失敗とする。PMD3 の MML（`-m pmd3`）は値をコメントとして書くので、同じく検査する。LFO・エンベロープの定義は、`M`・`E` で使うときに検査する。種類は LFO が `default`・`triangle`（同じ意味）、エンベロープが `default` に限る。値の数は LFO が 4、エンベロープが 4・5・6 で、各値は範囲の中の整数に限る。誤りは `unsupported LFO type: sw in L1 (supported: default, triangle)`、`invalid envelope values: E1 has 3 values (expected 4, 5 or 6)`、`invalid LFO L1 value 2: 300 (expected an integer from 0 to 255)` のように、定義と誤りを表示して変換の失敗とする。使わない定義と、最適化で取り除かれた命令の定義は検査しない。同じ条件で変換した生成物はバイト単位で一致する（要求仕様の Q-03）。PMD3 形式は末尾に `0x1A` を付ける。日本語のタイトルも CP932 で出力する。リズムの楽器別のパン（`p楽器 数値`）の値は、1 が右、2 が左、3 が中央で、それ以外は `invalid rhythm pan: 4 (expected an integer from 1 to 3)` のように値と範囲を表示して変換の失敗とする。PMD バイナリ生成物でも同じである。

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

### pmd-source の出力値の検査

pmd-source は、現在対応している命令・定義について、[PMDMML.MAN](https://github.com/d2lmirrors/pmd/blob/master/mc/PMDMML.MAN) から読み取れる制約を検査する。MC.EXE の受理や演奏を包括的に保証するものではなく、MC.EXE を実行して挙動を確かめることは、この検査方針の前提にしない。

FM・SSG・PCM の3区分で範囲が異なるものは分け、区分内の細かな音源・ドライバ差は最も広い範囲に合わせる。対象音源に記載がないことだけでは拒否せず、例えば SSG のパンもコマンド共通の0～3で検査する。値は正確な整数とする。

| 出力する指定 | 許容範囲 |
| --- | --- |
| 音量 | FM の `V`：0～127、SSG の `v`：0～15、PCM の `V`：0～255 |
| 通常音符・ポルタメント両端の音程 | o1c～o8b。PCM内の狭い範囲は検査しない |
| ゲートタイム `q` | 0～255 |
| デチューン `D` | −32768～32767 |
| 転調 `_`・`__` | −128～127 |
| 通常の相対音量 | 増減量 −255～255 |
| パン `p` | 0～3 |
| 出力選択 `P` | 1～3 |
| 絶対ノイズ周波数 `w` | 0～31 |
| 直接指定の Timer-B 値 `T` | 0～255。BPMからの変換時の既存検査も維持 |
| リズム全体の絶対音量 | 0～63 |
| リズム楽器別の絶対音量 | 0～31 |
| リズム楽器別の相対音量 | 増減量 −31～31 |
| 反復回数・最大同時入れ子数 | 回数1～255、深さ32段まで。既存の0回拒否は維持 |
| ポルタメントの音長 | 1～255 clock。分割・再構成やディレイ対応は追加しない |

通常音符・休符は一律255 clock以下に制限せず、既存の分割出力を維持する。FM音色定義は、出力する AL・FB・DT を0～7、AR・DR・SRを0～31、RR・SL・MLを0～15、TLを0～127、KSを0～3、AMSを0～1で検査する。出力しない DT2・SEG、未使用の音色定義、最適化で削除された命令は追加検査しない。共通登録を迂回した音色も出力値を検査し、出力時のDTは正規化済みの0～7に限る。

範囲外・非整数は、項目名・値・範囲を示して変換に失敗する。終了値と既存生成物の保護は従来どおり。pmd-binary の符号化や検査範囲はこの変更で揃えず、既存の制約を維持する。

## PMD バイナリ生成物（実験的）

`pmd-binary`・`pmd4-binary`・`pmd3-binary` は実験的な機能であり、外部コンパイラを使わずに PMD のバイナリを直接生成する。生成物が MC.EXE の出力と同じであることは保証しない（PMD 向け MML を MC.EXE でコンパイルした結果と一致するとは限らない）。正式機能とするかどうかは [issue #54](https://github.com/sayzbrdg/sxmml/issues/54)で判断する。既知の欠陥は、ラベル `pmd-binary` の付いた[開いている不具合の issue](https://github.com/sayzbrdg/sxmml/issues?q=is%3Aissue%20is%3Aopen%20label%3Apmd-binary%20label%3Abug)を参照する。

生成先は、`-o` で指定した出力先、指定がなければ入力ファイル名の拡張子を `.m` に替えた名前である。拡張子 `.m` の入力を `-o` なしで変換すると、出力先が入力ファイルと同じになるため使用法の誤り（終了値 2）となる。`filename` メタデータは PMD 向け生成物と同じく生成先の指定に使わず、警告を表示して無視する。トラックの登録（A～F が FM、G～I が SSG、J が PCM、L がリズム）も PMD 向けのモードと同じである。生成物はバイナリであり、生成先と同じディレクトリの一時ファイルに書いてから生成先へ移す。

両バックエンドで、音高の範囲外は内部の数値ではなく `invalid note: o9c (expected o1c - o8b)` のように音名で示す。ポルタメントでは `portamento start note` / `portamento end note` を区別する。BPM由来のTimer-Bの範囲違反は `invalid calculated Timer-B: 値 (from BPM 指定値; expected an integer from 下限 to 上限)` とし、計算結果であることと元の指定値を示す。換算処理の範囲は0～255、pmd-binaryの出力範囲は0～250のままとする。pmd-binaryの相対音量は `invalid FM relative volume: -128 (expected an integer from -127 to 127)` のように符号付きの指定値と範囲を示す。SSGは−15～15、PCMは−255～255とし、受理範囲と正常時の生成物は変更しない。

`pmd-binary` と `pmd4-binary` は同じ PMD4 向けの生成物を作る。PMD4 向けの生成物は、`title` などのメタデータの領域を含む。メタデータの値の検査は、PMD 向けの生成と同じである。`pmd3-binary` の生成物はこの領域を含まず、LFO のチャンネル `B`（`invalid LFO channel: B`）と、5 値・6 値のエンベロープ（`unsupported envelope values in PMD3: E6 has 6 values (expected 4)`）は変換の失敗となる。LFO・エンベロープの種類と値の検査は、PMD 向けの生成と同じである。どのモードでも、リズムの楽器別の音量（`v楽器 数値`）は 0 から 31 までで、範囲外は `invalid rhythm instrument volume: 32 (expected an integer from 0 to 31)` のように値と範囲を表示して変換の失敗とする（pmd-sourceも同じ範囲を検査する）。どのモードでも、音高（ポルタメントの開始音・終了音を含む）は o1c から o8b までを表せる。範囲外の音高は、`invalid note: o9c (expected o1c - o8b)` のように範囲外の音高と許される範囲を表示して、変換の失敗とする。255 tick を超える音符と休符は分割して書くが、ポルタメントの音長は分割できないので 255 tick までとし、超える場合は `invalid portamento ticks: 300 (expected an integer from 1 to 255)` のように音長と許される範囲を表示して、変換の失敗とする。生成物の大きさは 65535 バイトまでで、超える場合は `PMD data exceeds the size limit (65535 bytes) in track B` のように、超えた場所（トラック、`voice table`、`metadata`）を表示して変換の失敗とする。全体の大きさだけが超える場合は場所を表示しない。PMD4 向けの生成物には、L トラックを含む全トラックの最大の合計 tick と、`L` のあるトラックの最大の loop tick（`L` のあるトラックがなければ 0）を書く。合計 tick は 65535 までで、超える場合は `total ticks exceed the limit: 67200 (max 65535)` のように、合計 tick と上限を表示して変換の失敗とする。使用した FM 音色だけを、PMD の音色番号の順に出力する。音色番号の振り方と上限は PMD 向けの生成と同じである。

失敗時の扱い（既存の生成物を変更せず、途中までの生成物を残さない。要求仕様の Q-02）と、同じ条件で変換した生成物がバイト単位で一致すること（要求仕様の Q-03）は、他のモードと同じである。

`/tmp/sxmml-examples/pmd-output/modes.sxl`（内容は `(mml G "c8")`）を各モードで変換すると、次のようになる。次のコマンドは同じ `modes.m` を上書きするため、各回の出力を別名に保存する。

| コマンド | `Processing Mode` | 生成物の大きさ |
| --- | --- | --- |
| `sxlc -m pmd-binary /tmp/sxmml-examples/pmd-output/modes.sxl` | `pmd-binary` | 80 バイト |
| `sxlc -m pmd4-binary /tmp/sxmml-examples/pmd-output/modes.sxl` | `pmd4-binary` | 80 バイト（`pmd-binary` と同じ内容） |
| `sxlc -m pmd3-binary /tmp/sxmml-examples/pmd-output/modes.sxl` | `pmd3-binary` | 43 バイト |

## 失敗条件と境界値

`sxlc` の終了値は次のとおりである（[issue #37](https://github.com/sayzbrdg/sxmml/issues/37)）。ソース中の Scheme 式が `exit` を呼んだ場合は、その値で終了する。

| 終了値 | 意味 |
| --- | --- |
| 0 | 成功。`-c` を指定した場合は後処理も成功 |
| 1 | 変換の失敗。既知の入力エラーは `sxlc: error: [ファイル:行: ]本文[ on トラック track]` の形で標準エラー出力へ表示する。位置とトラックは取得できる文脈だけを使い、Gaucheの例外見出しやスタックトレースを付けない。未知のGaucheエラー、I/O、内部IR・最適化の整合性検査の失敗は、Gaucheの標準報告とスタックトレースを維持する |
| 2 | 使用法の誤り。不正なオプションや引数、入力ファイル名なし、`-m` のモードを選べない、`-c` の字句の誤り、出力先が入力ファイルと同じ、入力ファイル名より後ろの引数。誤りの内容を標準エラー出力に、使用法を標準出力に表示する |
| 3 | 後処理の失敗。`-c` の後処理が 0 以外の終了値やシグナルで終了した、起動できなかった、`-w` の `wslpath` が失敗した |

表中の Scheme 入力を試す場合は、それぞれ 1 行の `/tmp/sxmml-examples/invalid-input.sxl` に保存し、特記した条件以外は `sxlc -m pmd /tmp/sxmml-examples/invalid-input.sxl` を実行する。「モードを指定しない空ソース」は空ファイル、「未登録トラック」は `(mml G "c8r8")` だけを記したファイルを `-m` なしで読み込む。「入力ファイル名なし」は `sxlc` を実行する。

既知の入力エラーの結果欄は本文を示す。CLIは共通の見出しと取得済みの位置・トラックを付ける。例えば通常MMLの未知の文字は `sxlc: error: unknown MML command: "x" on G track` となる。

| 条件と入力 | 結果 |
| --- | --- |
| 入力ファイル名なし | `no source file specified` と使用法を表示し、終了値 2 |
| `/tmp/sxmml-examples/invalid-input.sxl` を `-o /tmp/sxmml-examples/invalid-input.sxl` を付けて変換 | `output file is the same as the source file: /tmp/sxmml-examples/invalid-input.sxl` と使用法を表示し、終了値 2。ソースは変更しない |
| `(mml G "c8")` を `/tmp/sxmml-examples/same.mml` に保存して `-o` なしで変換 | 既定の出力先が入力と同じになるため `output file is the same as the source file: /tmp/sxmml-examples/same.mml` と使用法を表示し、終了値 2。ソースは変更しない |
| `sxlc -m pmd /tmp/sxmml-examples/invalid-input.sxl -o /tmp/sxmml-examples/out.mml` のように入力ファイル名の後ろにオプションを書く | `unexpected argument after the source file: -o`、`options must be placed before the source file` と使用法を表示し、終了値 2。生成物は作らない |
| `-m nosuch` で存在しないモードを指定 | `invalid mode: nosuch: unbound variable: nosuch` と使用法を表示し、終了値 2 |
| モードを指定しない空ソース | `backend is not specified by option or source file`、終了値 1 |
| 未登録トラックへ `(mml G "c8r8")` を評価 | `undefined track: G`、終了値 1。モード未指定の判定より先に失敗 |
| `(sxmml-resolution 0)` | 正の整数が必要というエラー、終了値 1 |
| `(mml G "c0")` | `invalid note length: 0 (expected an integer >= 1)`、終了値 1 |
| `(rhythm L "@b0")` | `invalid note length: 0 (expected an integer >= 1)`、終了値 1 |
| `(mml G "c8x")` | `unknown MML command: "x"`、終了値 1 |
| `(rhythm L "@b8x")` | `unknown MML command: "x"`、終了値 1 |
| `(rhythm L "@x8")` | `invalid MML syntax near "x8"` とトラック情報、終了値 1 |
| `(mml G "[ab]0")`・`(mml G "[a:b]0")`・`(rhythm L "[@b8:@s8]0")` のように反復回数0を書く | `invalid repeat count:` と指定値、終了値 1。波括弧付き・ゼロ埋めの0も同じ |
| `(mml G "]1")` のように開始命令のない反復終端を書く | `repeat end appeared before repeat start`、終了値 1 |
| `(mml G "c8Ld8Le8")` や `(rhythm L "@b8L@s8L@h8")` のように同じトラックに `L` を複数書く | `loop command can be used only once in a track`、終了値 1。複数回の登録にまたがる2個目も同じ |
| `(mml G "c8[d8 L e8]2")` や `(rhythm L "@b8[@s8 L @b8]2")` のように反復の中に `L` を書く | `loop command cannot be used inside repeats`、終了値 1。入れ子の内側、`:` の後ろ、`mml-repeat` の本文、複数の `mml` にまたがる反復の中も同じ |
| `(mml A "@0c8")` で未定義の FM 音色 `@0` を使う | 生成中に `undefined voice`、終了値 1 |
| 通常トラック G で `(mml G "c%0")` | `invalid tick length: 0 (expected a positive exact integer)`、終了値 1。新規出力なし |
| `(rhythm L "@b%0")` | `invalid tick length: 0 (expected a positive exact integer)`、終了値 1。新規出力なし |
| `(rhythm L "r%0")` | `invalid tick length: 0 (expected a positive exact integer)`、終了値 1。新規出力なし |
| `(mml G "c8")` を `-o /tmp/sxmml-examples/no-such-dir/out.mml` を付けて変換 | `mkstemp failed: No such file or directory`、終了値 1 |
| `(mml G "c8")` を `-o /tmp/sxmml-examples` を付けて変換（出力先が既存のディレクトリ） | 一時ファイルを出力先へ改名できず `renaming` で始まるエラー、終了値 1 |
| `(mml G "c8")` を `-c false` を付けて変換 | 生成後に `post process failed: exit status 1`、終了値 3。生成物は残る |

値が 0 の tick 指定は、先頭に 0 を追加しても同じエラーになる。`8&%00` など加算する部分が 0 の入力も拒否する。一方、`c%01` と `c%00012` はそれぞれ 1 tick と 12 tick で成功する。失敗の種類によっては、生成を開始した後でエラーになる。`@0` の例では生成中にエラーとなり、`Translate` 行は表示されない。

上表の失敗のうち終了値 1 と 2 のものはいずれも、ソースの読み込み、生成、出力先への書き込みのどこで起きても、出力先の既存ファイルを変更せず、途中までの生成物や一時ファイルを残さない（要求仕様の Q-02）。生成物は出力先と同じディレクトリの一時ファイルに書き、書き終えてから出力先へ改名する。変換に失敗した場合は、出力先に残っているファイルは以前の変換の生成物であるため、後処理へ渡さず、原因を修正して再実行する。

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
| R-04 生成物 | 入力と実行順、PMD 向け生成物、PMD バイナリ生成物（実験的）、使用例 | 現在は PMD 向け MML と、実験的な PMD バイナリを生成する。PMD バイナリは MC.EXE の出力と同じであることを保証しない。[issue #54](https://github.com/sayzbrdg/sxmml/issues/54) |

各差の現行内容・影響・対応状況は [既知の課題（GitHub の issue）](https://github.com/sayzbrdg/sxmml/issues)にまとめる。

変更時の確認手順は[検証方針](verification.md)を参照する。
