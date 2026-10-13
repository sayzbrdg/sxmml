# sxmml の既知の課題

この文書は、sxmml の利用・開発・保守に関わる既知の課題の現在の動作、影響、対応状況、再現手順、回避方法を示す。動作環境: Gauche 0.9.15。

## 一覧

| ID | 分類 | 課題 | 対応状況 |
| --- | --- | --- | --- |
| [KI-001](#ki-001) | 文書誤り | `mml-repeat` の三本文形式の展開例 | 解決済み |
| [KI-002](#ki-002) | 不具合 | `sxmml-track-clear!` の不要な引数 | 解決済み |
| [KI-003](#ki-003) | 不具合 | CLI のモード指定がソースの指定に上書きされる | 未対応 |
| [KI-004](#ki-004) | 不具合 | `filename` の生成先と `Translate` 表示が一致しない | 表示の不一致が残る |
| [KI-005](#ki-005) | 不具合 | 通常の `-c` で引用されない `;` 入り生成名がエラーになる | 空白入り生成名は対応済み、`;` の制約が残る |
| [KI-006](#ki-006) | 未実装 | `mml-echo` の複数トラック形 | 未提供 |
| [KI-007](#ki-007) | 未実装 | ビルド規則による man ページの生成・配置 | 手動手順あり、自動化は未実装 |
| [KI-008](#ki-008) | 検証不足 | 最適化前後の意味保持 Q-01 | 比較器は未実装 |
| [KI-009](#ki-009) | 検証不足 | 公開機能の入力範囲と最適化の削減効果 | 境界入力・適用範囲のテストが不足 |
| [KI-010](#ki-010) | 検証不足 | 配布アーカイブからの標準配置先への導入 | 標準配置と `man` からの参照は未検証 |
| [KI-011](#ki-011) | 検証不足 | `-w` と `-c` による Windows ツールへの接続 | Windows ツールとの接続は未検証 |
| [KI-012](#ki-012) | 入力制約 | 0 tick 入力の扱いと診断 | 解決済み |
| [KI-013](#ki-013) | 将来対応 | `define-metadata` の値の指定範囲 | 文字列リテラルに限定、拡張は保留 |
| [KI-014](#ki-014) | 利用者責任 | slot の直接編集と文脈の整合性 | 関連値の整合は利用者の責任 |
| [KI-015](#ki-015) | 入力制約 | `define-track` の空リスト入力 | 解決済み |
| [KI-016](#ki-016) | 仕様確認待ち | 異常時と再現性の品質条件 | 要求条件は未確定 |

## 再現例の前提

[README の導入手順](../README.md#最初の変換)でビルド・インストールし、その `sxlc` と Scheme モジュールを同じシェルで使う。Gauche の例も同じモジュールを読み込む。[開発手順](developer-guide.md#隔離したコピーでの確認)の一時配置も使える。Gauche は 0.9.15 を使う。

CLI の例は、次のディレクトリで実行する。生成物は CP932 であり、内容は `iconv -f CP932 -t UTF-8 ファイル名` で読める。失敗する例も含むため、例ごとに終了値と生成物を確認する。

```console
output_dir=$(mktemp -d /tmp/sxmml-output.XXXXXX)
cd "$output_dir"
```

<a id="ki-001"></a>

## KI-001 — `mml-repeat` の三本文形式

**解決済み。** 三本文形式の `body3` は反復後に置く。反復内の後半は `body2` に記述する。[三本文形式の書式と例](sxmml-reference-manual.adoc#mml-repeat)を参照する。

<a id="ki-002"></a>

## KI-002 — `sxmml-track-clear!` の不要な引数

**解決済み。** `sxmml-track-clear!` は引数なしでトラック表全体を削除する。一件の削除には `sxmml-track-delete!` を使う。[内部リファレンス](internal-reference/environment.adoc#ref-environment-sxmml-track-clear-bang)を参照する。

<a id="ki-003"></a>

## KI-003 — CLI とソースのモード優先順位

**分類・状況：不具合、未対応。** ソース内のモード指定が CLI の `-m` 指定を上書きする。CLI 側の指定を優先する製品方針と異なり、利用者が `-m` で選んだモード以外の生成物を作る。

次の入力では、`Processing Mode: PMD3` と表示し、PMD3 向けの生成物を作る。

```console
printf '%s\n' '(sxmml-mode pmd3)' '(mml G "c8")' > modes.sxl
sxlc -m pmd modes.sxl
```

**回避方法：** CLI でモードを選ぶ場合は、ソースにモード指定を置かない。

関連資料：[sxlc.in の main](../sxlc.in)、[外部仕様](behavior-spec.md#pmd-向け生成物)。

<a id="ki-004"></a>

## KI-004 — `filename` と `Translate` 表示の不一致

**分類・状況：不具合、表示の不一致が残る。** `filename` を指定すると、実際の生成先と `-c` に渡す名前は指定値になる。`Translate` は入力名から計算した出力名を表示するため、表示から生成先を特定できない。

次の入力では `custom.mml` を生成し、後処理は `<custom.mml>` を表示する。`Translate` の出力名は `named.mml` になる。

```console
printf '%s\n' '(sxmml-mode pmd)' '(define-metadata filename "custom.mml")' '(mml G "c8")' > named.sxl
sxlc -c "printf '<%s>\n'" named.sxl
```

**回避方法：** 生成先は `filename` の指定値で確認する。後処理には `-c` で渡る名前を使う。

関連資料：[sxlc.in の main](../sxlc.in)、[pmd.scm の %output-file-get](../sxmml/backend/pmd.scm)、[外部仕様](behavior-spec.md#pmd-向け生成物)。

<a id="ki-005"></a>

## KI-005 — 通常の `-c` における `;` 入り生成名

**分類・状況：不具合、`;` の制約が残る。** `-w` なしの後処理では、生成名をエスケープしてコマンド文字列に追加する。空白入り生成名は一引数で渡るが、空白など引用の対象となる文字を含まない `semi;name.mml` は引用されず、引数への分割で `Bad shell syntax` エラーになる。変換後の生成物は残り、後処理は起動しない。

次の一つ目は、絶対パスの `space name.mml` を一引数として表示する。二つ目は生成後に後処理の引数分割で失敗し、終了値 70 になる。

```console
printf '%s\n' '(mml G "c8")' > 'space name.sxl'
sxlc -m pmd -c "printf '<%s>\n'" 'space name.sxl'
printf '%s\n' '(mml G "c8")' > 'semi;name.sxl'
sxlc -m pmd -c "printf '<%s>\n'" 'semi;name.sxl'
```

**回避方法：** `-c` を使う場合は、生成名に `;` を含めない。生成名を変えられない場合は `-c` を省略し、変換後に生成名を引用して別途コマンドを実行する。`-c` のコマンド文字列自身の引用は指定する利用者が行う。

関連資料：[sxlc.in の main](../sxlc.in)、[CLI の制約](sxlc.1.md#入力と出力)。

<a id="ki-006"></a>

## KI-006 — `mml-echo` の複数トラック形

**分類・状況：未実装、未提供。** `mml-echo` は単一トラック形のみを受け付ける。トラック名のリストを渡すと、リスト全体を一つのトラック名として扱い、未登録のトラックとしてエラーになる。複数トラックに同じ echo 加工を一度に指定できない。

```console
gosh -e '(use sxmml)' -e '(define-track (G H))' -e '(mml-echo (G H) ("g" 2 1) "c8r8")'
```

**回避方法：** `(mml-echo G ("g" 2 1) "c8r8")` のように、トラックごとに単一トラック形を使う。

関連資料：[processor.scm の mml-echo](../sxmml/processor.scm)、[書式](sxmml-reference-manual.adoc#mml-echo)。

<a id="ki-007"></a>

## KI-007 — man ページ生成・配置の自動化

**分類・状況：未実装、手動手順あり。** `Makefile.in` に man の生成・インストール規則はなく、`make`・`make install` では `sxlc.1` を生成・配置しない。コマンドの配置だけでは man ページを利用できない。Pandoc が使える環境での生成・配置の自動化は将来予定である。

**再現手順：** [生成・配置](release-guide.md#現行の生成配置)に従ってビルド・インストールし、配置先の `share/man/man1/` に `sxlc.1` がないことを確認する。

**回避方法：** [Pandoc による手動生成・配置](release-guide.md#man-ページの手動生成と配置)を使う。

関連資料：[Makefile.in の all・install](../Makefile.in)、[man ページの手順](release-guide.md#man-ページの手動生成と配置)。

<a id="ki-008"></a>

## KI-008 — 最適化の意味保持 Q-01

**分類・状況：検証不足、比較器は未実装、全入力の意味保持は未実証。** Q-01 は最適化前後のオブジェクトリストを解釈した結果の意味が等しいことを要求する。既存テストにはこの比較器がなく、入力ごとの意味の変化を自動検出できない。

**検証手順：** [Q-01 の比較方法](verification.md#q-01-音楽的な意味の保持)に従い、同一入力を水準ごとに独立したプロセスで評価し、時間配置・音高・状態変更などの解釈結果を比較する。反復や LFO など解釈方法が未確定の要素は未判定として残す。オブジェクト数の削減は R-03 の評価に使い、意味保持は解釈結果で判定する。

**利用時の対処：** 通常の最適化を避ける場合は `-O 0` を指定する。最適化を使う入力は、上記手順で最適化前後の解釈結果を比較する。

関連資料：[Q-01 の要求](requirements.md#境界品質互換性)、[optimizer.scm の sxmml-optimize-tree](../sxmml/optimizer.scm)、[既存テスト](../test.scm)、[検証方針](verification.md#q-01-音楽的な意味の保持)。

<a id="ki-009"></a>

## KI-009 — 公開機能の入力範囲と最適化の削減効果

**分類・状況：検証不足、境界入力・適用範囲のテストが不足。** `test.scm` は九モジュールの binding を検査する。公開マクロの有効な引数範囲、境界値、CLI・生成物の動作、最適化の適用範囲を網羅する動作テストはない。このため、これらの機能の回帰を `make check` だけでは検出できない。

**検証手順：** `make check` を実行した上で、[要求と検証の対応](verification.md#要求と検証の対応)に従って対象機能・境界入力・期待結果を定め、個別に実行する。R-03 の削減効果は [水準別のオブジェクト数](verification.md#r-03-オブジェクト数の削減)で比較する。

**利用時の対処：** 使用する入力について、変換結果とオブジェクト操作の結果を個別に確かめる。R-01・R-02 の受け入れ対象とする入力範囲は未確定である。

関連資料：[既存テスト](../test.scm)、[要求の未決事項](requirements.md#未決事項と判断者)、[検証方針](verification.md#要求と検証の対応)。

<a id="ki-010"></a>

## KI-010 — 配布アーカイブからの標準配置先への導入

**分類・状況：検証不足、標準配置と `man` からの参照は未検証。** 配布アーカイブの展開後は、設定・ビルド・テスト・インストールが必要である。システム標準ディレクトリへの導入と `man` コマンドからの参照には、配置先の権限と実行環境の検索先が関わる。標準配置での工程を通した検証が残る。

**検証手順：** [配布・互換性](release-guide.md)に従ってアーカイブを作成・展開し、設定、`make check`、配置、配置済み `sxlc` による変換、日本語メタデータの CP932 復号、man の生成・配置・参照を順に行う。配置前に、生成された `Makefile` の配置先と Gauche の検索先を照合する。

**利用時の対処：** [一時配置の手順](release-guide.md#現行の生成配置)で、配置先と検索先を明示して起動する。標準配置への導入時は、実際の配置先でコマンドと man の参照を確かめる。

関連資料：[Makefile.in の install](../Makefile.in)、[生成・配置](release-guide.md#現行の生成配置)、[アーカイブ](release-guide.md#配布アーカイブ)、[man の配置](release-guide.md#man-ページの手動生成と配置)。

<a id="ki-011"></a>

## KI-011 — `-w` と `-c` による Windows ツールへの接続

**分類・状況：検証不足、Windows ツールとの接続は未検証。** `-w` は後処理に渡す生成名を `wslpath -aw` で Windows 形式に変換し、エスケープする。`-w` と `-c` を組み合わせるには `wslpath` が必要である。実際の Windows ツールとの接続には、ツール側が受け付けるパスと引数形式の確認が必要になる。

次の入力では、`printf` が Windows 形式の生成先を一引数として表示する。`filename` による空白入り生成名も後処理へ渡る。

```console
printf '%s\n' '(define-metadata filename "windows path.mml")' '(mml G "c8")' > windows-path.sxl
sxlc -m pmd -w -c "printf '<%s>\n'" windows-path.sxl
```

**利用時の対処：** Windows ツールを呼び出す前に、上の手順で渡るパスと引数個数を確かめ、ツールの引数形式に合わせて `-c` を指定する。Windows 形式のパスを使わない後処理では `-w` を省略する。

関連資料：[sxlc.in の main](../sxlc.in)、[CLI のオプション](sxlc.1.md#オプション)。

<a id="ki-012"></a>

## KI-012 — 0 tick 入力の扱い

**解決済み。** `%tick数` は正の整数を十進数字で指定し、先頭の 0 を許す。`%0`・`%00` など値が 0 の指定はエラーになる。[音長の書式](sxmml-mml-manual.adoc#_音長)を参照する。

<a id="ki-013"></a>

## KI-013 — `define-metadata` の値の指定範囲

**分類・状況：将来対応、拡張は保留。** `define-metadata` の値は文字列リテラルに限る。文字列を保持する Scheme 変数は受け付けないため、変数に保存した値をそのまま指定できない。指定範囲の拡張は将来方針であり、具体的な範囲と実装時期は未確定である。

次の入力は `metadata value must be a string` エラーになり、終了値 70 となる。

```console
printf '%s\n' '(define title "曲")' '(define-metadata title title)' '(mml G "c8")' > metadata-variable.sxl
sxlc -m pmd metadata-variable.sxl
```

**回避方法：** `(define-metadata title "曲")` のように文字列リテラルを指定する。

関連資料：[environment.scm の define-metadata](../sxmml/environment.scm)、[書式](sxmml-reference-manual.adoc)、[R-01 の制約と要求範囲](requirements.md#要求と現行実装の対応)。

<a id="ki-014"></a>

## KI-014 — slot の直接編集と文脈の整合性

**分類・状況：利用者責任。** 音符オブジェクトの `ticks` を直接変更しても、解析時の文脈にある `current-ticks` は自動更新されない。関連値が一致しない状態で後続処理を行うと、オブジェクトの音長と文脈の時間情報が食い違う。slot を直接編集する利用者が、関連値の整合を保つ責任を持つ。

次の例は `6 ((12 . 0))` を表示する。音符の長さは 6 tick に変わり、文脈には解析時の 12 tick が残る。

```console
gosh -e '(use sxmml)' -e '(define ctx (make <sxmml-mml-context>))' -e '(define result (call-with-values (lambda () (sxmml-compile-mml "c8" ctx)) list))' -e '(define note (car (car result)))' -e '(slot-set! note (quote ticks) 6)' -e '(print (slot-ref note (quote ticks)) " " (slot-ref ctx (quote current-ticks)))'
```

**対処方法：** slot を直接編集する場合は、関連する文脈と集計値も整合させる。音長を変えた MML を解析して新しいオブジェクトと文脈を作る方法も使える。

関連資料：[compiler.scm の sxmml-compile-mml](../sxmml/compiler.scm)、[mml.scm の $NOTE](../sxmml/mml.scm)、[R-02 の要求](requirements.md#要求と現行実装の対応)。

<a id="ki-015"></a>

## KI-015 — `define-track` の空リスト入力

**解決済み。** `(define-track ())` はエラーになる。一つ以上のトラック名を指定する。[引数と登録結果](internal-reference/environment.adoc#ref-environment-define-track)を参照する。

<a id="ki-016"></a>

## KI-016 — 異常時と再現性の品質条件

**分類・状況：仕様確認待ち、要求条件は未確定。** 失敗時の既存・部分生成物の扱いと、同一入力からの再現性に対する品質条件は未確定である。失敗時の生成物の保持・削除について、全ての失敗に共通する要求はない。同一入力の出力比較でも、一致を要求する対象と条件が未確定である。

**検証手順：** 個々の失敗条件と終了値は [外部仕様](behavior-spec.md#失敗条件と境界値)に従って再現し、終了値と生成物の有無・内容を調べる。再現性を比較する場合は、ソース、モード、定義、入力、実行環境を固定し、比較対象を明示する。生成物には入力ファイル名が含まれるため、名前も比較条件に含める。品質条件の未決範囲は [要求仕様](requirements.md#境界品質互換性)を参照する。

**利用時の対処：** 変換に失敗した場合は生成物の有無と内容を確かめ、変換が成功した出力を後続処理に使う。

関連資料：[sxlc.in の main](../sxlc.in)、[pmd.scm の %pmd-source-output](../sxmml/backend/pmd.scm)、[品質条件の未決事項](requirements.md#未決事項と判断者)。
