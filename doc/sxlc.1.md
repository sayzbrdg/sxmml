% SXLC(1) | sxmml コマンドマニュアル
% Seiji Ohashi <sayzbrdg@gmail.com>

# 名前

sxlc — Scheme で記述した sxmml ソースを変換する

# 書式

    sxlc [-m mode] [-O level] [-c command] [-w] file [file ...]
    sxlc -l | -h | -v

長い形式は `--mode`、`--optimize`、`--command`、`--wsl`、`--list`、`--help`、`--version`。

# 説明

sxlc は最初の *file* を Scheme ソースとして読み込み、トラックに登録された MML から生成物を作る。複数のファイル名を渡しても処理するのは最初の一つだけ。現在の出力形式は PMD 向け MML。動作環境は Gauche 0.9.15。

ビルド・インストールの手順は [README](../README.md#最初の変換)を参照。入力には Scheme の式が含まれるため、信頼できるソースを指定する。

# オプション

**-m** *mode*, `--mode` *mode*
:   変換モードを選ぶ。PMD4 向けには `pmd`、PMD3 向けには `pmd3` を指定する。省略した場合はソース中の `(sxmml-mode pmd)` などが必要。CLI とソースの両方で指定すると、この指定が有効になり、ソース中の `sxmml-mode` は警告を表示して無視される。

**-O** *level*, `--optimize` *level*
:   整数の最適化水準。既定値は `1`。`0` 以下は通常の最適化なし、`1` は第 1 段階、`2` 以上は第 1・第 2 段階。コンパイル後のオブジェクト数を減らす処理であり、生成ファイルの文字列長を評価基準にはしない。通常の最適化を避けるには `-O 0` を指定する。品質上の制約は [issue #8](https://github.com/sayzbrdg/sxmml/issues/8)を参照。

**-c** *command*, `--command` *command*
:   生成後に *command* を起動し、出力名を後ろへ付ける。*command* の引用・エスケープは指定する側が行う。sxlc が追加する出力名は **-w** の有無にかかわらずエスケープし、空白入りの名前も一引数として渡す。後処理の標準出力は Shift_JIS として読み取る。後処理が異常終了しても、その終了状態を sxlc の失敗として扱わない。

**-w**, `--wsl`
:   **-c** と併用したとき、`wslpath -aw` で後処理へ渡す名前を Windows 形式のパスにする。`wslpath` が必要。

**-l**, `--list`
:   現在の実行環境で見つかるバックエンドモジュールを表示して終了する。内容は環境によって変わる。

**-h**, `--help`
:   使用法を表示して終了する。

**-v**, `--version`
:   パッケージのバージョンを表示して終了する。

# 入力と出力

通常は *file* の拡張子を `.mml` に替えたパスへ生成する。ソース中で `(define-metadata filename "出力先")` を指定すると、その名前が実際の生成先になる。**-c** へ渡す名前は実際の生成先と一致するが、`Translate` 表示には入力名由来の名前が残る。

**-c** で起動する後処理には、生成名を末尾の一引数として渡す。生成名はシェルの字句解析を通さないため、空白や `;` を含む名前もそのまま一引数になる。

PMD 向け生成物は CP932、改行は CRLF。変換時には出力名、モード、最適化水準を表示し、tick 数が正のトラックについて合計 tick 数を表示する。ループ位置を指定したトラックでは loop の tick 数も表示する。外部ドライバーでの演奏時間を示す値ではない。

# 例

導入済みの `sxlc` が使えるシェルで次を実行する。

```console
printf '%s\n' '(mml G "c8r8")' > first.sxl
sxlc -m pmd first.sxl
iconv -f CP932 -t UTF-8 first.mml
```

最初の変換では `first.mml` が作られ、`Track G: total: 24` と表示される。

# 終了とエラー

**-h**、**-v**、**-l** は正常終了する。入力ファイル名がなければ使用法とエラーを表示して終了値 1。モードが CLI とソースの双方で未指定ならエラーとなる。未登録トラック、不正な MML、未定義の FM 音色なども変換に失敗する。診断と終了値は[外部動作仕様](behavior-spec.md#失敗条件と境界値)を参照。

既知の問題の対応状況と詳細は [既知の課題（GitHub の issue）](https://github.com/sayzbrdg/sxmml/issues)を参照。

# 関連資料

[入門手順](getting-started.md)、[MML コマンド](sxmml-mml-manual.adoc)、[ソース中の公開機能](sxmml-reference-manual.adoc)、[配布・互換性](release-guide.md)。
