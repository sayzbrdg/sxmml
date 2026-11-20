# sxmml

sxmml は、Scheme で MML を組み立て、MML オブジェクトを加工して生成物を作るトランスレータです。古い MML の記述上の制約に困っている人を主な対象とします。PMD・FMP7・NSD.Lib 向け MML と PMD バイナリを生成できます。

## 必要なもの

- Gauche 0.9.15 と開発用ヘッダ
- C コンパイラ
- make
- Git（リポジトリを取得する場合）

## 取得する版

`main` ブランチは開発中の版です。リリースした版には `v0.2.0` の形式のタグを付けています。タグの一覧は [GitHub のタグ一覧](https://github.com/sayzbrdg/sxmml/tags)か、取得したリポジトリの `git tag -l` で確認できます。

リリースした版を使う場合は、次節の `git clone` に `-b タグ名` を付けて取得します。取得済みのリポジトリでは `git checkout タグ名` で切り替えます。タグを指定すると Git は detached HEAD の状態になると表示しますが、ビルドとインストールには影響しません。

```console
git clone -b タグ名 https://github.com/sayzbrdg/sxmml.git
```

この README と `doc` の文書は、同じ版の内容を説明します。タグを指定して取得した場合は、取得したリポジトリ内の README と文書に従ってください。`sxlc -v` が表示する版数は `package.scm` の値です。`v0.2.0` は `package.scm` を更新していないため、`0.1` と表示されます。

## 最初の変換

リポジトリを取得し、そのルートでビルド・インストールしてから最初のファイルを変換します。次の `git clone` は `main` の開発中の版を取得します。手元にリポジトリがある場合は、そのルートから始めてください。変換には `make` が生成し、`make install` で配置した `sxlc` コマンドを使います。

```console
git clone https://github.com/sayzbrdg/sxmml.git
cd sxmml
gosh -V
./configure
make
make install
printf '%s\n' '(mml G "c8r8")' > first.sxl
sxlc -m pmd first.sxl
```

`make install` は、コマンドを `bindir`、Scheme モジュールを `GAUCHE_PKGLIBDIR`、共有ライブラリを `GAUCHE_PKGARCHDIR` に配置します。C 拡張は必須です。既定の配置先には書き込み権限が必要です。実際の値は生成された `Makefile` で確認してください。コマンドの配置先を変えるには `./configure --prefix=配置先` を指定します。モジュール配置先の初期値は Gauche の設定から取得され、Make の変数を含む場合があります。`--prefix` を変更した場合も、展開後の配置先が Gauche の検索先に含まれるか確認してください。

利用者が書き込める場所へインストールする場合は、上記の `./configure` から `make install` までを次に置き換えます。

```console
sxmml_prefix="$HOME/.local"
./configure --prefix="$sxmml_prefix"
make
make GAUCHE_PKGLIBDIR="$sxmml_prefix/share/sxmml" GAUCHE_PKGARCHDIR="$sxmml_prefix/lib/sxmml" install
export PATH="$sxmml_prefix/bin:$PATH"
export GAUCHE_LOAD_PATH="$sxmml_prefix/share/sxmml${GAUCHE_LOAD_PATH:+:$GAUCHE_LOAD_PATH}"
export GAUCHE_DYNLOAD_PATH="$sxmml_prefix/lib/sxmml${GAUCHE_DYNLOAD_PATH:+:$GAUCHE_DYNLOAD_PATH}"
```

この場合は新しいシェルでも `PATH`・`GAUCHE_LOAD_PATH`・`GAUCHE_DYNLOAD_PATH` を設定してから `sxlc` を使ってください。`GAUCHE_LOAD_PATH` は Gauche が Scheme モジュールを探す追加のディレクトリです。`GAUCHE_DYNLOAD_PATH` は共有ライブラリを探す追加のディレクトリです。配置の詳細は[配布・互換性](doc/release-guide.adoc#現行の生成配置)、Gauche の設定は [Gauche 0.9.15 Users’ Reference, 3.1 と 9.8](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照してください。

`first.mml` が作られ、標準出力には `Track G: total: 24` と表示されます。出力は CP932、改行は CRLF です。UTF-8 の端末では `iconv -f CP932 -t UTF-8 first.mml` で内容を読めます。ソース中の Scheme 式は Gauche によって実行されるため、信頼できる入力ファイルを指定してください。

`first.mml` の G 行は `G l%12o4cr` になります。

## FMP7 用の曲を生成する

FMP7 用のソースは `sxlc -m fmp7 song.sxl` で生成します。初期トラックはありません。例えば次の曲は、SSG の三角波を使って演奏する `song.mwi` になります。

```scheme
(define-track lead :sound-source 'ssg)
(mml lead "@{triangle1} o4c8d8e4")
```

## NSD.Lib 用の曲を生成する

`song.sxl` にトラックの音源とフレーズを書きます。

```scheme
(define-track lead :sound-source 'pulse)
(mml lead "@2 v12 o4 c8d8e4")
```

`sxlc -m nsdlib song.sxl` で UTF-8 の `song.nsd` ができます。nsc がある環境では `sxlc -m nsdlib -c 'nsc -n' song.sxl` で続けて NSF に変換できます。APU、APU＋FDS、APU＋VRC6 に対応します。

## Emacs で編集・変換・再生する

配布物の `emacs/` を読み込みパスに追加します。次は FMP7 を通常使い、PMD バイナリも確認する設定例です。ディレクトリとプログラム名は使用環境に合わせてください。

```elisp
(add-to-list 'load-path "/path/to/sxmml/emacs")
(require 'sxmml-mode)
(setq sxmml-default-mode "fmp7"
      sxmml-default-optimization 1
      sxmml-mode-settings
      '(("fmp7" :compile-options ("-c" "fmp7-compile")
                :player ("fmp7-play") :play-extension ".bin")
        ("pmd-source" :compile-options ("-c" "pmd-compile")
                      :player ("pmd-play") :play-extension ".M")
        ("pmd-binary" :player ("pmd-play"))))
```

`fmp7-compile` 等は例示用の名前です。`-c` で起動するコマンドには、変換したファイルのパスを `sxlc` が渡します。`:player` はプログラム名と引数のリストです。再生ファイルはその末尾に渡します。`:play-extension` は外部コンパイラが生成する拡張子（先頭の点を含む）に合わせます。省略すると `sxlc` の出力を直接再生します。設定表のキーには `sxlc -l` に表示されるモードの正式名を使います。

| キー | 操作 |
|---|---|
| `C-c C-m` | この曲の通常のバックエンドを選ぶ。候補以外のモード名も入力できる。 |
| `C-c C-o` | この曲の通常の最適化レベルを選ぶ。 |
| `C-c C-c` | 保存して通常の設定でコンパイルする。`C-u C-c C-c` は今回だけバックエンド・最適化レベル・両方のいずれかを変更する。 |
| `C-c C-p` | この曲で最後に成功したコンパイル結果を再生する。 |

バッファを開き直すと通常の選択は全体の既定値に戻ります。「指定なし」（既定値の `nil`）では既存の引数に従います。`0` は明示的な `-O 0` です。全モード共通の引数は `sxmml-compile-program-options` に設定します。Emacs でモードを選んだ場合、そのモードの `:compile-options` も使用します。

`sxmml-play-after-compile` を `t` にすると成功直後に自動再生します。再生先は変換結果の出力パスに基づくため、`-D` に応じてソース側で出力名を変えた場合も反映されます。コンパイルが失敗した場合、手動再生の対象は前回の成功結果です。

メジャーモードの試験は、リポジトリのルートで次のように実行します。

```sh
emacs -Q --batch -L emacs -l test/test-sxmml-mode.el -f ert-run-tests-batch-and-exit
```

## Scheme でフレーズを組み立てる

次の内容を `phrase.sxl` に保存し、`sxlc -m pmd phrase.sxl` で変換できます。

```scheme
(define notes (map (lambda (name) (string-append name "8"))
                   '("c" "d" "e" "f")))
(mml G notes "r8")
```

`mml` に文字列のリストを渡すと、要素を順につないだ MML として扱われます。MML 文字列の意味は [MML コマンドマニュアル](doc/sxmml-mml-manual.adoc)、ソース中のマクロは [MML 記述者向けリファレンス](doc/sxmml-reference-manual.adoc)を参照してください。

## 同じソースを設定値で切り替える

次を`variant.sxl`に保存すると、CLIから渡した値で音量と既定の出力ファイル名を切り替えられます。

```scheme
(case (sxmml-option-get 'variant)
  [(soft)
   (sxmml-default-output-path-set! "variant-soft.mml")
   (mml G "v8c8")]
  [(hard)
   (sxmml-default-output-path-set! "variant-hard.mml")
   (mml G "v12c8")]
  [else (mml G "c8")])
```

```console
sxlc -m pmd -D variant soft variant.sxl
sxlc -m pmd --define variant hard variant.sxl
```

名前はsymbol、値はSchemeのデータとして渡します。ソースでは通常の`if`・`case`などを使えます。出力先は`-o`の明示指定が優先します。ソースの既定値もなければ、入力名とモードから決まります。未登録の値と更新方法は[利用者定義の設定](doc/sxmml-reference-manual.adoc#利用者定義の設定)、出力先の取得・解除は[ソースの既定出力先](doc/sxmml-reference-manual.adoc#ソースの既定出力先)を参照してください。

## 最適化と出力の指定

出力形式は `-m` で選びます。例えば `-m pmd` は PMD4 向けの MML、`-m pmd-binary`（別名 `pmd4-binary`）は直接演奏に使える PMD4 バイナリを生成します。PMD3 向けには `pmd3` と `pmd3-binary` を使います。各モードの生成物と拡張子は[製品概要](doc/product-overview.adoc#機能と責任の境界)を参照してください。

`-O` で最適化レベルを指定します。既定値は `1` で、適用する処理はバックエンドによって異なります。

- PMD では `-O 0` で通常の最適化を省き、`-O 1` で連続する設定命令を整理します。`-O 2` では状態追跡と休符結合も行います。`-O 3` は命令列を反復にまとめ、`-O 4` は短い既存の反復を展開してから圧縮します。`-O 9` は整理と展開の順序を変えた候補を区間全体で探索するため、長いトラックほど時間とメモリを使います。
- FMP7 では `-O 3` から反復圧縮を行い、`-O 9` の処理は `-O 4` と同じです。
- NSD.Lib では `-O` の値によらず、トラックの音長統計を使って既定音長を選びます。

水準ごとの変換例は[入門・目的別手順](doc/getting-started.adoc#最適化水準を比較する)、オプションの詳細は [`sxlc` マニュアル](doc/sxlc.1.md)を参照してください。

## 導入と詳しい資料

現行リポジトリでは `./configure`、`make check`、`make install` が用意されています。インストール先とステージングの注意点は[配布・互換性](doc/release-guide.adoc)に記します。導入後は `sxlc` コマンドで変換します。

- [入門・目的別手順](doc/getting-started.adoc)
- [製品の目的と範囲](doc/product-overview.adoc)
- [現行の外部動作仕様](doc/behavior-spec.adoc)
- [開発者向け手順](doc/developer-guide.adoc)
- [変更履歴](NEWS.md)

## 謝辞

PMD バイナリ形式の調査では、PMD 4.8s のソースを参照し、MML コマンドと命令コードの対応を確認しました。PMD の作者である梶原正裕氏に感謝します。

## ライセンス

ライセンスは [BSD 3-Clause License](LICENSE) です。
