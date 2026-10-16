# sxmml

sxmml は、Scheme で MML を組み立て、MML オブジェクトを加工して生成物を作るトランスレータです。古い MML の記述上の制約に困っている人を主な対象とします。現行実装が出力するのは PMD 向け MML です。

## 必要なもの

- Gauche 0.9.15
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

リポジトリを取得し、そのルートでビルド・インストールしてから最初のファイルを変換します。次の `git clone` は `main` の開発中の版を取得します。手元にリポジトリがある場合は、そのルートから始めてください。`sxlc.in` は生成用テンプレートです。変換には `make` が生成し、`make install` で配置した `sxlc` コマンドを使います。

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

`make install` は、コマンドを `bindir`、Scheme モジュールを `GAUCHE_PKGLIBDIR` に配置します。既定の配置先には書き込み権限が必要です。実際の値は生成された `Makefile` で確認してください。コマンドの配置先を変えるには `./configure --prefix=配置先` を指定します。モジュール配置先の初期値は Gauche の設定から取得され、Make の変数を含む場合があります。`--prefix` を変更した場合も、展開後の配置先が Gauche の検索先に含まれるか確認してください。

利用者が書き込める場所へインストールする場合は、上記の `./configure` から `make install` までを次に置き換えます。

```console
sxmml_prefix="$HOME/.local"
./configure --prefix="$sxmml_prefix"
make
make GAUCHE_PKGLIBDIR="$sxmml_prefix/share/sxmml" install
export PATH="$sxmml_prefix/bin:$PATH"
export GAUCHE_LOAD_PATH="$sxmml_prefix/share/sxmml${GAUCHE_LOAD_PATH:+:$GAUCHE_LOAD_PATH}"
```

この場合は新しいシェルでも `PATH` と `GAUCHE_LOAD_PATH` を設定してから `sxlc` を使ってください。`GAUCHE_LOAD_PATH` は Gauche が Scheme モジュールを探す追加のディレクトリです。配置の詳細は[配布・互換性](doc/release-guide.md#現行の生成配置)、Gauche の設定は [Gauche 0.9.15 Users’ Reference, 3.1 と 9.8](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照してください。

`first.mml` が作られ、標準出力には `Track G: total: 24` と表示されます。出力は CP932、改行は CRLF です。UTF-8 の端末では `iconv -f CP932 -t UTF-8 first.mml` で内容を読めます。ソース中の Scheme 式は Gauche によって実行されるため、信頼できる入力ファイルを指定してください。

`first.mml` の G 行は `G l%12o4cr` になります。

## Scheme でフレーズを組み立てる

次の内容を `phrase.sxl` に保存し、`sxlc -m pmd phrase.sxl` で変換できます。

```scheme
(define notes (map (lambda (name) (string-append name "8"))
                   '("c" "d" "e" "f")))
(mml G notes "r8")
```

`mml` に文字列のリストを渡すと、要素を順につないだ MML として扱われます。MML 文字列の意味は [MML コマンドマニュアル](doc/sxmml-mml-manual.adoc)、ソース中のマクロは [MML 記述者向けリファレンス](doc/sxmml-reference-manual.adoc)を参照してください。

## 最適化と出力の指定

`-O` の既定値は `1` です。通常の最適化を避けるには `-O 0` を指定します。`-O 2` は追加の整理も行います。最適化は、コンパイル後のオブジェクトリストに連結されたオブジェクト数を減らす処理です。生成テキストの長さを評価基準にはしません。`-m pmd` は PMD4 向け、`-m pmd3` は PMD3 向けのモードです。オプションと既知の制約は [`sxlc` マニュアル](doc/sxlc.1.md)を参照してください。

## 導入と詳しい資料

現行リポジトリでは `./configure`、`make check`、`make install` が用意されています。インストール先とステージングの注意点は[配布・互換性](doc/release-guide.md)に記します。導入後は `sxlc` コマンドで変換します。

- [入門・目的別手順](doc/getting-started.md)
- [製品の目的と範囲](doc/product-overview.md)
- [現行の外部動作仕様](doc/behavior-spec.md)
- [開発者向け手順](doc/developer-guide.md)
- [変更履歴](NEWS.md)

ライセンスは [BSD 3-Clause License](LICENSE) です。
