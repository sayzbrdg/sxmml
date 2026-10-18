# 開発・改修手順

この文書は sxmml の開発・改修手順を示す。検証基準は Gauche 0.9.15 とする。最初のコマンドはリポジトリのルートで実行する。構成と変更箇所は[構成と設計](architecture.md)、個々の内部定義は[内部リファレンス](internal-reference/index.adoc)、要求と検証範囲は[検証方針](verification.md)を参照する。

## 必要な環境と生成物

開発・検証環境は Gauche 0.9.15、GNU Make、`iconv` とする。`configure` は Gauche の `gauche.configure` を使用する。`package.scm` の依存条件は Gauche 0.9.15 以上である。`Makefile.in` は `gosh`、`gauche-config`、`gauche-package`、`gauche-install` を使用する。

以下の手順には、リポジトリの Git チェックアウトが必要である。取得方法は[README](../README.md#最初の変換)を参照する。

`./configure` は `Makefile`、`sxmml.gpd`、`VERSION` を生成する。`make` は `sxlc.in` に shebang を付けた `sxlc` を作る。`make check` は `test.scm`、`test-repeat-tree.scm`、`test-pmd-binary.scm` を順に実行し、`test.log` に結果を残す。設定と生成の規則は [`configure`](../configure)、[`Makefile.in`](../Makefile.in)、[`package.scm`](../package.scm)を参照する。

## 隔離したコピーでの確認

次の手順は現在の `HEAD` を `/tmp` に展開し、作業ツリーに生成物を置かない。別の版を検証するときは `source_rev` に対象のブランチやタグを指定する。`git archive` は未コミットの編集を含まない。Gauche 0.9.15 で `./configure`、`make check`、一時ディレクトリへのインストールと後述の変換を実行できる。配置したコマンドとモジュールを `PATH`・`GAUCHE_LOAD_PATH` で選ぶため、既存のインストールを上書きしない。

```console
scratch_dir=$(mktemp -d /tmp/sxmml-dev.XXXXXX)
source_rev=HEAD
git archive "$source_rev" | tar -x -C "$scratch_dir"
cd "$scratch_dir"
gosh -V
./configure
make check
cat test.log
install_dir=$(mktemp -d /tmp/sxmml-dev-install.XXXXXX)
make bindir="$install_dir/bin" GAUCHE_PKGLIBDIR="$install_dir/share/sxmml" install
export PATH="$install_dir/bin:$PATH"
export GAUCHE_LOAD_PATH="$install_dir/share/sxmml${GAUCHE_LOAD_PATH:+:$GAUCHE_LOAD_PATH}"
```

`make check` の成功時は、`test.log` に `sxmml.classes` から `sxmml` まで 9 モジュールの binding 検査と、`sxlc` の CLI の検査が全て `ok` と表示され、末尾が `passed.` となる。`test.scm` は `test-module` による binding 検査と、`sxlc.in` を `gosh` で起動する CLI の検査を行う。CLI の検査は、`-c` の後処理へ生成名を一引数で渡すこと、CLI とソースのモード指定の優先順位、出力先の決定、失敗時に既存の生成物を変更しないこと、同じ入力からの生成物の一致、失敗の種類ごとの終了値と後処理の失敗の通知、出力先が入力ファイルと同じ場合にソースを変更しないこと、後処理の出力の文字コードの扱い、入力ファイル名より後ろの引数を使用法の誤りとすることを対象とする。続く `test-repeat-tree.scm` は、局所反復木の圧縮器（`sxmml.repeat-tree`）の binding 検査と、設定と独立検査の受付・拒否、仕様の例の費用と木、小さな全列での独立した最小費用（展開の一致を直接確かめる全木列挙）との費用の一致、窓幅つきの最小費用との一致を検査する。最後の `test-pmd-binary.scm` は、実験的な PMD バイナリのバックエンド（`sxmml.backend.pmd-binary`）の binding 検査と、ビルドした `sxlc`（`make check` が `SXMML_SXLC=./sxlc` で渡す）を起動する検査を行う。対象は、3 つの PMD バイナリモードでの既定の出力先・`-o`・後処理へ渡す名前、入力と既定の出力先が同じ場合に上書きしないこと（`-m` とソース中のモード指定の双方）、`filename` メタデータの警告、ソース中の `output` の変更より CLI の出力先の決定を優先すること、ソースのモードからバイナリモードへの切り替え、ライブラリからの直接生成、`-m` の優先、使用した音色だけを番号順（同じ番号では ID 順）に出力すること、同じ入力からの生成物のバイト一致、読み込み・生成・出力先の失敗で既存のファイルを変更しないこと、後処理の失敗の終了値 3 である。命令列のバイト内容は、命令の群ごとの小さな入力と PMD4・PMD3 の生成物全体について、`sxlc` を起動せずに検査のプロセスの中で生成して比べる。期待値は現在の出力を固定するもので、MC.EXE の出力との一致は確認しない。それ以外の MML 変換、CLI 出力、最適化の検証は[検証方針](verification.md)の手順に従って別に行う。

同じコピー内で PMD 生成を確認するには、次を続けて実行する。出力ファイルは `melody-and-rest.sxl` と同じディレクトリにできる。

```console
printf '%s\n' '(mml G "c8r8")' > melody-and-rest.sxl
sxlc -m pmd -O 0 melody-and-rest.sxl
cat melody-and-rest.mml
```

この例では `Processing Mode: PMD`、`Optimize level: 0`、`Track G: total: 24` と表示し、生成物に `G l%12o4cr` を含む。日本語メタデータの符号化を確かめる手順は[配布・互換性](release-guide.md#符号化と生成物の確認)に示す。エラー・境界値の再現コマンドは[外部動作仕様](behavior-spec.md#失敗条件と境界値)を参照する。

## 文書生成の現状

man ページは `make man` で `doc/sxlc.1.md` から Pandoc により生成し、`make install` で配置する。Pandoc がない場合は man ページの生成・配置だけを省く。[配布・互換性](release-guide.md#man-ページの生成と配置)に手順を示す。`Makefile.in` には man ページ以外の文書を生成するターゲットはない。

内部リファレンスの AsciiDoc は、Docker の Asciidoctor で HTML 表示を確認できる。リポジトリのルートから次を実行する。`-R doc` により元のディレクトリ構造を出力先にも保ち、文書間の相対リンクを使えるようにする。既存の両 AsciiDoc マニュアルも参照先として生成する。出力は新規の一時ディレクトリへ置き、ソースは読み取り専用でコンテナーに渡す。

```console
render_dir=$(mktemp -d /tmp/sxmml-internal-reference.XXXXXX)
docker run --rm -v "$PWD:/work:ro" -v "$render_dir:/out" -w /work \
  asciidoctor/docker-asciidoctor:latest asciidoctor -R doc -D /out \
  doc/internal-reference/*.adoc \
  doc/sxmml-mml-manual.adoc doc/sxmml-reference-manual.adoc
```

## 変更種別ごとの調査と確認

変更前に[構成と設計の対応表](architecture.md#代表的な変更時の調査先)から関係モジュールを特定する。`make check` に加え、変更した振る舞いに対応する[検証方針](verification.md#要求と検証の対応)の入力・観測点を確認する。

| 変更種別 | 調査の入口 | 確認する結果 |
| --- | --- | --- |
| MML 構文・音長 | `sxmml/mml.scm`、`sxmml/classes.scm` | 通常・リズム双方の解析結果、tick、生成行、失敗条件 |
| マクロ・オブジェクト加工 | `sxmml/compiler.scm`、`sxmml/processor.scm`、`sxmml/environment.scm` | 反復・echo・delay の登録列と文脈、PMD 出力 |
| 最適化 | `sxmml/optimizer.scm`、`sxmml/backend/pmd.scm` | 水準別のオブジェクト数と列、生成物、[Q-01 の検証方法](verification.md#q-01-音楽的な意味の保持)で比較できる範囲 |
| CLI・出力先 | `sxlc.in`、`sxmml/environment.scm`、`sxmml/backend/pmd.scm` | 実際の生成先、表示、`-c` に渡す名前、モード優先関係 |
| 配布 | `configure`、`package.scm`、`Makefile.in`、`tools/distcheck.sh` | 生成物、テストログ、ステージングの配置、man ページの有無。`tools/distcheck.sh` で[配布前の確認](release-guide.md#配布前の確認)を行う |

## 障害調査の入口

`sxlc` がモード未指定で止まる場合は、CLI の `-m` またはソースの `sxmml-mode` と `backend` slot を確認する。構文エラーは入力付近とトラック名が出るため、`sxmml/mml.scm` と `sxmml/compiler.scm` を辿る。音色未定義は生成段階で起き得る。生成先は `sxlc` が `-o` の値（なければ `#f`）をソースの読み込み後に `<sxmml-environment>` の `output` slot へ設定し、`#f` の場合はバックエンドが既定値を設定する。エラーの条件と境界値は[外部動作仕様](behavior-spec.md#失敗条件と境界値)を参照する。

改修対象の現在の問題、再現条件と期待する動作は [既知の課題（GitHub の issue）](https://github.com/sayzbrdg/sxmml/issues)から確認する。

## 関連資料

ビルド規則は[`Makefile.in`](../Makefile.in)と[`configure`](../configure)、パッケージ情報は[`package.scm`](../package.scm)、テストは[`test.scm`](../test.scm)、[`test-repeat-tree.scm`](../test-repeat-tree.scm)、[`test-pmd-binary.scm`](../test-pmd-binary.scm)を参照する。Gauche の生成・テスト機能は [Gauche 0.9.15 Users’ Reference, 9.8 gauche.configure、9.34 gauche.test](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照する。
