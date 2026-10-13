# 配布・互換性

この文書は sxmml の配布手順、必要な環境、配布工程の制約を配布・保守担当者に示す。検証基準は Gauche 0.9.15 とする。利用者向けの導入は[README](../README.md)を参照する。

## 必要な環境と依存

配布工程の検証には Gauche 0.9.15、GNU Make、`iconv` を使用する。man ページの生成には Pandoc (任意。ない場合は man ページの生成・配置を省く)、表示の確認には groff、コンテナーでの文書生成には Docker を使用する。`package.scm` の依存条件は Gauche 0.9.15 以上である。`configure` は `gauche.configure` により Gauche とパッケージ用のコマンドを検出する。

## 現行の生成・配置

`./configure` は `Makefile`、`sxmml.gpd`、`VERSION` を生成する。`VERSION` と `sxmml.gpd` のバージョンは `0.1`。`make check` は `sxlc` を作り、`test.scm` のモジュール binding 検査と `-c` の生成名受け渡し・モード優先順位の検査を実行する。`Makefile.in` の `install` 規則は `sxlc`、`SCMFILES` に列挙された Scheme ファイル、`sxmml.gpd` をそれぞれ実行ファイルディレクトリ、Gauche のパッケージ用ライブラリ、`.packages` に配置する。Pandoc がある場合は、`sxlc.1` も `mandir` の `man1` に配置する ([man ページの生成と配置](#man-ページの生成と配置))。実際のインストール先は `./configure` の結果で変わるため、生成された `Makefile` を確認する。

配置先の変数は次のとおり決まる。いずれも `make` の引数で上書きできる。

| 変数 | 既定値 | 変更する `./configure` のオプション |
| --- | --- | --- |
| `bindir` | `${exec_prefix}/bin` (`exec_prefix` の既定は `${prefix}`) | `--prefix`、`--exec_prefix`、`--bindir` |
| `GAUCHE_PKGLIBDIR` | `gauche-config --pkglibdir` の値。Gauche 0.9.15 では `${datadir}/gauche-0.98/site/lib` | `--prefix`、`--datarootdir`、`--datadir` |
| `mandir` | `${datarootdir}/man` (`datarootdir` の既定は `${prefix}/share`) | `--prefix`、`--datarootdir`、`--mandir` |

`--prefix` の既定値は `/usr/local` である。Gauche 0.9.15 の `configure` は、ヘルプに `--exec-prefix` と表示するが、実際に受け付けるのは `--exec_prefix` である。`GAUCHE_PKGLIBDIR` を Gauche 自体のインストール先と異なる場所にした場合、その場所は Gauche の標準の検索先に含まれないため、実行時に `GAUCHE_LOAD_PATH` などで指定する。

### DESTDIR によるステージング

`make DESTDIR=DIR install` は、上記の各配置先の先頭に `DIR` を付けた場所へ配置する。例えば既定の設定では `DIR/usr/local/bin/sxlc`、`DIR/usr/local/share/gauche-0.98/site/lib/sxmml.scm` のようになる。配布パッケージの作成など、本来の配置先を変えずに別の場所へ配置する場合に使う。`DIR` は絶対パスで指定する。`make DESTDIR=DIR uninstall` は同じ場所から削除する。次の例の `scratch_dir` は[開発手順](developer-guide.md#隔離したコピーでの確認)で作ったコピーを指す。`DESTDIR` は `Makefile` の変数の値には含まれず、`install`・`uninstall` の各コマンドで付けるため、配置先の変数を上書きした場合も `DESTDIR` が付く。

```console
cd "$scratch_dir"
destdir=$(mktemp -d /tmp/sxmml-destdir.XXXXXX)
make DESTDIR="$destdir" install
find "$destdir" -type f
```

`sxlc` の先頭行には `configure` が検出した `gosh` のパスが入り、配置先のパスは生成物に埋め込まれない。このため、`DESTDIR` の下に配置した内容を本来の配置先へ移せば、そのまま使える。

### 一時配置での実行確認

配置した `sxlc` を実行して確かめる場合は、短いパスで扱えるよう配置先の変数を直接指定する。次の一時配置では `bindir` と `GAUCHE_PKGLIBDIR` の両方を明示し、配置先と実行時の検索先を合わせる。以下の `scratch_dir` は[開発手順](developer-guide.md#隔離したコピーでの確認)で作ったコピーを指す。`stage_dir` は毎回新しいパスにする。

```console
cd "$scratch_dir"
stage_dir=$(mktemp -d /tmp/sxmml-stage.XXXXXX)
mkdir -p "$stage_dir/bin"
make bindir="$stage_dir/bin" GAUCHE_PKGLIBDIR="$stage_dir/share/sxmml" install
test -f "$stage_dir/bin/sxlc"
export PATH="$stage_dir/bin:$PATH"
export GAUCHE_LOAD_PATH="$stage_dir/share/sxmml${GAUCHE_LOAD_PATH:+:$GAUCHE_LOAD_PATH}"
```

この手順は `bin/sxlc`、`share/sxmml/sxmml.scm` と各 `sxmml/` モジュール、`share/sxmml/.packages/sxmml.gpd` を配置する。配置した `sxlc` は、その Scheme モジュールの場所を `GAUCHE_LOAD_PATH` に追加して起動する。これは一時配置用のディレクトリ構成であり、標準のインストール先は生成された `Makefile` に従う。配布時には生成された `Makefile` の `bindir`・`GAUCHE_PKGLIBDIR` とステージングの内容を照合する。

## 符号化と生成物の確認

現行 PMD バックエンドは `:encoding "cp932"` で出力する ([`sxmml/backend/pmd.scm`](../sxmml/backend/pmd.scm))。単なる ASCII の例は符号化の判定にならないため、日本語メタデータを含む入力で確かめる。次のコマンドは上記のインストール後、コピー内で実行する。

```console
printf '%s\n' '(define-metadata title "曲")' '(mml G "c8")' > japanese-title.sxl
sxlc -m pmd japanese-title.sxl
iconv -f CP932 -t UTF-8 japanese-title.mml
```

`iconv` は終了値 0 となり、復号結果に `#title       曲` と `G l%12o4c` を含む。配布時には生成物の存在・出力先・内容・復号結果を確認する。モードと `filename` の既知の食い違いは[外部動作仕様](behavior-spec.md#要求との対応と既知の差)を参照する。

## 配布アーカイブ

隔離したソースのコピーで `gauche-package make-tarball` を実行すると配布アーカイブを作成する。`--dry-run` は実行予定の表示だけであり、アーカイブを作らない。

```console
gauche-package make-tarball --dry-run
gauche-package make-tarball
```

ソースの親ディレクトリに作成した `sxmml-0.1.tgz` を別のディレクトリへ展開し、展開先で `./configure`、`make check`、一時配置への `make install` と変換例を実行する。標準配置への導入は、配置先の権限と Gauche の検索先を確認してから行う。

## man ページの生成と配置

`doc/sxlc.1.md` から Pandoc で `sxlc.1` を生成する。`./configure` は `pandoc` を検索し、見つかった場合はそのパスを `Makefile` の `PANDOC` に設定する。生成した `sxlc.1` は Git で管理しない。

* `make man` は `sxlc.1` を生成する。`PANDOC` が空の場合はエラーで終了する。
* `make install` は、`PANDOC` が設定されていれば `sxlc.1` を生成して `$(mandir)/man1` に配置する。`PANDOC` が空の場合は `pandoc is not found; skip installing sxlc.1` と表示し、man ページの配置だけを省く。
* `make uninstall` は `$(mandir)/man1/sxlc.1` も削除し、`make clean` は生成した `sxlc.1` を削除する。

`mandir` の既定値は `${prefix}/share/man` であり、`./configure --mandir=DIR` または `make mandir=DIR install` で変更できる。`make DESTDIR=DIR install` では `DIR` の下の `mandir` に配置する。[一時配置の手順](#一時配置での実行確認)に合わせる場合は、`mandir` も明示する。

```console
cd "$scratch_dir"
make bindir="$stage_dir/bin" GAUCHE_PKGLIBDIR="$stage_dir/share/sxmml" mandir="$stage_dir/share/man" install
test -f "$stage_dir/share/man/man1/sxlc.1"
groff -Kutf8 -Tutf8 -man -rLL=120n "$stage_dir/share/man/man1/sxlc.1" > "$stage_dir/sxlc-rendered.txt"
```

配置した `sxlc.1` と表示結果を確認する。Pandoc のホストコマンドがない環境では、`./configure` の後に別の環境で生成した `sxlc.1` を配布先の `share/man/man1/` に手動で配置する。Pandoc の公式コンテナーでも生成できる。コンテナーで再現する場合は、リポジトリのルートから次を実行し、事前に `docker run --rm pandoc/core:latest --version` で Pandoc のバージョンを確認する。

```console
man_stage_dir=$(mktemp -d /tmp/sxmml-man.XXXXXX)
mkdir -p "$man_stage_dir/share/man/man1"
docker run --rm -v "$PWD:/work:ro" -v "$man_stage_dir:/out" pandoc/core:latest \
  --standalone --from=markdown --to=man /work/doc/sxlc.1.md \
  -o /out/share/man/man1/sxlc.1
```

配布確認の記録には、使用するソースと Gauche バージョン、`./configure`、`make check`、ステージングの配置、非 ASCII を含む PMD 生成物の復号、配布アーカイブと man ファイルの有無を残す。MML 変換と最適化の検証は[検証方針](verification.md)の手順に従って行う。

配布工程の検証不足は [issue #10](https://github.com/sayzbrdg/sxmml/issues/10)にまとめる。

## 関連資料

[`Makefile.in`](../Makefile.in)、[`configure`](../configure)、[`package.scm`](../package.scm)、[`sxlc.in`](../sxlc.in)、[`sxmml/backend/pmd.scm`](../sxmml/backend/pmd.scm)を参照する。Gauche の生成と文字コード変換は [Gauche 0.9.15 Users’ Reference, 9.8 gauche.configure、9.5 Character code conversion](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照する。
