# 開発・改修手順

この文書は sxmml の開発・改修手順を示す。検証基準は Gauche 0.9.15 とする。最初のコマンドはリポジトリのルートで実行する。構成と変更箇所は[構成と設計](architecture.md)、個々の内部定義は[内部リファレンス](internal-reference/index.adoc)、要求と検証範囲は[検証方針](verification.md)を参照する。

識別子の命名や改名では、[命名規約](internal-reference/naming-conventions.adoc)を参照する。適用範囲、接頭辞・接尾辞、構築・変換・処理の名前の選び方を定めている。

## 必要な環境と生成物

開発・検証環境は Gauche 0.9.15、GNU Make、`iconv` とする。`configure` は Gauche の `gauche.configure` を使用する。`package.scm` の依存条件は Gauche 0.9.15 以上である。`Makefile.in` は `gosh`、`gauche-config`、`gauche-package`、`gauche-install` を使用する。

以下の手順には、リポジトリの Git チェックアウトが必要である。取得方法は[README](../README.md#最初の変換)を参照する。

`./configure` は `Makefile`、`sxmml.gpd`、`VERSION` を生成する。`make` は `sxlc.in` に shebang を付けた `sxlc` を作る。`make check` は `test/` 配下のテストを実行し、`tools/` があれば `tools/test-measure-optimize.scm` も実行して、`test.log` に結果を残す。設定と生成の規則は [`configure`](../configure)、[`Makefile.in`](../Makefile.in)、[`package.scm`](../package.scm)を参照する。

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

`make check` の成功時は、`test.log` に各検査が `ok` と表示され、各ファイルの末尾が `passed.` となる。`test/` の `test-<モジュール名>.scm` は、そのモジュールを主な対象とする既存検査をまとめる。`test-pmd.scm` は pmd-source、`test-pmd-binary.scm` は pmd-binary の検査である。binding 検査だけのモジュールにも専用ファイルを置く。

`test/test-sxlc.scm` は CLI の引数・モード・出力先・診断・終了値・後処理などを検査する。`test/test-integration.scm` は、sxmml 全体として、読込み・生成・出力等の失敗時に既存ファイルを保持する結合試験をまとめる。`test/test-helpers.scm` の既存 2 件は、sxlc と pmd-binary のテストが指定 CLI を使い、その失敗を検出することを確認する。`tools/` がある場合は計測ツールの検査も実行する。

配置先と試験範囲は別である。モジュール別のファイルにも、ソース読込み・最適化・バックエンドを通して結果を観測する試験や、CLI 経由の試験が含まれる。各ファイルの保証範囲と不足は[検証方針](verification.md)を参照する。

同じコピー内で PMD 生成を確認するには、次を続けて実行する。出力ファイルは `melody-and-rest.sxl` と同じディレクトリにできる。

```console
printf '%s\n' '(mml G "c8r8")' > melody-and-rest.sxl
sxlc -m pmd -O 0 melody-and-rest.sxl
cat melody-and-rest.mml
```

この例では `Processing Mode: PMD`、`Optimize level: 0`、`Track G: total: 24` と表示し、生成物に `G l%12o4cr` を含む。日本語メタデータの符号化を確かめる手順は[配布・互換性](release-guide.md#符号化と生成物の確認)に示す。エラー・境界値の再現コマンドは[外部動作仕様](behavior-spec.md#失敗条件と境界値)を参照する。

## テストの単独実行

`test/` 配下のテストを単独で実行する際は、環境変数 `SXMML_SXLC` にテストで使用する `sxlc` を設定してください。以下は、`./configure` と `make` でビルドした後に、リポジトリのルートで実行する例です。

```sh
SXMML_SXLC=./sxlc gosh -I. test/test-sxlc.scm
```

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

## pmd-source の仕様根拠と値の検査

pmd-source の対応済み命令・定義は、公開されている [PMDMML.MAN](https://github.com/d2lmirrors/pmd/blob/master/mc/PMDMML.MAN) を仕様の根拠として検査する。FM・SSG・PCM の区分で範囲を分け、細かな種別差は最も広い範囲に合わせる。対象音源の列挙だけで使用を禁止せず、効果を再現しない。曖昧な記載から推測で制限を加えず、MC.EXE による実測はこの検査方針の前提にしない。MC.EXE の受理やドライバでの演奏を包括的に保証するものではない。

検査は生成する値の出力箇所に置く。最適化で消えた命令や出力されない音色を追加走査しない。各範囲は[外部動作仕様](behavior-spec.md#pmd-source-の出力値の検査)、内部手続きは[PMD内部リファレンス](internal-reference/pmd.adoc)を参照する。pmd-binary のバイナリ形式の確認については、従来の別の方針を維持する。

## 変更種別ごとの調査と確認

トラックとpartの対応情報の本体は、環境の名前→contextの表とcontextのpartとする。常設の逆引き表は持たず、登録時に自身以外のpart所有者を確認する。新規登録・同名再定義は新context、名前変更は既存contextの保持という区別を維持する。検査は各手続きの変更前に行い、複数登録全体の巻戻しは行わない。

contextや登録表を直接編集する利用者は、対応関係を維持する責務も持つ。一対一対応を崩した場合の候補選択と生成物の再現性は保証しない。検索時の重複拒否は追加せず、この境界を理由に既存の値・形式検査を削除しない。

`sxmml-track-list` の戻り値の順序は保証しない。順序に依存する処理は利用側で並べ替える。バックエンドは出力形式に従う決まった処理順を定め、ハッシュ表の反復順や `sxmml-track-list` の列挙順へ依存せず、要求仕様Q-03の生成物の再現性を満たす。

変更前に[構成と設計の対応表](architecture.md#代表的な変更時の調査先)から関係モジュールを特定する。`make check` に加え、変更した振る舞いに対応する[検証方針](verification.md#要求と検証の対応)の入力・観測点を確認する。

| 変更種別 | 調査の入口 | 確認する結果 |
| --- | --- | --- |
| MML 構文・音長 | `sxmml/mml.scm`、`sxmml/classes.scm` | 通常・リズム双方の解析結果、tick、生成行、失敗条件 |
| マクロ・オブジェクト加工 | `sxmml/compiler.scm`、`sxmml/processor.scm`、`sxmml/environment.scm` | 反復・echo・delay の登録列と文脈、PMD 出力 |
| 最適化 | `sxmml/optimizer.scm`、`sxmml/backend/pmd.scm` | 水準別のオブジェクト数と列、生成物、[Q-01 の検証方法](verification.md#q-01-音楽的な意味の保持)で比較できる範囲。サイズ・オブジェクト数・時間は[最適化の水準の比較](#最適化の水準の比較)で測る |
| CLI・出力先 | `sxlc.in`、`sxmml/environment.scm`、`sxmml/backend/pmd.scm` | 実際の生成先、表示、`-c` に渡す名前、モード優先関係 |
| 配布 | `configure`、`package.scm`、`Makefile.in`、`tools/distcheck.sh` | 生成物、テストログ、ステージングの配置、man ページの有無。`tools/distcheck.sh` で[配布前の確認](release-guide.md#配布前の確認)を行う |

## 最適化の水準の比較

`tools/measure-optimize.scm` は、一つのソースを最適化の水準ごとに変換し、生成物のバイト数、全トラックのオブジェクト数の合計、時間を表にする。`-O 3`・`-O 4` の圧縮の設定（展開の上限 E と窓幅 W）を変えたときの効果と時間を比べるのに使う。リポジトリのルートで次のように実行する。

```console
gosh -I. tools/measure-optimize.scm [-m モード] ソース.sxl [設定 ...]
```

モードの既定は `pmd` で、`pmd-binary` なども指定できる。設定は `O2`・`O3`・`O4` のいずれかで、`:E=数`・`:W=数` を付けると圧縮の設定を変えられる（例：`O4:E=128:W=256`）。既定は `sxlc` の `-O 3`・`-O 4` と同じで、E が 256、W が `O3` で 256、`O4` で 1024 である。設定を省略すると `O2`・`O3`・`O4` を測る。

```console
$ printf '%s\n' '(mml A "[aaaaaaaa]2 cdecdecd")' > repeat.sxl
$ gosh -I. tools/measure-optimize.scm repeat.sxl
config           bytes      vs O2    objects  generate   total
O2                      105 100.0%         18       0.02       0.62
O3                      100 95.2%          11       0.03       0.61
O4                       98 93.3%           9       0.03       0.61
```

列は、設定、生成物のバイト数、`O2` との比（`O2` を先に測ったときだけ）、オブジェクト数の合計、生成の時間（秒）、プロセス全体の時間（秒）である。設定ごとに別のプロセスで変換し、生成物は一時ディレクトリに置いて、測り終えたら消す。変換に失敗した設定は `failed (exit status N)` と表示する。圧縮の設定は `sxmml.optimizer` の内部の変数を書き換えて変えるので、その名前を変えたときは、この道具も合わせて直す。`make check` の `tools/test-measure-optimize.scm` が、合成した入力でこの道具の動作を確かめる。

## マクロの名前・種類・キー

展開時の単語は、利用者マクロ由来のidentifierにも対応するため `identifier?` で検査する。MMLから参照する音色・LFO・エンベロープの定義名は `sxmml.form` の `sxmml-form-name->symbol` に集約し、symbol・数値を正規化して空白・波括弧・空の名前を拒否する。

種類とトラック名は検査後にそのままquoteし、Gaucheにsymbol化させる。メタデータキーは `identifier->symbol` で元の名前を得て小文字化する。名前のために任意の入力を `x->string` へ渡さず、値の式を含むフォーム全体に `unwrap-syntax` を適用しない。診断用に不正な項目や名前を表示するときだけ、その項目のidentifierを取り除く。

## 既知の入力エラーの通知

明示的な入力検査は、違反した規則に応じて `error`・`errorf` のcondition-type指定で送出する。MML文字列の規則はmml-syntax、独自マクロの構文はform-syntax、登録・出力値はvalue、未定義参照はreferenceとして区別する。IRや最適化の内部整合性検査はそのまま残す。

位置は本文へ埋め込まず、既存の境界で `sxmml-error-with-context` により合成する。判定にはcompound対応の述語を使う。未知のGaucheエラーを各手続きでguardして入力エラーへ変換しない。CLIは既知の入力エラーだけを `sxlc: error: …` の形で標準エラーへ表示し、Gaucheの例外見出しやスタックトレースを付けない。未知の例外はGaucheの標準報告を維持する。診断文は項目・指定値・必要な条件を示し、位置とトラックはCLIが文脈から組み立てる。usageの標準出力と終了値は維持する。

## 障害調査の入口

`sxlc` がモード未指定で止まる場合は、CLI の `-m` またはソースの `sxmml-mode` と `backend` slot を確認する。構文エラーは入力付近とトラック名が出るため、`sxmml/mml.scm` と `sxmml/compiler.scm` を辿る。音色未定義は生成段階で起き得る。生成先は `sxlc` が `-o` の値（なければ `#f`）をソースの読み込み後に `<sxmml-environment>` の `output` slot へ設定し、`#f` の場合はバックエンドが既定値を設定する。エラーの条件と境界値は[外部動作仕様](behavior-spec.md#失敗条件と境界値)を参照する。

改修対象の現在の問題、再現条件と期待する動作は [既知の課題（GitHub の issue）](https://github.com/sayzbrdg/sxmml/issues)から確認する。

## 関連資料

ビルド規則は[`Makefile.in`](../Makefile.in)と[`configure`](../configure)、パッケージ情報は[`package.scm`](../package.scm)、テストの配置と保証範囲は[検証方針](verification.md)、実行対象は[`Makefile.in`](../Makefile.in)を参照する。Gauche の生成・テスト機能は [Gauche 0.9.15 Users’ Reference, 9.8 gauche.configure、9.34 gauche.test](https://practical-scheme.net/vault/Gauche-0.9.15-refe.pdf)を参照する。
