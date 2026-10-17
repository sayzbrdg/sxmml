#!/bin/sh
#
# 配布工程の確認 (distcheck)
#
# 指定したリビジョンから配布アーカイブを作成し、展開、ビルド、テスト、
# DESTDIR によるステージング配置、配置物での変換、削除までを順に確認する。
# 全ての処理は作業ディレクトリの中で行い、リポジトリの作業ツリーや
# システムの標準配置先には書き込まない。
#
# 使い方は -h で表示する。詳細は doc/release-guide.md を参照。

set -u

prog=$(basename "$0")
total_steps=9

usage() {
    cat <<EOF
使い方: $prog [-r REV] [-m MAKE] [-w DIR] [-k] [-h]

  -r REV   検証する Git のリビジョン (既定値: HEAD)
  -m MAKE  使用する make コマンド (既定値: 環境変数 MAKE、なければ make)
  -w DIR   作業ディレクトリを作成する場所 (既定値: 環境変数 TMPDIR、なければ /tmp)
  -k       成功時も作業ディレクトリを残す (失敗時は常に残す)
  -h       この使い方を表示して終了する

終了値: 0 成功、1 検証の失敗、2 使い方の誤りまたは前提の不足
EOF
}

die_usage() {
    echo "$prog: $*" >&2
    usage >&2
    exit 2
}

# ----------------------------------------------------------------------
# 引数の解析

rev=HEAD
make_cmd=${MAKE:-make}
work_base=${TMPDIR:-/tmp}
keep=no

while getopts r:m:w:kh opt; do
    case $opt in
        r) rev=$OPTARG ;;
        m) make_cmd=$OPTARG ;;
        w) work_base=$OPTARG ;;
        k) keep=yes ;;
        h) usage; exit 0 ;;
        *) usage >&2; exit 2 ;;
    esac
done
shift $((OPTIND - 1))
[ $# -eq 0 ] || die_usage "余分な引数があります: $*"

# ----------------------------------------------------------------------
# 共通の手続き

step_no=0
work=
logs=

# 段階を 1 つ実行する。$1 は表示名、$2 は実行する手続き名。
# 手続きの出力はログファイルに書き、失敗した場合はログの末尾を表示して終了する。
# 手続きは、段階を省いた場合は skip_reason に、補足がある場合は note に理由を設定する。
run_step() {
    step_no=$((step_no + 1))
    log="$logs/$step_no-$2.log"
    printf '[%d/%d] %s ... ' "$step_no" "$total_steps" "$1"
    skip_reason=
    note=
    if "$2" >"$log" 2>&1; then
        if [ -n "$skip_reason" ]; then
            echo "skip ($skip_reason)"
        elif [ -n "$note" ]; then
            echo "ok ($note)"
        else
            echo "ok"
        fi
    else
        echo "失敗"
        echo "---- $log の末尾 ----"
        tail -n 20 "$log"
        echo "--------"
        echo "作業ディレクトリを残しました: $work"
        exit 1
    fi
}

# 手続きの中で使う。メッセージを表示して手続きを失敗させる。
fail() {
    echo "失敗: $*"
    return 1
}

have() {
    command -v "$1" >/dev/null 2>&1
}

# make コマンドの最初の語 (コマンド名)
make_prog=${make_cmd%% *}

# ----------------------------------------------------------------------
# 前提の確認 (必須コマンドの有無は作業ディレクトリを作る前に確かめる)

missing=
for c in git gosh gauche-package "$make_prog" tar gzip iconv; do
    have "$c" || missing="$missing $c"
done
[ -z "$missing" ] || { echo "$prog: 必要なコマンドがありません:$missing" >&2; exit 2; }

script_dir=$(cd "$(dirname "$0")" && pwd)
repo=$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null) \
    || { echo "$prog: Git リポジトリが見つかりません" >&2; exit 2; }
commit=$(git -C "$repo" rev-parse --verify --quiet "$rev^{commit}") \
    || { echo "$prog: リビジョンが見つかりません: $rev" >&2; exit 2; }
[ -d "$work_base" ] || { echo "$prog: ディレクトリがありません: $work_base" >&2; exit 2; }

# DESTDIR に対応していない Makefile.in (#21 の修正前) は、install や make -n で
# システムの配置先に書き込むおそれがあるため、何も実行せずに拒否する
if ! git -C "$repo" show "$commit:Makefile.in" 2>/dev/null | grep -qF '"$(DESTDIR)$(bindir)"'; then
    echo "$prog: このリビジョンの Makefile.in は DESTDIR に対応していないため検証できません: $rev" >&2
    exit 2
fi

work=$(mktemp -d "$work_base/sxmml-distcheck.XXXXXX") \
    || { echo "$prog: 作業ディレクトリを作成できません" >&2; exit 2; }
logs="$work/logs"
mkdir "$logs"
trap 'echo; echo "中断しました。作業ディレクトリ: $work"; exit 130' INT TERM

echo "検証するリビジョン: $rev ($commit)"
echo "作業ディレクトリ: $work"

# 1. 前提の確認 (任意コマンドの有無を記録する)
step_prerequisites() {
    for c in git gosh gauche-package "$make_prog" tar gzip iconv; do
        echo "$c: $(command -v "$c")"
    done
    optional=
    for c in pandoc groff; do
        if have "$c"; then
            echo "$c: $(command -v "$c")"
        else
            echo "$c: なし"
            optional="$optional $c"
        fi
    done
    [ -z "$optional" ] || note="任意のコマンドなし:$optional"
    return 0
}
run_step "前提の確認" step_prerequisites

# ----------------------------------------------------------------------
# 2. ソースの取り出し

src="$work/src"

step_extract() {
    mkdir "$src" || return 1
    git -C "$repo" archive --format=tar -o "$work/src.tar" "$commit" || return 1
    tar -xf "$work/src.tar" -C "$src" || return 1
    rm -f "$work/src.tar"
    ls -A "$src"
}
run_step "ソースの取り出し" step_extract

# ----------------------------------------------------------------------
# 3. 配布アーカイブの作成
#
# make-tarball はカレントディレクトリを掃除して全体を tar にまとめ、
# 親ディレクトリ ($work) にアーカイブを出力する。

tarball=

step_tarball() {
    (cd "$src" && gauche-package make-tarball) || return 1
    set -- "$work"/*.tgz
    [ $# -eq 1 ] && [ -f "$1" ] || fail "アーカイブが 1 つだけ作成されていません: $*" || return 1
    tarball=$1
    echo "アーカイブ: $tarball"
}
run_step "配布アーカイブの作成" step_tarball
pkgname=$(basename "$tarball" .tgz)

# ----------------------------------------------------------------------
# 4. アーカイブの中身の照合
#
# 期待する中身は、リビジョンで Git 管理されているファイルから、
# VCS 関連ファイル (.git で始まる名前) と DIST_EXCLUDE の記載を除いたもの。
# DIST_EXCLUDE にはルートからの相対パス (ファイルまたはディレクトリ) を記載する。
# configure が生成する VERSION だけは、アーカイブにのみ存在してよい。

step_contents() {
    git -C "$repo" ls-tree -r --name-only "$commit" >"$work/tracked.txt" || return 1
    if git -C "$repo" cat-file -e "$commit:DIST_EXCLUDE" 2>/dev/null; then
        git -C "$repo" show "$commit:DIST_EXCLUDE" >"$work/exclude.txt" || return 1
    else
        : >"$work/exclude.txt"
    fi
    # DIST と DIST_EXCLUDE は make-tarball が常に除外する
    printf '%s\n' DIST DIST_EXCLUDE >>"$work/exclude.txt"
    awk '
        NR == FNR { sub(/^\.\//, ""); sub(/\/$/, ""); if ($0 != "") ex[$0] = 1; next }
        {
            n = split($0, parts, "/")
            if (parts[n] ~ /^\.git/) next
            path = ""
            for (i = 1; i <= n; i++) {
                path = (i == 1) ? parts[1] : path "/" parts[i]
                if (path in ex) next
            }
            print
        }
    ' "$work/exclude.txt" "$work/tracked.txt" | LC_ALL=C sort >"$work/expected.txt" || return 1
    tar -tzf "$tarball" >"$work/listing.txt" || return 1
    sed -n "s|^$pkgname/||p" "$work/listing.txt" \
        | grep -v '/$' | grep -v '^$' | grep -vx 'VERSION' \
        | LC_ALL=C sort >"$work/actual.txt"
    if ! grep -qx "$pkgname/VERSION" "$work/listing.txt"; then
        fail "VERSION がアーカイブに含まれていません" || return 1
    fi
    if ! diff "$work/expected.txt" "$work/actual.txt"; then
        fail "アーカイブの中身が期待と一致しません (< 期待のみ、> アーカイブのみ)" || return 1
    fi
    echo "$(wc -l <"$work/actual.txt") ファイルが一致"
}
run_step "アーカイブの中身の照合" step_contents

# ----------------------------------------------------------------------
# 5. 展開とビルド・テスト

build="$work/build"
pkgdir="$build/$pkgname"

step_build() {
    mkdir "$build" || return 1
    tar -xzf "$tarball" -C "$build" || return 1
    cd "$pkgdir" || return 1
    ./configure || return 1
    $make_cmd || return 1
    $make_cmd check || return 1
    tail -n 1 test.log
    [ "$(tail -n 1 test.log)" = "passed." ] || fail "test.log の末尾が passed. ではありません"
}
run_step "展開とビルド・テスト" step_build

# ----------------------------------------------------------------------
# 6. DESTDIR によるステージング配置

stage="$work/stage"
pandoc_configured=no

step_install() {
    cd "$pkgdir" || return 1
    if grep -q '^PANDOC *= *[^ ]' Makefile; then
        pandoc_configured=yes
    fi
    # 全ての配置先に DESTDIR が付くことを、実際の配置の前に確かめる
    dry="$work/nonexistent-destdir"
    if ! $make_cmd -n DESTDIR="$dry" install >"$work/install-dry.txt" 2>&1; then
        cat "$work/install-dry.txt"
        fail "$make_cmd -n install が失敗しました" || return 1
    fi
    grep -o ' -T "[^"]*"' "$work/install-dry.txt" | sort -u >"$work/install-targets.txt"
    cat "$work/install-targets.txt"
    [ -s "$work/install-targets.txt" ] || fail "配置先を取得できません" || return 1
    if grep -v " -T \"$dry/" "$work/install-targets.txt"; then
        fail "DESTDIR が付かない配置先があります" || return 1
    fi
    [ ! -e "$dry" ] || fail "make -n で $dry が作成されました" || return 1

    $make_cmd DESTDIR="$stage" install || return 1
    find "$stage" -type f | sort
    for f in sxlc sxmml.scm; do
        [ "$(find "$stage" -type f -name "$f" | wc -l)" -eq 1 ] \
            || fail "$f が 1 つだけ配置されていません" || return 1
    done
    for f in sxmml/backend/pmd.scm sxmml/backend/pmd-binary.scm .packages/sxmml.gpd; do
        [ -n "$(find "$stage" -type f -path "*/$f")" ] || fail "$f が配置されていません" || return 1
    done
    if [ "$pandoc_configured" = yes ]; then
        [ -n "$(find "$stage" -type f -path '*/man1/sxlc.1')" ] \
            || fail "sxlc.1 が配置されていません" || return 1
    else
        echo "PANDOC が空のため sxlc.1 の配置は確認しません"
    fi
}
run_step "DESTDIR によるステージング配置" step_install

# ----------------------------------------------------------------------
# 7. 配置物での変換

step_convert() {
    sxlc_path=$(find "$stage" -type f -name sxlc)
    lib_dir=$(dirname "$(find "$stage" -type f -name sxmml.scm)")
    t="$work/convert"
    mkdir "$t" || return 1
    printf '%s\n' '(define-metadata title "曲")' '(mml G "c8")' >"$t/japanese-title.sxl"
    PATH="$(dirname "$sxlc_path"):$PATH" GAUCHE_LOAD_PATH="$lib_dir" \
        sxlc -m pmd "$t/japanese-title.sxl" || return 1
    iconv -f CP932 -t UTF-8 "$t/japanese-title.mml" >"$t/decoded.txt" || return 1
    cat "$t/decoded.txt"
    grep -q '^#title  *曲' "$t/decoded.txt" || fail "復号結果に #title 曲 がありません" || return 1
    grep -qF 'G l%12o4c' "$t/decoded.txt" || fail "復号結果に G l%12o4c がありません"
}
run_step "配置物での変換" step_convert

# ----------------------------------------------------------------------
# 8. man ページの表示

step_man() {
    if [ "$pandoc_configured" != yes ]; then
        skip_reason="sxlc.1 を配置していない"
        return 0
    fi
    if ! have groff; then
        skip_reason="groff なし"
        return 0
    fi
    groff -Kutf8 -Tutf8 -man "$(find "$stage" -type f -path '*/man1/sxlc.1')" >"$work/sxlc-rendered.txt" \
        || return 1
    head -n 5 "$work/sxlc-rendered.txt"
}
run_step "man ページの表示" step_man

# ----------------------------------------------------------------------
# 9. 配置物の削除

step_uninstall() {
    cd "$pkgdir" || return 1
    $make_cmd DESTDIR="$stage" uninstall || return 1
    left=$(find "$stage" -type f)
    [ -z "$left" ] || fail "削除されずに残ったファイルがあります: $left"
}
run_step "配置物の削除" step_uninstall

# ----------------------------------------------------------------------
# 結果のまとめ (リリース確認の issue にそのまま貼れる形で環境を表示する)

make_version() {
    v=$($make_cmd --version 2>/dev/null | head -n 1)
    [ -n "$v" ] || v="$make_prog $($make_cmd -V MAKE_VERSION 2>/dev/null)"
    echo "$v"
}

echo
echo "== 環境 =="
echo "リビジョン: $rev ($commit)"
echo "アーカイブ: $(basename "$tarball")"
echo "Gauche: $(gosh -V | head -n 1)"
echo "make: $(make_version)"
for c in pandoc groff; do
    if have "$c"; then
        echo "$c: $($c --version 2>/dev/null | head -n 1)"
    else
        echo "$c: なし"
    fi
done

if [ "$keep" = yes ]; then
    echo "作業ディレクトリを残しました: $work"
else
    rm -rf "$work"
fi
echo "全ての確認が成功しました"
