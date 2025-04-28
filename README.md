# sxmml

sxmmlは専用のMML書式から他の音源ドライバー向けMMLへ変換するトランスレータです。  
本ツールは音源ドライバーを内蔵せず、単体での楽曲演奏機能はありません。

## 必要条件

- Gauche 0.9.15以上

## 特徴

- **Gaucheベース**: Scheme処理系`Gauche`で実装され、`Gauche`のモジュールとして動作  
- **S式統合**: S式の一部としてMMLを記述。MML文字列を`Gauche`の機能で操作可能。
- **MML最適化機能**: 重複コマンド削除などの最適化を実装し、メモリ制約の厳しい実機環境への対応を支援  

## インストール

sxmmlを利用するには、Gaucheが必要です。Gaucheがインストールされている環境で以下の手順を実行してください。

```console
$ git clone https://github.com/sayzbrdg/sxmml.git
$ cd sxmml
$ ./configure
$ make install
```

インストール後、以下のコマンドで動作確認ができます：

```console
$ sxlc --version
```

## 使い方

基本的なsxmmlの使い方を示します。

```scheme:test.sxl
;; 音色定義（@0: 音色番号, v110: 音量, p3: パン, o5: オクターブ）
(define tone-1 "@0 v110 p3 o5")

;; メロディ定義（t120: テンポ, l8: 音長, cdefgab: 音階）
(mml A tone-1 "t120 l8 cdefgab")
```

MMLの変換には一緒にインストールされるsxmml用トランスレータプログラム **sxlc** を使用するのが便利です。

```console
$ sxlc -m pmd test.sxl
```

現時点ではPMD向けMML出力のみをサポートしています。

## ライセンス

sxmmlは **BSD 3-Clause License** のもとで提供されます。
詳細は `LICENSE` ファイルを参照してください。
