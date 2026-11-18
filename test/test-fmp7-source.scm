(use gauche.test)
(use file.util)
(use srfi.13)
(use srfi.1)
(use gauche.charconv)
(use sxmml)
(test-start "sxmml.backend.fmp7-source")
(use sxmml.backend.fmp7-source)
(test-module 'sxmml.backend.fmp7-source)

(let1 dir (sys-mkdtemp (build-path (temporary-directory) "sxmml-fmp7"))
  (unwind-protect
      (parameterize ([sxmml-environment (make <sxmml-environment>)])
        (define-track lead :sound-source 'opn)
        (define-lfo 0 default '(8 2 1 24))
        (mml lead "M0 c8")
        (let* ([path (build-path dir "lfo.mwi")]
               [out (make-sxmml-output-environment (sxmml-environment) :output path :optimize 0)])
          (sxmml-fmp7-source-output out)
          (test* "M0 c8では先頭からLFOが有効" #t
                 (boolean (#/MPV8,2,1,24,0,1,0o4c#12/
                           (call-with-input-file path port->string :encoding "cp932"))))))
    (remove-directory* dir)))

(let1 dir (sys-mkdtemp (build-path (temporary-directory) "sxmml-fmp7"))
  (define path (build-path dir "out.mwi"))
  (define (render kind phrase :optional (setup (^[] #f)) (level 0))
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (sxmml-track-set! 'lead :sound-source kind)
      (setup)
      (mml lead phrase)
      (sxmml-fmp7-source-output
       (make-sxmml-output-environment (sxmml-environment) :output path :optimize level))
      (file->string path :encoding "cp932")))
  (define (has text . fragments)
    (every (^[fragment] (boolean (string-contains text fragment))) fragments))
  (define (lfo-setup)
    (define-lfo 0 '(8 2 1 24))
    (define-lfo 1 '(0 1 2 3)))
  (unwind-protect
      (begin
        (test* "分解能とパート属性" #t (has (render 'opn "") "ClockCount=96" "VolumeExtend=1" "PartOPNA=A0"))
        (test* "音符・休符・相対音量" #t (has (render 'ssg "v8c8r8v*2v*-1v*0") "v8o4c#12r#12)2(1"))
        (test* "テンポ・パン・ゲート・デチューン・移調" #t
               (has (render 'opm "t120T130p128q3D4K2K*1K$1c8") "T120T130P128q3D4K2K)1o4c+#12"))
        (test* "範囲外音量にトラック情報を付ける" '("invalid v: 128 (expected an integer from 0 to 127)" lead)
               (guard (e [(sxmml-value-error? e)
                          (list (condition-message e) (sxmml-track-context-track e))])
                 (render 'opm "v128")))
        (test* "手作りのゲート値を検査する" (test-error <sxmml-value-error>)
               (render 'opn "" (^[] (sxmml-ir-register! (make <sxmml-mml-gate-time> :value 1.5) 'lead))))
        (test* "反復・タイ・ループ" #t (has (render 'opm "[c8:c8&c8]2Lr8") "[o4c#12/o4c#12&o4c#12]2Lr#12"))
        (test* "ポルタメントの後も絶対音程を使う" #t (has (render 'opn "/{c >g}4 o6d8") "o4c#24_o5go6d#12"))
        (test* "SSGミキサーとノイズ" #t (has (render 'ssg "P1P2P3w31") "m1,0m0,1m1,1w31"))
        (test* "PCMファイルの先頭・位置・直接指定" #t
               (has (render 'pcm "@0@1@2"
                      (^[] (define-metadata pcmfile '("drums1" "drums2"))
                           (define-pcm-voice 0 0) (define-pcm-voice 1 '(1 25))
                           (define-pcm-voice 2 '("kick" 65535))))
                    "'@ P 0,\"drums1\",i0" "'@ P 1,\"drums2\",i25" "'@ P 2,\"kick\",i65535"))
        (test* "FMP7の数値PCM名にも定義が必要" (test-error <sxmml-reference-error>) (render 'pcm "@0"))
        (test* "PCMファイル名にパスは指定できない" (test-error <sxmml-value-error>)
               (render 'pcm "@0" (^[] (define-pcm-voice 0 '("dir/drums" 0)))))
        (test* "SSGの固定プリセット" #t
               (has (render 'ssg
                      (string-concatenate (map (cut format #f "@{~a}" <>) '(square triangle1 triangle2 pulse1 pulse2 pulse3 pulse4 pulse5 pulse6 pulse7 pulse8 pulse9 sawtooth1 sawtooth2))))
                    "@0,0@1,1@1,2@2,1@2,2@2,3@2,4@2,5@2,6@2,7@2,8@2,9@3,1@3,2"))
        (test* "SSGに未定義の数値プリセット" (test-error <sxmml-reference-error>) (render 'ssg "@0"))
        (test* "FMでエンベロープを拒否" (test-error <sxmml-value-error>) (render 'opm "E0"))
        (test* "O2で逐次クリッピング前の音量命令を保つ" #t
               (has (render 'opn "v120v*20v*-20c8" (^[] #f) 2) "v120)20(20o4c#12"))
        (for-each (^[phrase fragment]
                    (test* phrase #t (has (render 'opn phrase lfo-setup) fragment)))
                  '("Mf M0 c8 Mt" "Mf[c8 M0Mtd8]2" "M0Mt[c8 d8 M1]2" "M0Mf[c8 Mtd8]2"
                    "Mf L c8 M0Mt d8" "Mf[c8:M0d8Mt]2")
                  '("SP0MPV8,2,1,24,0,1,0SP0o4c#12SP1" "MPV8,2,1,24,0,1,0SP1o4d#12"
                    "MPV0,1,2,3,0,1,0]2" "SP0[o4c#12SP1o4d#12]2"
                    "Lo4c#12MPV8,2,1,24,0,1,0SP1" "/MPV8,2,1,24,0,1,0SP0o4d#12SP1"))
        (test* "反復で有効・無効が混在する単独M" (test-error <sxmml-value-error>)
               (render 'opn "Mf[M0 c8 Mt d8]2" lfo-setup))
        (test* "LFOの速度0" (test-error <sxmml-value-error>) (render 'ssg "M0" (^[] (define-lfo 0 '(0 0 1 24)))))
        (test* "未定義のLFO" (test-error <sxmml-reference-error>) (render 'ssg "M0"))
        (test* "未対応のLFOチャネル" (test-error <sxmml-value-error>) (render 'ssg "M{0 E}" lfo-setup))
        (let ([op (make-list 11 0)] [env6 (make-list 6 10)] [env13 (make-list 13 20)])
          (define (fm id)
            (apply sxmml-opn-voice-set! id '(1 2) (make-list 4 op)))
          (define (voices count)
            (dotimes [i count] (fm (string->symbol (number->string i)))))
          (test* "未使用定義を採番せず初出を0にする" #t
                 (has (render 'opn "@256" (cut voices 257)) "'@ FA 0" "'A0 @0"))
          (test* "257個の使用音色は出力できない" (test-error <sxmml-value-error>)
                 (render 'opn (string-concatenate (map (cut format #f "@~a" <>) (iota 257))) (cut voices 257)))
          (parameterize ([sxmml-environment (make <sxmml-environment>)])
            (define-track a :sound-source 'opn)
            (define-track b :sound-source 'opm)
            (define-track c :sound-source 'pcm)
            (define-track d :sound-source 'ssg)
            (fm '|0|)
            (define-opm-voice 0 '(2 3) op op op op)
            (define-pcm-voice 0 '("bank" 25))
            (define-envelope 0 default env6)
            (define-envelope 1 extended env13)
            (mml (a b) "@0c8")
            (mml c "@0E0c8")
            (mml d "E1c8")
            (let1 out (make-sxmml-output-environment (sxmml-environment) :output path :optimize 0)
              (sxmml-fmp7-source-output out)
              (let1 first (file->string path :encoding "cp932")
                (test* "FM共有番号、PCM・E独立番号" #t
                       (has first "'@ FA 0" "'@ FC 1" "'@ P 0" "'A0 @0" "'A1 @1" "'A2 @0E0" "'A3 E1"))
                (test* "PCMとSSGのE/EX" #t
                       (has first "'@ E 0,10,10,10,10,10,10" "'@ EX 1,20,20,20,20,20,20,20,20,20,20,20,20,20"))
                (sxmml-fmp7-source-output out)
                (test* "同じ環境で再出力しても割当と生成物を保持する" (list #f first)
                       (list (slot-ref (sxmml-track-get 'a) 'part) (file->string path :encoding "cp932")))))))
        (for-each (^[kind wave]
                    (test* #"波形 ~kind" #t
                           (has (render 'opn "M0" (^[] (sxmml-lfo-set! '|0| kind '(8 2 1 24 0 -3))))
                                (format #f "MPV8,2,1,24,~a,0,-3" wave))))
                  '(default triangle saw square one-shot random) '(0 0 1 2 3 4))
        (test* "省略とA、B、C、Dのチャネル" #t
               (has (render 'opm "M0M{0 A}M{0 B}M{0 C}M{0 D}" lfo-setup)
                    "MPV8,2,1,24,0,1,0MPV" "MQV" "MRV" "MSV"))
        (test* "日本語のCP932、CRLF、メモと作成者" #t
               (let1 text (render 'pcm ""
                            (^[] (define-metadata creator "作成者")
                                 (define-metadata arranger "編曲者")
                                 (define-metadata memo '("その一" "その二"))))
                 (and (has text "DataCreator=作成者" "Comment=その一 / その二" "\r\n")
                      (not (string-contains text "編曲者")))))
        (test* "出力先未指定" (test-error <sxmml-value-error>)
               (sxmml-fmp7-source-output (make <sxmml-output-environment>))))
    (remove-directory* dir)))

(test-end :exit-on-failure #t)
