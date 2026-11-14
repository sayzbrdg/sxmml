;;; Copyright (c) 2024-2025 Seiji Ohashi <sayzbrdg@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module sxmml.backend.pmd-source
  (use gauche.charconv)
  (use gauche.collection)
  (use file.util)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (use sxmml.output)
  (use sxmml.backend.pmd)
  (use sxmml.voice)
  (use sxmml.util)
  (export sxmml-pmd3-source-output sxmml-pmd4-source-output sxmml-pmd-source-output))
(select-module sxmml.backend.pmd-source)

;;;
;;; class
;;;
(define-class <sxmml-pmd-track-environment> ()
  ([octave :init-keyword :octave :init-value 255]
   [mode :init-keyword :mode :init-value 0]
   [voice-output :init-keyword :voice-output :init-value #f]
   [volume-output :init-keyword :volume-output :init-value #f]
   [volume-change-output :init-keyword :volume-change-output :init-value #f]
   [tempo-output :init-keyword :tempo-output :init-value #f]
   [lfo-output :init-keyword :lfo-output :init-value #f]
   [envelope-output :init-keyword :envelope-output :init-value #f]))

;;;
;;; translate procedure
;;;
(define (sxmml-pmd3-source-output env)
  (%pmd-source-output env #t))

(define (sxmml-pmd4-source-output env)
  (%pmd-source-output env #f))

(define (sxmml-pmd-source-output env)
  (%pmd-source-output env #f))

(define (%track-output env track part voice-proc! vol-set-proc! vol-change-proc!)
  (let ([lis (sxmml-pmd-ir-optimize (slot-ref track 'ir) (slot-ref env 'optimize))]
        [mode (%most-frequent-ticks-find track)])
    (unless (null? lis)
      (let1 state (make <sxmml-pmd-track-environment> :mode (or mode 0)
                       :voice-output voice-proc! :volume-output vol-set-proc!
                       :volume-change-output vol-change-proc!
                       :tempo-output (cut %pmd-tempo-output env <>)
                       :lfo-output (cut %pmd-lfo-output env <>)
                       :envelope-output (cut %pmd-envelope-output env <>))
        (format #t "~a " part)
        (when mode (format #t "l%~d" mode))
        (%pmd-track-output! state lis)
        (display "\r\n\r\n")))))

(define (%fm-track-output env track part voice-proc)
  (%track-output env track part voice-proc %pmd-fm-volume-output %pmd-fm-volume-change-output))

(define (%ssg-track-output env track part)
  (%track-output env track part %pmd-ssg-voice-output %pmd-ssg-volume-output %pmd-ssg-volume-change-output))

(define (%pcm-track-output env track part)
  (%track-output env track part %pmd-pcm-voice-output %pmd-pcm-volume-output %pmd-pcm-volume-change-output))

(define (%pmd-track-output! state lis)
  (let loop ([rest lis] [loop-seen? #f] [depth 0])
    (unless (null? rest)
      (receive (next-loop-seen? next-depth)
          (%pmd-ir-output! state (car rest) loop-seen? depth)
        (loop (cdr rest) next-loop-seen? next-depth)))))

(define (%pmd-ir-output! state obj loop-seen? depth)
  (cond [(is-a? obj <sxmml-mml-rest>)
         (%pmd-rest-output state obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-portamento>)
         (%pmd-portamento-output! state obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-note>)
         (%pmd-note-output! state obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-voice>)
         ((slot-ref state 'voice-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-gate-time>)
         (%pmd-gate-time-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-volume>)
         ((slot-ref state 'volume-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-tempo-absolute>)
         (%pmd-tempo-absolute-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-tempo>)
         ((slot-ref state 'tempo-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-tie>)
         (%pmd-tie-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-detune>)
         (%pmd-detune-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-repeat-start>)
         (%pmd-value-check "repeat depth" (+ depth 1) 1 32)
         (%pmd-repeat-start-output obj)
         (values loop-seen? (+ depth 1))]
        [(is-a? obj <sxmml-mml-repeat-end>)
         (%pmd-repeat-end-output obj)
         (values loop-seen? (- depth 1))]
        [(is-a? obj <sxmml-mml-repeat-break>)
         (%pmd-repeat-break-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-loop>)
         (when loop-seen?
           (error <sxmml-mml-syntax-error> "loop command can be used only once in a track"))
         (%pmd-loop-output obj)
         (values #t depth)]
        [(is-a? obj <sxmml-mml-transpose>)
         (%pmd-transpose-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-lfo-switch>)
         (%pmd-lfo-switch-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-lfo>)
         ((slot-ref state 'lfo-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-envelope>)
         ((slot-ref state 'envelope-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-pan>)
         (%pmd-pan-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-transpose-change>)
         (%pmd-transpose-change-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-volume-change>)
         ((slot-ref state 'volume-change-output) obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-noise-frequency>)
         (%pmd-noise-freq-output obj)
         (values loop-seen? depth)]
        [(is-a? obj <sxmml-mml-output-switch>)
         (%pmd-output-switch-output obj)
         (values loop-seen? depth)]
        [else
         (values loop-seen? depth)]))

(define (%pmd-rhythm-track-output env track)
  (let* ([lis (sxmml-pmd-ir-optimize (slot-ref track 'ir) (slot-ref env 'optimize))]
         [has-loop? (boolean (find (^o (is-a? o <sxmml-mml-loop>)) lis))]
         [state (make <sxmml-pmd-track-environment>)])
    (%pmd-rhythm-definition-track-output state has-loop? lis)
    (%pmd-rhythm-deploy-track-output! state track has-loop? lis)))

(define (%pmd-rhythm-definition-track-output state has-loop? lis)
  (define (output-ir obj depth)
    (cond [(is-a? obj <sxmml-mml-rest>)
           (%pmd-rest-output state obj)
           depth]
          [(is-a? obj <sxmml-rhythm-emit>)
           (%pmd-rhythm-emit-output state obj)
           depth]
          [(is-a? obj <sxmml-mml-repeat-start>)
           (%pmd-value-check "repeat depth" (+ depth 1) 1 32)
           (%pmd-repeat-start-output obj)
           (+ depth 1)]
          [(is-a? obj <sxmml-mml-repeat-end>)
           (%pmd-repeat-end-output obj)
           (- depth 1)]
          [(is-a? obj <sxmml-mml-repeat-break>)
           (%pmd-repeat-break-output obj)
           depth]
          [(is-a? obj <sxmml-rhythm-volume>)
           (%pmd-rhythm-volume-output obj)
           depth]
          [(is-a? obj <sxmml-rhythm-volume-change>)
           (%pmd-rhythm-volume-change-output obj)
           depth]
          [(is-a? obj <sxmml-rhythm-pan>)
           (%pmd-rhythm-pan-output obj)
           depth]
          [else depth]))
  (define (loop rest loop-seen? depth)
    (cond [(null? rest)
           (display "\r\n")
           ()]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (when loop-seen?
             (error <sxmml-mml-syntax-error> "loop command can be used only once in a track"))
           (display "\r\n")
           (cdr rest)]
          [else
           (loop (cdr rest) loop-seen? (output-ir (car rest) depth))]))
  (unless (null? lis)
    (display "R0 ")
    (let1 rest (loop lis #f 0)
      (when has-loop?
        (display "R1 ")
        (loop rest #t 0)))
    (display "\r\n")))

(define (%pmd-rhythm-deploy-track-output! state track has-loop? lis)
  (unless (null? lis)
    (display "K ")
    (and-let1 mode (%most-frequent-ticks-find track)
              (slot-set! state 'mode mode)
              (format #t "l%~d " mode))
    (display "R0")
    (when has-loop?
      (display " L R1"))
    (display "\r\n\r\n")))

(define (%pmd-source-output env pmd3?)
  (let ([output-file (slot-ref env 'output)]
        [voices (slot-ref env 'voice)]
        [voice-numbers (sxmml-pmd-voice-number-table-compute (slot-ref env 'voice))]
        [used-voice-ids (make-hash-table eq-comparator)]
        [tracks (map (cut sxmml-output-track-find env <>) '(A B C D E F G H I J L))])
    (define (fm-voice-output obj)
      (%pmd-fm-voice-output voices voice-numbers obj)
      (hash-table-put! used-voice-ids (slot-ref obj 'value) #t))
    (unless output-file
      (error <sxmml-value-error> "output file is not specified"))
    (sxmml-pmd-check-obsolete-metadata (slot-ref env 'metadata))
    (call-with-temporary-file
        (^[p n]
          (with-output-conversion p
            (^[]
              (%pmd-source-head-output env pmd3?)
              (%pmd-source-meta-output env pmd3?)
              (let1 text
                  (with-output-to-string
                    (^[]
                      (for-each (^[t part] (%fm-track-output env t part fm-voice-output))
                                (take tracks 6) '(A B C D E F))
                      (for-each (^[t part] (%ssg-track-output env t part))
                                (take (drop tracks 6) 3) '(G H I))
                      (%pcm-track-output env (list-ref tracks 9) 'J)
                      (%pmd-rhythm-track-output env (list-ref tracks 10))))
                (%pmd-source-voice-output voices voice-numbers used-voice-ids)
                (display text))
              (when pmd3? (display #\x1a)))
            :encoding "cp932")
          (close-port p)
          (sys-rename n output-file))
      :directory (sys-dirname output-file))
    (make <sxmml-output-result> :output output-file
          :track-names (map (cut slot-ref <> 'name) tracks))))

(define (%pmd-source-head-output env pmd3?)
  (format #t ";; This file was generated by sxmml~a\r\n;; mode: ~a\r\n\r\n"
          (if (slot-ref env 'source)
            (string-append " from " (sys-basename (slot-ref env 'source))) "")
          (if pmd3? "PMDver3.3" "PMDver4.8")))

(define (%pmd-source-meta-output env pmd3?)
  (for-each (^[k] (let1 value (sxmml-pmd-metadata-value (slot-ref env 'metadata) k)
                    (when value
                      (when pmd3?
                        (display "; "))
                      (format #t "#~10a  ~a\r\n" k value))))
            '(title composer arranger pcmfile ppzfile ppsfile))
  (for-each (^[m] (when pmd3?
                    (display "; "))
              (format #t "#~10a  ~a\r\n" "memo" m))
            (sxmml-pmd-metadata-value (slot-ref env 'metadata) 'memo))
  (display "\r\n"))

;; PMD が使うメタデータの値を検査して返す。title などは文字列か、定義がなければ #f を
;; 返す。memo は文字列 1 つか文字列のリストで、文字列のリスト (定義がなければ ()) を返す。
;; -m pmd と pmd-binary が使う


;; PMD は音色を 0 から 255 の番号で指定するので、定義した全ての音色に、id の
;; 辞書順で 0 から番号を振る。id から番号への表を返す


;; 同じ入力から同じ生成物を得るため、音色表の反復順ではなく番号順に出力する
(define (%pmd-source-voice-output voices numbers used-voice-ids)
  (define (check kind value high)
    (%pmd-value-check (string-append "FM voice " kind) value 0 high))
  (for-each
    (^[voice]
      (let ([number (%pmd-fm-voice-number numbers voice)]
            [value (slot-ref voice 'value)])
        (format #t "@~3,'0d" number)
        (match (car value)
          [(al fb)
           (format #t " ~3,'0d ~3,'0d\r\n" (check "AL" al 7) (check "FB" fb 7))])
        (for-each
         (^[operator]
           (match operator
             [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
              (format #t " ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d\r\n"
                      (check "AR" ar 31) (check "DR" dr 31) (check "SR" sr 31)
                      (check "RR" rr 15) (check "SL" sl 15) (check "TL" tl 127)
                      (check "KS" ks 3) (check "ML" ml 15) (check "DT" dt 7)
                      (check "AMS" ams 1))]))
         (cdr value))))
    (sort (filter (^[voice] (hash-table-exists? used-voice-ids (slot-ref voice 'id)))
                  (hash-table-values voices))
          (^[a b] (< (%pmd-fm-voice-number numbers a) (%pmd-fm-voice-number numbers b)))))
  (display "\r\n"))

;;;
;;; operators
;;;

;; 出力する値だけを、項目ごとの両端を含む範囲で検査する
(define (%pmd-value-check kind value min max)
  (unless (and (exact-integer? value) (<= min value max))
    (errorf <sxmml-value-error> "invalid ~a: ~s (expected an integer from ~a to ~a)" kind value min max))
  value)

(define (%pmd-note-check note :optional (kind "note"))
  (unless (exact-integer? note)
    (%pmd-value-check kind note 12 107))
  (unless (<= 12 note 107)
    (let1 named (%note-number->pmd-note note)
      (errorf <sxmml-value-error> "invalid ~a: o~a~a (expected o1c - o8b)"
              kind (car named) (cdr named))))
  note)

;; PMD operator
(define (%pmd-rest-output state obj)
  (let recur ([ticks (slot-ref obj 'ticks)])
    (cond [(> ticks 255)
           (format #t "r%255")
           (recur (- ticks 255))]
          [(= ticks (slot-ref state 'mode))
           (format #t "r")]
          [else
           (format #t "r%~d" ticks)])))

(define (%pmd-note-output! state obj)
  (let recur ([note (%note-number->pmd-note (%pmd-note-check (slot-ref obj 'value)))]
              [ticks (slot-ref obj 'ticks)]
              [octave (slot-ref state 'octave)])
    (cond [(or (> (- (car note) octave) 1) (< (- (car note) octave) -1))
           (slot-set! state 'octave (car note))
           (format #t "o~d~a" (car note) (cdr note))]
          [(= (- (car note) octave) 1)
           (slot-set! state 'octave (car note))
           (format #t ">~a" (cdr note))]
          [(= (- (car note) octave) -1)
           (slot-set! state 'octave (car note))
           (format #t "<~a" (cdr note))]
          [else
           (format #t "~a" (cdr note))])
    (cond [(> ticks 255)
           (format #t "%255&")
           (recur note (- ticks 255) (car note))]
          [(not (= ticks (slot-ref state 'mode)))
           (format #t "%~d" ticks)])))

(define (%pmd-fm-voice-output voices numbers obj)
  (let1 inst (hash-table-get voices (slot-ref obj 'value) #f)
    (unless inst
      (errorf <sxmml-reference-error> "undefined voice: ~a" (slot-ref obj 'value)))
    (format #t "@~d" (%pmd-fm-voice-number numbers inst))))

(define (%pmd-fm-voice-number numbers voice)
  (hash-table-get numbers (slot-ref voice 'id)))

(define (%pmd-gate-time-output obj)
  (format #t "q~d" (%pmd-value-check "gate time" (slot-ref obj 'value) 0 255)))

(define (%pmd-fm-volume-output obj)
  (format #t "V~d" (%pmd-value-check "FM volume" (slot-ref obj 'value) 0 127)))

(define (%pmd-ssg-volume-output obj)
  (format #t "v~d" (%pmd-value-check "SSG volume" (slot-ref obj 'value) 0 15)))

(define (%pmd-pcm-volume-output obj)
  (format #t "V~d" (%pmd-value-check "PCM volume" (slot-ref obj 'value) 0 255)))

(define (%pmd-tempo-absolute-output obj)
  (format #t "T~d" (%pmd-value-check "Timer-B" (slot-ref obj 'value) 0 255)))

(define (%pmd-tempo-output env obj)
  (format #t "T~d" (sxmml-bpm->timer-b (slot-ref obj 'value) (slot-ref env 'resolution))))

(define (%pmd-tie-output obj)
  (format #t "&"))

(define (%pmd-detune-output obj)
  (format #t "D~d" (%pmd-value-check "detune" (slot-ref obj 'value) -32768 32767)))

(define (%pmd-repeat-start-output obj)
  (format #t "["))

(define (%pmd-repeat-end-output obj)
  (format #t "]~d" (%pmd-value-check "repeat count" (slot-ref obj 'value) 1 255)))

(define (%pmd-repeat-break-output obj)
  (format #t ":"))

(define (%pmd-loop-output obj)
  (format #t "L"))

(define (%pmd-transpose-output obj)
  (format #t "_~d" (%pmd-value-check "transpose" (slot-ref obj 'value) -128 127)))

(define (%pmd-lfo-output env obj)
  (let1 lfo (hash-table-get (slot-ref env 'lfo) (slot-ref obj 'value) #f)
    (unless lfo
      (errorf <sxmml-reference-error> "undefined LFO: ~a" (slot-ref obj 'value)))
    (let ([channel (slot-ref obj 'channel)])
      (unless (memq channel '(#f A B))
        (errorf <sxmml-value-error> "invalid LFO channel: ~a" channel))
      (let1 value (sxmml-pmd-lfo-values lfo)
        (display "M")
        (when (eq? channel 'B)
          (display "B"))
        (format #t "~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value))))))

(define (%pmd-lfo-switch-output obj)
  (let ([channel (slot-ref obj 'channel)]
        [switch (slot-ref obj 'value)])
    (unless (memq channel '(#f A B))
      (errorf <sxmml-value-error> "invalid LFO channel: ~a" channel))
    (display "*")
      (when (eq? channel 'B)
        (display "B"))
    (format #t "~d" (if switch 1 0))))

(define (%pmd-envelope-output env obj)
  (let1 envelope (hash-table-get (slot-ref env 'envelope) (slot-ref obj 'value) #f)
    (unless envelope
      (errorf <sxmml-reference-error> "undefined envelope: ~a" (slot-ref obj 'value)))
    (let1 value (sxmml-pmd-envelope-values envelope)
      ;; 書式は値の数で決まるので、5 値 (6 値の書式の 6 つ目を省いた形) も値をそのまま書く
      (format #t "E~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value))
      (for-each (^v (format #t ",~d" v)) (cddddr value)))))

;; PMD の LFO・エンベロープの規則。種類 (波形の種類) の一覧と、値の数ごとの範囲。
;; LFO の triangle は default と同じ意味である。-m pmd と pmd-binary が使う

;; LFO の定義を検査し、値のリストを返す


;; エンベロープの定義を検査し、値のリストを返す


;; 定義 obj の種類、値の数、各値 (正確な整数で範囲の中) を順に検査する。
;; ranges は値の数から範囲の並びへの連想リスト、expected は値の数の説明である


(define (%pmd-pan-output obj)
  (format #t "p~d" (%pmd-value-check "pan" (slot-ref obj 'value) 0 3)))

(define (%pmd-transpose-change-output obj)
  (format #t "__~d" (%pmd-value-check "relative transpose" (slot-ref obj 'value) -128 127)))

(define (%pmd-fm-volume-change-output obj)
  (let1 value (%pmd-value-check "relative volume" (slot-ref obj 'value) -255 255)
    (cond [(= value 4)
           (display ")")]
          [(= value -4)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" value)]
          [else
           (format #t "(%~d" (* value -1))])))

(define (%pmd-ssg-volume-change-output obj)
  (let1 value (%pmd-value-check "relative volume" (slot-ref obj 'value) -255 255)
    (cond [(= value 1)
           (display ")")]
          [(= value -1)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" value)]
          [else
           (format #t "(%~d" (* value -1))])))

(define (%pmd-pcm-volume-change-output obj)
  (let1 value (%pmd-value-check "relative volume" (slot-ref obj 'value) -255 255)
    (cond [(= value 16)
           (display ")")]
          [(= value -16)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" value)]
          [else
           (format #t "(%~d" (* value -1))])))

(define (%pmd-portamento-output! state obj)
  (define (octave-output noteoct)
    (let1 octave (slot-ref state 'octave)
      (cond [(or (> (- noteoct octave) 1) (< (- noteoct octave) -1))
             (slot-set! state 'octave noteoct)
             (format #t "o~d" noteoct)]
            [(= (- noteoct octave) 1)
             (slot-set! state 'octave noteoct)
             (format #t ">")]
            [(= (- noteoct octave) -1)
             (slot-set! state 'octave noteoct)
             (format #t "<")])))
  (let ([start (%note-number->pmd-note
                (%pmd-note-check (car (slot-ref obj 'value)) "portamento start note"))]
        [end (%note-number->pmd-note
              (%pmd-note-check (cdr (slot-ref obj 'value)) "portamento end note"))]
        [ticks (%pmd-value-check "portamento ticks" (slot-ref obj 'ticks) 1 255)])
    (octave-output (car start))
    (format #t "{~a" (cdr start))
    (octave-output (car end))
    (format #t "~a}%~d" (cdr end) ticks)))

(define (%pmd-ssg-voice-output obj)
  (format #t "@~d" (%pmd-voice-number obj 9 "SSG voice number")))

(define (%pmd-pcm-voice-output obj)
  (format #t "@~d" (%pmd-voice-number obj 255 "PCM voice number")))

;; SSG・PCM の音色の値は PMD の @ の数値としてそのまま出力するので、0 から max までの
;; 整数に限る。数でない値は、エラーに書かれた値を示す
(define (%pmd-voice-number obj max kind)
  (let* ([name (slot-ref obj 'value)]
         [value (or (string->number (symbol->string name)) name)])
    (unless (and (exact-integer? value) (<= 0 value max))
      (errorf <sxmml-value-error> "invalid ~a: ~s (expected an integer from 0 to ~a)" kind value max))
    value))

(define (%pmd-noise-freq-output obj)
  (format #t "w~d" (%pmd-value-check "noise frequency" (slot-ref obj 'value) 0 31)))

(define (%pmd-output-switch-output obj)
  (format #t "P~d" (%pmd-value-check "output switch" (slot-ref obj 'value) 1 3)))

;; Rhythm operator
(define (%pmd-rhythm-emit-output state obj)
  (let* ([b (if (slot-ref obj 'bass) (if (slot-ref obj 'value) "\\b" "\\bp") "")]
         [s (if (slot-ref obj 'snare) (if (slot-ref obj 'value) "\\s" "\\sp") "")]
         [t (if (slot-ref obj 'tom) (if (slot-ref obj 'value) "\\t" "\\tp") "")]
         [c (if (slot-ref obj 'cymbal) (if (slot-ref obj 'value) "\\c" "\\cp") "")]
         [h (if (slot-ref obj 'hihat) (if (slot-ref obj 'value) "\\h" "\\hp") "")]
         [i (if (slot-ref obj 'rimshot) (if (slot-ref obj 'value) "\\i" "\\ip") "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)
    (when (slot-ref obj 'value)
      (%pmd-rest-output state (make <sxmml-mml-rest> :value #f :ticks (slot-ref obj 'ticks))))))

(define (%pmd-rhythm-volume-output obj)
  (let ([v (if-let1 a (slot-ref obj 'value) (format "\\V~d" (%pmd-value-check "rhythm volume" a 0 63)) "")]
        [b (if-let1 a (slot-ref obj 'bass) (format "\\vb~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~d" (%pmd-value-check "rhythm instrument volume" a 0 31)) "")])
    (format #t "~a~a~a~a~a~a~a" v b s t c h i)))

(define (%pmd-rhythm-volume-change-output obj)
  (let ([b (if-let1 a (slot-ref obj 'bass) (format "\\vb~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~@d" (%pmd-value-check "relative rhythm volume" a -31 31)) "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)))

(define (%pmd-rhythm-pan-output obj)
  ;; PMD のリズムのパンは右・左・中央だけで、1・2・3 に当たる
  (define (n->c num)
    (case num
      [(1) #\r]
      [(2) #\l]
      [(3) #\m]
      [(#f) #f]
      [else (errorf <sxmml-value-error> "invalid rhythm pan: ~s (expected an integer from 1 to 3)" num)]))
  (let ([b (if-let1 a (n->c (slot-ref obj 'bass)) (format "\\~ab" a) "")]
        [s (if-let1 a (n->c (slot-ref obj 'snare)) (format "\\~as" a) "")]
        [t (if-let1 a (n->c (slot-ref obj 'tom)) (format "\\~at" a) "")]
        [c (if-let1 a (n->c (slot-ref obj 'cymbal)) (format "\\~ac" a) "")]
        [h (if-let1 a (n->c (slot-ref obj 'hihat)) (format "\\~ah" a) "")]
        [i (if-let1 a (n->c (slot-ref obj 'rimshot)) (format "\\~ai" a) "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)))

;; Misc
;; 音高は 12 * オクターブ + 音名 (0～11) なので、負の音高も floor/ でオクターブと音名に分ける
(define-inline (%note-number->pmd-note note)
  (receive (octave notenum) (floor/ note 12)
    (cons octave (case notenum
                   [(0) "c"]
                   [(1) "c+"]
                   [(2) "d"]
                   [(3) "e-"]
                   [(4) "e"]
                   [(5) "f"]
                   [(6) "f+"]
                   [(7) "g"]
                   [(8) "a-"]
                   [(9) "a"]
                   [(10) "b-"]
                   [(11) "b"])
                       )))

;; 環境の output slot の出力先を返す。未設定の場合は入力名の拡張子を .mml に
;; 替えたもの (入力名がなければ ./a.mml) を既定値として slot に設定して返す


;; 出力先の指定に使われていた filename メタデータは無視し、警告を表示する


(define (%most-frequent-ticks-find track)
  (and-let1 mode-pair (find-max (slot-ref track 'ticks-statistics) :key (^p (cdr p)))
    (car mode-pair)))

(sxmml-mode-register!
 (make <sxmml-mode> :name 'pmd-source :aliases '(pmd pmd4-source)
       :setup sxmml-pmd-setup! :backend sxmml-pmd-source-output
       :output-extension "mml"))
(sxmml-mode-register!
 (make <sxmml-mode> :name 'pmd3-source :aliases '(pmd3)
       :setup sxmml-pmd-setup! :backend sxmml-pmd3-source-output
       :output-extension "mml"))
