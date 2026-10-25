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

(define-module sxmml.backend.pmd
  (use gauche.charconv)
  (use gauche.collection)
  (use file.util)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (use sxmml.optimizer)
  (use sxmml.voice)
  (use sxmml.util)
  (export sxmml-pmd3-source-output sxmml-pmd4-source-output sxmml-pmd-source-output
          pmd3-source pmd4-source pmd-source pmd3 pmd
          sxmml-pmd-voice-number-table-compute sxmml-pmd-lfo-values sxmml-pmd-envelope-values
          sxmml-pmd-metadata-value
          ))
(select-module sxmml.backend.pmd)

;;;
;;; class
;;;
(define-class <sxmml-pmd-track-environment> ()
  ([octave :init-keyword :octave :init-value 255]
   [mode :init-keyword :mode :init-value 0]))

(define sxmml-pmd-track-environment
  (make-parameter #f))

;; 生成中の音色番号の表 (sxmml-pmd-voice-number-table-compute の値)
(define %pmd-voice-numbers
  (make-parameter #f))

;;;
;;; mode
;;;
(define (pmd3-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd3-source-output)
  (slot-set! (sxmml-environment) 'mode "PMD3")
  (%pmd-source))
(define (pmd4-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd4-source-output)
  (slot-set! (sxmml-environment) 'mode "PMD4")
  (%pmd-source))
(define (pmd-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd-source-output)
  (slot-set! (sxmml-environment) 'mode "PMD")
  (%pmd-source))
(define pmd3 pmd3-source)
(define pmd pmd-source)

(define (%pmd-source)
  (slot-set! (sxmml-environment) 'output-extension "mml")
  (define-track (A B C D E F) :volume-delta 4)
  (define-track (G H I L))
  (define-track J :volume-delta 16))

;;;
;;; translate procedure
;;;
(define (sxmml-pmd3-source-output)
  (%sxmml-pmdx-source-output #t))

(define (sxmml-pmd4-source-output)
  (%sxmml-pmdx-source-output #f))

(define (sxmml-pmd-source-output :optional (mode #f))
  (%sxmml-pmdx-source-output mode))

(define (%sxmml-pmdx-source-output mode)
  (parameterize ([%pmd-voice-numbers (sxmml-pmd-voice-number-table-compute)])
    (%pmd-source-output mode))
  (sxmml-environment))

(define (%track-output track part voice-proc! vol-set-proc! vol-change-proc!)
  (let ([lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize))]
        [mode (%most-frequent-ticks-find track)])
    (unless (null? lis)
      (parameterize ([sxmml-pmd-track-environment (make <sxmml-pmd-track-environment>)])
        (format #t "~a " part)
        (when mode
          (slot-set! (sxmml-pmd-track-environment) 'mode mode)
          (format #t "l%~d" mode))
        (%pmd-track-output lis
                             voice-proc!
                             vol-set-proc!
                             vol-change-proc!)
        (display "\r\n\r\n")))))

(define (%fm-track-output track part voice-proc)
  (%track-output track part voice-proc %pmd-fm-volume-output %pmd-fm-volume-change-output))

(define (%ssg-track-output track part)
  (%track-output track part %pmd-ssg-voice-output %pmd-ssg-volume-output %pmd-ssg-volume-change-output))

(define (%pcm-track-output track part)
  (%track-output track part %pmd-pcm-voice-output %pmd-pcm-volume-output %pmd-pcm-volume-change-output))

(define (%pmd-track-output lis voiceproc volproc volcproc)
  (let loop ([rest lis]
             [loop-seen? #f])
    (cond [(null? rest)
           ()]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (%pmd-rest-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-portamento>)
           (%pmd-portamento-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-note>)
           (%pmd-note-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (voiceproc (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-gate-time>)
           (%pmd-gate-time-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (volproc (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-tempo-absolute>)
           (%pmd-tempo-absolute-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-tempo>)
           (%pmd-tempo-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-tie>)
           (%pmd-tie-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (%pmd-detune-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (%pmd-repeat-start-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (%pmd-repeat-end-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (%pmd-repeat-break-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (when loop-seen?
             (error "loop command can be used only once in a track"))
           (%pmd-loop-output (car rest))
           (loop (cdr rest) #t)]
          [(is-a? (car rest) <sxmml-mml-transpose>)
           (%pmd-transpose-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (%pmd-lfo-switch-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (%pmd-lfo-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (%pmd-envelope-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (%pmd-pan-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-transpose-change>)
           (%pmd-transpose-change-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (volcproc (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-noise-frequency>)
           (%pmd-noise-freq-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-output-switch>)
           (%pmd-output-switch-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [else
           (loop (cdr rest) loop-seen?)])))

(define (%pmd-rhythm-track-output track)
  (define lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize)))
  (define has-loop? (boolean (find (^o (is-a? o <sxmml-mml-loop>)) lis)))
  (parameterize ([sxmml-pmd-track-environment (make <sxmml-pmd-track-environment>)])
    ;; R-channel
    (%pmd-rhythm-definition-track-output has-loop? lis)
    ;; K-channel
    (%pmd-rhythm-deploy-track-output has-loop? lis)))

(define (%pmd-rhythm-definition-track-output has-loop? lis)
  (define (loop rest loop-seen?)
    (cond [(null? rest)
           (display "\r\n")
           ()]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (when loop-seen?
             (error "loop command can be used only once in a track"))
           (display "\r\n")
           (cdr rest)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (%pmd-rest-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-rhythm-emit>)
           (%pmd-rhythm-emit-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (%pmd-repeat-start-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (%pmd-repeat-end-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (%pmd-repeat-break-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-rhythm-volume>)
           (%pmd-rhythm-volume-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-rhythm-volume-change>)
           (%pmd-rhythm-volume-change-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [(is-a? (car rest) <sxmml-rhythm-pan>)
           (%pmd-rhythm-pan-output (car rest))
           (loop (cdr rest) loop-seen?)]
          [else
           (loop (cdr rest) loop-seen?)]))
  (unless (null? lis)
    (display "R0 ")
    (let1 rest (loop lis #f)
      (when has-loop?
        (display "R1 ")
        (loop rest #t)))
    (display "\r\n")))

(define (%pmd-rhythm-deploy-track-output has-loop? lis)
  (unless (null? lis)
    (display "K ")
    (and-let1 mode (%most-frequent-ticks-find 'L)
              (slot-set! (sxmml-pmd-track-environment) 'mode mode)
              (format #t "l%~d " mode))
    (display "R0")
    (when has-loop?
      (display " L R1"))
    (display "\r\n\r\n")))

(define (%pmd-source-output :optional (pmd3? #f))
  (%check-obsolete-metadata)
  (let ([output-file (%output-file-get)]
        [used-voice-ids (make-hash-table eq-comparator)])
    (define (fm-voice-output obj)
      (%pmd-fm-voice-output obj)
      (hash-table-put! used-voice-ids (slot-ref obj 'value) #t))
    (call-with-temporary-file
        (^[p n]
          (with-output-conversion p
            (^[]
              (%pmd-source-head-output pmd3?)
              (%pmd-source-meta-output pmd3?)
              (let1 tracks
                  (with-output-to-string
                    (^[]
                      (for-each (^[t p] (%fm-track-output t p fm-voice-output))
                                (map sxmml-track-find '(A B C D E F)) '(A B C D E F))
                      (for-each (^[t p] (%ssg-track-output t p))
                                (map sxmml-track-find '(G H I)) '(G H I))
                      (%pcm-track-output (sxmml-track-find 'J) 'J)
                      (%pmd-rhythm-track-output (sxmml-track-find 'L))))
                (%pmd-source-voice-output used-voice-ids)
                (display tracks))
              (when pmd3?
                (display #\x1a)))
            :encoding "cp932")
          (close-port p)
          (sys-rename n output-file))
      :directory (sys-dirname output-file))
    output-file))

(define (%pmd-source-head-output :optional (pmd3? #f))
  (format #t ";; This file was generated by sxmml from ~a\r\n;; mode: ~a\r\n\r\n"
          (sys-basename (sxmml-source-file)) (if pmd3? "PMDver3.3" "PMDver4.8")))

(define (%pmd-source-meta-output :optional (pmd3? #f))
  (for-each (^[k] (let1 value (sxmml-pmd-metadata-value k)
                    (when value
                      (when pmd3?
                        (display "; "))
                      (format #t "#~10a  ~a\r\n" k value))))
            '(title composer arranger pcmfile ppzfile ppsfile))
  (for-each (^[m] (when pmd3?
                    (display "; "))
              (format #t "#~10a  ~a\r\n" "memo" m))
            (sxmml-pmd-metadata-value 'memo))
  (display "\r\n"))

;; PMD が使うメタデータの値を検査して返す。title などは文字列か、定義がなければ #f を
;; 返す。memo は文字列 1 つか文字列のリストで、文字列のリスト (定義がなければ ()) を返す。
;; -m pmd と pmd-binary が使う
(define (sxmml-pmd-metadata-value key)
  (let1 value (sxmml-metadata-get key)
    (if (eq? key 'memo)
      (cond [(not value) ()]
            [(string? value) (list value)]
            [(and (list? value) (every string? value)) value]
            [else (errorf "invalid metadata value: memo is ~s (expected a string or a list of strings)" value)])
      (begin
        (unless (or (not value) (string? value))
          (errorf "invalid metadata value: ~a is ~s (expected a string)" key value))
        value))))

;; PMD は音色を 0 から 255 の番号で指定するので、定義した全ての音色に、id の
;; 辞書順で 0 から番号を振る。id から番号への表を返す
(define (sxmml-pmd-voice-number-table-compute)
  (let ([ids (sort (hash-table-keys (slot-ref (sxmml-environment) 'voice))
                   (^[a b] (string<? (symbol->string a) (symbol->string b))))]
        [table (make-hash-table eq-comparator)])
    (when (> (length ids) 256)
      (errorf "too many voices: ~a (max 256)" (length ids)))
    (for-each (^[id number] (hash-table-put! table id number)) ids (iota (length ids)))
    table))

;; 同じ入力から同じ生成物を得るため、音色表の反復順ではなく番号順に出力する
(define (%pmd-source-voice-output used-voice-ids)
  (for-each
    (^[voice]
      (let ([number (%pmd-fm-voice-number voice)]
            [value (slot-ref voice 'value)])
        (format #t "@~3,'0d" number)
        (match (car value)
          [(al fb)
           (format #t " ~3,'0d ~3,'0d\r\n" al fb)])
        (let loop ([rest (cdr value)])
          (unless (null? rest)
            (match (car rest)
              [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
               (format #t " ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d\r\n"
                       ar dr sr rr sl tl ks ml dt ams)
               (loop (cdr rest))])))))
    (sort (filter (^[voice] (hash-table-exists? used-voice-ids (slot-ref voice 'id)))
                  (hash-table-values (slot-ref (sxmml-environment) 'voice)))
          (^[a b] (< (%pmd-fm-voice-number a) (%pmd-fm-voice-number b)))))
  (display "\r\n"))

;;;
;;; operators
;;;

;; PMD operator
(define (%pmd-rest-output obj)
  (let recur ([ticks (slot-ref obj 'ticks)])
    (cond [(> ticks 255)
           (format #t "r%255")
           (recur (- ticks 255))]
          [(= ticks (slot-ref (sxmml-pmd-track-environment) 'mode))
           (format #t "r")]
          [else
           (format #t "r%~d" ticks)])))

(define (%pmd-note-output obj)
  (let recur ([note (%note-number->pmd-note (slot-ref obj 'value))]
              [ticks (slot-ref obj 'ticks)]
              [octave (slot-ref (sxmml-pmd-track-environment) 'octave)])
    (cond [(or (> (- (car note) octave) 1) (< (- (car note) octave) -1))
           (slot-set! (sxmml-pmd-track-environment) 'octave (car note))
           (format #t "o~d~a" (car note) (cdr note))]
          [(= (- (car note) octave) 1)
           (slot-set! (sxmml-pmd-track-environment) 'octave (car note))
           (format #t ">~a" (cdr note))]
          [(= (- (car note) octave) -1)
           (slot-set! (sxmml-pmd-track-environment) 'octave (car note))
           (format #t "<~a" (cdr note))]
          [else
           (format #t "~a" (cdr note))])
    (cond [(> ticks 255)
           (format #t "%255&")
           (recur note (- ticks 255) (car note))]
          [(not (= ticks (slot-ref (sxmml-pmd-track-environment) 'mode)))
           (format #t "%~d" ticks)])))

(define (%pmd-fm-voice-output obj)
  (let1 inst (hash-table-get (slot-ref (sxmml-environment) 'voice) (slot-ref obj 'value) #f)
    (unless inst
      (error "no instrument define: " (slot-ref obj 'value)))
    (format #t "@~d" (%pmd-fm-voice-number inst))))

(define (%pmd-fm-voice-number voice)
  (hash-table-get (%pmd-voice-numbers) (slot-ref voice 'id)))

(define (%pmd-gate-time-output obj)
  (format #t "q~d" (slot-ref obj 'value)))

(define (%pmd-fm-volume-output obj)
  (format #t "V~d" (slot-ref obj 'value)))

(define (%pmd-ssg-volume-output obj)
  (format #t "v~d" (slot-ref obj 'value)))

(define (%pmd-pcm-volume-output obj)
  (format #t "V~d" (slot-ref obj 'value)))

(define (%pmd-tempo-absolute-output obj)
  (format #t "T~d" (slot-ref obj 'value)))

(define (%pmd-tempo-output obj)
  (format #t "T~d" (sxmml-bpm->timer-b (slot-ref obj 'value))))

(define (%pmd-tie-output obj)
  (format #t "&"))

(define (%pmd-detune-output obj)
  (format #t "D~d" (slot-ref obj 'value)))

(define (%pmd-repeat-start-output obj)
  (format #t "["))

(define (%pmd-repeat-end-output obj)
  (format #t "]~d" (slot-ref obj 'value)))

(define (%pmd-repeat-break-output obj)
  (format #t ":"))

(define (%pmd-loop-output obj)
  (format #t "L"))

(define (%pmd-transpose-output obj)
  (format #t "_~d" (slot-ref obj 'value)))

(define (%pmd-lfo-output obj)
  (let1 lfo (hash-table-get (slot-ref (sxmml-environment) 'lfo) (slot-ref obj 'value) #f)
    (unless lfo
      (error "no lfo define: " (slot-ref obj 'value)))
    (let ([channel (slot-ref obj 'channel)])
      (unless (memq channel '(#f A B))
        (errorf "invalid lfo channel: ~a" channel))
      (let1 value (sxmml-pmd-lfo-values lfo)
        (display "M")
        (when (eq? channel 'B)
          (display "B"))
        (format #t "~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value))))))

(define (%pmd-lfo-switch-output obj)
  (let ([channel (slot-ref obj 'channel)]
        [switch (slot-ref obj 'value)])
    (unless (memq channel '(#f A B))
      (errorf "invalid lfo channel: ~a" channel))
    (display "*")
      (when (eq? channel 'B)
        (display "B"))
    (format #t "~d" (if switch 1 0))))

(define (%pmd-envelope-output obj)
  (let1 envelope (hash-table-get (slot-ref (sxmml-environment) 'envelope) (slot-ref obj 'value) #f)
    (unless envelope
      (error "no envelope define: " (slot-ref obj 'value)))
    (let1 value (sxmml-pmd-envelope-values envelope)
      ;; 書式は値の数で決まるので、5 値 (6 値の書式の 6 つ目を省いた形) も値をそのまま書く
      (format #t "E~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value))
      (for-each (^v (format #t ",~d" v)) (cddddr value)))))

;; PMD の LFO・エンベロープの規則。種類 (波形の種類) の一覧と、値の数ごとの範囲。
;; LFO の triangle は default と同じ意味である。-m pmd と pmd-binary が使う
(define %pmd-lfo-types '(default triangle))
(define %pmd-lfo-ranges
  '((4 . ((0 255) (0 255) (-128 127) (0 255)))))
(define %pmd-envelope-types '(default))
(define %pmd-envelope-ranges
  '((4 . ((0 255) (-15 15) (0 255) (0 255)))
    (5 . ((0 31) (0 31) (0 31) (0 15) (0 15)))
    (6 . ((0 31) (0 31) (0 31) (0 15) (0 15) (0 15)))))

;; LFO の定義を検査し、値のリストを返す
(define (sxmml-pmd-lfo-values lfo)
  (%pmd-definition-values "lfo" lfo %pmd-lfo-types %pmd-lfo-ranges "4"))

;; エンベロープの定義を検査し、値のリストを返す
(define (sxmml-pmd-envelope-values envelope)
  (%pmd-definition-values "envelope" envelope %pmd-envelope-types %pmd-envelope-ranges "4, 5 or 6"))

;; 定義 obj の種類、値の数、各値 (正確な整数で範囲の中) を順に検査する。
;; ranges は値の数から範囲の並びへの連想リスト、expected は値の数の説明である
(define (%pmd-definition-values kind obj types ranges expected)
  (let ([id (slot-ref obj 'id)]
        [type (slot-ref obj 'type)]
        [value (slot-ref obj 'value)])
    (unless (memq type types)
      (errorf "unsupported ~a type: ~a in ~a (supported: ~a)"
              kind type id (string-join (map symbol->string types) ", ")))
    (let1 range (assv (length value) ranges)
      (unless range
        (errorf "invalid ~a values: ~a has ~a value~a (expected ~a)"
                kind id (length value) (if (= (length value) 1) "" "s") expected))
      (for-each (^[v r i]
                  (unless (and (exact-integer? v) (<= (car r) v (cadr r)))
                    (errorf (if (number? v)
                              "invalid ~a value: ~a, value ~a is ~a (~a - ~a)"
                              "invalid ~a value: ~a, value ~a is ~s (~a - ~a)")
                            kind id i v (car r) (cadr r))))
                value (cdr range) (iota (length value) 1))
      value)))

(define (%pmd-pan-output obj)
  (format #t "p~d" (slot-ref obj 'value)))

(define (%pmd-transpose-change-output obj)
  (format #t "__~d" (slot-ref obj 'value)))

(define (%pmd-fm-volume-change-output obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 4)
           (display ")")]
          [(= value -4)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-ssg-volume-change-output obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 1)
           (display ")")]
          [(= value -1)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-pcm-volume-change-output obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 16)
           (display ")")]
          [(= value -16)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-portamento-output obj)
  (define (octave-output noteoct)
    (let1 octave (slot-ref (sxmml-pmd-track-environment) 'octave)
      (cond [(or (> (- noteoct octave) 1) (< (- noteoct octave) -1))
             (slot-set! (sxmml-pmd-track-environment) 'octave noteoct)
             (format #t "o~d" noteoct)]
            [(= (- noteoct octave) 1)
             (slot-set! (sxmml-pmd-track-environment) 'octave noteoct)
             (format #t ">")]
            [(= (- noteoct octave) -1)
             (slot-set! (sxmml-pmd-track-environment) 'octave noteoct)
             (format #t "<")])))
  (let ([start (%note-number->pmd-note (car (slot-ref obj 'value)))]
        [end (%note-number->pmd-note (cdr (slot-ref obj 'value)))]
        [ticks (slot-ref obj 'ticks)])
    (octave-output (car start))
    (format #t "{~a" (cdr start))
    (octave-output (car end))
    (format #t "~a}%~d" (cdr end) ticks)))

(define (%pmd-ssg-voice-output obj)
  (format #t "@~d" (%pmd-voice-number obj 9)))

(define (%pmd-pcm-voice-output obj)
  (format #t "@~d" (%pmd-voice-number obj 255)))

;; SSG・PCM の音色の値は PMD の @ の数値としてそのまま出力するので、0 から max までの
;; 整数に限る。数でない値は、エラーに書かれた値を示す
(define (%pmd-voice-number obj max)
  (let* ([name (slot-ref obj 'value)]
         [value (or (string->number (symbol->string name)) name)])
    (unless (and (exact-integer? value) (<= 0 value max))
      (error "invalid voice parameter: " value))
    value))

(define (%pmd-noise-freq-output obj)
  (format #t "w~d" (slot-ref obj 'value)))

(define (%pmd-output-switch-output obj)
  (format #t "P~d" (slot-ref obj 'value)))

;; Rhythm operator
(define (%pmd-rhythm-emit-output obj)
  (let* ([b (if (slot-ref obj 'bass) (if (slot-ref obj 'value) "\\b" "\\bp") "")]
         [s (if (slot-ref obj 'snare) (if (slot-ref obj 'value) "\\s" "\\sp") "")]
         [t (if (slot-ref obj 'tom) (if (slot-ref obj 'value) "\\t" "\\tp") "")]
         [c (if (slot-ref obj 'cymbal) (if (slot-ref obj 'value) "\\c" "\\cp") "")]
         [h (if (slot-ref obj 'hihat) (if (slot-ref obj 'value) "\\h" "\\hp") "")]
         [i (if (slot-ref obj 'rimshot) (if (slot-ref obj 'value) "\\i" "\\ip") "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)
    (when (slot-ref obj 'value)
      (%pmd-rest-output (make <sxmml-mml-rest> :value #f :ticks (slot-ref obj 'ticks))))))

(define (%pmd-rhythm-volume-output obj)
  (let ([v (if-let1 a (slot-ref obj 'value) (format "\\V~d" a) "")]
        [b (if-let1 a (slot-ref obj 'bass) (format "\\vb~d" a) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~d" a) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~d" a) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~d" a) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~d" a) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~d" a) "")])
    (format #t "~a~a~a~a~a~a~a" v b s t c h i)))

(define (%pmd-rhythm-volume-change-output obj)
  (let ([b (if-let1 a (slot-ref obj 'bass) (format "\\vb~@d" a) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~@d" a) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~@d" a) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~@d" a) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~@d" a) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~@d" a) "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)))

(define (%pmd-rhythm-pan-output obj)
  ;; PMD のリズムのパンは右・左・中央だけで、1・2・3 に当たる
  (define (n->c num)
    (case num
      [(1) #\r]
      [(2) #\l]
      [(3) #\m]
      [(#f) #f]
      [else (errorf "rhythm pan out of range: ~a (1 - 3)" num)]))
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
(define (%output-file-get)
  (or (slot-ref (sxmml-environment) 'output)
      (let1 output (or (and-let* ([current-file (sxmml-source-file)])
                         (path-swap-extension current-file
                                              (slot-ref (sxmml-environment) 'output-extension)))
                       "./a.mml")
        (slot-set! (sxmml-environment) 'output output)
        output)))

;; 出力先の指定に使われていた filename メタデータは無視し、警告を表示する
(define (%check-obsolete-metadata)
  (when (sxmml-metadata-get 'filename)
    (format (current-error-port)
            "warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file\n")))

(define (%most-frequent-ticks-find track)
  (and-let1 mode-pair
            (find-max (slot-ref (hash-table-get (slot-ref (sxmml-environment) 'track) track) 'ticks-statistics)
                      :key (^p (cdr p)))
            (car mode-pair)))
