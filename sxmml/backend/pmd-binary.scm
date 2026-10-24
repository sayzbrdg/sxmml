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

(define-module sxmml.backend.pmd-binary
  (use gauche.charconv)
  (use gauche.uvector)
  (use file.util)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (use sxmml.optimizer)
  (use sxmml.voice)
  (use sxmml.util)
  (use sxmml.backend.pmd :only (sxmml-pmd-voice-number-table-compute sxmml-pmd-lfo-values sxmml-pmd-envelope-values
                                sxmml-pmd-metadata-value))
  (export <sxmml-pmd-binary-environment>
          sxmml-pmd3-binary-output sxmml-pmd4-binary-output sxmml-pmd-binary-output
          pmd3-binary pmd4-binary pmd-binary
          ))
(select-module sxmml.backend.pmd-binary)

;;;
;;; class
;;;
(define-class <sxmml-pmd-binary-environment> ()
  ([offset :init-keyword :offset] [mode :init-keyword :mode :init-value #f]
   [head :init-form (make-u8vector 26 0)] [head-length :init-keyword :head-length :init-value 26]
   [track :init-form (make-vector 13)] [track-length :init-form (make-vector 13 0)]
   [max-total-ticks :init-value 0] [max-loop-ticks :init-value 0]
   [used-fm-voices :init-form (make-hash-table eq-comparator)]
   ;; 音色番号の表。環境は生成の最初に作るので、何かを書く前に番号が決まる
   [voice-numbers :init-form (sxmml-pmd-voice-number-table-compute)]))

(define-method initialize ([self <sxmml-pmd-binary-environment>] initargs)
  (next-method)
  (let1 track (slot-ref self 'track)
    (vector-for-each-with-index (^[i v] (vector-set! track i (make-u8vector 65600 0))) track)))

(define sxmml-pmd-binary-environment
  (make-parameter #f))

;; 大きさの上限を超えたときに、エラーに表示する生成中の場所
(define %pmd-section (make-parameter #f))

;;;
;;; mode
;;;
(define (pmd3-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd3-binary-output)
  (slot-set! (sxmml-environment) 'mode "pmd3-binary")
  (%pmd-binary))
(define (pmd4-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd4-binary-output)
  (slot-set! (sxmml-environment) 'mode "pmd4-binary")
  (%pmd-binary))
(define (pmd-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd-binary-output)
  (slot-set! (sxmml-environment) 'mode "pmd-binary")
  (%pmd-binary))
(define (%pmd-binary)
  (slot-set! (sxmml-environment) 'output-extension "m")
  (define-track (A B C D E F) :volume-delta 4)
  (define-track (G H I L))
  (define-track J :volume-delta 16))

;;;
;;; translate procedure
;;;
(define (sxmml-pmd3-binary-output)
  (%sxmml-pmdx-binary-output 'fc88))

(define (sxmml-pmd4-binary-output)
  (%sxmml-pmdx-binary-output #f))

(define (sxmml-pmd-binary-output :optional (mode #f))
  (%sxmml-pmdx-binary-output mode))

(define (%sxmml-pmdx-binary-output mode)
  (parameterize ([sxmml-pmd-binary-environment
                  (make <sxmml-pmd-binary-environment> :offset 26 :head-length 26 :mode mode)])
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 0 (slot-ref (sxmml-pmd-binary-environment) 'head-length))
    (for-each (^[name i h] (let1 t (sxmml-track-find name)
                             (parameterize ([%pmd-section #"track ~name"])
                               (%fm-track-u8vector-set! i
                                                        h
                                                        (sxmml-optimize-tree t :optimize (slot-ref (sxmml-environment) 'optimize))))
                             (%ticks-update! t)))
              '(A B C D E F)
              (iota 6)
              (iota 6 2 2))
    (for-each (^[name i h] (let1 t (sxmml-track-find name)
                             (parameterize ([%pmd-section #"track ~name"])
                               (%ssg-track-u8vector-set! i
                                                         h
                                                         (sxmml-optimize-tree t :optimize (slot-ref (sxmml-environment) 'optimize))))
                             (%ticks-update! t)))
              '(G H I)
              (iota 3 6)
              (iota 3 14 2))
    (let1 pcm-track (sxmml-track-find 'J)
      (parameterize ([%pmd-section "track J"])
        (%pcm-track-u8vector-set! 9 20 (sxmml-optimize-tree pcm-track :optimize (slot-ref (sxmml-environment) 'optimize))))
      (%ticks-update! pcm-track))
    ;; R トラックの末尾に全トラックの最大の tick を書くので、L トラックの tick は書く前に集計する。
    ;; トラックの tick は sxmml-optimize-tree が設定する
    (let* ([rhythm-track (sxmml-track-find 'L)]
           [lis (sxmml-optimize-tree rhythm-track :optimize (slot-ref (sxmml-environment) 'optimize))])
      (%ticks-update! rhythm-track)
      (parameterize ([%pmd-section "track L"])
        (%pmd-rhythm-track-u8vector-set! lis)))
    (%pmd-meta-track-u8vector-set!)
    ;; 書き込みの途中の検査は作業用のバッファを守るもので、最後に全体の大きさを検査する
    (%size-check (slot-ref (sxmml-pmd-binary-environment) 'offset))
    (%pmd-binary-output)
    (sxmml-environment)))


(define (%fm-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-fm-voice-set!
                            %pmd-fm-volume-set!
                            %pmd-fm-volume-change-set!))

(define (%ssg-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-ssg-voice-set!
                            %pmd-ssg-volume-set!
                            %pmd-ssg-volume-change-set!))

(define (%pcm-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-pcm-voice-set!
                            %pmd-pcm-volume-set!
                            %pmd-pcm-volume-change-set!))

(define (%pmd-track-u8vector-set! index hoffset lis voiceproc volproc volcproc)
  (define loop-seen? #f)
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) index))
  (define overall-offset (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define repeat-stack ())
  (let loop ([rest lis]
             [track-offset 0])
    (%size-check (+ overall-offset track-offset))
    (cond [(null? rest)
           (u8vector-set! u8vec track-offset #x80)
           (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) index (+ track-offset 1))
           (slot-set! (sxmml-pmd-binary-environment) 'offset (+ overall-offset track-offset 1))
           (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
             (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) hoffset lb)
             (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) (+ hoffset 1) hb))
           (sxmml-pmd-binary-environment)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) (%pmd-rest-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-portamento>)
           (loop (cdr rest) (%pmd-portamento-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (%pmd-note-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (loop (cdr rest) (voiceproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-gate-time>)
           (loop (cdr rest) (%pmd-gate-time-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (loop (cdr rest) (volproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tempo-absolute>)
           (loop (cdr rest) (%pmd-tempo-absolute-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tempo>)
           (loop (cdr rest) (%pmd-tempo-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tie>)
           (loop (cdr rest) (%pmd-tie-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (loop (cdr rest) (%pmd-detune-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (push! repeat-stack (cons (+ track-offset 1) 0))
           (loop (cdr rest) (%pmd-repeat-start-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (loop (cdr rest) (%pmd-repeat-end-set! u8vec track-offset (car rest) (pop! repeat-stack) overall-offset))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (set! (cdar repeat-stack) (+ track-offset 1))
           (loop (cdr rest) (%pmd-repeat-break-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (when loop-seen?
             (error "loop command can be used only once in a track"))
           (set! loop-seen? #t)
           (loop (cdr rest) (%pmd-loop-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-transpose>)
           (loop (cdr rest) (%pmd-transpose-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (loop (cdr rest) (%pmd-lfo-switch-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (loop (cdr rest) (%pmd-lfo-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (loop (cdr rest) (%pmd-envelope-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (loop (cdr rest) (%pmd-pan-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-transpose-change>)
           (loop (cdr rest) (%pmd-transpose-change-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (loop (cdr rest) (volcproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-noise-frequency>)
           (loop (cdr rest) (%pmd-noise-freq-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-output-switch>)
           (loop (cdr rest) (%pmd-output-switch-set! u8vec track-offset (car rest)))]
          [else
           (loop (cdr rest) track-offset)])))

(define (%pmd-rhythm-track-u8vector-set! lis)
  (define has-loop? (boolean (find (^o (is-a? o <sxmml-mml-loop>)) lis)))
  ;; Track11(K-channel)
  (%pmd-rhythm-deploy-track-u8vector-set! has-loop? lis)
  (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 22 lb)
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 23 hb))
  ;; Track12(R-channel)
  (%pmd-rhythm-definition-track-u8vector-set! has-loop? lis))

(define (%pmd-rhythm-deploy-track-u8vector-set! has-loop? lis)
  (define (overall-offset) (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 10))
  (cond [(null? lis)
         (u8vector-set! u8vec 0 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 1)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 1))]
        [has-loop?
         (u8vector-set! u8vec 0 #x00)
         (u8vector-set! u8vec 1 #xf6)
         (u8vector-set! u8vec 2 #x01)
         (u8vector-set! u8vec 3 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 4)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 4))]
        [else
         (u8vector-set! u8vec 0 #x00)
         (u8vector-set! u8vec 1 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 2)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 2))]))

(define (%pmd-rhythm-definition-track-u8vector-set! has-loop? lis)
  (define loop-seen? #f)
  (define (overall-offset) (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 11))
  (define repeat-stack ())
  ;; 先頭はパターンの位置の表で、L がなければ 1 つ、あれば 2 つ。リズムがなければ
  ;; パターンを作らず、位置の表も中身も書かない
  (define table-size (cond [(null? lis) 0] [has-loop? 4] [else 2]))
  (unless (null? lis)
    (receive (lb hb) (%short->le-bytes (+ (slot-ref (sxmml-pmd-binary-environment) 'offset) table-size))
      (u8vector-set! u8vec 0 lb)
      (u8vector-set! u8vec 1 hb)))
  (let loop ([rest lis]
             [track-offset table-size])
    (%size-check (+ (overall-offset) track-offset))
    (cond [(null? rest)
           ;; パターンの終わりの ff の次（パターンがなければ先頭）から、PMD4 の末尾の 12 バイトを書く
           (let ([metasize (if (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) 0 12)]
                 [end (if (null? lis)
                        0
                        (begin (u8vector-set! u8vec track-offset #xff)
                               (+ track-offset 1)))])
             (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11 (+ end metasize))
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) end metasize))
             (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
               (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 24 lb)
               (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 25 hb))
             (unless (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
               ;; 合計 tick は 2 バイトで書く。loop tick は合計 tick を超えない
               (let1 ticks (slot-ref (sxmml-pmd-binary-environment) 'max-total-ticks)
                 (when (> ticks 65535)
                   (errorf "total ticks exceed the limit: ~a (max 65535)" ticks)))
               (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'max-total-ticks))
                 (u8vector-set! u8vec end lb)
                 (u8vector-set! u8vec (+ end 1) hb))
               (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'max-loop-ticks))
                 (u8vector-set! u8vec (+ end 4) lb)
                 (u8vector-set! u8vec (+ end 5) hb))
               (u8vector-set! u8vec (- (+ end metasize) 2) #x48)
               (u8vector-set! u8vec (- (+ end metasize) 1) #xfe)))
           (sxmml-pmd-binary-environment)]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (when loop-seen?
             (error "loop command can be used only once in a track"))
           (set! loop-seen? #t)
           (u8vector-set! u8vec track-offset #xff)
           (receive (lb hb) (%short->le-bytes (+ (overall-offset) track-offset 1))
             (u8vector-set! u8vec 2 lb)
             (u8vector-set! u8vec 3 hb))
           (loop (cdr rest) (+ track-offset 1))]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) (%pmd-rest-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-emit>)
           (loop (cdr rest) (%pmd-rhythm-emit-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (push! repeat-stack (cons (+ track-offset 1) 0))
           (loop (cdr rest) (%pmd-repeat-start-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (loop (cdr rest) (%pmd-repeat-end-set! u8vec track-offset (car rest) (pop! repeat-stack) (overall-offset)))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (set! (cdar repeat-stack) (+ track-offset 1))
           (loop (cdr rest) (%pmd-repeat-break-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-volume>)
           (loop (cdr rest) (%pmd-rhythm-volume-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-volume-change>)
           (loop (cdr rest) (%pmd-rhythm-volume-change-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-pan>)
           (loop (cdr rest) (%pmd-rhythm-pan-set! u8vec track-offset (car rest)))]
          [else
           (loop (cdr rest) track-offset)])))


(define (%pmd-meta-track-u8vector-set!)
  (define voice-ht (slot-ref (sxmml-environment) 'voice))
  (define t12u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 11))
  (define t13u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 12))
  (define track-start-pos (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (let1 track-offset
      (parameterize ([%pmd-section "voice table"])
        (fold (^[value offset]
                (%size-check (+ track-start-pos offset 26))
                (u8vector-set! t13u8vec offset (%pmd-fm-voice-number value))
                (u8vector-copy! t13u8vec (+ offset 1) (list->u8vector (%opn-voice-form->register-data (slot-ref value 'value))))
                (+ offset 26))
              0
              (sort (filter (cut hash-table-exists? (slot-ref (sxmml-pmd-binary-environment) 'used-fm-voices) <>)
                            (hash-table-values voice-ht))
                    ;; 番号順に並べ、音色表の反復順に依存させない。
                    (^[x y] (< (%pmd-fm-voice-number x) (%pmd-fm-voice-number y))))))
    (if (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
      (begin (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 track-offset)
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos track-offset)))
      (begin (u8vector-set! t13u8vec track-offset #x00)
             (u8vector-set! t13u8vec (+ track-offset 1) #xff)
             (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 (+ track-offset 2))
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos track-offset 2))))
    ;; for PMD4 meta informations.
    (unless (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
      (parameterize ([%pmd-section "metadata"])
        (let loop ([rest (list 'ppzfile 'ppsfile 'pcmfile 'title 'composer 'arranger 'memo)]
                   [meta-offset (+ track-offset 2)]
                   [offset-stack ()])
          (cond [(null? rest)
                 ;; write pos info to the rhythm definition track
                 (receive (lb hb) (%short->le-bytes (+ track-start-pos meta-offset))
                   (u8vector-set! t12u8vec (- (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11) 4) lb)
                   (u8vector-set! t12u8vec (- (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11) 3) hb))
                 (let recur ([stack (reverse offset-stack)]
                             [toffset meta-offset])
                   (if (null? stack)
                     (begin (u8vector-set! t13u8vec toffset #x00)
                            (u8vector-set! t13u8vec (+ toffset 1) #x00)
                            (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 (+ toffset 2))
                            (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos toffset 2))
                            (+ toffset 2))
                     (receive (lb hb) (%short->le-bytes (+ track-start-pos (car stack)))
                       (%size-check (+ track-start-pos toffset 2))
                       (u8vector-set! t13u8vec toffset lb)
                       (u8vector-set! t13u8vec (+ toffset 1) hb)
                       (recur (cdr stack) (+ toffset 2)))))]
                [(eq? (car rest) 'memo)
                 ;; memo は文字列のリストとして受け取り、要素の数だけ書く。空なら書かない
                 (let recur ([stack (sxmml-pmd-metadata-value 'memo)]
                             [moffset meta-offset]
                             [ostack offset-stack])
                   (if (null? stack)
                     (loop (cdr rest) moffset ostack)
                     (let1 doffset (%shift-jis-u8vector-copy! t13u8vec (car stack) moffset track-start-pos)
                       (recur (cdr stack) doffset (cons moffset ostack)))))]
                [else
                 (if-let1 data (sxmml-pmd-metadata-value (car rest))
                   (let1 doffset (%shift-jis-u8vector-copy! t13u8vec data meta-offset track-start-pos)
                     (loop (cdr rest) doffset (cons meta-offset offset-stack)))
                   (begin (u8vector-set! t13u8vec meta-offset #x00)
                          (loop (cdr rest) (+ meta-offset 1) (cons meta-offset offset-stack))))]))))))


(define (%pmd-binary-output)
  (%check-obsolete-metadata)
  (let1 output-file (%output-file-get)
    (call-with-temporary-file
        (^[p n]
          (display #\null p)
          (write-uvector (slot-ref (sxmml-pmd-binary-environment) 'head) p 0
                         (slot-ref (sxmml-pmd-binary-environment) 'head-length))
          (vector-for-each (^[uvec len]
                             (write-uvector uvec p 0 len))
                           (slot-ref (sxmml-pmd-binary-environment) 'track)
                           (slot-ref (sxmml-pmd-binary-environment) 'track-length))
          (close-port p)
          (sys-rename n output-file))
      :directory (sys-dirname output-file))
    output-file))


;;;
;;; operators
;;;

;; PMD operator
(define (%pmd-rest-set! u8vec offset obj)
  (let recur ([ticks (slot-ref obj 'ticks)]
              [cur offset])
    (%size-check (+ (slot-ref (sxmml-pmd-binary-environment) 'offset) cur 2))
    (u8vector-set! u8vec cur #x0f)
    (if (> ticks 255)
      (begin (u8vector-set! u8vec (+ cur 1) #xff)
             (recur (- ticks 255) (+ cur 2)))
      (begin (u8vector-set! u8vec (+ cur 1) ticks)
             (+ cur 2)))))

(define (%pmd-note-set! u8vec offset obj)
  (let recur ([note (%note-number->pmd-note (slot-ref obj 'value))]
              [ticks (slot-ref obj 'ticks)]
              [cur offset])
    (%size-check (+ (slot-ref (sxmml-pmd-binary-environment) 'offset) cur (if (> ticks 255) 3 2)))
    (u8vector-set! u8vec cur note)
    (if (> ticks 255)
      (begin (u8vector-set! u8vec (+ cur 1) #xff)
             (u8vector-set! u8vec (+ cur 2) #xfb)
             (recur note (- ticks 255) (+ cur 3)))
      (begin (u8vector-set! u8vec (+ cur 1) ticks)
             (+ cur 2)))))

(define (%pmd-fm-voice-set! u8vec offset obj)
  (let1 inst (hash-table-get (slot-ref (sxmml-environment) 'voice) (slot-ref obj 'value) #f)
    (unless inst
      (error "no instrument define: " (slot-ref obj 'value)))
    (hash-table-put! (slot-ref (sxmml-pmd-binary-environment) 'used-fm-voices) inst #t)
    (%pmd-1byte-set! u8vec offset #xff (%pmd-fm-voice-number inst) 255 0)))

(define (%pmd-fm-voice-number voice)
  (hash-table-get (slot-ref (sxmml-pmd-binary-environment) 'voice-numbers) (slot-ref voice 'id)))

(define (%pmd-gate-time-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfe (slot-ref obj 'value) 255 0))

(define (%pmd-fm-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 127 0))

(define (%pmd-ssg-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 15 0))

(define (%pmd-pcm-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 255 0))

(define (%pmd-tempo-absolute-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfc (slot-ref obj 'value) 250 0))

(define (%pmd-tempo-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfc (sxmml-bpm->timer-b (slot-ref obj 'value)) 250 0))
  ;; (when (or (> value 255) (< value 18))
  ;;   (errorf "value exceeds the range: ~a (~a - ~a): ~a" value 18 255 obj))
  ;; (u8vector-set! u8vec offset #xfc)
  ;; (u8vector-set! u8vec (+ offset 1) #xff)
  ;; (u8vector-set! u8vec (+ offset 2) value)
  ;; (+ offset 3))

(define (%pmd-tie-set! u8vec offset obj)
  (%pmd-single-set! u8vec offset #xfb))

(define (%pmd-detune-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xfa (slot-ref obj 'value) 32767 -32768))

(define (%pmd-repeat-start-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xf9 0 0 0))

(define (%pmd-repeat-end-set! u8vec offset obj addr overall)
  (receive (lb hb) (%short->le-bytes (+ overall offset 1))
    (u8vector-set! u8vec (car addr) lb)
    (u8vector-set! u8vec (+ (car addr) 1) hb)
    (unless (zero? (cdr addr))
      (u8vector-set! u8vec (cdr addr) lb)
      (u8vector-set! u8vec (+ (cdr addr) 1) hb)))
  (let1 repcnt (slot-ref obj 'value)
    (when (> repcnt 255)
      (errorf "value exceeds the max: ~a (>255): ~a" repcnt obj))
    (u8vector-set! u8vec offset #xf8)
    (u8vector-set! u8vec (+ offset 1) repcnt)
    ;; 回数はコンパイル時に正の正確な整数を保証する。3 バイト目は脱出があれば回数 - 1。
    (u8vector-set! u8vec (+ offset 2) (if (zero? (cdr addr))
                                        repcnt
                                        (- repcnt 1)))
    (receive (lb hb) (%short->le-bytes (+ overall (car addr)))
      (u8vector-set! u8vec (+ offset 3) lb)
      (u8vector-set! u8vec (+ offset 4) hb)
      (+ offset 5))))

(define (%pmd-repeat-break-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xf7 0 0 0))

(define (%pmd-loop-set! u8vec offset obj)
  (%pmd-single-set! u8vec offset #xf6))

(define (%pmd-transpose-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xf5 (slot-ref obj 'value) 127 -128))

(define (%pmd-lfo-set! u8vec offset obj)
  (let1 lfo (hash-table-get (slot-ref (sxmml-environment) 'lfo) (slot-ref obj 'value) #f)
    (unless lfo
      (error "no lfo define: " (slot-ref obj 'value)))
    (let ([channel (slot-ref obj 'channel)])
      (when (or (not (memq channel '(#f A B)))
                (and (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) (eq? channel 'B)))
        (errorf "invalid lfo channel: ~a" channel))
      (let1 value (sxmml-pmd-lfo-values lfo)
        (u8vector-set! u8vec offset (if (or (eq? channel #f) (eq? channel 'A)) #xf2 #xbf))
        (u8vector-set! u8vec (+ offset 1) (car value))
        (u8vector-set! u8vec (+ offset 2) (cadr value))
        (u8vector-set! u8vec (+ offset 3) (logand (caddr value) #xff))
        (u8vector-set! u8vec (+ offset 4) (cadddr value))
        (+ offset 5)))))

(define (%pmd-lfo-switch-set! u8vec offset obj)
  (let ([channel (slot-ref obj 'channel)]
        [switch (slot-ref obj 'value)])
    (when (or (and (not (eq? channel 'A)) (not (eq? channel 'B)) (not (eq? channel #f)))
              (and (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) (eq? channel 'B)))
        (errorf "invalid lfo channel: ~a" channel))
    (u8vector-set! u8vec offset (if (or (eq? channel #f) (eq? channel 'A)) #xf1 #xbe))
    (u8vector-set! u8vec (+ offset 1) (if switch 1 0))
    (+ offset 2)))

(define (%pmd-envelope-set! u8vec offset obj)
  (let1 envelope (hash-table-get (slot-ref (sxmml-environment) 'envelope) (slot-ref obj 'value) #f)
    (unless envelope
      (error "no envelope define: " (slot-ref obj 'value)))
    (let1 value (sxmml-pmd-envelope-values envelope)
      ;; PMD3 は 4 値の書式だけを書ける
      (when (and (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) (> (length value) 4))
        (errorf "unsupported envelope values in PMD3: ~a has ~a values (expected 4)"
                (slot-ref envelope 'id) (length value)))
      (if (= (length value) 4)
        (begin (u8vector-set! u8vec offset #xf0)
               (u8vector-set! u8vec (+ offset 1) (car value))
               (u8vector-set! u8vec (+ offset 2) (logand (cadr value) #xff))
               (u8vector-set! u8vec (+ offset 3) (caddr value))
               (u8vector-set! u8vec (+ offset 4) (cadddr value))
               (+ offset 5))
        (begin (u8vector-set! u8vec offset #xcd)
               (u8vector-set! u8vec (+ offset 1) (car value))
               (u8vector-set! u8vec (+ offset 2) (cadr value))
               (u8vector-set! u8vec (+ offset 3) (caddr value))
               (u8vector-set! u8vec (+ offset 4) (logior (cadddr value) (ash (cadddr (cdr value)) 4)))
               (u8vector-set! u8vec (+ offset 5) (if (= (length value) 6) (cadddr (cddr value)) 0))
               (+ offset 6))))))

(define (%pmd-pan-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xec (slot-ref obj 'value) 3 0))

(define (%pmd-transpose-change-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xe7 (slot-ref obj 'value) 127 -128))

(define (%pmd-fm-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 4)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -4)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 127 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 127 0)])))

(define (%pmd-ssg-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 1)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -1)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 15 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 15 0)])))

(define (%pmd-pcm-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 16)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -16)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 255 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 255 0)])))

;; ポルタメントの音長は 1 バイトなので、音符のように分割できない。255 tick までとする
(define (%pmd-portamento-set! u8vec offset obj)
  (let ([start (%note-number->pmd-note (car (slot-ref obj 'value)))]
        [end (%note-number->pmd-note (cdr (slot-ref obj 'value)))]
        [ticks (slot-ref obj 'ticks)])
    (unless (<= 1 ticks 255)
      (errorf "portamento ticks out of range: ~a (1 - 255)" ticks))
    (u8vector-set! u8vec offset #xda)
    (u8vector-set! u8vec (+ offset 1) start)
    (u8vector-set! u8vec (+ offset 2) end)
    (u8vector-set! u8vec (+ offset 3) ticks)
    (+ offset 4)))

(define (%pmd-ssg-voice-set! u8vec offset obj)
  (define (data-set! p1 p2 p3 p4)
    (u8vector-set! u8vec offset #xf0)
    (u8vector-set! u8vec (+ offset 1) p1)
    (u8vector-set! u8vec (+ offset 2) (logand p2 #xff))
    (u8vector-set! u8vec (+ offset 3) p3)
    (u8vector-set! u8vec (+ offset 4) p4)
    (+ offset 5))
  (let1 value (slot-ref obj 'value)
    (case value
      [(|0|) (data-set! 0 0 0 0)]
      [(|1|) (data-set! 2 -1 0 1)]
      [(|2|) (data-set! 2 -2 0 1)]
      [(|3|) (data-set! 2 -2 0 8)]
      [(|4|) (data-set! 2 -1 24 1)]
      [(|5|) (data-set! 2 -2 24 1)]
      [(|6|) (data-set! 2 -2 4 1)]
      [(|7|) (data-set! 2 1 0 1)]
      [(|8|) (data-set! 1 2 0 1)]
      [(|9|) (data-set! 1 2 24 1)]
      [else
       (error "invalid voice parameter: " value)])))

(define (%pmd-pcm-voice-set! u8vec offset obj)
  ;; sxmml.backend.pmd と同じく 0 から 255 までの整数に限り、数でない値は書かれた値を示す
  (let* ([name (slot-ref obj 'value)]
         [value (or (string->number (symbol->string name)) name)])
    (unless (and (exact-integer? value) (<= 0 value 255))
      (error "invalid voice parameter: " value))
    (%pmd-1byte-set! u8vec offset #xff value 255 0)))

(define (%pmd-noise-freq-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xee (slot-ref obj 'value) 31 0))

(define (%pmd-output-switch-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (when (or (> value 3) (< value 1))
      (errorf "value exceeds the range: ~a (~a - ~a): ~a" value 1 3 #xed))
    (u8vector-set! u8vec offset #xed)
    (u8vector-set! u8vec (+ offset 1) (case value ([1] #x07) ([2] #x38) ([3] #x3f)))
    (+ offset 2)))

;; Rhythm operator
(define (%pmd-rhythm-emit-set! u8vec offset obj)
  (let* ([sw (if (slot-ref obj 'value) 0 128)]
         [b (if (slot-ref obj 'bass) 1 0)]
         [s (if (slot-ref obj 'snare) 2 0)]
         [t (if (slot-ref obj 'tom) 16 0)]
         [c (if (slot-ref obj 'cymbal) 4 0)]
         [h (if (slot-ref obj 'hihat) 8 0)]
         [i (if (slot-ref obj 'rimshot) 32 0)]
         [cur (%pmd-1byte-set! u8vec offset #xeb (+ sw b s t c h i) 255 1)])
    (if (slot-ref obj 'value)
      (%pmd-rest-set! u8vec cur (make <sxmml-mml-rest> :value #f :ticks (slot-ref obj 'ticks)))
      cur)))

(define (%pmd-rhythm-volume-set! u8vec offset obj)
  (define (data-set! offset cmd value)
    (u8vector-set! u8vec offset cmd)
    (u8vector-set! u8vec (+ offset 1) value)
    (+ offset 2))
  ;; 全体の音量は 0～63
  (define (whole-value value)
    (when (or (> value 63) (< value 0))
      (error "invalid rhythm volume parameter: " value))
    value)
  ;; 楽器別の音量は楽器 * 32 + 音量の 1 バイトなので、符号化の前に 0～31 を検査する
  (define (instrument-value inst value)
    (unless (<= 0 value 31)
      (errorf "rhythm volume out of range: ~a (0 - 31)" value))
    (logior (ash inst 5) value))
  (let ([v (cons 0 (slot-ref obj 'value))]
        [b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list v b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(and (= (caar rest) 0) (cdar rest))
             (recur (cdr rest) (data-set! cur #xe8 (whole-value (cdar rest))))]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur #xea (instrument-value (caar rest) (cdar rest))))]
            [else
             (recur (cdr rest) cur)]))))

(define (%pmd-rhythm-volume-change-set! u8vec offset obj)
  (define (data-set! offset inst value)
    (when (or (> value 31) (< value -31))
      (error "invalid rhythm volume parameter: " value))
    (u8vector-set! u8vec offset #xe5)
    (u8vector-set! u8vec (+ offset 1) inst)
    (u8vector-set! u8vec (+ offset 2) (logand value #xff))
    (+ offset 3))
  (let ([b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur (caar rest) (cdar rest)))]
            [else
             (recur (cdr rest) cur)]))))

(define (%pmd-rhythm-pan-set! u8vec offset obj)
  ;; パンは楽器 * 32 + 値の 1 バイト。値は 1（右）・2（左）・3（中央）で、符号化の前に検査する
  (define (data-set! offset inst value)
    (unless (<= 1 value 3)
      (errorf "rhythm pan out of range: ~a (1 - 3)" value))
    (u8vector-set! u8vec offset #xe9)
    (u8vector-set! u8vec (+ offset 1) (logior (ash inst 5) value))
    (+ offset 2))
  (let ([b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur (caar rest) (cdar rest)))]
            [else
             (recur (cdr rest) cur)]))))


;; Misc
;; 生成物は 65535 バイトまでとする。先頭の 0x00 を除いたデータの長さ length は 65534 までとなる
(define (%size-check length)
  (when (> length 65534)
    (errorf "PMD data exceeds the size limit (65535 bytes)~a"
            (if-let1 section (%pmd-section) #" in ~section" ""))))

(define-inline (%pmd-single-set! u8vec offset cmd)
  (u8vector-set! u8vec offset cmd)
  (+ offset 1))

(define-inline (%pmd-1byte-set! u8vec offset cmd value max min)
  (when (or (> value max) (< value min))
    (errorf "value exceeds the range: ~a (~a - ~a): ~a" value min max cmd))
  (u8vector-set! u8vec offset cmd)
  (u8vector-set! u8vec (+ offset 1) (logand value #xff))
  (+ offset 2))

(define-inline (%pmd-2bytes-set! u8vec offset cmd value max min)
  (when (or (> value max) (< value min))
    (errorf "value exceeds the range: ~a (~a - ~a): ~a" value min max cmd))
  (receive (lb hb) (%short->le-bytes value)
    (u8vector-set! u8vec offset cmd)
    (u8vector-set! u8vec (+ offset 1) lb)
    (u8vector-set! u8vec (+ offset 2) hb)
    (+ offset 3)))

(define-inline (%short->le-bytes shortint)
  (values (logand #b11111111 shortint) (ash (logand #b1111111100000000 shortint) -8)))

;; 音高は 12 * オクターブ + 音名 (0～11) なので、負の音高も floor/ でオクターブと音名に分ける。
;; PMD の音高の 1 バイトは、上位 4 ビットがオクターブ - 1、下位 4 ビットが音名で、
;; o1c から o8b までを表す
(define-inline (%note-number->pmd-note note)
  (unless (<= 12 note 107)
    (errorf "note out of range: ~a (o1c - o8b)" (%note-number->name note)))
  (receive (octave notenum) (floor/ note 12)
    (+ (ash (- octave 1) 4) notenum)))

;; エラーの表示用に、音高を sxmml.backend.pmd と同じ MML の表記にする
(define (%note-number->name note)
  (receive (octave notenum) (floor/ note 12)
    (format "o~a~a" octave (case notenum
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
                             [(11) "b"]))))

(define (%opn-voice-form->register-data form)
  (define (operator->register opform)
    (match opform
      [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
       (list (logior (ash dt 4) ml)
             tl
             (logior (ash ks 6) ar)
             (logior (ash ams 7) dr)
             sr
             (logior (ash sl 4) rr))]))
  (match form
    [((al fb) op1 op2 op3 op4)
     (sxmml-opmn-operator-zip (operator->register op1)
                              (operator->register op2)
                              (operator->register op3)
                              (operator->register op4)
                              (logior (ash fb 3) al))]))

;; CLI が設定した出力先を使い、ライブラリからの利用時だけ既定値を補う。
(define (%output-file-get)
  (or (slot-ref (sxmml-environment) 'output)
      (let1 output (or (and-let* ([current-file (sxmml-source-file)])
                         (path-swap-extension current-file "m"))
                       "./a.m")
        (slot-set! (sxmml-environment) 'output output)
        output)))

;; 出力先の指定に使われていた filename メタデータは無視し、警告を表示する
(define (%check-obsolete-metadata)
  (when (sxmml-metadata-get 'filename)
    (format (current-error-port)
            "warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file\n")))

(define (%shift-jis-u8vector-copy! u8vector str start base)
  (let* ([u8str (ces-convert-to <u8vector> str "*jp" "cp932")]
         [u8len (u8vector-length u8str)])
    (%size-check (+ base start u8len 1))
    (u8vector-copy! u8vector start u8str)
    (+ start u8len 1)))

(define (%ticks-update! track)
  (when (> (slot-ref (sxmml-track-get track) 'total-ticks)
           (slot-ref (sxmml-pmd-binary-environment) 'max-total-ticks))
    (slot-set! (sxmml-pmd-binary-environment) 'max-total-ticks
               (slot-ref (sxmml-track-get track) 'total-ticks)))
  ;; L のないトラックの loop-ticks は合計 tick と同じ値なので、loop tick の最大には数えない
  (when (and (slot-ref (sxmml-track-get track) 'loop-exists)
             (> (slot-ref (sxmml-track-get track) 'loop-ticks)
                (slot-ref (sxmml-pmd-binary-environment) 'max-loop-ticks)))
    (slot-set! (sxmml-pmd-binary-environment) 'max-loop-ticks
               (slot-ref (sxmml-track-get track) 'loop-ticks))))
