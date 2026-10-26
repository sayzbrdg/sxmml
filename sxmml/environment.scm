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

(define-module sxmml.environment
  (use util.match)
  (use gauche.unicode)
  (use sxmml.classes)
  (use sxmml.form)
  (export sxmml-environment sxmml-source-file sxmml-mode sxmml-mode-fixed
          define-metadata sxmml-metadata-get sxmml-metadata-set!
          sxmml-resolution sxmml-resolution-get sxmml-resolution-set!
          define-lfo sxmml-lfo-get sxmml-lfo-set!
          define-envelope sxmml-envelope-get sxmml-envelope-set!
          define-track sxmml-track-get sxmml-track-set! sxmml-track-delete! sxmml-track-clear!
          sxmml-track-list sxmml-track-find sxmml-ir-register!
          ))
(select-module sxmml.environment)

;;
;;  environment information
;;
(define sxmml-environment (make-parameter (make <sxmml-environment>)))
(define sxmml-source-file (make-parameter #f))
;; 真の場合、ソース中の sxmml-mode を無視する (sxlc の -m 指定を優先するため)
(define sxmml-mode-fixed (make-parameter #f))

(define (sxmml-resolution-get)
  (slot-ref (sxmml-environment) 'resolution))
(define (sxmml-resolution-set! resolution)
  (unless (and (integer? resolution) (positive? resolution))
    (errorf <sxmml-value-error> "invalid resolution: ~s (expected a positive integer)" resolution))
  (slot-set! (sxmml-environment) 'resolution resolution))

(define sxmml-resolution sxmml-resolution-set!)

;; モードの呼び出し (proc args ...) を thunk にして %sxmml-mode-select に渡す。引数は
;; 式として評価する。モードが決まっているときは、モードも引数も評価しない。形は展開時に
;; 検査する。形の誤り、無視するときの警告、モードの呼び出しの誤りには、このフォームの
;; 位置を付ける
(define-syntax sxmml-mode
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ proc args ...)
         (quasirename rename
           `(%sxmml-mode-select ,(sxmml-form-location form)
                                (lambda () ,(sxmml-form-wrap form rename `(,proc ,@args)))))]
        [_ (sxmml-form-error form "invalid sxmml-mode form: expected (sxmml-mode mode arg ...)")]))))

(define (%sxmml-mode-select location thunk)
  (if (sxmml-mode-fixed)
    (sxmml-form-warn-at location "sxmml-mode in source is ignored; mode is already set to ~a"
                        (slot-ref (sxmml-environment) 'mode))
    (thunk)))

;;
;;  metadata information
;;
(define (sxmml-metadata-get key)
  (hash-table-get (slot-ref (sxmml-environment) 'metadata) key #f))
(define (sxmml-metadata-set! key value)
  (hash-table-set! (slot-ref (sxmml-environment) 'metadata) key value))

;; 値は式として評価する。括弧の中が全て文字列の旧書式 (key ("a" "b")) は、展開時に
;; 警告を出して文字列のリストとして受け付ける。値の型はバックエンドが検査する
(define-syntax define-metadata
  (er-macro-transformer
    (^[form rename id=?]
      (define (keysym key) (string->symbol (string-downcase (x->string key))))
      (define (old-form? x) (and (pair? x) (list? x) (every string? x)))
      (match form
        [(_ key (? old-form? strings))
         (sxmml-form-warn form "(define-metadata ~a (\"...\" ...)) is deprecated; use (list \"...\" ...) or '(\"...\" ...)"
                          (keysym key))
         (sxmml-form-wrap form rename
           (quasirename rename `(sxmml-metadata-set! ',(keysym key) (list ,@strings))))]
        [(_ key value)
         (sxmml-form-wrap form rename
           (quasirename rename `(sxmml-metadata-set! ',(keysym key) ,value)))]
        [_ (sxmml-form-error form "invalid define-metadata form: expected (define-metadata key value)")]))))

;;
;;  LFO
;;

(define (sxmml-lfo-get id)
  (hash-table-get (slot-ref (sxmml-environment) 'lfo) id))
(define (sxmml-lfo-set! id type data)
  (hash-table-set! (slot-ref (sxmml-environment) 'lfo) id
                   (make <sxmml-lfo> :id id :type type :value data)))

(define-syntax define-lfo
  (er-macro-transformer
    (^[form rename id=?]
      (%define-lfo/envelope-expand form rename "define-lfo" 'sxmml-lfo-set!))))


;;
;;  envelope
;;

(define (sxmml-envelope-get id)
  (hash-table-get (slot-ref (sxmml-environment) 'envelope) id))
(define (sxmml-envelope-set! id type data)
  (hash-table-set! (slot-ref (sxmml-environment) 'envelope) id
                   (make <sxmml-envelope> :id id :type type :value data)))

(define-syntax define-envelope
  (er-macro-transformer
    (^[form rename id=?]
      (%define-lfo/envelope-expand form rename "define-envelope" 'sxmml-envelope-set!))))

;; define-lfo・define-envelope の展開。枠の中の値は式として評価し、種類 (type) は
;; リテラルのシンボルとする。形は展開時に検査し、誤りと既知の値の誤りには定義の位置を付ける
(define (%define-lfo/envelope-expand form rename name set-proc)
  (define (expand id type exprs)
    (let1 idsym (string->symbol (x->string id))
      (sxmml-form-wrap form rename
        (quasirename rename
          `(,(rename set-proc) ',idsym ',type (list ,@exprs))))))
  (match form
    [(_ id (? list? exprs)) (expand id 'default exprs)]
    [(_ id (? symbol? type) (? list? exprs)) (expand id type exprs)]
    [_ (sxmml-form-error form "invalid ~a form: expected (~a id [type] (value ...))" name name)]))

;;
;;  track
;;

(define (sxmml-track-get track)
  (hash-table-get (slot-ref (sxmml-environment) 'track) track #f))
(define (sxmml-track-set! track . params)
  (hash-table-set! (slot-ref (sxmml-environment) 'track) track
                   (apply make <sxmml-mml-context> :track track params)))
(define (sxmml-track-delete! track)
  (hash-table-delete! (slot-ref (sxmml-environment) 'track) track))
(define (sxmml-track-clear!)
  (hash-table-clear! (slot-ref (sxmml-environment) 'track)))
(define (sxmml-track-list)
  (hash-table-keys (slot-ref (sxmml-environment) 'track)))
(define (sxmml-track-find part)
  (hash-table-find (slot-ref (sxmml-environment) 'track)
                   (^[k v] (and (eq? (slot-ref v 'part) part) k))))

(define (sxmml-ir-register! ir track)
  (if-let1 ctx (sxmml-track-get track)
    (slot-set! ctx 'compiled-data (if (null? (slot-ref ctx 'compiled-data))
                                    ir
                                    (cons (slot-ref ctx 'compiled-data) ir)))))

(define-syntax define-track
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-track-registration track param)
        (quasirename rename
          `(sxmml-track-set! ',track ,@param)))
      (define (build-part track params)
        (if (find (^[p] (eq? p 'part)) params)
          params
          (cons* :part `',track params)))
      (match form
        [(_ () param ...)
         (sxmml-form-error form "define-track requires at least one track name")]
        [(_ (track ..1) param ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-track-registration t (build-part t param)) r))
                      ()
                      track)))]
        [(_ track param ...)
         (build-track-registration track (build-part track param))]
        ))))
