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
          sxmml-mode-register! sxmml-mode-find sxmml-mode-list
          sxmml-mode-select! sxmml-mode-get sxmml-mode-selected?
          define-metadata sxmml-metadata-get sxmml-metadata-set!
          sxmml-resolution sxmml-resolution-get sxmml-resolution-set!
          define-lfo sxmml-lfo-get sxmml-lfo-set!
          define-envelope sxmml-envelope-get sxmml-envelope-set!
          define-track sxmml-track-get sxmml-track-set! sxmml-track-delete! sxmml-track-clear!
          rename-track sxmml-track-rename! sxmml-track-list sxmml-track-find sxmml-ir-register!
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

;; モードの登録情報はGaucheの実行環境で共有する。
;; 登録情報の変更は、それを参照する楽曲環境にも及ぶ。
;; 楽曲環境は選択結果だけを保持し、楽曲ごとの定義群の切替えは現在提供しない。
(define %sxmml-modes (make-hash-table eq-comparator))

(define (%mode-name-check name)
  (unless (symbol? name)
    (errorf <sxmml-value-error> "invalid mode name: ~s (expected a symbol)" name)))

(define (sxmml-mode-register! mode)
  (unless (is-a? mode <sxmml-mode>)
    (errorf <sxmml-value-error> "invalid mode: ~s (expected a <sxmml-mode> instance)" mode))
  (%mode-name-check (slot-ref mode 'name))
  (let ([aliases (slot-ref mode 'aliases)]
        [setup (slot-ref mode 'setup)]
        [backend (slot-ref mode 'backend)]
        [extension (slot-ref mode 'output-extension)])
    (unless (and (list? aliases) (every symbol? aliases))
      (errorf <sxmml-value-error> "invalid mode aliases: ~s (expected a list of symbols)" aliases))
    (unless (or (not setup) (procedure? setup))
      (errorf <sxmml-value-error> "invalid mode setup: ~s (expected a procedure or #f)" setup))
    (unless (procedure? backend)
      (errorf <sxmml-value-error> "invalid mode backend: ~s (expected a procedure)" backend))
    (unless (or (not extension) (string? extension))
      (errorf <sxmml-value-error> "invalid mode output extension: ~s (expected a string or #f)" extension))
    (let1 names (cons (slot-ref mode 'name) aliases)
      (for-each (^[name]
                  (and-let1 owner (hash-table-get %sxmml-modes name #f)
                    (errorf <sxmml-value-error> "invalid mode name: ~a (already registered for ~a)"
                            name (slot-ref owner 'name))))
                names)
      (for-each (cut hash-table-set! %sxmml-modes <> mode) names))))

(define (sxmml-mode-find name)
  (%mode-name-check name)
  (hash-table-get %sxmml-modes name #f))

(define (sxmml-mode-list)
  (delete-duplicates (hash-table-values %sxmml-modes) eq?))

(define (sxmml-mode-get)
  (slot-ref (sxmml-environment) 'mode))

(define (sxmml-mode-selected? name)
  (and-let1 mode (sxmml-mode-find name)
    (eq? mode (sxmml-mode-get))))

(define (sxmml-mode-select! name)
  (let1 mode (or (sxmml-mode-find name)
                (errorf <sxmml-reference-error> "undefined mode: ~a" name))
    (slot-set! (sxmml-environment) 'mode mode)
    (and-let1 setup (slot-ref mode 'setup) (setup))))

(define-syntax sxmml-mode
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ name)
         (unless (identifier? name)
           (sxmml-form-error form "invalid sxmml-mode name: ~s (expected a symbol)" (unwrap-syntax name)))
         (sxmml-form-wrap form rename
           (quasirename rename
             `(%sxmml-mode-select ,(sxmml-form-location form) ',name)))]
        [_ (sxmml-form-error form "invalid sxmml-mode form: expected (sxmml-mode mode)")]))))

(define (%sxmml-mode-select location name)
  (if (sxmml-mode-fixed)
    (sxmml-form-warn-at location "sxmml-mode in source is ignored; mode is already set to ~a"
                        (slot-ref (sxmml-mode-get) 'name))
    (sxmml-mode-select! name)))

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
      (define (keysym key)
        (unless (identifier? key)
          (sxmml-form-error form "invalid define-metadata key: ~s (expected a symbol)" (unwrap-syntax key)))
        (string->symbol (string-downcase (symbol->string (identifier->symbol key)))))
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
    (let1 idsym (sxmml-form-name->symbol form name id)
      (unless (identifier? type)
        (sxmml-form-error form "invalid ~a type: ~s (expected a symbol)" name (unwrap-syntax type)))
      (sxmml-form-wrap form rename
        (quasirename rename
          `(,(rename set-proc) ',idsym ',type (list ,@exprs))))))
  (match form
    [(_ id (? list? exprs)) (expand id 'default exprs)]
    [(_ id (? (^[x] (not (list? x))) type) (? list? exprs)) (expand id type exprs)]
    [_ (sxmml-form-error form "invalid ~a form: expected (~a id [type] (value ...))" name name)]))

;;
;;  track
;;

(define (sxmml-track-get track)
  (hash-table-get (slot-ref (sxmml-environment) 'track) track #f))
(define (sxmml-track-set! track . params)
  (let* ([tracks (slot-ref (sxmml-environment) 'track)]
         [ctx (apply make <sxmml-mml-context> :track track
                     :part (get-keyword :part params track) params)]
         [part (slot-ref ctx 'part)]
         [owner (hash-table-find tracks
                  (^[name other]
                    (and (not (eq? name track)) (eq? (slot-ref other 'part) part)
                         (cons name other))))])
    (when owner
      (errorf <sxmml-value-error> "invalid track ~s part: ~s (already assigned to track ~s)"
              track part (car owner)))
    (hash-table-set! tracks track ctx)))
(define (sxmml-track-rename! old-name new-name)
  (unless (symbol? old-name)
    (errorf <sxmml-value-error> "invalid rename-track source: ~s (expected a symbol)" old-name))
  (unless (symbol? new-name)
    (errorf <sxmml-value-error> "invalid rename-track destination: ~s (expected a symbol)" new-name))
  (let* ([tracks (slot-ref (sxmml-environment) 'track)]
         [ctx (sxmml-track-get old-name)])
    (unless ctx
      (errorf <sxmml-reference-error> "undefined track: ~a" old-name))
    (when (hash-table-exists? tracks new-name)
      (errorf <sxmml-value-error> "invalid rename-track destination: ~s (already registered)" new-name))
    (slot-set! ctx 'track new-name)
    (hash-table-set! tracks new-name ctx)
    (hash-table-delete! tracks old-name)))
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
        (unless (identifier? track)
          (sxmml-form-error form "invalid define-track name: ~s (expected a symbol)" (unwrap-syntax track)))
        (sxmml-form-wrap form rename
          (quasirename rename
            `(sxmml-track-set! ',track ,@param))))
      (match form
        [(_ () param ...)
         (sxmml-form-error form "define-track requires at least one track name")]
        [(_ (track ..1) param ...)
         (when (any (^[kv] (eq? (car kv) :part))
                    (slices param 2))
           (sxmml-form-error
            form
            "invalid define-track form: :part requires a single track"))
         (quasirename rename
           `(begin
              ,@(map (cut build-track-registration <> param) track)))]
        [(_ track param ...)
         (build-track-registration track param)]
        ))))

(define-syntax rename-track
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ old-name new-name)
         (unless (identifier? old-name)
           (sxmml-form-error form "invalid rename-track source: ~s (expected a symbol)" (unwrap-syntax old-name)))
         (unless (identifier? new-name)
           (sxmml-form-error form "invalid rename-track destination: ~s (expected a symbol)" (unwrap-syntax new-name)))
         (sxmml-form-wrap form rename
           (quasirename rename
             `(sxmml-track-rename! ',old-name ',new-name)))]
        [_ (sxmml-form-error form "invalid rename-track form: expected (rename-track old-name new-name)")]))))
