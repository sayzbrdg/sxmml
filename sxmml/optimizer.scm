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

(define-module sxmml.optimizer
  (use gauche.record)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.mml)
  (use sxmml.repeat-tree)
  (export sxmml-optimize-tree
          ))
(select-module sxmml.optimizer)

;;;
;;; optimizer
;;;

(define (sxmml-optimize-tree track :key (optimize #t))
  (let* ([env (hash-table-get (slot-ref (sxmml-environment) 'track) track)]
         [ct (car (slot-ref env 'current-ticks))])
    (slot-set! env 'total-ticks (car ct))
    (slot-set! env 'loop-ticks (- (car ct) (cdr ct)))
    (cond [(memq optimize '(:repeat :repeat-expand))
           (%sxmml-optimize-repeat (%sxmml-optimize-2nd-pass! (%sxmml-optimize-tree! env))
                                   (eq? optimize :repeat-expand))]
          [(eq? optimize :full)
           (%sxmml-optimize-2nd-pass! (%sxmml-optimize-tree! env))]
          [optimize
           (%sxmml-optimize-tree! env)]
          [else
           (reverse!
            (let recur ([rest (slot-ref env 'compiled-data)]
                        [objs ()])
              (cond [(null? rest)
                     objs]
                    [(pair? (car rest))
                     (recur (cdr rest) (recur (car rest) objs))]
                    [(is-a? (car rest) <sxmml-mml-nop>)
                     (recur (cdr rest) objs)]
                    [else
                     (recur (cdr rest) (cons (car rest) objs))])))]
          )))

(define (%sxmml-optimize-tree! mmlenv)
  (reverse!
   (let* ([env (make <sxmml-optimize-environment>)]
          [optobjs (let recur ([rest (slot-ref mmlenv 'compiled-data)]
                               [objs ()])
                     (cond [(null? rest)
                            objs]
                           [(pair? (car rest))
                            (recur (cdr rest) (recur (car rest) objs))]
                           [(any ($ is-a? (car rest) $) (list <sxmml-mml-volume-change>)) ; sum values
                            (object-sum-update! env (car rest))
                            (recur (cdr rest) objs)]
                           [(any ($ is-a? (car rest) $) (list <sxmml-mml-note> <sxmml-mml-loop> <sxmml-mml-repeat-start>
                                                              <sxmml-mml-repeat-end> <sxmml-mml-repeat-break>))
                            (recur (cdr rest) (append (object-flush! env (car rest)) objs))]
                           [(is-a? (car rest) <sxmml-mml-nop>)
                            (recur (cdr rest) objs)]
                           [(any ($ is-a? (car rest) $) (list <sxmml-rhythm-volume> <sxmml-rhythm-pan>)) ; rhythm update
                            (object-rhythm-update! env (car rest))
                            (recur (cdr rest) objs)]
                           [(any ($ is-a? (car rest) $) (list <sxmml-rhythm-volume-change>)) ; rhythm sum values
                            (object-rhythm-sum-update! env (car rest))
                            (recur (cdr rest) objs)]
                           [else                     ; replace data
                            (object-set! env (car rest))
                            (recur (cdr rest) objs)]
                           ))])
     (append (object-flush! env (eof-object)) optobjs))))

(define (object-sum-update! env obj)
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (if (hash-table-contains? ht class)
      (hash-table-update! ht class (^o (slot-set! o 'value (+ (slot-ref o 'value) (slot-ref obj 'value))) o))
      (begin
        (hash-table-set! ht class obj)
        (tree-map-put! tm (tree-map-num-entries tm) class)))))

(define (object-flush! env obj)
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (when (and (hash-table-contains? ht <sxmml-mml-volume-change>)
               (hash-table-contains? ht <sxmml-mml-volume>))
      (let ([vol (hash-table-get ht <sxmml-mml-volume>)]
            [volchg (hash-table-get ht <sxmml-mml-volume-change>)])
        (slot-set! vol 'value (+ (slot-ref vol 'value) (slot-ref volchg 'value)))
        (hash-table-delete! ht <sxmml-mml-volume-change>)))
    (let1 objs (tree-map-fold tm
                              (^[_ v r] (if-let1 obj (hash-table-get ht v #f)
                                          (cons obj r)
                                          r))
                              ())
      (hash-table-clear! ht)
      (tree-map-clear! tm)
      (if (eof-object? obj)
        objs
        (cons obj objs)))))

(define (object-set! env obj)
  (let ([key (%object-key obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (unless (hash-table-contains? ht key)
      (tree-map-put! tm (tree-map-num-entries tm) key))
    (hash-table-set! ht key obj)))

;; 後の命令が置き換える命令の鍵。LFO の状態はチャンネルごとにあるので、
;; LFO の命令はクラスとチャンネルの組を鍵とする。チャンネルの値の意味は
;; バックエンドが決めるので、省略 (#f) と A も別の鍵とする
(define (%object-key obj)
  (if (is-a? obj <sxmml-mml-lfo>)
    (cons (class-of obj) (slot-ref obj 'channel))
    (class-of obj)))

(define (object-rhythm-update! env obj)
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (if (hash-table-contains? ht class)
      (hash-table-update! ht class (^o (for-each (^s (when (slot-ref obj s)
                                                       (slot-set! o s (slot-ref obj s))))
                                                 (map slot-definition-name (class-slots class)))
                                       o))
      (begin
        (hash-table-set! ht class obj)
        (tree-map-put! tm (tree-map-num-entries tm) class)))))

(define (object-rhythm-sum-update! env obj)
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (if (hash-table-contains? ht class)
      (hash-table-update! ht class (^o (for-each (^s (cond [(and (slot-ref o s) (slot-ref obj s))
                                                            (slot-set! o s (+ (slot-ref o s) (slot-ref obj s)))]
                                                           [(slot-ref obj s)
                                                            (slot-set! o s (slot-ref obj s))]))
                                                 (map slot-definition-name (class-slots class)))
                                       o))
      (begin
        (hash-table-set! ht class obj)
        (tree-map-put! tm (tree-map-num-entries tm) class)))))

;;
;; 第2パス
(define (%sxmml-optimize-2nd-pass! lis)
  (define (skip?! ctx rctx obj slot pslot)
    (cond [(and (null? rctx)  (equal? (slot-ref obj 'value) (slot-ref ctx slot)))
           #t]                          ; outer repeat
          [(and (slot-ref ctx pslot) (equal? (slot-ref obj 'value) (slot-ref ctx slot)))
           #t]
          [(and (not (null? rctx))
                (equal? (slot-ref obj 'value) (slot-ref ctx slot))
                (equal? (slot-ref obj 'value) (slot-ref (car rctx) slot)))
           #t]
          [else
           (slot-set! ctx slot (slot-ref obj 'value))
           (slot-set! ctx pslot #t)
           #f]))
  ;; skip?! の LFO 版。LFO の状態はチャンネルごとの連想リストで持ち、
  ;; 置いたチャンネルをリストで持つ。文脈は複製されるので、リストは作り直す
  (define (lfo-skip?! ctx rctx obj slot pslot default)
    (let* ([channel (slot-ref obj 'channel)]
           [value (slot-ref obj 'value)]
           [same? (equal? value (%channel-ref (slot-ref ctx slot) channel default))])
      (cond [(and (null? rctx) same?)
             #t]                        ; outer repeat
            [(and (memq channel (slot-ref ctx pslot)) same?)
             #t]
            [(and (not (null? rctx))
                  same?
                  (equal? value (%channel-ref (slot-ref (car rctx) slot) channel default)))
             #t]
            [else
             (slot-set! ctx slot (acons channel value (slot-ref ctx slot)))
             (slot-set! ctx pslot (cons channel (slot-ref ctx pslot)))
             #f])))
  (define (placed-init! ctx)
    (for-each (^s (slot-set! ctx s #f))
              '(voice-placed volume-placed gate-time-placed pan-placed detune-placed
                              envelope-placed))
    (for-each (^s (slot-set! ctx s ()))
              '(lfo-placed-channels lfo-switch-placed-channels))
    ctx)
  (define (ctx-copy ctx)
    (let1 new (make <sxmml-optimize-2nd-pass-context>)
      (for-each (^s (slot-set! new s (slot-ref ctx s)))
                (map slot-definition-name (class-slots <sxmml-optimize-2nd-pass-context>)))
      new))
  (let loop ([rest lis]
             [ctx (make <sxmml-optimize-2nd-pass-context>)]
             [rctx ()]
             [sctx ()]
             [result ()])
    (cond [(null? rest)
           (reverse result)]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (loop (cdr rest) ctx rctx (cons (ctx-copy ctx) sctx) (cons (car rest) result))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (if (car sctx)
             (loop (cdr rest) (car sctx) (cdr rctx) (cdr sctx) (cons (car rest) result))
             (loop (cdr rest) ctx (cdr rctx) (cdr sctx) (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (loop (cdr rest) (placed-init! ctx) (cons (%make-repeat-end-context (cdr rest)) rctx) (cons #f sctx) (cons (car rest) result))]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (if (skip?! ctx rctx (car rest) 'voice 'voice-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (if (skip?! ctx rctx (car rest) 'volume 'volume-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (slot-set! ctx 'volume (+ (slot-ref ctx 'volume) (slot-ref (car rest) 'value)))
           (loop (cdr rest) ctx rctx sctx (cons (car rest) result))]
          [(is-a? (car rest) <sxmml-mml-gate-time>)
           (if (skip?! ctx rctx (car rest) 'gate-time 'gate-time-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (if (skip?! ctx rctx (car rest) 'pan 'pan-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (if (skip?! ctx rctx (car rest) 'detune 'detune-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (if (skip?! ctx rctx (car rest) 'envelope 'envelope-placed)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (if (lfo-skip?! ctx rctx (car rest) 'lfo-switch 'lfo-switch-placed-channels 'init)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (if (lfo-skip?! ctx rctx (car rest) 'lfo 'lfo-placed-channels #f)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(and (is-a? (car rest) <sxmml-mml-rest>)
                (not (null? (cdr rest)))
                (is-a? (cadr rest) <sxmml-mml-rest>))
           (slot-set! (cadr rest) 'ticks (+ (slot-ref (car rest) 'ticks)
                                            (slot-ref (cadr rest) 'ticks)))
           (loop (cdr rest) ctx rctx sctx result)]
          [else
           (loop (cdr rest) ctx rctx sctx (cons (car rest) result))])))

;; チャンネルごとの連想リスト alist から channel の値を返す。なければ default を返す
(define (%channel-ref alist channel default)
  (if-let1 p (assq channel alist)
    (cdr p)
    default))

(define (%make-repeat-end-context lis)
  (let loop ([rest lis]
             [ctx (make <sxmml-optimize-2nd-pass-context>)])
    (cond [(null? rest)
           (error "MML terminated before repeat end")]
          [(and (is-a? (car rest) <sxmml-mml-repeat-end>)
                (= (slot-ref ctx 'repeat-depth) 0))
           ctx]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (slot-set! ctx 'repeat-depth (- (slot-ref ctx 'repeat-depth) 1))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (slot-set! ctx 'repeat-depth (+ (slot-ref ctx 'repeat-depth) 1))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (slot-set! ctx 'voice (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (slot-set! ctx 'volume (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-gate-time>)
           (slot-set! ctx 'gate-time (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (slot-set! ctx 'pan (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (slot-set! ctx 'detune (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (slot-set! ctx 'envelope (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (slot-set! ctx 'lfo-switch (acons (slot-ref (car rest) 'channel) (slot-ref (car rest) 'value)
                                             (slot-ref ctx 'lfo-switch)))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (slot-set! ctx 'lfo (acons (slot-ref (car rest) 'channel) (slot-ref (car rest) 'value)
                                      (slot-ref ctx 'lfo)))
           (loop (cdr rest) ctx)]
          [else
           (loop (cdr rest) ctx)]
          )))

;;
;; repeat
;; 既存のリピートのブロック。front は : の前、back は : の後の要素のリストで、
;; : がなければ break と back は #f。要素はオブジェクトかブロック
(define-record-type <repeat-block>
  (%make-repeat-block start front break back end) %repeat-block?
  (start %repeat-block-start)
  (front %repeat-block-front)
  (break %repeat-block-break)
  (back %repeat-block-back)
  (end %repeat-block-end))

;; 平坦なオブジェクトのリストを、オブジェクトとブロックの要素のリストにする
(define (%repeat-structure lis)
  ;; rest の先頭から、] か終わりまでの要素を読む。(values 要素 : の後の要素 終わりの ] 残り)
  (define (body rest)
    (let loop ([rest rest] [items ()] [front #f] [break #f])
      (cond [(null? rest)
             (values (reverse items) front break #f ())]
            [(is-a? (car rest) <sxmml-mml-repeat-end>)
             (values (reverse items) front break (car rest) (cdr rest))]
            [(and (is-a? (car rest) <sxmml-mml-repeat-break>) (not break))
             (loop (cdr rest) () (reverse items) (car rest))]
            [(is-a? (car rest) <sxmml-mml-repeat-start>)
             (receive (items* front* break* end rest*) (body (cdr rest))
               (unless end (error "MML terminated before repeat end"))
               (loop rest*
                     (cons (if break*
                             (%make-repeat-block (car rest) front* break* items* end)
                             (%make-repeat-block (car rest) items* #f #f end))
                           items)
                     front break))]
            [else
             (loop (cdr rest) (cons (car rest) items) front break)])))
  (receive (items front break end rest) (body lis)
    (when (or end break) (error "repeat end appeared before repeat start"))
    items))

;; %repeat-structure の逆。要素のリストを平坦なオブジェクトのリストにする
(define (%repeat-flatten items)
  (append-map (^i (if (%repeat-block? i)
                    `(,(%repeat-block-start i) ,@(%repeat-flatten (%repeat-block-front i))
                      ,@(if (%repeat-block-break i)
                          `(,(%repeat-block-break i) ,@(%repeat-flatten (%repeat-block-back i)))
                          ())
                      ,(%repeat-block-end i))
                    (list i)))
              items))

;; 要素のリストを展開したオブジェクトのリスト。反復回数 r は正の正確な整数で、
;; ブロックは (front back)^(r-1) front とする
(define (%repeat-expansion items)
  (append-map (^i (if (%repeat-block? i)
                    (let ([r (slot-ref (%repeat-block-end i) 'value)]
                          [front (%repeat-expansion (%repeat-block-front i))]
                          [back (%repeat-expansion (or (%repeat-block-back i) ()))])
                      (append (concatenate (make-list (- r 1) (append front back))) front))
                    (list i)))
              items))

;; クラスごとのスロット名。%repeat-same? は圧縮で何度も呼ぶので覚えておく
(define %repeat-slot-names (make-hash-table eq-comparator))

;; 圧縮で同じとみなすか。オブジェクトはクラスが同じで、全スロットが、両方とも未束縛か、
;; 両方とも束縛されて equal? なら同じ。ブロックは回数と : の有無が同じで、本文の要素が
;; 順に同じなら同じ。音符と & をまとめた要素（オブジェクトのリスト）は要素ごとに比べる
(define (%repeat-same? a b)
  (define (items-same? x y)
    (and (= (length x) (length y)) (every %repeat-same? x y)))
  (cond [(and (%repeat-block? a) (%repeat-block? b))
         (and (equal? (slot-ref (%repeat-block-end a) 'value) (slot-ref (%repeat-block-end b) 'value))
              (eq? (not (%repeat-block-break a)) (not (%repeat-block-break b)))
              (items-same? (%repeat-block-front a) (%repeat-block-front b))
              (items-same? (or (%repeat-block-back a) ()) (or (%repeat-block-back b) ())))]
        [(or (%repeat-block? a) (%repeat-block? b)) #f]
        [(and (pair? a) (pair? b)) (items-same? a b)]
        [(or (pair? a) (pair? b)) #f]
        [else
         (and (eq? (class-of a) (class-of b))
              (every (^s (let ([ba (slot-bound? a s)] [bb (slot-bound? b s)])
                           (and (eq? ba bb)
                                (or (not ba) (equal? (slot-ref a s) (slot-ref b s))))))
                     (or (hash-table-get %repeat-slot-names (class-of a) #f)
                         (rlet1 names (map slot-definition-name (class-slots (class-of a)))
                           (hash-table-put! %repeat-slot-names (class-of a) names)))))]))

;; 圧縮の上限と既定値。回数の上限 255 と入れ子の上限 32 は PMD の局所ループの範囲と
;; ネストの上限 (PMDMML.MAN §10-1)。展開すると周期が延びるので、展開するときは
;; 窓幅を広げる
(define %repeat-count-max 255)
(define %repeat-depth-max 32)
(define %repeat-window 256)
(define %repeat-expand-window 1024)
(define %repeat-expand-limit 256)

;; 第 2 段階の後の列を、反復で圧縮する。既存のリピートはひとまとめ（原子）にし、
;; その本文も同じように圧縮する。expand? なら、展開後の長さが %repeat-expand-limit
;; 以下の既存のリピートを先に展開し、窓幅 %repeat-expand-window で圧縮する
(define (%sxmml-optimize-repeat lis expand?)
  (let* ([input (let1 items (%repeat-structure lis)
                  (if expand? (%repeat-expand-small items %repeat-expand-limit) items))]
         [counts (iota (- %repeat-count-max 1) 2)]
         [settings (make-repeat-tree-settings (map (cut cons <> 2) counts) (map (cut cons <> 3) counts)
                                              #t (if expand? %repeat-expand-window %repeat-window))]
         [output (%repeat-compress input 0 settings)])
    (%repeat-check output input)
    output))

;; 回数が 1 以上で、展開後の長さが limit 以下のブロックを、内側から順に展開する。
;; 展開した列は元のインスタンスを並べ、複製しない
(define (%repeat-expand-small items limit)
  (append-map (^i (if (%repeat-block? i)
                    (let1 b (%make-repeat-block (%repeat-block-start i)
                                                (%repeat-expand-small (%repeat-block-front i) limit)
                                                (%repeat-block-break i)
                                                (and-let1 back (%repeat-block-back i)
                                                  (%repeat-expand-small back limit))
                                                (%repeat-block-end i))
                      ;; L を含むブロックと、2 個目以降の : を持つブロックは展開しない
                      (if (and (>= (slot-ref (%repeat-block-end b) 'value) 1)
                               (not (%repeat-loop? b))
                               (not (any (cut is-a? <> <sxmml-mml-repeat-break>)
                                         (or (%repeat-block-back b) ())))
                               (<= (%repeat-expanded-length (list b)) limit))
                        (%repeat-expansion (list b))
                        (list b)))
                    (list i)))
              items))

;; %repeat-expansion の長さ。入れ子の深いリピートでも展開せずに求める
(define (%repeat-expanded-length items)
  (fold (^[i total]
          (+ total
             (if (%repeat-block? i)
               (let ([r (slot-ref (%repeat-block-end i) 'value)]
                     [front (%repeat-expanded-length (%repeat-block-front i))]
                     [back (%repeat-expanded-length (or (%repeat-block-back i) ()))])
                 (+ (* (- r 1) (+ front back)) front))
               1)))
        0 items))

;; 要素のリスト items を圧縮した平坦なオブジェクトのリストを返す。depth は外側の
;; ブロックの深さ。L、L を含むブロック、ブロックの 2 個目以降の : の前後は別の区間として
;; 圧縮し、これらをループに入れない。& は音符の直後に置くので、音符と後に続く & を
;; 一つの要素（オブジェクトのリスト）にまとめ、間にループの境界を入れない
(define (%repeat-compress items depth settings)
  ;; ブロックは本文を圧縮した出力を持つ原子にする。(要素 . 出力)
  (define (atom i)
    (cons i (if (%repeat-block? i)
              `(,(%repeat-block-start i) ,@(%repeat-compress (%repeat-block-front i) (+ depth 1) settings)
                ,@(if (%repeat-block-break i)
                    `(,(%repeat-block-break i)
                      ,@(%repeat-compress (%repeat-block-back i) (+ depth 1) settings))
                    ())
                ,(%repeat-block-end i))
              (list i))))
  (define (segment atoms)
    (if (null? atoms)
      ()
      (let* ([elements (list->vector (map car atoms))]
             [outputs (list->vector (map cdr atoms))]
             [raw (append-map cdr atoms)])
        (receive (cost tree)
            (repeat-tree-compress elements (vector-map length outputs) %repeat-same? settings)
          (let1 compressed (%repeat-tree->ir tree outputs)
            ;; 入れ子が上限を超え、圧縮前より深くなる区間は圧縮しない
            (if (and (> (+ depth (%repeat-depth compressed)) %repeat-depth-max)
                     (> (%repeat-depth compressed) (%repeat-depth raw)))
              raw
              compressed))))))
  (define (barrier? element)
    (or (is-a? element <sxmml-mml-loop>) (is-a? element <sxmml-mml-repeat-break>)
        (and (%repeat-block? element) (%repeat-loop? element))))
  ;; & を直前の要素にまとめる。直前がオブジェクトかまとめた要素のときだけ
  (define (tie-groups atoms)
    (reverse
     (fold (^[a groups]
             (if (and (is-a? (car a) <sxmml-mml-tie>) (pair? groups)
                      (not (%repeat-block? (caar groups))) (not (barrier? (caar groups))))
               (let1 prev (car groups)
                 (cons (cons (append (if (pair? (car prev)) (car prev) (list (car prev))) (list (car a)))
                             (append (cdr prev) (cdr a)))
                       (cdr groups)))
               (cons a groups)))
           () atoms)))
  (let loop ([rest (tie-groups (map atom items))] [atoms ()] [result ()])
    (cond [(null? rest)
           (concatenate (reverse (cons (segment (reverse atoms)) result)))]
          [(barrier? (caar rest))
           (loop (cdr rest) () (cons* (cdar rest) (segment (reverse atoms)) result))]
          [else
           (loop (cdr rest) (cons (car rest) atoms) result)])))

;; ブロックの本文（入れ子を含む）に L があるか
(define (%repeat-loop? block)
  (any (^i (if (%repeat-block? i) (%repeat-loop? i) (is-a? i <sxmml-mml-loop>)))
       (append (%repeat-block-front block) (or (%repeat-block-back block) ()))))

;; 圧縮した木を平坦なオブジェクトのリストにする。葉は outputs の出力に置き換え、
;; 反復には新しい [、:、] のオブジェクトを作る
(define (%repeat-tree->ir tree outputs)
  (cond [(literal? tree) (vector-ref outputs (literal-index tree))]
        [(concat? tree) (append (%repeat-tree->ir (concat-left tree) outputs)
                                (%repeat-tree->ir (concat-right tree) outputs))]
        [(repeat? tree)
         `(,(make <sxmml-mml-repeat-start>) ,@(%repeat-tree->ir (repeat-body tree) outputs)
           ,(make <sxmml-mml-repeat-end> :value (repeat-count tree)))]
        [(final? tree)
         `(,(make <sxmml-mml-repeat-start>) ,@(%repeat-tree->ir (final-prefix tree) outputs)
           ,(make <sxmml-mml-repeat-break>) ,@(%repeat-tree->ir (final-suffix tree) outputs)
           ,(make <sxmml-mml-repeat-end> :value (final-count tree)))]))

;; 平坦なオブジェクトのリストの、リピートの入れ子の最大の深さ
(define (%repeat-depth lis)
  (let loop ([rest lis] [depth 0] [deepest 0])
    (cond [(null? rest) deepest]
          [(is-a? (car rest) <sxmml-mml-repeat-start>) (loop (cdr rest) (+ depth 1) (max deepest (+ depth 1)))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>) (loop (cdr rest) (- depth 1) deepest)]
          [else (loop (cdr rest) depth deepest)])))

;; 圧縮の出力 output が、圧縮の入力 input（要素のリスト）と同じ列を表すかを確かめる。
;; 生成したループは一つの出現を繰り返し使うので、%repeat-same? で比べる。入れ子の深い
;; 既存のリピートは展開すると大きくなるので、生成したループ（[ が入力にないもの）だけを
;; 展開し、既存のリピートは回数・: の有無・本文で比べる
(define (%repeat-check output input)
  (define originals
    (rlet1 h (make-hash-table eq-comparator)
      (let mark ([items input])
        (dolist [i items]
          (when (%repeat-block? i)
            (hash-table-put! h (%repeat-block-start i) #t)
            (mark (%repeat-block-front i))
            (mark (or (%repeat-block-back i) ())))))))
  (define (unfold items)
    (append-map (^i (if (and (%repeat-block? i)
                             (not (hash-table-contains? originals (%repeat-block-start i))))
                      (let ([r (slot-ref (%repeat-block-end i) 'value)]
                            [front (unfold (%repeat-block-front i))]
                            [back (unfold (or (%repeat-block-back i) ()))])
                        (append (concatenate (make-list (- r 1) (append front back))) front))
                      (list i)))
                items))
  ;; xs は出力の、ys は入力の要素のリスト
  (define (match? xs ys)
    (and (= (length xs) (length ys))
         (every (^[x y] (cond [(and (%repeat-block? x) (%repeat-block? y))
                               (and (equal? (slot-ref (%repeat-block-end x) 'value)
                                            (slot-ref (%repeat-block-end y) 'value))
                                    (eq? (not (%repeat-block-break x)) (not (%repeat-block-break y)))
                                    (match? (unfold (%repeat-block-front x)) (%repeat-block-front y))
                                    (match? (unfold (or (%repeat-block-back x) ())) (or (%repeat-block-back y) ())))]
                              [(or (%repeat-block? x) (%repeat-block? y)) #f]
                              [else (%repeat-same? x y)]))
                xs ys)))
  (unless (match? (unfold (%repeat-structure output)) input)
    (error "repeat optimization changed the expanded objects")))
