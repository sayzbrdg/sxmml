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
  (use srfi.1)
  (use sxmml.classes)
  (use sxmml.repeat-tree)
  (export sxmml-ir-optimize-local sxmml-ir-remove-redundant-settings sxmml-ir-merge-rests
          sxmml-ir-compress-repeats sxmml-ir-optimize sxmml-ir-optimize-with-repeats
          sxmml-ir-optimize-with-repeat-expansion
          ))
(select-module sxmml.optimizer)

;;;
;;; optimizer
;;;

(define-record-type <setting-effect>
  (%make-setting-effect key relative value origin position)
  %setting-effect?
  (key %setting-effect-key)
  (relative %setting-effect-relative?)
  (value %setting-effect-value)
  (origin %setting-effect-origin)
  (position %setting-effect-position))

(define (%single-setting-effect-fold proc state obj position key relative?)
  (values #t (proc key relative? (slot-ref obj 'value) obj position state)))

(define (%rhythm-setting-effects-fold proc state obj position key relative?)
  (let loop ([slots '(value bass snare tom cymbal hihat rimshot)] [state state])
    (cond [(null? slots) (values #t state)]
          [else
           (let1 value (slot-ref obj (car slots))
             (loop (cdr slots)
                   (if value (proc (cons key (car slots)) relative? value obj position state) state)))])))

(define (%ir-setting-effects-fold proc state obj position)
  (cond [(and (is-a? obj <sxmml-rhythm-volume-change>) (slot-ref obj 'value))
         (values #f state)]
        [(is-a? obj <sxmml-rhythm-pan>)
         (%rhythm-setting-effects-fold proc state obj position 'rhythm-pan
                                      (is-a? obj <sxmml-rhythm-volume-change>))]
        [(is-a? obj <sxmml-rhythm-volume-change>)
         (%rhythm-setting-effects-fold proc state obj position 'rhythm-volume #t)]
        [(is-a? obj <sxmml-rhythm-volume>)
         (%rhythm-setting-effects-fold proc state obj position 'rhythm-volume #f)]
        [(is-a? obj <sxmml-mml-volume-change>)
         (%single-setting-effect-fold proc state obj position 'volume #t)]
        [(is-a? obj <sxmml-mml-volume>)
         (%single-setting-effect-fold proc state obj position 'volume #f)]
        [(is-a? obj <sxmml-mml-transpose-change>)
         (%single-setting-effect-fold proc state obj position 'transpose #t)]
        [(is-a? obj <sxmml-mml-transpose>)
         (%single-setting-effect-fold proc state obj position 'transpose #f)]
        [(is-a? obj <sxmml-mml-lfo-switch>)
         (%single-setting-effect-fold proc state obj position (cons 'lfo-switch (slot-ref obj 'channel)) #f)]
        [(is-a? obj <sxmml-mml-lfo>)
         (%single-setting-effect-fold proc state obj position (cons 'lfo (slot-ref obj 'channel)) #f)]
        [(or (is-a? obj <sxmml-mml-tempo>) (is-a? obj <sxmml-mml-tempo-absolute>))
         (%single-setting-effect-fold proc state obj position 'tempo #f)]
        [(is-a? obj <sxmml-mml-voice>)
         (%single-setting-effect-fold proc state obj position 'voice #f)]
        [(is-a? obj <sxmml-mml-gate-time>)
         (%single-setting-effect-fold proc state obj position 'gate-time #f)]
        [(is-a? obj <sxmml-mml-pan>)
         (%single-setting-effect-fold proc state obj position 'pan #f)]
        [(is-a? obj <sxmml-mml-detune>)
         (%single-setting-effect-fold proc state obj position 'detune #f)]
        [(is-a? obj <sxmml-mml-envelope>)
         (%single-setting-effect-fold proc state obj position 'envelope #f)]
        [(is-a? obj <sxmml-mml-noise-frequency>)
         (%single-setting-effect-fold proc state obj position 'noise-frequency #f)]
        [(is-a? obj <sxmml-mml-output-switch>)
         (%single-setting-effect-fold proc state obj position 'output-switch #f)]
        [else (values #f state)]))

(define (%compose-setting-effects key relative? value origin position later)
  (cond [(not later) (%make-setting-effect key relative? value origin position)]
        [(not (%setting-effect-relative? later)) later]
        [else (%make-setting-effect key relative? (+ value (%setting-effect-value later)) origin position)]))

(define (%ir-same-class? a b)
  (equal? (class-of a) (class-of b)))

(define (%ir-copy-except origin omitted-slots)
  (rlet1 result (allocate-instance (class-of origin) ())
    (for-each
     (^[definition]
       (let1 slot (slot-definition-name definition)
         (when (and (eq? (slot-definition-allocation definition) :instance)
                    (not (memq slot omitted-slots)) (slot-bound? origin slot))
           (slot-set! result slot (slot-ref origin slot)))))
     (class-slots (class-of origin)))))

(define (%rhythm-setting? obj)
  (or (is-a? obj <sxmml-rhythm-volume-change>)
      (is-a? obj <sxmml-rhythm-volume>) (is-a? obj <sxmml-rhythm-pan>)))

(define (%single-setting-effect->ir effect)
  (rlet1 result (%ir-copy-except (%setting-effect-origin effect) '(value))
    (slot-set! result 'value (%setting-effect-value effect))))

(define (%rhythm-setting-effects->ir effects)
  (let* ([origin (%setting-effect-origin (car effects))]
         [result (%ir-copy-except origin '(value bass snare tom cymbal hihat rimshot))]
         [remaining
          (let loop ([rest effects])
            (cond [(and (pair? rest) (%rhythm-setting? (%setting-effect-origin (car rest)))
                        (%ir-same-class? origin (%setting-effect-origin (car rest))))
                   (slot-set! result (cdr (%setting-effect-key (car rest))) (%setting-effect-value (car rest)))
                   (loop (cdr rest))]
                  [else rest]))])
    (for-each (^[slot] (unless (slot-bound? result slot) (slot-set! result slot #f)))
              '(value bass snare tom cymbal hihat rimshot))
    (values result remaining)))

(define (%setting-effects->ir table tail)
  (let loop ([effects (sort (hash-table-values table)
                           (^[a b] (< (%setting-effect-position a) (%setting-effect-position b))))]
             [result ()])
    (cond [(null? effects) (reverse! result tail)]
          [(%rhythm-setting? (%setting-effect-origin (car effects)))
           (receive (obj remaining) (%rhythm-setting-effects->ir effects)
             (loop remaining (cons obj result)))]
          [else (loop (cdr effects) (cons (%single-setting-effect->ir (car effects)) result))])))

(define (sxmml-ir-optimize-local ir)
  (define (compose! key relative? value origin position table)
    (hash-table-set! table key
                     (%compose-setting-effects key relative? value origin position (hash-table-get table key #f)))
    table)
  (let loop ([rest (reverse ir)] [position 0] [table (make-hash-table equal-comparator)] [result ()])
    (cond [(null? rest) (%setting-effects->ir table result)]
          [(is-a? (car rest) <sxmml-mml-nop>)
           (loop (cdr rest) (- position 1) table result)]
          [else
           (let1 obj (car rest)
             (receive (recognized? table) (%ir-setting-effects-fold compose! table obj position)
               (if recognized?
                 (loop (cdr rest) (- position 1) table result)
                 (let1 suffix (%setting-effects->ir table result)
                   (hash-table-clear! table)
                   (loop (cdr rest) (- position 1) table (cons obj suffix))))))])))

;;
;; 第2パス
(define-record-type <state-effect>
  (%make-state-effect relative value)
  %state-effect?
  (relative %state-effect-relative?)
  (value %state-effect-value))

(define (%state-object-key obj)
  (cond [(or (is-a? obj <sxmml-mml-volume-change>) (is-a? obj <sxmml-mml-volume>)) 'volume]
        [(is-a? obj <sxmml-mml-lfo-switch>) (cons 'lfo-switch (slot-ref obj 'channel))]
        [(is-a? obj <sxmml-mml-lfo>) (cons 'lfo (slot-ref obj 'channel))]
        [(is-a? obj <sxmml-mml-voice>) 'voice]
        [(is-a? obj <sxmml-mml-gate-time>) 'gate-time]
        [(is-a? obj <sxmml-mml-pan>) 'pan]
        [(is-a? obj <sxmml-mml-detune>) 'detune]
        [(is-a? obj <sxmml-mml-envelope>) 'envelope]
        [else #f]))

(define (%state-set state key value)
  (acons key value (alist-delete key state equal?)))

(define (%state-effect-compose effects key effect)
  (let1 previous (assoc key effects)
    (%state-set effects key
                (if (and (%state-effect-relative? effect) previous)
                  (%make-state-effect (%state-effect-relative? (cdr previous))
                                      (+ (%state-effect-value (cdr previous))
                                         (%state-effect-value effect)))
                  effect))))

(define (%state-effects-compose earlier later)
  (fold (^[entry result] (%state-effect-compose result (car entry) (cdr entry)))
        earlier later))

(define (%state-effects-power effects count)
  (if (zero? count)
    ()
    (map (^[entry]
           (cons (car entry)
                 (if (%state-effect-relative? (cdr entry))
                   (%make-state-effect #t (* count (%state-effect-value (cdr entry))))
                   (cdr entry))))
         effects)))

(define (%state-effects-apply state effects)
  (fold (^[entry result]
          (let ([key (car entry)] [effect (cdr entry)])
            (if (%state-effect-relative? effect)
              (if-let1 known (assoc key result)
                (%state-set result key (+ (cdr known) (%state-effect-value effect)))
                result)
              (%state-set result key (%state-effect-value effect)))))
        state effects))

(define (%state-join left right)
  (filter (^[entry]
            (and-let* ([other (assoc (car entry) right)])
              (equal? (cdr entry) (cdr other))))
          left))

(define (%state-region-effects items cache)
  (fold (^[item effects]
          (cond [(%repeat-block? item)
                 (%state-effects-compose effects (%state-repeat-effects item cache))]
                [(%state-object-key item)
                 => (^[key]
                      (%state-effect-compose effects key
                                             (%make-state-effect (is-a? item <sxmml-mml-volume-change>)
                                                                 (slot-ref item 'value))))]
                [else effects]))
        () items))

(define (%state-repeat-effects block cache)
  (or (hash-table-get cache block #f)
      (let* ([front (%state-region-effects (%repeat-block-front block) cache)]
             [count (slot-ref (%repeat-block-end block) 'value)]
             [effects (if (= count 1)
                        front
                        (%state-effects-compose
                         (%state-effects-power
                          (%state-effects-compose
                           front (%state-region-effects (or (%repeat-block-back block) ()) cache))
                          (- count 1))
                         front))])
        (hash-table-set! cache block effects)
        effects)))

(define (%state-optimize-repeat block state cache)
  (let* ([front (%repeat-block-front block)] [back (%repeat-block-back block)]
         [count (slot-ref (%repeat-block-end block) 'value)]
         [entry (if (= count 1)
                  state
                  (%state-join
                   state
                   (%state-effects-apply
                    state (%state-effects-compose (%state-region-effects front cache)
                                                  (%state-region-effects (or back ()) cache)))))])
    (receive (new-front middle) (%state-optimize-region front entry cache)
      (receive (new-back ignored)
          (if (and back (> count 1))
            (%state-optimize-region back middle cache)
            (values back middle))
        (values (%make-repeat-block (%repeat-block-start block) new-front
                                    (%repeat-block-break block) new-back (%repeat-block-end block))
                (%state-effects-apply state (%state-repeat-effects block cache)))))))

(define (%state-optimize-region items state cache)
  (let loop ([rest items] [state state] [result ()])
    (cond [(null? rest) (values (reverse! result) state)]
          [(%repeat-block? (car rest))
           (receive (block outgoing) (%state-optimize-repeat (car rest) state cache)
             (loop (cdr rest) outgoing (cons block result)))]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (let1 entry (%state-join state (%state-effects-apply state (%state-region-effects (cdr rest) cache)))
             (receive (tail outgoing) (%state-optimize-region (cdr rest) entry cache)
               (values (reverse! result (cons (car rest) tail)) outgoing)))]
          [else
           (let* ([obj (car rest)] [key (%state-object-key obj)]
                  [known (and key (assoc key state))])
             (cond [(not key) (loop (cdr rest) state (cons obj result))]
                   [(is-a? obj <sxmml-mml-volume-change>)
                    (loop (cdr rest)
                          (if known (%state-set state key (+ (cdr known) (slot-ref obj 'value))) state)
                          (cons obj result))]
                   [(and known (equal? (cdr known) (slot-ref obj 'value)))
                    (loop (cdr rest) state result)]
                   [else
                    (loop (cdr rest) (%state-set state key (slot-ref obj 'value)) (cons obj result))]))])))

(define (sxmml-ir-merge-rests ir)
  (reverse!
   (fold (^[obj result]
           (if (and (is-a? obj <sxmml-mml-rest>) (pair? result)
                    (is-a? (car result) <sxmml-mml-rest>))
             (let* ([ticks (+ (slot-ref obj 'ticks) (slot-ref (car result) 'ticks))]
                    [combined (%ir-copy-except obj '(ticks))])
               (slot-set! combined 'ticks ticks)
               (cons combined (cdr result)))
             (cons obj result)))
         () ir)))

(define (sxmml-ir-remove-redundant-settings ir)
  (receive (items outgoing)
      (%state-optimize-region (%repeat-structure ir) () (make-hash-table eq-comparator))
    (%repeat-flatten items ())))

(define (sxmml-ir-compress-repeats ir :key (expand-small #f))
  (%sxmml-optimize-repeat ir expand-small))

(define (sxmml-ir-optimize ir)
  (sxmml-ir-merge-rests
   (sxmml-ir-remove-redundant-settings (sxmml-ir-optimize-local ir))))

(define (sxmml-ir-optimize-with-repeats ir)
  (sxmml-ir-compress-repeats (sxmml-ir-optimize ir)))

(define (sxmml-ir-optimize-with-repeat-expansion ir)
  (sxmml-ir-compress-repeats (sxmml-ir-optimize ir) :expand-small #t))

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
             (values (reverse! items) front break #f ())]
            [(is-a? (car rest) <sxmml-mml-repeat-end>)
             (values (reverse! items) front break (car rest) (cdr rest))]
            [(and (is-a? (car rest) <sxmml-mml-repeat-break>) (not break))
             (loop (cdr rest) () (reverse! items) (car rest))]
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
(define (%repeat-flatten items tail)
  (fold-right
   (^[i tail]
     (if (%repeat-block? i)
       (cons (%repeat-block-start i)
             (%repeat-flatten
              (%repeat-block-front i)
              (if (%repeat-block-break i)
                (cons (%repeat-block-break i)
                      (%repeat-flatten (%repeat-block-back i) (cons (%repeat-block-end i) tail)))
                (cons (%repeat-block-end i) tail))))
       (cons i tail)))
   tail items))

;; ブロックは (front back)^(r-1) front。展開した本文は各出現のセルの構築に再利用する。
(define (%repeat-expansion items tail)
  (fold-right
   (^[i tail]
     (if (%repeat-block? i)
       (let1 count (slot-ref (%repeat-block-end i) 'value)
         (if (= count 1)
           (%repeat-expansion (%repeat-block-front i) tail)
           (let ([front (%repeat-expansion (%repeat-block-front i) ())]
                 [back (%repeat-expansion (or (%repeat-block-back i) ()) ())])
             (let loop ([remaining (- count 1)] [result (fold-right cons tail front)])
               (if (zero? remaining)
                 result
                 (loop (- remaining 1) (fold-right cons (fold-right cons result back) front)))))))
       (cons i tail)))
   tail items))

;; クラスごとのスロット名。%repeat-same? は圧縮で何度も呼ぶので覚えておく
(define %repeat-slot-names (make-hash-table eq-comparator))

;; 圧縮で同じとみなすか。オブジェクトはクラスが同じで、全スロットが、両方とも未束縛か、
;; 両方とも束縛されて equal? なら同じ。ブロックは回数と : の有無が同じで、本文の要素が
;; 順に同じなら同じ。音符と & をまとめた要素（オブジェクトのリスト）は要素ごとに比べる
(define (%repeat-same? a b)
  (define (items-same? x y)
    (let loop ([x x] [y y])
      (cond [(null? x) (null? y)]
            [(null? y) #f]
            [else (and (%repeat-same? (car x) (car y)) (loop (cdr x) (cdr y)))])))
  (cond [(and (%repeat-block? a) (%repeat-block? b))
         (and (equal? (slot-ref (%repeat-block-end a) 'value) (slot-ref (%repeat-block-end b) 'value))
              (eq? (not (%repeat-block-break a)) (not (%repeat-block-break b)))
              (items-same? (%repeat-block-front a) (%repeat-block-front b))
              (items-same? (or (%repeat-block-back a) ()) (or (%repeat-block-back b) ())))]
        [(or (%repeat-block? a) (%repeat-block? b)) #f]
        [(and (pair? a) (pair? b)) (items-same? a b)]
        [(or (pair? a) (pair? b)) #f]
        [else
         (and (%ir-same-class? a b)
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

;; 平坦なIR列を、反復で圧縮する。既存のリピートはひとまとめ（原子）にし、
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
  (fold-right
   (^[i tail]
     (if (%repeat-block? i)
       (let1 b (%make-repeat-block (%repeat-block-start i)
                                  (%repeat-expand-small (%repeat-block-front i) limit)
                                  (%repeat-block-break i)
                                  (and-let1 back (%repeat-block-back i)
                                    (%repeat-expand-small back limit))
                                  (%repeat-block-end i))
         (if (and (>= (slot-ref (%repeat-block-end b) 'value) 1)
                  (not (%repeat-loop? b))
                  (not (any (cut is-a? <> <sxmml-mml-repeat-break>) (or (%repeat-block-back b) ())))
                  (<= (%repeat-expanded-length (list b)) limit))
           (%repeat-expansion (list b) tail)
           (cons b tail)))
       (cons i tail)))
   () items))

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
  (define (segment atoms count)
    (if (zero? count)
      ()
      (let ([elements (make-vector count)] [outputs (make-vector count)] [weights (make-vector count)])
        (let fill ([rest atoms] [index (- count 1)])
          (unless (null? rest)
            (let* ([entry (car rest)] [group? (pair? (car entry))]
                   [element (if group? (reverse! (car entry)) (car entry))]
                   [output (if group? element (cdr entry))])
              (vector-set! elements index element)
              (vector-set! outputs index output)
              (vector-set! weights index (length output))
              (fill (cdr rest) (- index 1)))))
        (receive (cost tree) (repeat-tree-compress elements weights %repeat-same? settings)
          (let* ([compressed (%repeat-tree->ir tree outputs ())]
                 [compressed-depth (%repeat-depth compressed)])
            (if (> (+ depth compressed-depth) %repeat-depth-max)
              (let1 raw (let loop ([index (- count 1)] [tail ()])
                          (if (< index 0) tail
                              (loop (- index 1) (fold-right cons tail (vector-ref outputs index)))))
                (if (> compressed-depth (%repeat-depth raw)) raw compressed))
              compressed))))))
  (define (barrier? element)
    (or (is-a? element <sxmml-mml-loop>) (is-a? element <sxmml-mml-repeat-break>)
        (and (%repeat-block? element) (%repeat-loop? element))))
  (let loop ([rest items] [atoms ()] [count 0] [result ()])
    (cond [(null? rest)
           (fold (^[part tail] (fold-right cons tail part)) () (cons (segment atoms count) result))]
          [(barrier? (car rest))
           (loop (cdr rest) () 0 (cons* (cdr (atom (car rest))) (segment atoms count) result))]
          [(and (is-a? (car rest) <sxmml-mml-tie>) (pair? atoms)
                (not (%repeat-block? (caar atoms))) (not (barrier? (caar atoms))))
           (let1 group (cons (car rest) (if (pair? (caar atoms)) (caar atoms) (list (caar atoms))))
             (loop (cdr rest) (cons (cons group group) (cdr atoms)) count result))]
          [else (loop (cdr rest) (cons (atom (car rest)) atoms) (+ count 1) result)])))

;; ブロックの本文（入れ子を含む）に L があるか
(define (%repeat-loop? block)
  (define (loop? i)
    (if (%repeat-block? i) (%repeat-loop? i) (is-a? i <sxmml-mml-loop>)))
  (or (any loop? (%repeat-block-front block))
      (any loop? (or (%repeat-block-back block) ()))))

;; 圧縮木の葉を出力テンプレートに置き換え、tailの前へIRを構築する。
(define (%repeat-tree->ir tree outputs tail)
  (cond [(literal? tree) (fold-right cons tail (vector-ref outputs (literal-index tree)))]
        [(concat? tree) (%repeat-tree->ir (concat-left tree) outputs
                                         (%repeat-tree->ir (concat-right tree) outputs tail))]
        [(repeat? tree)
         (cons (make <sxmml-mml-repeat-start>)
               (%repeat-tree->ir (repeat-body tree) outputs
                                 (cons (make <sxmml-mml-repeat-end> :value (repeat-count tree)) tail)))]
        [(final? tree)
         (cons (make <sxmml-mml-repeat-start>)
               (%repeat-tree->ir
                (final-prefix tree) outputs
                (cons (make <sxmml-mml-repeat-break>)
                      (%repeat-tree->ir (final-suffix tree) outputs
                                        (cons (make <sxmml-mml-repeat-end> :value (final-count tree)) tail)))))]))

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
  (define (unfold items tail)
    (fold-right
     (^[i tail]
       (if (and (%repeat-block? i)
                (not (hash-table-contains? originals (%repeat-block-start i))))
         (let ([count (slot-ref (%repeat-block-end i) 'value)]
               [front (unfold (%repeat-block-front i) ())]
               [back (unfold (or (%repeat-block-back i) ()) ())])
           (let loop ([remaining (- count 1)] [result (fold-right cons tail front)])
             (if (zero? remaining)
               result
               (loop (- remaining 1) (fold-right cons (fold-right cons result back) front)))))
         (cons i tail)))
     tail items))
  (define (match? xs ys)
    (cond [(null? xs) (null? ys)]
          [(null? ys) #f]
          [else
           (let ([x (car xs)] [y (car ys)])
             (and (cond [(and (%repeat-block? x) (%repeat-block? y))
                         (and (equal? (slot-ref (%repeat-block-end x) 'value)
                                      (slot-ref (%repeat-block-end y) 'value))
                              (eq? (not (%repeat-block-break x)) (not (%repeat-block-break y)))
                              (match? (unfold (%repeat-block-front x) ()) (%repeat-block-front y))
                              (match? (unfold (or (%repeat-block-back x) ()) ()) (or (%repeat-block-back y) ())))]
                        [(or (%repeat-block? x) (%repeat-block? y)) #f]
                        [else (%repeat-same? x y)])
                  (match? (cdr xs) (cdr ys))))]))
  (unless (match? (unfold (%repeat-structure output) ()) input)
    (error "repeat optimization changed the expanded objects")))
