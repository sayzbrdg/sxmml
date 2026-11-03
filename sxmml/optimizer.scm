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

(define-record-type <setting-effect>
  (%make-setting-effect key relative? value origin position)
  %setting-effect?
  (key %setting-effect-key)
  (relative? %setting-effect-relative?)
  (value %setting-effect-value)
  (origin %setting-effect-origin)
  (position %setting-effect-position))

(define (%ir->setting-effects obj position)
  (define (effect key relative? value)
    (%make-setting-effect key relative? value obj position))
  (let1 class (class-of obj)
    (cond [(eq? class <sxmml-mml-volume-change>)
           (list (effect <sxmml-mml-volume> #t (slot-ref obj 'value)))]
          [(eq? class <sxmml-mml-transpose-change>)
           (list (effect <sxmml-mml-transpose> #t (slot-ref obj 'value)))]
          [(memq class (list <sxmml-mml-tempo> <sxmml-mml-tempo-absolute>))
           (list (effect 'tempo #f (slot-ref obj 'value)))]
          [(memq class (list <sxmml-mml-lfo> <sxmml-mml-lfo-switch>))
           (list (effect (cons class (slot-ref obj 'channel)) #f (slot-ref obj 'value)))]
          [(memq class (list <sxmml-rhythm-volume> <sxmml-rhythm-volume-change> <sxmml-rhythm-pan>))
           (let1 relative? (eq? class <sxmml-rhythm-volume-change>)
             (and (not (and relative? (slot-ref obj 'value)))
                  (filter-map (^[slot] (let1 value (slot-ref obj slot)
                                       (and value
                                            (effect (cons (if relative? <sxmml-rhythm-volume> class) slot)
                                                    relative? value))))
                              '(value bass snare tom cymbal hihat rimshot))))]
          [(memq class (list <sxmml-mml-volume> <sxmml-mml-transpose> <sxmml-mml-voice>
                             <sxmml-mml-gate-time> <sxmml-mml-pan> <sxmml-mml-detune>
                             <sxmml-mml-envelope> <sxmml-mml-noise-frequency> <sxmml-mml-output-switch>))
           (list (effect class #f (slot-ref obj 'value)))]
          [else #f])))

(define (%compose-setting-effects earlier later)
  (cond [(not later) earlier]
        [(not (%setting-effect-relative? later)) later]
        [else
         (%make-setting-effect (%setting-effect-key earlier)
                               (%setting-effect-relative? earlier)
                               (+ (%setting-effect-value earlier) (%setting-effect-value later))
                               (%setting-effect-origin earlier)
                               (%setting-effect-position earlier))]))

(define (%setting-effects->ir table)
  (reverse!
   (fold (^[effect result]
           (let* ([origin (%setting-effect-origin effect)]
                  [class (class-of origin)]
                  [value (%setting-effect-value effect)])
             (cond [(memq class (list <sxmml-rhythm-volume> <sxmml-rhythm-volume-change> <sxmml-rhythm-pan>))
                    (let* ([slot (cdr (%setting-effect-key effect))]
                           [previous (and (pair? result) (eq? class (class-of (car result))))]
                           [obj (if previous (car result) (make class))])
                      (slot-set! obj slot value)
                      (if previous result (cons obj result)))]
                   [(is-a? origin <sxmml-mml-lfo>)
                    (cons (make class :value value :channel (slot-ref origin 'channel)) result)]
                   [else (cons (make class :value value) result)])))
         ()
         (sort (hash-table-values table)
               (^[a b] (< (%setting-effect-position a) (%setting-effect-position b)))))))

(define (%sxmml-optimize-tree! mmlenv)
  (let* ([objects (let recur ([rest (slot-ref mmlenv 'compiled-data)] [result ()])
                    (cond [(null? rest) result]
                          [(pair? (car rest)) (recur (cdr rest) (recur (car rest) result))]
                          [else (recur (cdr rest) (cons (car rest) result))]))]
         [table (make-hash-table equal-comparator)])
    (let loop ([rest objects] [position (length objects)] [result ()])
      (cond [(null? rest) (append (%setting-effects->ir table) result)]
            [(is-a? (car rest) <sxmml-mml-nop>)
             (loop (cdr rest) (- position 1) result)]
            [else
             (let* ([obj (car rest)]
                    [effects (%ir->setting-effects obj position)])
               (if effects
                 (begin
                   (for-each (^[effect]
                               (let1 key (%setting-effect-key effect)
                                 (hash-table-set! table key
                                                  (%compose-setting-effects effect
                                                                            (hash-table-get table key #f)))))
                             effects)
                   (loop (cdr rest) (- position 1) result))
                 (let1 suffix (%setting-effects->ir table)
                   (hash-table-clear! table)
                   (loop (cdr rest) (- position 1) (cons obj (append suffix result))))))]))))

;;
;; 第2パス
(define-record-type <state-effect>
  (%make-state-effect relative? value)
  %state-effect?
  (relative? %state-effect-relative?)
  (value %state-effect-value))

(define (%state-object-key obj)
  (cond [(is-a? obj <sxmml-mml-volume-change>) <sxmml-mml-volume>]
        [(is-a? obj <sxmml-mml-lfo-switch>)
         (cons <sxmml-mml-lfo-switch> (slot-ref obj 'channel))]
        [(is-a? obj <sxmml-mml-lfo>)
         (cons <sxmml-mml-lfo> (slot-ref obj 'channel))]
        [else
         (find (^[class] (is-a? obj class))
               (list <sxmml-mml-volume> <sxmml-mml-voice> <sxmml-mml-gate-time>
                     <sxmml-mml-pan> <sxmml-mml-detune> <sxmml-mml-envelope>))]))

(define (%state-set state key value)
  (acons key value (alist-delete key state equal?)))

(define (%state-effects-compose earlier later)
  (fold (^[entry result]
          (let* ([key (car entry)] [effect (cdr entry)] [previous (assoc key result)])
            (%state-set result key
                        (if (and (%state-effect-relative? effect) previous)
                          (%make-state-effect (%state-effect-relative? (cdr previous))
                                              (+ (%state-effect-value (cdr previous))
                                                 (%state-effect-value effect)))
                          effect))))
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
          (%state-effects-compose
           effects
           (if (%repeat-block? item)
             (%state-repeat-effects item cache)
             (if-let1 key (%state-object-key item)
               (list (cons key (%make-state-effect (is-a? item <sxmml-mml-volume-change>)
                                                  (slot-ref item 'value))))
               ()))))
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
               (values (append (reverse! result) (cons (car rest) tail)) outgoing)))]
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

(define (%state-merge-rests! lis)
  (let loop ([rest lis] [result ()])
    (cond [(null? rest) (reverse! result)]
          [(and (pair? (cdr rest)) (is-a? (car rest) <sxmml-mml-rest>)
                (is-a? (cadr rest) <sxmml-mml-rest>))
           (slot-set! (cadr rest) 'ticks (+ (slot-ref (car rest) 'ticks) (slot-ref (cadr rest) 'ticks)))
           (loop (cdr rest) result)]
          [else (loop (cdr rest) (cons (car rest) result))])))

(define (%sxmml-optimize-2nd-pass! lis)
  (receive (items outgoing)
      (%state-optimize-region (%repeat-structure lis) () (make-hash-table eq-comparator))
    (%state-merge-rests! (%repeat-flatten items))))

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
