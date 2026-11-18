(define-module sxmml.backend.fmp7
  (use srfi.1)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.optimizer)
  (export sxmml-fmp7-tracks-prepare sxmml-fmp7-ir-optimize sxmml-fmp7-lfo-output-prepare
          sxmml-fmp7-lfo-values sxmml-fmp7-envelope-values sxmml-fmp7-pcm-reference-resolve))
(select-module sxmml.backend.fmp7)

(define (%part-name part)
  (let1 name (and (symbol? part) (symbol->string part))
    (unless (and name (#/^[A-Z][0-9]?$/ name))
      (errorf <sxmml-value-error> "invalid FMP7 part: ~s (expected A-Z or A0-Z9)" part))
    (if (= (string-length name) 1) (string-append name "0") name)))

(define (%part-index->name index)
  (format #f "~a~a" (integer->char (+ (char->integer #\A) (quotient index 10)))
          (remainder index 10)))

(define (sxmml-fmp7-tracks-prepare env)
  (define (collect name track tracks)
    (let1 kind (slot-ref track 'sound-source)
      (cond [(memq kind '(opn opm ssg pcm)) (cons track tracks)]
            [(not kind)
             (errorf <sxmml-value-error> "invalid FMP7 track ~a sound-source: ~s" name kind)]
            [else
             (format (current-error-port) "warning: fmp7-source: ignoring track ~a (unsupported sound-source: ~s)~%" name kind)
             tracks])))
  (let* ([tracks (hash-table-fold (slot-ref env 'track) collect '())]
         [occupied (make-hash-table string-comparator)]
         [counts (make-hash-table eq-comparator)])
    (define (entry track part)
      (list track part (slot-ref track 'sound-source)))
    (when (> (length tracks) 64)
      (errorf <sxmml-value-error> "too many FMP7 tracks: ~a (max 64)" (length tracks)))
    (let1 pending
        (filter (^[track]
                  (let ([kind (slot-ref track 'sound-source)] [part (slot-ref track 'part)])
                    (when (> (hash-table-update! counts kind (cut + 1 <>) 0) 32)
                      (errorf <sxmml-value-error> "too many FMP7 ~a tracks (max 32)" kind))
                    (when part
                      (let1 native (%part-name part)
                        (when (hash-table-exists? occupied native)
                          (errorf <sxmml-value-error> "duplicate FMP7 part: ~a" native))
                        (hash-table-put! occupied native (entry track native))))
                    (not part)))
                tracks)
      (let allocate ([rest (sort-by pending (^[t] (symbol->string (slot-ref t 'name))))]
                     [index 0])
        (cond [(null? rest) (sort-by (hash-table-values occupied) cadr)]
              [else
               (let1 name (%part-index->name index)
                 (if (hash-table-exists? occupied name)
                   (allocate rest (+ index 1))
                   (begin (hash-table-put! occupied name (entry (car rest) name))
                          (allocate (cdr rest) (+ index 1)))))])))))

;; 終端を消費せずに返し、呼び出し側の文脈に従って反復を組み立てる。
(define (%repeat-sequence ir)
  (define (sequence xs nested?)
    (let loop ([rest xs] [acc '()] [loop-seen? #f])
      (cond [(null? rest) (values (reverse! acc) rest)]
            [(is-a? (car rest) <sxmml-mml-repeat-start>)
             (receive (node tail) (block (car rest) (cdr rest))
               (loop tail (cons node acc) loop-seen?))]
            [(or (is-a? (car rest) <sxmml-mml-repeat-break>)
                 (is-a? (car rest) <sxmml-mml-repeat-end>))
             (values (reverse! acc) rest)]
            [(is-a? (car rest) <sxmml-mml-loop>)
             (when (or nested? loop-seen?)
               (error <sxmml-value-error> "invalid FMP7 loop position"))
             (loop (cdr rest) (cons (car rest) acc) #t)]
            [else (loop (cdr rest) (cons (car rest) acc) loop-seen?)])))
  (define (block start body)
    (receive (front stop) (sequence body #t)
      (let1 escape (and (pair? stop) (is-a? (car stop) <sxmml-mml-repeat-break>) (car stop))
        (receive (back tail) (if escape (sequence (cdr stop) #t) (values '() stop))
          (unless (and (pair? tail) (is-a? (car tail) <sxmml-mml-repeat-end>))
            (error <sxmml-value-error> "missing FMP7 repeat end"))
          (let1 n (slot-ref (car tail) 'value)
            (unless (and (exact-integer? n) (<= 1 n 255))
              (errorf <sxmml-value-error> "invalid repeat count: ~s (expected an integer from 1 to 255)" n)))
          (values (list 'repeat start front escape back (car tail)) (cdr tail))))))
  (receive (tree rest) (sequence ir #f)
    (unless (null? rest) (error <sxmml-value-error> "unexpected FMP7 repeat delimiter"))
    tree))

(define (%canonical-lfo obj)
  (let1 ch (slot-ref obj 'channel)
    (cond [(not ch)
           (rlet1 result (allocate-instance (class-of obj) ())
             (for-each (^[definition]
                         (let1 slot (slot-definition-name definition)
                           (when (and (eq? (slot-definition-allocation definition) :instance)
                                      (slot-bound? obj slot))
                             (slot-set! result slot (slot-ref obj slot)))))
                       (class-slots (class-of obj)))
             (slot-set! result 'channel 'A))]
          [(memq ch '(A B C D)) obj]
          [else (errorf <sxmml-value-error> "invalid LFO channel: ~s (expected A, B, C or D)" ch)])))

(define (sxmml-fmp7-ir-optimize ir level)
  (%repeat-sequence ir)
  (let1 normalized (if (any (cut is-a? <> <sxmml-mml-lfo>) ir)
                     (map (^[obj] (if (is-a? obj <sxmml-mml-lfo>) (%canonical-lfo obj) obj)) ir)
                     ir)
    (if (< level 3) normalized
        (sxmml-ir-compress-repeats normalized :expand-small (> level 3)))))

;; 効果は各チャネルの最後のスイッチ値。0は変更なし、1は無効、2は有効。
;; 到達状態では両方の可能性を3で表し、到達しない区間を#fとする。
(define %channels '(A B C D))
(define %keep '(0 0 0 0))
(define (%after effect state)
  (and state (map (^[new old] (if (zero? new) old new)) effect state)))
(define (%join a b)
  (cond [(not a) b] [(not b) a] [else (map logior a b)]))

;; 区間は効果と、入口状態から注釈付きの列を求める手続きの対。
(define (%lfo-region nodes)
  (define (sequence regions)
    (cons (fold (^[r s] (%after (car r) s)) %keep regions)
          (^[entry]
            (let loop ([rest regions] [state entry] [rev '()])
              (if (null? rest) (reverse! rev)
                  (let1 r (car rest)
                    (loop (cdr rest) (%after (car r) state)
                          (cons ((cdr r) state) rev))))))))
  (define (repeat-region start front escape back end)
    (let* ([f (%lfo-region front)] [b (%lfo-region back)]
           [cycle (%after (car b) (car f))] [n (slot-ref end 'value)])
      (cons (if (= n 1) (car f) (%after (car f) cycle))
            (^[s]
              (let* ([later (%after cycle s)]
                     [front-in (if (= n 1) s (%join s later))]
                     [back-in (case n [(1) #f] [(2) (%after (car f) s)]
                                    [else (%after (car f) front-in)])])
                (list 'repeat start ((cdr f) front-in) escape ((cdr b) back-in) end))))))
  (define (switch-effect obj)
    (map (^[ch] (if (eq? ch (slot-ref obj 'channel))
                  (if (slot-ref obj 'value) 2 1) 0)) %channels))
  (define (setting obj switch)
    (let1 index (list-index (cut eq? <> (slot-ref obj 'channel)) %channels)
      (cons (if switch (switch-effect switch) %keep)
            (^[s]
              (let1 state (and s (list-ref s index))
                (when (and (eqv? state 3) (not switch))
                  (errorf <sxmml-value-error> "ambiguous LFO switch state: ~a" (slot-ref obj 'channel)))
                (list 'lfo-set obj (eqv? state 2) switch))))))
  (let loop ([xs nodes] [acc '()])
    (match xs
      [() (sequence (reverse! acc))]
      [(('repeat start f escape b end) . rest)
       (loop rest (cons (repeat-region start f escape b end) acc))]
      [(obj . rest)
       (cond [(is-a? obj <sxmml-mml-lfo-switch>)
              (loop rest (cons (cons (switch-effect obj) (^_ obj)) acc))]
             [(is-a? obj <sxmml-mml-lfo>)
              (let1 paired (and (pair? rest) (is-a? (car rest) <sxmml-mml-lfo-switch>)
                                (eq? (slot-ref obj 'channel) (slot-ref (car rest) 'channel))
                                (car rest))
                (loop (if paired (cdr rest) rest) (cons (setting obj paired) acc)))]
             [else (loop rest (cons (cons %keep (^_ obj)) acc))])])))

(define (sxmml-fmp7-lfo-output-prepare ir)
  (let1 nodes (%repeat-sequence ir)
    (receive (prefix tail) (break (cut is-a? <> <sxmml-mml-loop>) nodes)
      (let* ([first (%lfo-region prefix)] [initial '(2 2 2 2)]
             [head ((cdr first) initial)])
        (if (null? tail) head
            (let* ([cycle (%lfo-region tail)] [entry (%after (car first) initial)])
              (append! head ((cdr cycle) (%join entry (%after (car cycle) entry))))))))))

(define (%bounded label value low high)
  (unless (and (exact-integer? value) (<= low value high))
    (errorf <sxmml-value-error> "invalid ~a: ~s (expected an integer from ~a to ~a)" label value low high))
  value)

(define (sxmml-fmp7-lfo-values lfo)
  (let* ([type (slot-ref lfo 'type)]
         [wave (assq type '((default . 0) (triangle . 0) (saw . 1) (square . 2) (one-shot . 3) (random . 4)))]
         [data (slot-ref lfo 'value)])
    (unless wave (errorf <sxmml-value-error> "unsupported LFO type: ~s" type))
    (unless (and (list? data) (<= 4 (length data) 6))
      (errorf <sxmml-value-error> "invalid LFO values: ~s (expected 4 to 6 values)" data))
    (match-let1 (delay speed rate depth . options) data
      (list (%bounded "LFO delay" delay 0 128) (%bounded "LFO speed" speed 1 255)
            (%bounded "LFO rate" rate -32768 32767) (%bounded "LFO depth" depth -32768 32767)
            (cdr wave) (%bounded "LFO sync" (list-ref options 0 1) 0 1)
            (%bounded "LFO offset" (list-ref options 1 0) -32768 32767)))))

(define (sxmml-fmp7-envelope-values env)
  (let* ([type (slot-ref env 'type)] [data (slot-ref env 'value)]
         [spec (assq type '((default E 6) (extended EX 13)))])
    (unless spec (errorf <sxmml-value-error> "unsupported envelope type: ~s" type))
    (unless (and (list? data) (= (length data) (caddr spec)))
      (errorf <sxmml-value-error> "invalid envelope values: ~s (expected ~a values)" data (caddr spec)))
    (for-each (cut %bounded "envelope value" <> 0 255) data)
    (list (cadr spec) data)))

(define (sxmml-fmp7-pcm-reference-resolve env voice)
  (define (indexed index)
    (let* ([files (hash-table-get (slot-ref env 'metadata) 'pcmfile '())]
           [names (if (string? files) (list files) files)])
      (unless (and (list? names) (every string? names) (exact-integer? index) (<= 0 index) (< index (length names)))
        (errorf <sxmml-value-error> "invalid PCM file index: ~s for pcmfile ~s" index files))
      (list-ref names index)))
  (define (reference file sample)
    (unless (and (string? file) (not (#/[\/\\"\r\n]/ file)))
      (errorf <sxmml-value-error> "invalid PCM filename: ~s" file))
    (list file (%bounded "PCM sample" sample 0 65535)))
  (match (slot-ref voice 'value)
    [((? string? file) sample) (reference file sample)]
    [(index sample) (reference (indexed index) sample)]
    [(? exact-integer? sample) (reference (indexed 0) sample)]
    [other (errorf <sxmml-value-error> "invalid PCM definition: ~s" other)]))
