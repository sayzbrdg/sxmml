(define-module sxmml.backend.fmp7
  (use srfi.1)
  (use sxmml.classes)
  (use sxmml.optimizer)
  (export sxmml-fmp7-tracks-prepare sxmml-fmp7-ir-optimize))
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
  (let* ([tracks (hash-table-values (slot-ref env 'track))]
         [occupied (make-hash-table string-comparator)]
         [counts (make-hash-table eq-comparator)])
    (define (entry track part)
      (list track part (slot-ref track 'sound-source)))
    (when (> (length tracks) 64)
      (errorf <sxmml-value-error> "too many FMP7 tracks: ~a (max 64)" (length tracks)))
    (let1 pending
        (filter (^[track]
                  (let ([kind (slot-ref track 'sound-source)] [part (slot-ref track 'part)])
                    (unless (memq kind '(opn opm ssg pcm))
                      (errorf <sxmml-value-error> "invalid FMP7 track ~a sound-source: ~s"
                              (slot-ref track 'name) kind))
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
