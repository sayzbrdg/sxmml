(define-module sxmml.backend.nsdlib
  (use srfi.1)
  (use util.match)
  (use sxmml.classes)
  (export sxmml-nsdlib-tracks-prepare sxmml-nsdlib-sequence-prepare
          sxmml-nsdlib-dpcm-key))
(select-module sxmml.backend.nsdlib)

(define %channels
  '((pulse 1 2) (triangle 3) (noise 4) (dpcm 5)
    (fds 6) (vrc6-pulse 6 7) (vrc6-saw 8)))

(define (sxmml-nsdlib-tracks-prepare env)
  (define (collect name track groups)
    (let1 kind (slot-ref track 'sound-source)
      (cond [(null? (slot-ref track 'ir)) groups]
            [(assq kind %channels) (acons kind (cons track (assq-ref groups kind '())) groups)]
            [(not kind) (errorf <sxmml-value-error> "missing nsdlib sound-source for track ~a" name)]
            [else (format (current-error-port)
                          "warning: nsdlib: ignoring track ~a (unsupported sound-source: ~s)~%" name kind)
                  groups])))
  (let* ([groups (hash-table-fold (slot-ref env 'track) collect '())]
         [fds? (assq 'fds groups)]
         [vrc6? (or (assq 'vrc6-pulse groups) (assq 'vrc6-saw groups))])
    (when (and fds? vrc6?)
      (error <sxmml-value-error> "nsdlib cannot combine FDS and VRC6 tracks"))
    (values
     (cond [fds? "nsd_fds.bin"] [vrc6? "nsd_vrc6.bin"] [else #f])
     (append-map
      (match-lambda
        [(kind . slots)
         (let1 tracks (sort-by (assq-ref groups kind '()) (^[t] (symbol->string (slot-ref t 'name))))
           (when (> (length tracks) (length slots))
             (errorf <sxmml-value-error> "too many nsdlib ~a tracks (max ~d)" kind (length slots)))
           (map (^[track tr] (list tr kind track)) tracks slots))])
      %channels))))

(define (sxmml-nsdlib-dpcm-key key)
  (let1 m (and (string? key) (#/^o(\d+)([a-g])([-+]*)$/ key))
    (unless m (errorf <sxmml-value-error> "invalid DPCM note key: ~s" key))
    (let1 n (+ (* 12 (- (string->number (m 1)) 1))
               (vector-ref '#(9 11 0 2 4 5 7) (- (char->integer (string-ref (m 2) 0)) 97))
               (fold (^[c sum] (+ sum (if (char=? c #\+) 1 -1))) 0 (string->list (m 3))))
      (unless (<= 0 n 95) (errorf <sxmml-value-error> "DPCM note out of range: ~s" key))
      n)))

;; 反復を一度だけ構造化し、一回反復でも省略前に区切りと L の位置を検査する。
(define (%sequence-tree ir)
  (define (sequence xs nested?)
    (let loop ([rest xs] [result '()] [seen? #f])
      (if (null? rest) (values (reverse! result) '())
          (let1 obj (car rest)
            (cond [(is-a? obj <sxmml-mml-repeat-start>)
                   (receive (node tail) (block (cdr rest))
                     (loop tail (cons node result) seen?))]
                  [(or (is-a? obj <sxmml-mml-repeat-end>) (is-a? obj <sxmml-mml-repeat-break>))
                   (values (reverse! result) rest)]
                  [(is-a? obj <sxmml-mml-loop>)
                   (when (or seen? nested?) (error <sxmml-value-error> "invalid nsdlib loop position"))
                   (loop (cdr rest) (cons obj result) #t)]
                  [else (loop (cdr rest) (cons obj result) seen?)])))))
  (define (block xs)
    (receive (front stop) (sequence xs #t)
      (let1 escape? (and (pair? stop) (is-a? (car stop) <sxmml-mml-repeat-break>))
        (receive (back end) (if escape? (sequence (cdr stop) #t) (values '() stop))
          (unless (and (pair? end) (is-a? (car end) <sxmml-mml-repeat-end>))
            (error <sxmml-value-error> "missing nsdlib repeat end"))
          (let1 count (slot-ref (car end) 'value)
            (unless (and (exact-integer? count) (<= 1 count 255))
              (errorf <sxmml-value-error> "invalid nsdlib repeat count: ~s (expected 1 to 255)" count))
            (values (list 'repeat count front escape? back) (cdr end)))))))
  (receive (nodes rest) (sequence ir #f)
    (unless (null? rest) (error <sxmml-value-error> "unexpected nsdlib repeat delimiter"))
    nodes))

;; 区間の効果は定義・スイッチへの定数代入。恒等写像と定数代入の合成は
;; 冪等なので、二周目以降の入口は初回の出口一つで表せる。
(define (%lfo-region nodes)
  (define (join xs ys) (lset-union equal? xs ys))
  (define (actual state) (and (cdr state) (car state)))
  (define (sequence regions)
    (cons (^[s] (fold (^[r state] ((car r) state)) s regions))
          (^[states]
            (let loop ([rs regions] [in states] [acc '()])
              (if (null? rs) (reverse! acc)
                  (let1 region (car rs)
                    (loop (cdr rs) (delete-duplicates (map (car region) in))
                          (cons ((cdr region) in) acc))))))))
  (define (repeat-region n front escape? back)
    (let* ([f (%lfo-region front)] [b (%lfo-region back)]
           [cycle (^[s] ((car b) ((car f) s)))])
      (cons (^[s] ((car f) (if (= n 1) s (cycle s))))
            (^[in]
              (let* ([later (map cycle in)]
                     [front-in (if (= n 1) in (join in later))]
                     [back-in (map (car f) (if (= n 2) in front-in))])
                (list 'repeat n ((cdr f) front-in) escape?
                      (if (= n 1) '() ((cdr b) back-in))))))))
  (define (lfo-region setting switch)
    (define (step s)
      (cons (if setting (slot-ref setting 'value) (car s))
            (if switch (slot-ref switch 'value) (cdr s))))
    (for-each (^[obj]
                (when (and obj (not (memq (slot-ref obj 'channel) '(#f A))))
                  (errorf <sxmml-value-error> "unsupported nsdlib LFO channel: ~s" (slot-ref obj 'channel))))
              (list setting switch))
    (cons step
          (^[in]
            (let* ([targets (map (^[s] (actual (step s))) in)]
                   [first (car targets)]
                   [action
                    (cond [(and (every (cut equal? first <>) targets)
                                (or first switch)) first]
                          [(every (^[s t] (equal? (actual s) t)) in targets) #t]
                          [else (error <sxmml-value-error>
                                       (if switch "ambiguous nsdlib LFO resume definition"
                                           "ambiguous nsdlib LFO switch state"))])])
              (list 'lfo (and setting (slot-ref setting 'value)) action)))))
  (let loop ([xs nodes] [rev '()])
    (match xs
      [() (sequence (reverse! rev))]
      [(('repeat n f escape? b) . tail)
       (loop tail (cons (repeat-region n f escape? b) rev))]
      [(obj . tail)
       (cond [(is-a? obj <sxmml-mml-lfo-switch>)
              (loop tail (cons (lfo-region #f obj) rev))]
             [(is-a? obj <sxmml-mml-lfo>)
              (let1 paired (and (pair? tail) (is-a? (car tail) <sxmml-mml-lfo-switch>) (car tail))
                (loop (if paired (cdr tail) tail) (cons (lfo-region obj paired) rev)))]
             [else (loop tail (cons (cons identity (^_ obj)) rev))])])))

(define (sxmml-nsdlib-sequence-prepare ir)
  (let1 nodes (%sequence-tree ir)
    (if (not (any (cut is-a? <> <sxmml-mml-lfo>) ir)) nodes
        (receive (prefix tail) (break (cut is-a? <> <sxmml-mml-loop>) nodes)
          (let* ([head (%lfo-region prefix)] [initial (cons #f #t)]
                 [prepared ((cdr head) (list initial))])
            (if (null? tail) prepared
                (let* ([cycle (%lfo-region tail)] [entry ((car head) initial)])
                  (append! prepared ((cdr cycle) (delete-duplicates (list entry ((car cycle) entry))))))))))))
