(define-module sxmml.backend.nsdlib-source
  (use file.util)
  (use gauche.charconv)
  (use srfi.1)
  (use srfi.13)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.voice)
  (use sxmml.backend.nsdlib)
  (export sxmml-nsdlib-source-output))
(select-module sxmml.backend.nsdlib-source)

(define (%integer label n lo hi)
  (unless (and (exact-integer? n) (<= lo n hi))
    (errorf <sxmml-value-error> "invalid ~a: ~s (expected an integer from ~a to ~a)" label n lo hi))
  n)

(define (%lookup table id kind)
  (or (and table (hash-table-get table id #f))
      (errorf <sxmml-reference-error> "undefined ~a: ~a" kind id)))

(define (%metadata-output table)
  (define (quoted value)
    (write-char #\")
    (string-for-each
     (^[c] (case c
             [(#\") (display "\\\"")] [(#\\) (display "\\\\")]
             [(#\newline) (display "\\n")] [(#\return) (display "\\r")] [(#\tab) (display "\\t")]
             [else (write-char c)]))
     value)
    (write-char #\"))
  (for-each
   (match-lambda
     [(key field)
      (when (hash-table-exists? table key)
        (let* ([v (hash-table-get table key)]
               [items (if (and (eq? key 'memo) (list? v)) v (list v))])
          (unless (every string? items) (errorf <sxmml-value-error> "invalid metadata ~a: ~s" key v))
          (for-each (^[s] (format #t "#~a " field) (quoted s) (newline)) items)))])
   '((title Title) (composer Composer) (creator Maker) (copyright Copyright) (memo Text))))

(define (%dpcm-output env)
  (define (entry id voice entries)
    (match (slot-ref voice 'value)
      [(key (? string? file) rate mode . volume)
       (unless (and (not (string-scan file #\")) (or (null? volume) (length=? volume 1)))
         (errorf <sxmml-value-error> "invalid DPCM definition ~a: ~s" id (slot-ref voice 'value)))
       (cons (list (sxmml-nsdlib-dpcm-key key) id file
                   (%integer "DPCM rate" rate 0 15) (%integer "DPCM mode" mode 0 1)
                   (and (pair? volume) (%integer "DPCM initial output" (car volume) -1 127)))
             entries)]
      [_ (errorf <sxmml-value-error> "invalid DPCM definition ~a: ~s" id (slot-ref voice 'value))]))
  (and-let* ([table (sxmml-voice-table-find env 'pcm)] [(positive? (hash-table-num-entries table))])
    (let ([offset (hash-table-get (slot-ref env 'metadata) 'offsetpcm #xc000)]
          [entries (sort-by (hash-table-fold table entry '()) car)])
      (%integer "offsetpcm" offset #xc000 #xffc0)
      (unless (zero? (modulo offset 64))
        (errorf <sxmml-value-error> "invalid offsetpcm: ~s (expected a 64-byte boundary)" offset))
      (format #t "#OffsetPCM $~a~%DPCM{~%" (string-upcase (number->string offset 16)))
      (let loop ([rest entries] [previous #f])
        (unless (null? rest)
          (match-let1 (n id file rate mode volume) (car rest)
            (when (and previous (= n (car previous)))
              (errorf <sxmml-value-error> "duplicate DPCM key ~d: ~a and ~a" n (cadr previous) id))
            (format #t "n~d,\"~a\",~d,~d" n file rate mode)
            (when volume (format #t ",~d" volume))
            (newline)
            (loop (cdr rest) (car rest)))))
      (display "}\n"))))

(define (%pattern env kind id sound)
  (let* ([def (%lookup (slot-ref env kind) id kind)]
         [data (slot-ref def 'value)]
         [pitch? (eq? kind 'lfo)]
         [lo (if pitch? -64 0)]
         [hi (cond [pitch? 63] [(eq? sound 'triangle) 4] [(memq sound '(fds vrc6-saw)) 63] [else 15])])
    (define (value n) (%integer id n lo hi))
    (unless (eq? (slot-ref def 'type) 'default)
      (errorf <sxmml-value-error> "unsupported nsdlib ~a type: ~s" kind (slot-ref def 'type)))
    (unless (pair? data) (errorf <sxmml-value-error> "empty nsdlib ~a: ~a" kind id))
    (string-join
     (map (match-lambda
            ['l "L"] ['r "R"] [('d n) (format #f "D~d" (%integer "pattern hold" n 0 255))]
            [(a b n)
             (format #f "(~d,~d,~d)" (value a) (value b) (%integer "interpolation length" n 1 255))]
            [n (number->string (value n))])
          data) " ")))

(define (%numbering env definitions)
  (let ([numbers (make-hash-table equal-comparator)] [checked (make-hash-table equal-comparator)]
        [waves 0] [patterns 0])
    (^[kind id sound]
      (let* ([key (cons kind id)] [old (hash-table-get numbers key #f)]
             [usage (list kind id sound)])
        (unless (hash-table-exists? checked usage)
          (if (eq? kind 'waveform)
              (let1 data (slot-ref (%lookup (sxmml-voice-table-find env kind) id kind) 'value)
                (unless (and (list? data) (= (length data) 64))
                  (errorf <sxmml-value-error> "invalid waveform ~a: expected 64 samples" id))
                (format definitions "FDSC(~d){~a}~%" waves
                        (string-join (map (^[v] (number->string (%integer id v 0 63))) data) ","))
                (hash-table-put! numbers key waves)
                (inc! waves))
              (let1 pattern (%pattern env kind id sound)
                (unless old
                  (format definitions "Envelop(~d){~a}~%" patterns pattern)
                  (hash-table-put! numbers key patterns)
                  (inc! patterns))))
          (hash-table-put! checked usage #t))
        (hash-table-get numbers key)))))

(define %names '#("c" "c+" "d" "d+" "e" "f" "f+" "g" "g+" "a" "a+" "b"))

(define (%track-output env tr sound track number)
  (define (pitch n)
    (%integer "pitch" (+ n (case sound [(triangle) 12] [(fds) 24] [else 0])) 12 107))
  (define (name n) (vector-ref %names (modulo n 12)))
  (define (duration obj)
    (let1 n (slot-ref obj 'ticks)
      (unless (and (exact-integer? n) (positive? n))
        (errorf <sxmml-value-error> "invalid duration: ~s (expected positive ticks)" n))
      n))
  (define (segments first more sep ticks)
    (let loop ([label first] [n ticks])
      (let1 part (+ (modulo (- n 1) 255) 1)
        (format #t "~a%~d" label part)
        (when (> n part) (display sep) (loop more (- n part))))))
  (define (emit obj)
    (cond
     [(pair? obj)
      (match obj
        [('repeat 1 front _ _) (sequence front)]
        [('repeat n front escape? back)
         (display "[") (sequence front)
         (when escape? (display " :") (sequence back))
         (format #t " ]~d" n)]
        [('lfo setting action)
         (when setting (number 'lfo setting sound))
         (cond [(eq? action #t) #f] [(not action) (display "Em*")]
               [else (format #t "Em~d" (number 'lfo action sound))])])]
     [(is-a? obj <sxmml-mml-loop>) (display "L")]
     [(is-a? obj <sxmml-mml-tie>) (display "&")]
     [else
      (let1 v (slot-ref obj 'value)
        (cond
         [(is-a? obj <sxmml-mml-rest>)
          (let1 rest (if (eq? sound 'dpcm) "r-" "r")
            (segments rest rest " " (duration obj)))]
         [(is-a? obj <sxmml-mml-portamento>)
          (match v
            [(a . b)
             (let* ([start (pitch a)] [end (pitch b)] [oct (quotient start 12)]
                    [difference (- (quotient end 12) oct)])
               (format #t "o~d{~a~a~a}%~d" oct (name start)
                       (make-string (abs difference) (if (positive? difference) #\> #\<))
                       (name end) (%integer "portamento duration" (duration obj) 1 255)))]
            [_ (errorf <sxmml-value-error> "invalid portamento: ~s" v)])]
         [(is-a? obj <sxmml-mml-note>)
          (let1 n (pitch v)
            (segments (format #f "o~d~a" (quotient n 12) (name n)) (name n) "&" (duration obj)))]
         [(is-a? obj <sxmml-mml-voice>)
          (if (eq? sound 'fds) (format #t "@FC~d" (number 'waveform v sound))
              (let1 hi (case sound [(pulse) 3] [(noise) 1] [(vrc6-pulse) 7] [else #f])
                (unless hi (errorf <sxmml-value-error> "unsupported nsdlib ~a voice: ~s" sound v))
                (format #t "@~d" (%integer "voice" (and (symbol? v) (string->number (symbol->string v))) 0 hi))))]
         [(is-a? obj <sxmml-mml-envelope>)
          (when (eq? sound 'dpcm) (error <sxmml-value-error> "unsupported nsdlib dpcm envelope"))
          (format #t "Ev~d" (number 'envelope v sound))]
         [(or (is-a? obj <sxmml-mml-volume>) (is-a? obj <sxmml-mml-volume-change>))
          (when (memq sound '(triangle dpcm))
            (errorf <sxmml-value-error> "unsupported nsdlib ~a volume: ~s" sound v))
          (if (is-a? obj <sxmml-mml-volume>) (format #t "v~d" (%integer "volume" v 0 15))
              (begin (unless (exact-integer? v) (errorf <sxmml-value-error> "invalid relative volume: ~s" v))
                     (unless (zero? v) (format #t "~a~d" (if (positive? v) ")" "(") (abs v)))))]
         [(or (is-a? obj <sxmml-mml-tempo>) (is-a? obj <sxmml-mml-tempo-absolute>))
          (unless (and (exact-integer? v) (positive? v)
                       (<= 1 (quotient (* v (slot-ref env 'resolution)) 24) 255))
            (errorf <sxmml-value-error> "tempo out of range: ~s (timebase ~d)" v (slot-ref env 'resolution)))
          (format #t "t~d" v)]
         [(is-a? obj <sxmml-mml-transpose>) (format #t "_~d" (%integer "transpose" v -128 127))]
         [(is-a? obj <sxmml-mml-transpose-change>) (format #t "__~d" (%integer "relative transpose" v -128 127))]
         [(is-a? obj <sxmml-mml-detune>) (format #t "D~d" (%integer "detune" v -128 127))]
         [(is-a? obj <sxmml-mml-gate-time>) (format #t "q~d" (%integer "gate" v 0 255))]
         [else (errorf <sxmml-value-error> "unsupported nsdlib ~a command: ~a ~s"
                       sound (class-name (class-of obj)) v)]))]))
  (define (sequence nodes)
    (for-each
     (^[node]
       (match node
         [('repeat 1 front _ _) (sequence front)]
         [('lfo setting #t) (when setting (number 'lfo setting sound))]
         [(? (cut is-a? <> <sxmml-mml-nop>)) #f]
         [_ (write-char #\space) (emit node)])) nodes))
  (guard (e [(sxmml-error? e)
             (raise (make-compound-condition e (make-condition <sxmml-track-context> 'track-name (slot-ref track 'name))))])
    (format #t "TR~d" tr)
    (sequence (sxmml-nsdlib-sequence-prepare (slot-ref track 'ir)))
    (newline)))

(define (sxmml-nsdlib-source-output env)
  (receive (driver rows) (sxmml-nsdlib-tracks-prepare env)
    (let* ([path (slot-ref env 'output)] [definitions (open-output-string)]
           [number (%numbering env definitions)])
      (unless path (error <sxmml-value-error> "output file is not specified"))
      (let1 body (with-output-to-string
                   (^[] (for-each (match-lambda [(tr sound track) (%track-output env tr sound track number)]) rows)))
        (call-with-temporary-file
         (^[port name]
           (with-output-conversion port
             (^[] (%metadata-output (slot-ref env 'metadata))
                  (when driver (format #t "#code \"~a\"~%" driver))
                  (format #t "#timebase ~d~%" (slot-ref env 'resolution))
                  (when (any (^[row] (eq? (cadr row) 'dpcm)) rows) (%dpcm-output env))
                  (display (get-output-string definitions))
                  (display "BGM(0){\n") (display body) (display "}\n"))
             :encoding "UTF-8")
           (close-port port)
           (sys-rename name path))
         :directory (sys-dirname path)))
      (make <sxmml-output-result> :output path :track-names (map (^[row] (slot-ref (caddr row) 'name)) rows)))))

(sxmml-mode-register!
 (make <sxmml-mode> :name 'nsdlib :setup #f :backend sxmml-nsdlib-source-output :output-extension "nsd"))
