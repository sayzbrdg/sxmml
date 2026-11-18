(define-module sxmml.backend.fmp7-source
  (use file.util)
  (use gauche.charconv)
  (use srfi.1)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.voice)
  (use sxmml.optimizer)
  (use sxmml.backend.fmp7)
  (export sxmml-fmp7-source-output))
(select-module sxmml.backend.fmp7-source)

(define (%integer kind value low high)
  (unless (and (exact-integer? value) (<= low value high))
    (errorf <sxmml-value-error> "invalid ~a: ~s (expected an integer from ~a to ~a)" kind value low high))
  value)

(define (%csv values) (string-join (map number->string values) ","))

(define (%pitch n)
  (%integer "pitch" n 12 107)
  (format #f "o~a~a" (quotient n 12)
          (vector-ref '#("c" "c+" "d" "d+" "e" "f" "f+" "g" "g+" "a" "a+" "b") (modulo n 12))))

(define (%channel obj)
  (case (slot-ref obj 'channel) [(A) "P"] [(B) "Q"] [(C) "R"] [(D) "S"]))

(define (%lookup table id label)
  (or (and table (hash-table-get table id #f))
      (errorf <sxmml-reference-error> "undefined ~a: ~a" label id)))

(define (%fm-definition kind number data)
  (match data
    [(alfb op1 op2 op3 op4)
     (define (operator op)
       (match op
         [(ar dr sr rr sl tl ks ml dt dt2 seg am)
          (let1 values (list ar dr sr rr sl tl ks ml dt)
            (for-each (^[v hi] (%integer "FM operator" v 0 hi)) values '(31 31 31 15 15 127 3 15 7))
            (format #t "'@ ~a\r\n" (%csv (append values
                  (if (eq? kind 'opm) (list (%integer "DT2" dt2 0 3)) '())
                  (list (%integer "AM" am 0 1))))))]
         [_ (errorf <sxmml-value-error> "invalid FM operator: ~s" op)]))
     (format #t "'@ ~a ~d\r\n" (if (eq? kind 'opn) "FA" "FC") number)
     (for-each operator (list op1 op2 op3 op4))
     (match alfb
       [(al fb) (format #t "'@ ~d,~d\r\n" (%integer "AL" al 0 7) (%integer "FB" fb 0 7))]
       [_ (errorf <sxmml-value-error> "invalid AL/FB: ~s" alfb)])]
    [_ (errorf <sxmml-value-error> "invalid FM definition: ~s" data)]))

(define (%definition-output env kind id number)
  (if (eq? kind 'envelope)
    (match-let1 (tag data) (sxmml-fmp7-envelope-values (%lookup (slot-ref env 'envelope) id "envelope"))
      (format #t "'@ ~a ~d,~a\r\n" tag number (%csv data)))
    (let1 voice (%lookup (sxmml-voice-table-find env kind) id kind)
      (if (eq? kind 'pcm)
        (match-let1 (file sample) (sxmml-fmp7-pcm-reference-resolve env voice)
          (format #t "'@ P ~d,\"~a\",i~d\r\n" number file sample))
        (%fm-definition kind number (slot-ref voice 'value))))))

(define %ssg-presets
  '((square . "0,0") (triangle1 . "1,1") (triangle2 . "1,2")
    (pulse1 . "2,1") (pulse2 . "2,2") (pulse3 . "2,3") (pulse4 . "2,4")
    (pulse5 . "2,5") (pulse6 . "2,6") (pulse7 . "2,7") (pulse8 . "2,8")
    (pulse9 . "2,9") (sawtooth1 . "3,1") (sawtooth2 . "3,2")))

(define (%with-track-context track thunk)
  (guard (e [(sxmml-error? e)
             (raise (make-compound-condition e (make-condition <sxmml-track-context> 'track-name (slot-ref track 'name))))])
    (thunk)))

(define (%track-output env ir part kind number)
  (define (ticks obj) (%integer "duration" (slot-ref obj 'ticks) 1 65535))
  (define (v obj) (slot-ref obj 'value))
  (define (command text obj low high)
    (format #t "~a~d" text (%integer text (v obj) low high)))
  (define (relative prefix obj max)
    (let1 n (%integer prefix (v obj) (- max) max)
      (unless (zero? n) (format #t "~a~a~d" prefix (if (positive? n) ")" "(") (abs n)))))
  (define (switch obj)
    (format #t "S~a~d" (%channel obj) (if (v obj) 1 0)))
  (define (emit obj)
    (cond
     [(pair? obj)
      (match obj
        [('repeat _ front escape back end)
         (display "[") (for-each emit front)
         (when escape (display "/") (for-each emit back))
         (format #t "]~d" (v end))]
        [('lfo-set setting enabled paired)
         (let1 data (sxmml-fmp7-lfo-values (%lookup (slot-ref env 'lfo) (v setting) "LFO"))
           (format #t "M~aV~a" (%channel setting) (%csv data)))
         (cond [paired (switch paired)] [(not enabled) (format #t "S~a0" (%channel setting))])])]
     [(is-a? obj <sxmml-mml-lfo-switch>) (switch obj)]
     [(is-a? obj <sxmml-mml-rest>) (format #t "r#~d" (ticks obj))]
     [(is-a? obj <sxmml-mml-portamento>)
      (match (v obj)
        [(a . b) (format #t "~a#~d_~a" (%pitch a) (ticks obj) (%pitch b))]
        [_ (errorf <sxmml-value-error> "invalid portamento: ~s" (v obj))])]
     [(is-a? obj <sxmml-mml-note>) (format #t "~a#~d" (%pitch (v obj)) (ticks obj))]
     [(is-a? obj <sxmml-mml-volume>) (command "v" obj 0 127)]
     [(is-a? obj <sxmml-mml-volume-change>) (relative "" obj 127)]
     [(or (is-a? obj <sxmml-mml-tempo>) (is-a? obj <sxmml-mml-tempo-absolute>)) (command "T" obj 6 400)]
     [(is-a? obj <sxmml-mml-transpose>) (command "K" obj -96 96)]
     [(is-a? obj <sxmml-mml-transpose-change>) (relative "K" obj 96)]
     [(is-a? obj <sxmml-mml-pan>) (command "P" obj 1 255)]
     [(is-a? obj <sxmml-mml-detune>) (command "D" obj -6144 6144)]
     [(is-a? obj <sxmml-mml-gate-time>) (command "q" obj 0 65535)]
     [(is-a? obj <sxmml-mml-loop>) (display "L")]
     [(is-a? obj <sxmml-mml-tie>) (display "&")]
     [(is-a? obj <sxmml-mml-voice>)
      (format #t "@~a" (if (eq? kind 'ssg)
                         (or (assoc-ref %ssg-presets (v obj))
                             (errorf <sxmml-reference-error> "undefined SSG preset: ~s" (v obj)))
                         (number kind (v obj))))]
     [(and (memq kind '(ssg pcm)) (is-a? obj <sxmml-mml-envelope>))
      (format #t "E~d" (number 'envelope (v obj)))]
     [(and (eq? kind 'ssg) (is-a? obj <sxmml-mml-output-switch>))
      (display (vector-ref '#("" "m1,0" "m0,1" "m1,1") (%integer "output switch" (v obj) 1 3)))]
     [(and (eq? kind 'ssg) (is-a? obj <sxmml-mml-noise-frequency>)) (command "w" obj 0 31)]
     [else (errorf <sxmml-value-error> "unsupported FMP7 ~a command: ~s" kind obj)]))
  (format #t "'~a " part)
  (for-each emit (sxmml-fmp7-lfo-output-prepare ir))
  (display "\r\n"))

(define (sxmml-fmp7-source-output env)
  (let ([path (slot-ref env 'output)] [tracks (sxmml-fmp7-tracks-prepare env)]
        [numbers (make-hash-table equal-comparator)] [counts (make-hash-table eq-comparator)]
        [defs (open-output-string)])
    (define (number kind id)
      (let ([key (cons kind id)] [bank (if (memq kind '(opn opm)) 'fm kind)])
        (or (hash-table-get numbers key #f)
            (let1 n (hash-table-get counts bank 0)
              (when (> n 255) (errorf <sxmml-value-error> "too many ~a definitions (max 256)" bank))
              (with-output-to-port defs (cut %definition-output env kind id n))
              (hash-table-put! counts bank (+ n 1))
              (hash-table-put! numbers key n)
              n))))
    (define level (slot-ref env 'optimize))
    (define jobs (slot-ref env 'jobs))
    (define (prepare row)
      (let1 track (car row)
        (%with-track-context track (cut sxmml-fmp7-ir-prepare (slot-ref track 'ir)))))
    (define (emit row ir)
      (match-let1 (track part kind) row
        (%with-track-context track (cut %track-output env ir part kind number))))
    (define (emit-tracks)
      (if (and (>= level 3) (> jobs 1)
               (> (count (^[row] (pair? (slot-ref (car row) 'ir))) tracks) 1))
        (for-each emit tracks
                  (sxmml-ir-compress-repeats-batch (map prepare tracks)
                                                   :jobs jobs :expand-small (> level 3)))
        (for-each (match-lambda
                    [(track part kind)
                     (%with-track-context
                      track (^[] (%track-output env (sxmml-fmp7-ir-optimize (slot-ref track 'ir) level)
                                                part kind number)))])
                  tracks)))
    (unless path (error <sxmml-value-error> "output file is not specified"))
    (let1 text (with-output-to-string emit-tracks)
      (call-with-temporary-file
       (^[port name]
         (with-output-conversion port
           (^[]
             (format #t "'{ ClockCount=~d VolumeExtend=1 }\r\n"
                     (* 4 (%integer "resolution" (slot-ref env 'resolution) 2 96)))
             (for-each (^[kind attr]
                         (let1 parts (filter-map (match-lambda [(_ p k) (and (eq? k kind) p)]) tracks)
                           (unless (null? parts) (format #t "'{ Part~a=~a }\r\n" attr (string-join parts "")))))
                       '(opn opm ssg pcm) '(OPNA OPM SSG PCM))
             (for-each (^[key field]
                         (and-let* ([value (hash-table-get (slot-ref env 'metadata) key #f)])
                           (let1 s (if (and (eq? key 'memo) (list? value) (every string? value))
                                     (string-join value " / ") value)
                             (unless (and (string? s) (not (#/[\r\n]/ s)))
                               (errorf <sxmml-value-error> "invalid FMP7 metadata ~a: ~s" key value))
                             (format #t "'{ ~a=~a\r\n}\r\n" field s))))
                       '(title composer creator memo) '(Title Composer DataCreator Comment))
             (display (get-output-string defs)) (display text))
           :encoding "cp932")
         (close-port port)
         (sys-rename name path))
       :directory (sys-dirname path)))
    (make <sxmml-output-result> :output path
          :track-names (map (^[row] (slot-ref (car row) 'name)) tracks))))

(sxmml-mode-register!
 (make <sxmml-mode> :name 'fmp7 :setup #f :backend sxmml-fmp7-source-output :output-extension "mwi"))
