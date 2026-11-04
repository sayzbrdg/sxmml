;; 文脈を明示的に受け取り、設定の更新・音長の集計・IR生成を行う。
(define-module sxmml.command
  (use sxmml.classes)
  (export sxmml-command-default-length-set! sxmml-command-octave-set!
          sxmml-command-octave-shift! sxmml-command-note-transpose-set!
          sxmml-command-volume-up sxmml-command-volume-down
          sxmml-command-note! sxmml-command-rest! sxmml-command-portamento!
          sxmml-command-loop! sxmml-command-rhythm-on!
          sxmml-command-repeat-start! sxmml-command-repeat-break! sxmml-command-repeat-end!
          sxmml-command-note-pitch sxmml-command-note-length->ticks))
(select-module sxmml.command)

(define (sxmml-command-default-length-set! ctx denominator)
  (%check-note-length denominator)
  (slot-set! ctx 'default-length denominator)
  #f)

(define (sxmml-command-octave-set! ctx octave)
  (slot-set! ctx 'current-octave octave)
  #f)

(define (sxmml-command-octave-shift! ctx delta)
  (slot-set! ctx 'current-octave (+ (slot-ref ctx 'current-octave) delta))
  #f)

(define (sxmml-command-note-transpose-set! ctx semitones)
  (slot-set! ctx 'current-transpose semitones)
  #f)

(define (sxmml-command-volume-up ctx)
  (make <sxmml-mml-volume-change> :value (slot-ref ctx 'volume-delta)))

(define (sxmml-command-volume-down ctx)
  (make <sxmml-mml-volume-change> :value (- (slot-ref ctx 'volume-delta))))

(define (%record-ticks! ctx ticks)
  (let1 current (car (slot-ref ctx 'current-ticks))
    (set-car! current (+ (car current) ticks)))
  (hash-table-update! (slot-ref ctx 'ticks-statistics) ticks (cut + 1 <>) 0))

(define (sxmml-command-note! ctx pitch ticks)
  (%record-ticks! ctx ticks)
  (make <sxmml-mml-note> :value pitch :ticks ticks))

(define (sxmml-command-rest! ctx ticks)
  (%record-ticks! ctx ticks)
  (make <sxmml-mml-rest> :value #f :ticks ticks))

(define (sxmml-command-portamento! ctx start-pitch end-pitch ticks)
  (%record-ticks! ctx ticks)
  (make <sxmml-mml-portamento> :value (cons start-pitch end-pitch) :ticks ticks))

(define (sxmml-command-loop! ctx)
  (unless (null? (slot-ref ctx 'repeat-stack))
    (error <sxmml-value-error> "loop command cannot be used inside repeats"))
  (when (slot-ref ctx 'loop-exists)
    (error <sxmml-value-error> "loop command can be used only once in a track"))
  (slot-set! ctx 'loop-exists #t)
  (let1 current (car (slot-ref ctx 'current-ticks))
    (set-cdr! current (car current)))
  (make <sxmml-mml-loop>))

(define (sxmml-command-rhythm-on! ctx instruments ticks)
  (let1 args (fold (^[instrument rest]
                     (cons* (make-keyword instrument) #t rest))
                   () instruments)
    (%record-ticks! ctx ticks)
    (apply make <sxmml-rhythm-emit> :value #t :ticks ticks args)))

(define (sxmml-command-repeat-start! ctx)
  (slot-push! ctx 'repeat-stack #f)
  (slot-push! ctx 'current-ticks (cons 0 0))
  (make <sxmml-mml-repeat-start>))

(define (sxmml-command-repeat-break! ctx)
  (let1 stack (slot-ref ctx 'repeat-stack)
    (cond [(null? stack)
           (error <sxmml-value-error> "break command cannot be used outside of repeats")]
          [(car stack)
           (error <sxmml-value-error> "break command can be used only once in a repeat")])
    (let1 current (car (slot-ref ctx 'current-ticks))
      (set-cdr! current (car current)))
    (set-car! stack (list (slot-ref ctx 'default-length)
                          (slot-ref ctx 'current-octave)
                          (slot-ref ctx 'current-transpose)))
    (make <sxmml-mml-repeat-break>)))

(define (sxmml-command-repeat-end! ctx count)
  (when (null? (slot-ref ctx 'repeat-stack))
    (error <sxmml-value-error> "repeat end appeared before repeat start"))
  (unless (and (exact-integer? count) (positive? count))
    (errorf <sxmml-value-error> "invalid repeat count: ~s (expected a positive exact integer)" count))
  (let* ([settings (slot-pop! ctx 'repeat-stack)]
         [repeat-ticks (slot-pop! ctx 'current-ticks)]
         [current (car (slot-ref ctx 'current-ticks))]
         [ticks (- (* (car repeat-ticks) count)
                   (if settings (- (car repeat-ticks) (cdr repeat-ticks)) 0))])
    (set-car! current (+ (car current) ticks))
    (when settings
      (slot-set! ctx 'default-length (car settings))
      (slot-set! ctx 'current-octave (cadr settings))
      (slot-set! ctx 'current-transpose (caddr settings)))
    (make <sxmml-mml-repeat-end> :value count)))

(define (sxmml-command-note-pitch ctx pitch-class accidental)
  (+ (* (slot-ref ctx 'current-octave) 12) pitch-class accidental
     (slot-ref ctx 'current-transpose)))

(define (%check-note-length denominator)
  (when (eqv? denominator 0)
    (error <sxmml-value-error> "invalid note length: 0 (expected an integer >= 1)")))

(define (sxmml-command-note-length->ticks ctx resolution denominator dots)
  (let* ([length (or denominator (slot-ref ctx 'default-length))]
         [base (begin (%check-note-length length) (/ (* resolution 4) length))])
    (let loop ([value base]
               [extra base]
               [remaining dots])
      (cond [(zero? remaining) value]
            [(not (logbit? 0 extra))
             (let1 half (ash extra -1)
               (loop (+ value half) half (- remaining 1)))]
            [else (error <sxmml-value-error> "number of dots exceeded resolution range")]))))
