;;;
;;; IRの意味比較に使う独立した参照解釈器
;;;

(define-module test.ir-meaning
  (use sxmml.classes)
  (export compare-ir-meaning))
(select-module test.ir-meaning)

(define (%repeat-ends code)
  (let* ([size (vector-length code)]
         [ends (make-vector size #f)])
    (let loop ([position 0] [starts '()])
      (if (= position size)
        ends
        (let1 class (class-of (vector-ref code position))
          (cond [(eq? class <sxmml-mml-repeat-start>)
                 (loop (+ position 1) (cons position starts))]
                [(eq? class <sxmml-mml-repeat-end>)
                 (vector-set! ends (car starts) position)
                 (loop (+ position 1) (cdr starts))]
                [else (loop (+ position 1) starts)]))))))

(define (%volume-add volume delta)
  (if (number? volume)
    (+ volume delta)
    (list 'initial-volume (+ (cadr volume) delta))))

(define (%interpret-ir objects step-limit)
  (let* ([code (list->vector objects)]
         [size (vector-length code)]
         [ends (%repeat-ends code)])
    (let loop ([position 0] [steps 0] [ticks 0] [volume '(initial-volume 0)]
               [stack '()] [notes '()] [loop? #f])
      (define (finish reason)
        (values (reverse notes) ticks volume reason loop?))
      (cond [(= position size) (finish #f)]
            [(>= steps step-limit) (finish (list 'step-limit step-limit))]
            [else
             (let* ([obj (vector-ref code position)]
                    [class (class-of obj)])
               (cond [(eq? class <sxmml-mml-note>)
                      (let1 duration (slot-ref obj 'ticks)
                        (loop (+ position 1) (+ steps 1) (+ ticks duration) volume stack
                              (cons (list ticks (slot-ref obj 'value) duration volume) notes) loop?))]
                     [(eq? class <sxmml-mml-rest>)
                      (loop (+ position 1) (+ steps 1) (+ ticks (slot-ref obj 'ticks)) volume stack notes loop?)]
                     [(eq? class <sxmml-mml-volume>)
                      (loop (+ position 1) (+ steps 1) ticks (slot-ref obj 'value) stack notes loop?)]
                     [(eq? class <sxmml-mml-volume-change>)
                      (loop (+ position 1) (+ steps 1) ticks
                            (%volume-add volume (slot-ref obj 'value)) stack notes loop?)]
                     [(eq? class <sxmml-mml-repeat-start>)
                      (let1 end (vector-ref ends position)
                        (loop (+ position 1) (+ steps 1) ticks volume
                              (cons (list (+ position 1) end (slot-ref (vector-ref code end) 'value)) stack)
                              notes loop?))]
                     [(eq? class <sxmml-mml-repeat-break>)
                      (if (= (caddar stack) 1)
                        (loop (+ (cadar stack) 1) (+ steps 1) ticks volume (cdr stack) notes loop?)
                        (loop (+ position 1) (+ steps 1) ticks volume stack notes loop?))]
                     [(eq? class <sxmml-mml-repeat-end>)
                      (let1 frame (car stack)
                        (if (> (caddr frame) 1)
                          (loop (car frame) (+ steps 1) ticks volume
                                (cons (list (car frame) (cadr frame) (- (caddr frame) 1)) (cdr stack))
                                notes loop?)
                          (loop (+ position 1) (+ steps 1) ticks volume (cdr stack) notes loop?)))]
                     [(eq? class <sxmml-mml-loop>)
                      (loop (+ position 1) (+ steps 1) ticks volume stack notes #t)]
                     [else (finish (list 'unsupported-ir (+ position 1) (class-name class)))]))]))))

(define (%note-difference before after left-stop right-stop left-ticks right-ticks)
  (let loop ([left before] [right after] [index 1])
    (cond [(and (pair? left) (pair? right))
           (let compare-fields ([fields '(start-tick pitch ticks volume)]
                                [a (car left)] [b (car right)])
             (cond [(null? fields) (loop (cdr left) (cdr right) (+ index 1))]
                   [(equal? (car a) (car b)) (compare-fields (cdr fields) (cdr a) (cdr b))]
                   [else (list 'note index (car fields) (car a) (car b))]))]
          [(and (pair? left) (or (not right-stop) (< (caar left) right-ticks)))
           (list 'note index 'presence #t #f)]
          [(and (pair? right) (or (not left-stop) (< (caar right) left-ticks)))
           (list 'note index 'presence #f #t)]
          [else #f])))

(define (compare-ir-meaning before after :key (step-limit 100000))
  (receive (left-notes left-ticks left-volume left-stop left-loop?)
      (%interpret-ir before step-limit)
    (receive (right-notes right-ticks right-volume right-stop right-loop?)
        (%interpret-ir after step-limit)
      (cond [(%note-difference left-notes right-notes left-stop right-stop left-ticks right-ticks)
             => (^[difference] (values 'mismatch difference))]
            [(and (not left-stop) (not right-stop) (not (= left-ticks right-ticks)))
             (values 'mismatch (list 'end 'ticks left-ticks right-ticks))]
            [(and (not left-stop) (not right-stop) (not (equal? left-volume right-volume)))
             (values 'mismatch (list 'end 'volume left-volume right-volume))]
            [left-stop (values 'undecided (cons (car left-stop) (cons 'left (cdr left-stop))))]
            [right-stop (values 'undecided (cons (car right-stop) (cons 'right (cdr right-stop))))]
            [(or left-loop? right-loop?) (values 'undecided '(loop))]
            [else (values 'match #f)]))))
