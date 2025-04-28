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
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.mml)
  (export sxmml-optimize-tree
          ))
(select-module sxmml.optimizer)

;;;
;;; optimizer
;;;

(define (sxmml-optimize-tree track :key (optimize #t))
  (let1 env (hash-table-get (slot-ref (sxmml-environment) 'track) track)
    (cond [(eq? optimize :full)
           (%sxmml-optimize-2path (%sxmml-optimize-tree env))]
          [optimize
           (%sxmml-optimize-tree env)]
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

(define (%sxmml-optimize-tree mmlenv)
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
                           [(is-a? (car rest) <sxmml-mml-note>)
                            (recur (cdr rest) (append (note-store env (car rest)) objs))]
                           [(is-a? (car rest) <sxmml-mml-repeat-start>)
                            (recur (cdr rest) (append (repeat-start-store env (car rest)) objs))]
                           [(is-a? (car rest) <sxmml-mml-repeat-end>)
                            (recur (cdr rest) (append (repeat-end-store env (car rest)) objs))]
                           [(any ($ is-a? (car rest) $) (list <sxmml-mml-loop> <sxmml-mml-repeat-break>)) ; store data
                            (recur (cdr rest) (append (loop-store env (car rest)) objs))]
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
     (append (last-store env mmlenv) optobjs))))


(define (object-sum-update! env obj)
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (if (hash-table-contains? ht class)
      (hash-table-update! ht class (^o (slot-set! o 'value (+ (slot-ref o 'value) (slot-ref obj 'value))) o))
      (begin
        (hash-table-set! ht class obj)
        (tree-map-put! tm (tree-map-num-entries tm) class)))))

(define (note-store env obj)
  (let ([p (car (slot-ref env 'current-ticks))]
        [t (slot-ref obj 'ticks)])
    (set! (car p) (+ (or t 0) (car p))) ; rhythm-off doesn't have ticks data
    (set! (cdr p) (+ (or t 0) (cdr p))))
  (object-store env obj))

(define (loop-store env obj)
  (let1 p (car (slot-ref env 'current-ticks))
    (set! (cdr p) 0))
  (object-store env obj))

(define (repeat-start-store env obj)
  (let1 p (slot-ref env 'current-ticks)
    (slot-set! env 'current-ticks (acons 0 0 p)))
  (object-store env obj))

(define (repeat-end-store env obj)
  (let* ([p (slot-ref env 'current-ticks)]
         [rc (slot-ref obj 'value)]
         [cp (cadr p)]
         [c1 (caar p)]
         [c2 (cdar p)]
         [cs (- (* c1 rc) (if (= c1 c2) 0 c2))])
    (set! (caadr p) (+ (caadr p) cs))
    (set! (cdadr p) (+ (cdadr p) cs))
    (slot-set! env 'current-ticks (cdr p)))
  (object-store env obj))

(define (last-store env mmlenv)
  (slot-set! mmlenv 'total-ticks (caar (slot-ref env 'current-ticks)))
  (slot-set! mmlenv 'loop-ticks (cdar (slot-ref env 'current-ticks)))
  (object-store env (eof-object)))

(define (object-store env obj)
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
  (let ([class (class-of obj)]
        [ht (slot-ref env 'inter-objects)]
        [tm (slot-ref env 'objects-order)])
    (unless (hash-table-contains? ht class)
      (tree-map-put! tm (tree-map-num-entries tm) class))
    (hash-table-set! ht class obj)))

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
;; 2path
(define (%sxmml-optimize-2path lis)
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
  (define (placed-init! ctx)
    (for-each (^s (slot-set! ctx s #f))
              '(voice-placed? volume-placed? gatetime-placed? pan-placed? detune-placed?
                              envelope-placed? lfo-placed? lfo-switch-placed?))
    ctx)
  (define (ctx-copy ctx)
    (let1 new (make <sxmml-optimize-2path-context>)
      (for-each (^s (slot-set! new s (slot-ref ctx s)))
                (map slot-definition-name (class-slots <sxmml-optimize-2path-context>)))
      new))
  (let loop ([rest lis]
             [ctx (make <sxmml-optimize-2path-context>)]
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
           (loop (cdr rest) (placed-init! ctx) (cons (%repeat-end-ctx (cdr rest)) rctx) (cons #f sctx) (cons (car rest) result))]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (if (skip?! ctx rctx (car rest) 'voice 'voice-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (if (skip?! ctx rctx (car rest) 'volume 'volume-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (slot-set! ctx 'volume (+ (slot-ref ctx 'volume) (slot-ref (car rest) 'value)))
           (loop (cdr rest) ctx rctx sctx (cons (car rest) result))]
          [(is-a? (car rest) <sxmml-mml-gatetime>)
           (if (skip?! ctx rctx (car rest) 'gatetime 'gatetime-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (if (skip?! ctx rctx (car rest) 'pan 'pan-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (if (skip?! ctx rctx (car rest) 'detune 'detune-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (if (skip?! ctx rctx (car rest) 'envelope 'envelope-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (if (skip?! ctx rctx (car rest) 'lfo-switch 'lfo-switch-placed?)
             (loop (cdr rest) ctx rctx sctx result)
             (loop (cdr rest) ctx rctx sctx (cons (car rest) result)))]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (if (skip?! ctx rctx (car rest) 'lfo 'lfo-placed?)
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


(define (%repeat-end-ctx lis)
  (let loop ([rest lis]
             [ctx (make <sxmml-optimize-2path-context>)])
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
          [(is-a? (car rest) <sxmml-mml-gatetime>)
           (slot-set! ctx 'gatetime (slot-ref (car rest) 'value))
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
           (slot-set! ctx 'lfo-switch (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (slot-set! ctx 'lfo (slot-ref (car rest) 'value))
           (loop (cdr rest) ctx)]
          [else
           (loop (cdr rest) ctx)]
          )))
