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

(define-module sxmml.environment
  (use util.match)
  (use gauche.unicode)
  (use sxmml.classes)
  (export sxmml-environment sxmml-source-file sxmml-output-file sxmml-mode
          define-metadata sxmml-metadata-get sxmml-metadata-set!
          sxmml-resolution sxmml-resolution-get sxmml-resolution-set!
          define-lfo sxmml-lfo-get sxmml-lfo-set!
          define-envelope sxmml-envelope-get sxmml-envelope-set!
          define-track sxmml-track-get sxmml-track-set! sxmml-track-delete! sxmml-track-clear!
          sxmml-track-list sxmml-track-find sxmml-sio-regist!
          ))
(select-module sxmml.environment)

;;
;;  environment information
;;
(define sxmml-environment (make-parameter (make <sxmml-environment>)))
(define sxmml-source-file (make-parameter #f))
(define sxmml-output-file (make-parameter #f))

(define (sxmml-resolution-get)
  (slot-ref (sxmml-environment) 'resolution))
(define (sxmml-resolution-set! resolution)
  (unless (and (integer? resolution) (positive? resolution))
    (error "resolution must be positive integer, but got:" resolution))
  (slot-set! (sxmml-environment) 'resolution resolution))

(define sxmml-resolution sxmml-resolution-set!)

(define-syntax sxmml-mode
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ proc args ...)
         (quasirename rename
           `(apply ,proc ',args))]))))

;;
;;  metadata information
;;
(define (sxmml-metadata-get key)
  (hash-table-get (slot-ref (sxmml-environment) 'metadata) key #f))
(define (sxmml-metadata-set! key value)
  (hash-table-set! (slot-ref (sxmml-environment) 'metadata) key value))

(define-syntax define-metadata
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ key)
         (error "value does not exist:" key)]
        [(_ key (value ..1))
         (if-let1 obj (find (^v (not (string? v))) value)
           (error "metadata value must be a string: " obj)
           (let ([keysym (string->symbol (string-downcase (x->string key)))])
             (quasirename rename
               `(sxmml-metadata-set! ',keysym (list ,@value)))))]
        [(_ key value)
         (if (not (string? value))
           (error "metadata value must be a string: " value)
           (let ([keysym (string->symbol (string-downcase (x->string key)))])
             (quasirename rename
               `(sxmml-metadata-set! ',keysym ,value))))]))))

;;
;;  LFO
;;

(define (sxmml-lfo-get id)
  (hash-table-get (slot-ref (sxmml-environment) 'lfo) id))
(define (sxmml-lfo-set! id type data)
  (hash-table-set! (slot-ref (sxmml-environment) 'lfo) id
                   (make <sxmml-lfo> :id id :type type :value data)))

(define-syntax define-lfo
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ id (datum ...))
         (quasirename rename
           (define-lfo ,id default ,datum))]
        [(_ id (datum ...) ...)
         (quasirename rename
           (define-lfo ,id default ,@datum))]
        [(_ id type (datum ...))
         (let ([idsym (string->symbol (x->string id))])
           (quasirename rename
             `(sxmml-lfo-set! ',idsym ',type (apply list ',datum))))]
        [(_ id type (datum ...) ...)
         (let ([idsym (string->symbol (x->string id))])
           (quasirename rename
             `(sxmml-lfo-set! ',idsym ',type (apply list ',datum))))]
        ))))


;;
;;  envelope
;;

(define (sxmml-envelope-get id)
  (hash-table-get (slot-ref (sxmml-environment) 'envelope) id))
(define (sxmml-envelope-set! id type data)
  (hash-table-set! (slot-ref (sxmml-environment) 'envelope) id
                   (make <sxmml-envelope> :id id :type type :value data)))

(define-syntax define-envelope
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ id (datum ...))
         (quasirename rename
           (define-envelope ,id default ,datum))]
        [(_ id (datum ...) ...)
         (quasirename rename
           (define-envelope ,id default ,@datum))]
        [(_ id type (datum ...))
         (let ([idsym (string->symbol (x->string id))])
           (quasirename rename
             `(sxmml-envelope-set! ',idsym ',type (apply list ',datum))))]
        [(_ id type (datum ...) ...)
         (let ([idsym (string->symbol (x->string id))])
           (quasirename rename
             `(sxmml-envelope-set! ',idsym ',type (apply list ',datum))))]
        ))))

;;
;;  track
;;

(define (sxmml-track-get track)
  (hash-table-get (slot-ref (sxmml-environment) 'track) track #f))
(define (sxmml-track-set! track . params)
  (hash-table-set! (slot-ref (sxmml-environment) 'track) track
                   (apply make <sxmml-mml-context> :track track params)))
(define (sxmml-track-delete! track)
  (hash-table-delete! (slot-ref (sxmml-environment) 'track) track))
(define (sxmml-track-clear! track)
  (hash-table-clear! (slot-ref (sxmml-environment) 'track)))
(define (sxmml-track-list)
  (hash-table-keys (slot-ref (sxmml-environment) 'track)))
(define (sxmml-track-find part)
  (hash-table-find (slot-ref (sxmml-environment) 'track)
                   (^[k v] (and (eq? (slot-ref v 'part) part) k))))

(define (sxmml-sio-regist! sio track)
  (if-let1 ctx (sxmml-track-get track)
    (slot-set! ctx 'compiled-data (if (null? (slot-ref ctx 'compiled-data))
                                    sio
                                    (cons (slot-ref ctx 'compiled-data) sio)))))

(define-syntax define-track
  (er-macro-transformer
    (^[form rename id=?]
      (define (regist-context! track param)
        (quasirename rename
          `(sxmml-track-set! ',track ,@param)))
      (define (build-part track params)
        (if (find (^[p] (eq? p 'part)) params)
          params
          (cons* :part `',track params)))
      (match form
        [(_ (track ..1) param ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (regist-context! t (build-part t param)) r))
                      ()
                      track)))]
        [(_ track param ...)
         (regist-context! track (build-part track param))]
        ))))
