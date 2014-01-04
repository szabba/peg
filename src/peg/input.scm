;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;; Position type
(define-record-type position
  (make-position line column)
  position?
  (line position-line)
  (column position-column))

;; Abstract input type. Most of it's fields contain procedures which get
;; extracted and called by the public API.
(define-record-type input
  (make-input data empty?-proc match?-proc first-proc rest-proc position-proc)
  input?
  (data input-data)
  (empty?-proc input-empty?-proc)
  (match?-proc input-match?-proc)
  (first-proc input-first-proc)
  (rest-proc input-rest-proc)
  (position-proc input-position-proc))

;; Macro for defining input methods. The definitions that use it are
;; clearer and less dense.
(define-syntax define-input-method
  (syntax-rules ()

    ((_ (name receiver args ...)
        data-extractor
        method-extractor)

     (define (name receiver args ...)

       ((method-extractor receiver) (data-extractor receiver)

        args ...)))))

(define-input-method (input-empty? input)
  input-data
  input-empty?-proc)

(define-input-method (input-match? input char)
  input-data
  input-match?-proc)

(define-input-method (input-first input)
  input-data
  input-first-proc)

(define-input-method (input-rest input)
  input-data
  input-rest-proc)

(define-input-method (input-position input)
  input-data
  input-position-proc)
