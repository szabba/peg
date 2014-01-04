;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-record-type data
  (make-data chars pos)
  data?
  (chars data-chars)
  (pos position))

(define (empty? d)
  (null? (data-chars d)))

(define (first d)

  (when (empty? d)
    (error "input-first -- no input left"))

  (car (data-chars d)))

(define (match? d char)
  (and (not (empty? d))
       (char=? char (first d))))

(define (next-position d)
  (let ((line (position-line (position d)))
        (col (position-column (position d))))

    (if (match? d #\newline)
        (make-position (+ line 1) 1)
        (make-position line (+ 1 col)))))

(define (make-input-with-data d)
  (make-input d empty? match? first rest position))

(define (rest d)

  (when (empty? d)
    (error "input-rest -- no input left"))

  (make-input-with-data (make-data (cdr (data-chars d))
                                   (next-position d))))


(define (string->input str)
  (make-input-with-data (make-data (string->list str)
                                   (make-position 1 1))))
