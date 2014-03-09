;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Converts a port to an input
(define (port->input port)

  (data->input (port->input-data port)))

;;; Converts a piece of input data into an input
(define (data->input data)

  (make-input data empty? match? first rest position))

;;; Convert a port to a piece of input data. The single argument form creates
;;; the input data for the beginning of an input. The two-argument form
;;; produces the input data for the next character.
;;;
;;; Either way -- the initial one does not contain a read character. The actual
;;; reading and determining of the character position happens when `force!` is
;;; called on the input data.
(define port->input-data
  (case-lambda

    ((port)
     (make-input-data port (make-position 1 1) #f))

    ((port after)
     (make-input-data port (input-pos after) #f))))

;;; The input-data record type
(define-record-type input-data
  (make-input-data datum pos rest)
  input-data?
  (datum input-datum set-input-datum!)
  (pos input-pos set-input-pos!)
  (rest input-rest set-input-rest!))

;;; Reads the actual character and computes it's position in the input.
;;; Idempotent.
(define (force! data)

  (let ((datum (input-datum data)))

    (when (port? datum)

      (let ((char (read-char datum))
            (pos (input-pos data)))

        (set-input-datum! data char)
        (set-input-pos! data (next-position char pos)))

      (set-input-rest! data (data->input (port->input-data datum data))))))

;;; Calculates a character's position given the position of the previous one.
(define (next-position char pos)

  (let ((line (position-line pos))
        (col (position-column pos)))

    (if (and (not (eof-object? char))
             (char=? char #\newline))
        (make-position (+ line 1) 1)
        (make-position line (+ col 1)))))

;;; Implementations for the input operations

(define (empty? d)
  (force! d)
  (eof-object? (input-datum d)))

(define (first d)
  (force! d)
  (input-datum d))

(define (match? d char)
  (and (not (empty? d))
       (char=? char (first d))))

(define (rest d)
  (force! d)

  (when (empty? d)
    (error "input-rest -- no input left"))

  (input-rest d))

(define (position d)
  (force! d)

  (input-pos d))
