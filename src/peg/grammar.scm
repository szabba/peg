;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;; The grammar predicate registry. As new grammar types get defined,
;; they are consed onto it.
(define grammar-preds '())

;; Tell if g is a grammar.
(define (grammar? g)

  (and (node? g)
       (let ((label (node-label g)))

         (not (null?
               (filter (cut <> label)
                       grammar-preds))))))

;; Defines new grammar types.
(define-syntax define-grammar-type
  (syntax-rules ()

    ((_ (name args ...) pred-name data-expr)

     (begin

      ;; Each grammar is a node. Kinds of grammars get differentiated
      ;; by their labels.
      (define (name args ... )
        (make-node '(peg name) data-expr))


      ;; Each grammar type has an associated type predicate
      (define pred-name

        ;; The label predicate gets generated once... It's the function
        ;; that gets used both by the grammar? predicate and the grammar
        ;; type's own one.
        (let ((pred-func (cut equal? '(peg name) <>)))

          ;; Add the label predicate to the registry
          (set! grammar-preds
                (cons pred-func grammar-preds))

          ;; Construct the types' own predicate
          (lambda (g)
            (and (node? g)
                 (pred-func (node-label g))))))))))


;; Empty string
(define-grammar-type (empty-string)

  empty-string? '())

;; Any character
(define-grammar-type (any-char)

  any-char? '())

;; Exact character
(define-grammar-type (lit-char char)

  lit-char? (list char))

;; Optional match
(define-grammar-type (optional g)

  optional? (list g))

;; Zero or more matches
(define-grammar-type (many g)

  many? (list g))

;; One or more matches
(define-grammar-type (one-or-more g)

  one-or-more? (list g))

;; Fail if the input doesn't match the supplied grammar
(define-grammar-type (must-match g)

  must-match? (list g))

;; Fail if the input does match the supplied grammar
(define-grammar-type (must-not-match g)

  must-not-match? (list g))

;; Sequence of grammars to match
(define-grammar-type (group gs)

  group? gs)

(define group
  (let ((old group))
    (lambda gs (old gs))))

;; Ordered choice between grammars
(define-grammar-type (choice gs)

  choice? gs)

(define choice
  (let ((old choice))
    (lambda gs (old gs))))

;; A named rule. It's important and different enough to get implemented
;; by hand.
(define (rule name g)
  (make-node `(peg rule ,name) (list g)))

;; The label predicate for rules checks that they have the form
;;
;;     (peg rule rest ...)
;;
(define (rule-label-predicate label)

  (let ((pattern '(peg rule)))

    (= 1 (apply * (map (lambda (p l)
                         (if (eq? p l) 1 0))
                       pattern
                       label)))))

(set! grammar-preds
  (cons rule-label-predicate grammar-preds))

(define (rule? g)
  (and (node? g)
       (rule-label-predicate (node-label g))))

;; Grammar defining macro
(define-syntax define-grammar
  (syntax-rules ()

    ((_ (grammar start-rule)
        (rule-name parse-expr) ...)

     (define grammar
       (let ((rule-name (rule 'rule-name '())) ...)

         (set-node-children! rule-name (list parse-expr)) ...

         start-rule)))))
