;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;; Helper syntax
(define-syntax define-adt
  (syntax-rules ()
    ((_ type?
        (variant? ...))

     (define (type? obj)
       (or (variant? obj) ...)))))

;; The type of parse results
(define-adt parse-result? (parse-go-on? parse-fail? parse-tree?))

;; The parser has consumed input and produced a parse tree
(define-record-type parse-tree
  (make-parse-tree tree rest) parse-tree?
  (tree parse-tree-tree)
  (rest parse-tree-rest))

;; The parser has failed and might've consumed input
(define-record-type parse-fail
  (make-parse-fail message rest) parse-fail?
  (message parse-fail-message)
  (rest parse-fail-rest))

;; The parser did not consume input, but matched it
(define-record-type parse-go-on
  (make-parse-go-on rest) parse-go-on?
  (rest parse-go-on-rest))

;; False if the result has no tree, a tree otherwise
(define (parse-result-tree result)

  (when (not (parse-result? result))

    (error "parse-result-tree -- argument is not a parse-result"))

  (if (parse-tree? result) (parse-tree-tree result) #f))

;; Input not consumed by the parser
(define (parse-result-rest result)

  (when (not (parse-result? result))

    (error "parse-result-rest -- argument is not a parse-result"))

  (cond ((parse-tree? result) (parse-tree-rest result))
        ((parse-fail? result) (parse-fail-rest result))
        ((parse-go-on? result) (parse-go-on-rest result))))

;; Associative list of parsing handlers
(define parsing-handlers '())

;; False if g isn't a recognised grammar, a parsing handler otherwise.
(define (handler-for g)
  (assoc g parsing-handlers (lambda (g pred)
                              (pred g))))

;; Parse the input using the grammar given
(define (parse g input)

  (cond ((handler-for g)
         =>
         (lambda (h)
           ((cdr h) g input)))

        (else (error "parse -- invalid grammar"))))

;; Allows defining parsing handlers
(define (register-handler predicate handler)
  (set! parsing-handlers
        (cons (cons predicate handler)
              parsing-handlers)))

;; Handler for the empty-string grammar
(register-handler
 empty-string?
 (lambda (g input)
   (make-parse-go-on input)))

;; Handler for the any-char grammar
(register-handler
 any-char?
 (lambda (g input)

   (if (input-empty? input)
       (make-parse-fail "Unexpected end of input" input)

       (make-parse-tree

        (make-node (cons 'match (node-label g))
                   (list (input-first input)))

        (input-rest input)))))

;; Handler for the lit-char grammar
(register-handler
 lit-char?
 (lambda (g input)

   (let ((char (car (node-children g))))

     (cond ((input-empty? input)
            (make-parse-fail "Unexpected end of input" input))

           ((input-match? input char)
            (make-parse-tree

             (make-node (cons 'match (node-label g))
                        (list (input-first input)))

             (input-rest input)))

           (else (make-parse-fail `("Character doesn't match" ,char)
                                  input))))))

;; Handler for the optional grammar
(register-handler
 optional?
 (lambda (g input)

   (let* ((inner-g (car (node-children g)))
          (result (parse inner-g input))
          (rest (parse-result-rest result)))

     (cond ((parse-tree? result)

            (make-parse-tree

             (make-node (cons 'match (node-label g))
                        (list (parse-result-tree result)))

             rest))

           (else (make-parse-go-on rest))))))

;; Handler for the many grammar
(register-handler
 many?
 (lambda (g input)

   (let ((inner-g (car (node-children g))))

     (let loop ((input input)
                (reverse-results '()))

       (let ((result (parse inner-g input)))

         (cond ((parse-tree? result)

                (loop (parse-result-rest result)
                      (cons (parse-result-tree result)
                            reverse-results)))

               ((null? reverse-results)
                (make-parse-go-on input))

               (else (make-parse-tree

                      (make-node (cons 'match (node-label g))
                                 (reverse reverse-results))

                      (parse-result-rest result)))))))))

;; Handler for the one-or-more grammar
;;
;; TODO: Implement parser-map, so the parse tree of this looks
;; sensibly.
(register-handler
 one-or-more?
 (lambda (g input)

   (let ((inner-g (car (node-children g))))

     (parse (group inner-g (many g)) input))))

;; Handler for the syntactic and predicate
(register-handler
 must-match?
 (lambda (g input)

   (let* ((inner-g (car (node-children g)))
          (result (parse inner-g input)))

     (if (parse-fail? result)
         (make-parse-fail (list "Input should match grammar"
                                inner-g
                                result)
                          input)
         (make-parse-go-on input)))))

;; Handler for the syntacti and predicate
(register-handler
 must-not-match?
 (lambda (g input)

   (let* ((inner-g (car (node-children g)))
          (result (parse inner-g input)))

     (if (parse-fail? result)
         (make-parse-go-on input)
         (make-parse-fail (list "Input shouldn't match grammar" inner-g)
                          input)))))


;; Handler for the group grammar
(register-handler
 group?
 (lambda (g input)

   (let loop ((inner-gs (node-children g))
              (rest input)
              (reverse-results '()))

     (if (null? inner-gs)

         (make-parse-tree (make-node (cons 'match (node-label g))
                                     (reverse reverse-results))
                          rest)

         (let ((result (parse (car inner-gs) rest)))

           (cond ((parse-fail? result) result)

                 ((parse-go-on? result)
                  (loop (cdr inner-gs) rest reverse-results))

                 ((parse-tree? result)
                  (loop (cdr inner-gs)
                        (parse-result-rest result)
                        (cons (parse-result-tree result)
                              reverse-results)))))))))

;; Handler for the choice grammar
(register-handler
 choice?
 (lambda (g input)

   (let loop ((inner-gs (node-children g)))

     (if (null? inner-gs)
         (make-parse-fail "No choice was valid" input)

         (let ((result (parse (car inner-gs) input)))

           (cond ((parse-fail? result) (loop (cdr inner-gs)))
                 ((parse-go-on? result) result)
                 ((parse-tree? result)

                  (make-parse-tree

                   (make-node (cons 'match (node-label g))
                              (list (parse-result-tree result)))

                   (parse-result-rest result)))))))))

;; Handler for named rules
(register-handler
 rule?
 (lambda (g input)

   (let* ((inner-g (car (node-children g)))
          (result (parse inner-g input))
          (rest (parse-result-rest result)))

     (cond ((or (parse-fail? result)
                (parse-go-on? result))

            result)

           ((parse-tree? result)

            (make-parse-tree (make-node (cons 'match (node-label g))
                                        (list (parse-result-tree result)))
                             rest))))))
