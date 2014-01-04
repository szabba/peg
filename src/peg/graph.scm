;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;; Node type, used for constructing syntax graphs.
(define-record-type node
  (make-node label children)
  node?
  (label node-label)
  (children node-children set-node-children!))

;; Construct a node without children
(define (empty-node label)
  (make-node label (list)))

;; Tell whether the argument is an empty node
(define (empty-node? node)
  (and (node? node)
       (null? (node-children node))))

;; Find all the nodes directly referenced by the argument
(define (node-references node)
  (filter node? (node-children node)))

;; Find all nodes reachable from the argument
(define (find-nodes root)

  (let node-loop ((nodes (list root))
                  (candidates (node-references root))
                  (new-nodes (list)))

    (if (and (null? candidates)
             (null? new-nodes))

      nodes

      (node-loop (append nodes new-nodes)
                 (append-map node-references new-nodes)
                 (filter (lambda (n)
                           (not (member n nodes eqv?)))
                         candidates)))))

;; Rewrite direct references of the node using the associative list
;; old->new as a guide.
(define (rewrite-node-references! node old->new)
  (set-node-children! node (map (lambda (node-or-not)
                              (if (node? node-or-not)
                                (cdr (assoc node-or-not old->new eqv?))
                                node-or-not))
                            (node-children node))))

;; Transform a graph starting at node root using the given
;; transformation function.
(define (node-map transform root)

  (let* ((nodes (find-nodes root))
         (copies (map transform nodes))
         (node->copy (map cons nodes copies)))

    (for-each (cut rewrite-node-references! <> node->copy)
              copies)

    (cdr (assoc root node->copy eqv?))))

;; Perform a shallow copy of the node
(define (node-shallow-copy node)

  (make-node (list-copy (node-label node))
             (list-copy (node-children node))))
