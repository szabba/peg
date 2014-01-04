;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-library (peg graph)

  (export node? make-node empty-node empty-node?
          node-label node-children set-node-children!
          node-references node-shallow-copy
          find-nodes node-map)

  (import (scheme base)
          (srfi 1) (srfi 26))

  (include "graph.scm"))

