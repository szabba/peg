;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-library (peg parse)

  (export parse

          parse-result? parse-tree? parse-fail? parse-go-on?

          parse-result-tree parse-result-rest)

  (import (scheme base)
          (srfi 26)
          (peg graph) (peg grammar) (peg input))

  (include "parse.scm"))
