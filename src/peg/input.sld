;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-library (peg input)

  (export make-position position?
          position-line position-column

          make-input input?

          input-empty? input-match?
          input-first input-rest
          input-position)

  (import (scheme base))

  (include "input.scm"))
