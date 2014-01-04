;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-library (peg grammar)

  (export grammar?

          empty-string empty-string? any-char any-char? lit-char lit-char?
          optional optional? many many? one-or-more one-or-more?
          must-match must-match? must-not-match must-not-match?
          group group? choice choice?
          rule rule?

          define-grammar)

  (import (scheme base)
          (srfi 1) (srfi 26)
          (peg graph))

  (include "grammar.scm"))
