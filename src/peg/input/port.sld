;;  This Source Code Form is subject to the terms of the Mozilla Public
;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(define-library (peg input port)

  (export port->input)

  (import (scheme base)
	  (scheme case-lambda)
          (peg input))

  (include "port.scm"))
