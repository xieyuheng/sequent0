;; use chez scheme

(print-graph #t)

(include "lib/define-macro.scm")
(include "lib/flower-barcket.scm")
(include "lib/match.scm")
(include "lib/helper.scm")
(include "sequent0.scm")
(print-define+)

;; (type-check+)
;; (cover-check+)
;; (recur-check+)
