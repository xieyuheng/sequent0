;; use chez scheme

(print-graph #t)
(print-graph #f)

(load "lib/helper.scm")
(include "lib/hat.scm")

(include "sequent0.scm")

(print-define+)

(type-check+)
;; (cover-check+)
;; (recur-check+)

;; (steper+)
