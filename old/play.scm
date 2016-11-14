;; use chez scheme

(print-graph #t)

(include "lib/define-macro.scm")
(include "lib/flower-barcket.scm")
(include "lib/match.scm")
(include "lib/helper.scm")



(include "sequent0.scm")
(print-define+)
(type-check+)

;; (cover-check+)
;; (recur-check+)


(meaning-function
 (pre-arrow ((pre-call natural) (pre-call natural)) ((pre-call natural)))
 ((pre-arrow ((pre-var :m) (pre-call zero)) ((pre-var :m))) (pre-arrow ((pre-var :m) (pre-var :n) (pre-call succ)) ((pre-var :m) (pre-var :n) (pre-call add) (pre-call succ)))))

(pre-arrow ((pre-call natural) (pre-call natural)) ((pre-call natural)))
((arrow ((call natural) (call natural)) ((call natural))) ())
