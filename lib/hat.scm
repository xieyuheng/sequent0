;; fetch
(define (^ object field)
  (let ([p (assq field object)])
    (if p
      (cdr p)
      (orz '^
        ("- object does not have field~%")
        ("  object : ~a~%" object)
        ("  field : ~a~%" field)))))

;; clone-with-new-fields
(define (% object . rest)
  (define (loop o l)
    (if (null? l)
      o
      (loop (%-one o (car l))
            (cdr l))))
  (loop object (pair-even-list rest)))

(define (%-one object p)
  (let* ([field (car p)]
         [p0 (assq field object)])
    (if p0
      (let ()
        (substitute p
                    (lambda (v) (equal? field (car v)))
                    object))
      (orz '%-one
        ("- object does not have field~%")
        ("  object : ~a~%" object)
        ("  field : ~a~%" field)
        ("  value : ~a~%" value)))))

;; apply-method
;;   object may has a field called method-record
;;   I do not need this for now
;; (define (@ object message . rest)
;;   (let ([])
;;     ))

(define (new-object . rest)
  (cons (cons 'method-record '())
        (pair-even-list rest)))



;;;; test

;; (define o1
;;   (new-object
;;     'a 1
;;     'b 2
;;     'c 3))

;; (% o1 'a 4)
;; (^ (% o1 'a 4) 'a)
