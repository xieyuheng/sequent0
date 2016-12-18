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
;;   which does no side-effect on object
(define (% object . rest)
  (define (loop o l)
    (if (null? l)
      o
      (loop (%-one o (car l))
            (cdr l))))
  (loop object (apply pair-list rest)))

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
;;   methods in it are called by (apply methods object rest)
(define (@ object message . rest)
  (let ([mrc (assq 'method-record object)])
    (if mrc
      (let ([p (assq message (cdr mrc))])
        (if p
          (apply (cdr p) object rest)
          (orz '@
            ("- method-record of object does not have method for message~%")
            ("  object : ~a~%" object)
            ("  method-record : ~a~%" mrc)
            ("  message : ~a~%" message))))
      (orz '@
        ("- object does not have 'method-record field~%")
        ("  object : ~a~%" object)))))

(define (new-object vpl mpl)
  (cons (cons 'method-record mpl)
        vpl))

(define (new-struct vpl)
  (cons (cons 'method-record '())
        vpl))

;;;; test

;; (define o1
;;   (new-object
;;    (pair-list
;;     'a 1
;;     'b 2
;;     'c 3)
;;    (pair-list)))

;; (% o1 'a 4)
;; (^ (% o1 'a 4) 'a)
