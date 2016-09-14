(define data-stack '())
(define return-stack '())
(define binding-stack '())
(define name-stack '())

(define-macro (define-type type )
  )

(define-macro (match1 info e . cl)
  (let ([v (gensym "match1/call-by-value/var")])
    `(let ([,v ,e]) ;; to call by value
       (match2 ,info ,v . ,cl))))
