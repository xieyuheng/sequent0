;; code taken from a example in chez repo :: ChezScheme/examples/compat.ss

;;; thanks to Michael Lenaghan (MichaelL@frogware.com) for suggesting
;;; various improvements.

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      [(k (name arg1 ... . args)
          form1
          form2
          ...)
       #'(k name (arg1 ... . args)
            form1
            form2
            ...)]
      [(k (name arg1 arg2 ...)
          form1
          form2
          ...)
       #'(k name (arg1 arg2 ...)
            form1
            form2
            ...)]
      [(k name args . forms)
       (identifier? #'name)
       (letrec ((add-car
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cadr ,@(cdr access)))
                     ((cadr) `(caadr ,@(cdr access)))
                     ((cddr) `(caddr ,@(cdr access)))
                     ((cdddr) `(cadddr ,@(cdr access)))
                     (else `(car ,access)))))
                (add-cdr
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cddr ,@(cdr access)))
                     ((cadr) `(cdadr ,@(cdr access)))
                     ((cddr) `(cdddr ,@(cdr access)))
                     ((cdddr) `(cddddr ,@(cdr access)))
                     (else `(cdr ,access)))))
                (parse
                 (lambda (l access)
                   (cond
                    ((null? l) '())
                    ((symbol? l) `((,l ,access)))
                    ((pair? l)
                     (append!
                       (parse (car l) (add-car access))
                       (parse (cdr l) (add-cdr access))))
                    (else
                     (syntax-error #'args
                                   (format "invalid ~s parameter syntax" (datum k))))))))
         (with-syntax ((proc (datum->syntax-object #'k
                                                   (let ((g (gensym)))
                                                     `(lambda (,g)
                                                        (let ,(parse (datum args) `(cdr ,g))
                                                          ,@(datum forms)))))))
           #'(define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((k1 . r)
                    (datum->syntax-object #'k1
                                          (proc (syntax-object->datum x)))))))))])))

;; {...} can not occur in the position of literal cdr
;; for example `(1 . {2}) can not be handled
;; this error will be reported by chez as
;; Exception in read: more than one item found after dot (.)

(define (flower-barcket f l)
  ;; (: (list -> parsed-result) list -> parsed-list)
  (cond [(not (pair? l)) l]
        [(pair? (car l))
         (cons (flower-barcket f (car l))
               (flower-barcket f (cdr l)))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (f dl)
                 (flower-barcket f rl)))]
        [else
         (cons (car l)
               (flower-barcket f (cdr l)))]))

(define (flower-barcket/read f l)
  ;; (: (list -> parsed-result) list -> (readed-list . not-parsed-rest-list))
  (cond [(null? l) (error 'flower-barcket/read "sexp in lack of }")]
        [(eq? '|}| (car l))
         (cons '()
               (cdr l))]
        [(eq? '|{| (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)]
                [pair1 (flower-barcket/read f rl)]
                [dl1 (car pair1)]
                [rl1 (cdr pair1)])
           (cons (cons (f dl) dl1)
                 rl1))]
        [(pair? (car l))
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (flower-barcket f (car l)) dl)
                 rl))]
        [else
         (let* ([pair (flower-barcket/read f (cdr l))]
                [dl (car pair)]
                [rl (cdr pair)])
           (cons (cons (car l) dl)
                 rl))]))

(define (flower-barcket/list l)
  ;; (: list -> parsed-list)
  (flower-barcket (lambda (dl) (cons 'list dl)) l))

;; (flower-barcket/list
;;  '({a {a b c b {a b c} c}))
;; Exception in flower-barcket/read: sexp in lack of }

;; (flower-barcket/list
;;  '({a {a b c} } } } b {a b c} c}))
;; sadly additional |}| is not reported as error
;; => ((list a (list a b c)) |}| |}| b (list a b c) c |}|)

;; (flower-barcket/list
;;  '({a {a b c} b {a b c} c}))
;; => ((list a (list a b c) b (list a b c) c))

;; (flower-barcket/list
;;  '({{a b c} ({a b c} a {a b c} . b)} {a b c} . b))
;; => ((list (list a b c) ((list a b c) a (list a b c) . b)) (list a b c) . b)

;; I learn this from oleg
;; http://okmij.org/ftp/Scheme/macros.html#match-case-simple

(define-macro (match . body)
  `(match1 (quote (match . ,body)) . ,(flower-barcket/list body)))

(define-macro (match1 info e . cl)
  (let ([v (gensym "match1/call-by-value/var")])
    `(let ([,v ,e]) ;; to call by value
       (match2 ,info ,v . ,cl))))

(define-macro (match2 info v . cl)
  (cond [(null? cl)
         `(let ()
            (format #t "\n")
            (format #t "<begin-match-report>\n")
            (format #t ":value:\n")
            (display ,v) (newline)
            (format #t ":body:\n")
            (display ,info) (newline)
            (format #t "<end-match-report>\n")
            (error 'match ">_<"))]
        [else
         (let* ([v v] [c (car cl)]
                [p (car c)]
                [el (cdr c)]
                [false-body (gensym "match2/false-body/var")])
           `(let ([,false-body (lambda () (match2 ,info ,v . ,(cdr cl)))])
              ;; note that
              ;; match3 may do binding here
              ;; other clauses are outside of these binding
              (match3 ,info ,v ,p (let () . ,el) (,false-body))))]))

(define-macro (match3 info v p t f)
  ;; (: info value pattern true-body false-body -> body)
  (cond [(eq? p '__)
         t]
        [(eq? p '())
         `(if (null? ,v) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'list))
         ;; this is for (list ...)
         ;; return by {...}
         `(match3 ,info ,v ,(cdr p) ,t ,f)]
        [(and (pair? p)
              (eq? (car p) 'quote))
         `(if (equal? ,v ,p) ,t ,f)]
        [(pair? p)
         (let ([x (car p)]
               [y (cdr p)])
           `(if (pair? ,v)
              (match3 ,info (car ,v) ,x
                      (match3 ,info (cdr ,v) ,y ,t ,f)
                      ,f)
              ,f))]
        [(symbol? p)
         `(let ([,p ,v]) ,t)]
        [else ;; p is literal value
         `(if (equal? ,v ,p) ,t ,f)]))

;; (let ()
;;   (define (test-match x)
;;     (match x
;;       [() "null"]
;;       [#t "bool"]
;;       [#f "bool"]
;;       [(x . y)
;;        (string-append "pair of " (test-match x) " and " (test-match y))]
;;       [__
;;        (if (symbol? x)
;;          "symbol"
;;          "something else")]))
;;   (newline)
;;   (for-each (lambda (x) (display (test-match x)) (newline))
;;             '(cctv
;;               #t
;;               "str"
;;               (#t #f "str"))))

;; (match 3
;;   [a a])
;; ;; => 3

;; (match {'b 2 3}
;;   [{'a b c}
;;    (let ([a 1])
;;      {{a b c} a {a b c} b {a b c} c {a b c}})])
;; ;; error report

;; (match {'a 2 3}
;;     [{'a b c}
;;      (let ([a 1])
;;        {{a b c} a {a b c} b {a b c} c {a b c}})])
;; ;; => ((1 2 3) 1 (1 2 3) 2 (1 2 3) 3 (1 2 3))

;; (let ([s (list 'a 2 3)])
;;   (match s
;;     [('a b c)
;;      (let ([a 1])
;;        (list (list a b c) a (list a b c) b (list a b c) c (list a b c)))]))

;; (let ([s (list 'a 2 3)])
;;   (match s
;;     [{'a b c}
;;      (let ([a 1])
;;        {{a b c} a {a b c} b {a b c} c {a b c}})]))

(define-macro (cat-one e)
  (cond [(and (pair? e)
              (string? (car e)))
         (let ([str (car e)]
               [args (cdr e)])
           `(format #t ,str . ,args))]
        [(and (pair? e)
              (not (string? (car e))))
         e]
        [else
         `(error 'cat-one)]))

(define-macro (cat . l)
  (if (null? l)
    `(void)
    (let* ([h (car l)]
           [r (cdr l)])
      `(let ()
         (cat-one ,h)
         (cat . ,r)))))

(define-macro (orz who . l)
  `(let ()
     (cat ("~%")
          ("<~a>~%" ,who)
          (cat . ,l)
          ("~%")
          ("</~a>~%" ,who)
          ("~%"))
     (error ,who "")))

(define-syntax if3
  (syntax-rules ()
    [(if3 [a ...]
          [b ...]
          [])
     (if (let () a ...)
       (let () b ...))]
    [(if3 [a ...]
          []
          [c ...])
     (if (not (let () a ...))
       (let () c ...))]
    [(if3 [a ...]
          [b ...]
          [c ...])
     (if (let () a ...)
       (let () b ...)
       (let () c ...))]))

(define-syntax type
  (syntax-rules ()
    [(type . body)
     (void)]))

(define-syntax :
  (syntax-rules ()
    [(: . body)
     (void)]))

(define-syntax test
  (syntax-rules ()
    [(test b1 b2)
     (if (equal? b1 b2)
       #t
       (let ()
         (cat ("~%")
              ("<begin-test-fail-report>~%")
              (":actual-form:~%"))
         (pretty-print (quote b1))
         (cat (":actual-value:~%"))
         (pretty-print b1)
         (cat (":expect-form:~%"))
         (pretty-print (quote b2))
         (cat (":expect-value:~%"))
         (pretty-print b2)
         (cat ("<test-fail-report-end>~%"))
         (orz 'test (">_<"))))]))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (left-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) '()]
        [else (cons (car l) (left-of s (cdr l)))]))

(define (right-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) (cdr l)]
        [else (right-of s (cdr l))]))

(define (sublist l start end)
  (: list index index -> list)
  (cond [(and (eq? 0 start) (<= end 0)) '()]
        [(and (not (eq? 0 start)))
         (sublist (cdr l) (- start 1) (- end 1))]
        [(and (eq? 0 start) (not (eq? 0 end)))
         (cons (car l) (sublist (cdr l) 0 (- end 1)))]))

(define (genlist len)
  (: length -> list)
  (letrec ([recur
            (lambda (len counter)
              (cond [(eq? len counter) '()]
                    [else (cons counter
                                (recur len (+ 1 counter)))]))])
    (recur len 0)))

(define (substitute e p? l)
  (: element (element -> bool) (element ...) -> (element ...))
  (cond [(eq? '() l) '()]
        [(p? (car l)) (cons e (cdr l))]
        [else (cons (car l) (substitute e p? (cdr l)))]))

(define (list-every? p? l)
  (: (element -> bool) l -> bool)
  (not (member #f (map p? l))))

(define (list-any? p? l)
  (: (element -> bool) l -> bool)
  (member #t (map p? l)))

(define (last l)
  (cond [(eq? '() (cdr l))
         (car l)]
        [else
         (last (cdr l))]))

(define (drop-last l)
  (cond [(eq? '() (cdr l))
         '()]
        [else
         (cons (car l) (drop-last (cdr l)))]))

(define (list-sub l1 l2)
  (cond [(eq? l1 '())
         '()]
        [(eq? l1 l2)
         '()]
        [else
         (cons (car l1) (list-sub (cdr l1) l2))]))

(define (pair-list . l)
  (match l
    [{} '()]
    [{x} (orz 'pair-list
           ("meet uneven list with ending : ~a~%" x))]
    [(x y . z) (cons (cons x y)
                     (apply pair-list z))]))

(define (sexp->string s)
  (format #f "~a" s))

(define (find-char c s)
  (: char string -> (or curser #f))
  (find-char/curser c s 0))

(define (find-char/curser c s curser)
  (: char string curser -> (or curser #f))
  (if (>= curser (string-length s))
    #f
    (let ([c0 (substring s curser (+ 1 curser))])
      (if (equal? c c0)
        curser
        (find-char/curser c s (+ 1 curser))))))

(define (symbol-append . l)
  (: symbol ... -> symbol)
  (string->symbol
   (apply string-append
     (map symbol->string l))))

(define (symbol-car v)
  (string->symbol (substring (symbol->string v) 0 1)))

(define (symbol-cdr v)
  (let ([str (symbol->string v)])
    (string->symbol (substring str 1 (string-length str)))))
