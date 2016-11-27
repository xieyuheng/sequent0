(define ns '())
(define ds '())
(define bs '())
(define rs '())
(define gs '())

(define-macro (push s v) `(set! ,s (cons ,v ,s)))
(define-macro (put s l)
  `(set! ,s (append ,l ,s)))

(define (tos s) (car s))
(define-macro (pop s)
  (let ([v (gensym "pop/v")])
    `(let ([,v (car ,s)])
       (set! ,s (cdr ,s))
       ,v)))
(define-macro (fetch s n)
  (let ([v (gensym "fetch/v")])
    `(let ([,v (take ,s ,n)])
       (set! ,s (drop ,s ,n))
       ,v)))

(define (bs/commit)
  (define (recur bs0)
    (cond [(equal? '(commit-point) (car bs0))
           (set! bs (cdr bs0))]
          [else
           (let ([id (car (car bs0))]
                 [ls (cdr (car bs0))])
             (id/commit id ls)
             (recur (cdr bs0)))]))
  (recur bs))

(define (id/commit id ls)
  (: id ls -> id [with effect on id])
  (let ()
    (vector-set! id 1 (append ls (vector-ref id 1)))
    id))

(define (bs/extend v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (let ([found/ls (assq id bs)])
       (if found/ls
         (set! bs (substitute `(,id . ((,level . ,d) . ,(cdr found/ls)))
                              (lambda (pair) (eq? (car pair) id))
                              bs))
         (push bs `(,id . ((,level . ,d))))))]))

(define (bs/extend-up v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (let ([level (+ 1 level)]
           [found/ls (assq id bs)])
       (if found/ls
         (set! bs (substitute `(,id . ((,level . ,d) . ,(cdr found/ls)))
                              (lambda (pair) (eq? (car pair) id))
                              bs))
         (push bs `(,id . ((,level . ,d))))))]))

;; in compose/var & cut/var
;;   extend bs whenever meet a new var
;;   this helps commit

;; not using ><><><
(define (bs/extend-new v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (push bs `(,id . ((,level . ,d))))]))

;; (define (bs/extend-new v d)
;;   (: var data -> !)
;;   (match v
;;     [{'var id level}
;;      (let ([found/ls (assq id bs)])
;;        (if found/ls
;;          (void)
;;          (push bs `(,id . ()))))]))

(define (id->ls id)
  (vector-ref id 1))

(define (bs/find v)
  (: var -> (or data #f))
  (match v
    [{'var id level}
     (let* ([found/commit (assq level (id->ls id))])
       (if found/commit
         (cdr found/commit)
         (let* ([found/ls (assq id bs)]
                [found/bind
                 (if found/ls
                   (assq level (cdr found/ls))
                   #f)])
           (if found/bind
             (cdr found/bind)
             #f))))]))

(define (bs/walk d)
  (: data -> data)
  (match d
    [{'var id level}
     (let ([found (bs/find d)])
       (if found
         (bs/walk found)
         d))]
    [__ d]))

(define (bs/deep d)
  (: data -> data)
  (let ([d (bs/walk d)])
    (match d
      ;; a var is fresh after bs/walk
      [{'cons n dl}             {'cons n (bs/deep-list dl)}]
      [{'bind d sd}             {'bind (bs/deep d) (bs/deep sd)}]
      [{'trunk t k i}           {'trunk t (bs/deep-trunky k) i}]
      [__                    d])))

(define (bs/deep-list dl)
  (map (lambda (x) (bs/deep x)) dl))

(define (bs/deep-trunky k)
  (vector-set!
    k 0
    (match (vector-ref k 0)
      [{'todo b dl} {'todo b (bs/deep-list dl)}]
      [{'done dl}   {'done (bs/deep-list dl)}])))

(define (bs/find-up v)
  (: var -> (or data #f))
  (match v
    [{'var id level}
     (let* ([level (+ 1 level)]
            [found/commit (assq level (id->ls id))])
       (if found/commit
         (cdr found/commit)
         (let* ([found/ls (assq id bs)]
                [found/bind
                 (if found/ls
                   (assq level (cdr found/ls))
                   #f)])
           (if found/bind
             (cdr found/bind)
             #f))))]))

(define (var/fresh? v)
  (: var -> bool)
  (equal? (bs/walk v)
          v))

(define (var/eq? v1 v2)
  (match {v1 v2}
    [{{'var id1 level1} {'var id2 level2}}
     (and (eq? id1 id2)
          (eq? level1 level2))]))

(define (occur-check/data v d)
  (: fresh-var data -> bool)
  (match (bs/deep d)
    [{'var id level} (not (var/eq? v d))]
    [{'cons n dl}    (occur-check/data-list v dl)]
    [{'bind d sd}    (occur-check/data-list v {d sd})]
    [{'trunk t k i}  (occur-check/trunk v d)]
    [__              #t]))

(define (occur-check/data-list v dl)
  (: fresh-var {data ...} -> bool)
  (match dl
    [{} #t]
    [(d . r)
     (if (occur-check/data v d)
       (occur-check/data-list v r)
       #f)]))

(define (occur-check/trunk v t)
  (: fresh-var trunk -> bool)
  (match t
    [{'trunk t k i}
     (match (vector-ref k 0)
       [{'todo b dl} (occur-check/data-list dl)]
       [{'done dl}   (occur-check/data-list dl)])]))

(define (gs/exit) (void))

(define (gs/next)
  (: -> bool)
  (match (tos gs)
    [{c ex end {dl1 dl2}}
     (ex)]))

(define (try-trunk t)
  (: trunk -> (or #f data))
  (match t
    [{'trunk t k i}
     (match (vector-ref k 0)
       [{'done dl} (list-ref dl i)]
       [{'todo b dl}
        (put ds dl)
        (compose/function t b)
        (let ([result (pop ds)])
          (cond [(equal? result t) #f]
                [else result]))])]))

(define (cover)
  (: -> bool)
  (match (pop gs)
    [{c ex end {dl1 dl2}}
     (cond [(>= c (length dl1))
            (end)
            #t]
           [else
            (let ([d1 (list-ref dl1 c)]
                  [d2 (list-ref dl2 c)])
              (push gs {(+ 1 c) ex end {dl1 dl2}})
              (if (cover/data/data d1 d2)
                (gs/next)
                #f))])]))

(define (cover/data/data d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check
      [{{'bind d v} __} (cover/data/data d d2)]
      [{__ {'bind d v}} (cover/data/data d1 d)]
      ;; var is the hero
      ;; this should pass occur-check
      [{{'var id1 level1} {'var id2 level2}}
       (cond [(var/eq? d1 d2) #t] ;; no self-cover
             [else (cover/var/data d1 d2)])]
      [{{'var id level} __} (cover/var/data d1 d2)]
      [{__ {'var id level}} #f] ;; the only difference from unify/data/data
      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs {0 cover gs/exit {dl1 dl2}})
              (gs/next)]
             [else #f])]
      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}} (cover/trunk/trunk d1 d2)]
      [{{'trunk t k i} __} (cover/trunk/data d1 d2)]
      [{__ {'trunk t k i}} (cover/data/trunk d1 d2)]
      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

;; ;; the equal? of scheme can handle circle
;; (let ([p1 (cons 1 1)]
;;       [p2 (cons 1 1)])
;;   (set-cdr! p1 p1)
;;   (set-cdr! p2 p2)
;;   (list p1 p2 (equal? p1 p2)))
;; ;; => (#0=(1 . #0#) #1=(1 . #1#) #t)

(define (cover/var/data v d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data v d)
    (bs/extend v d)
    #f))

(define (cover/trunk/data t d)
  (let ([result (try-trunk t)])
    (if result
      (cover/data/data result d)
      #f)))

(define (cover/data/trunk d t)
  (let ([result (try-trunk t)])
    (if result
      (cover/data/data d result)
      #f)))

(define (cover/trunk/trunk t1 t2)
  (let ([result1 (try-trunk t1)]
        [result2 (try-trunk t2)])
    (cond [result1 (cover/data/trunk result1 t2)]
          [result2 (cover/trunk/data t1 result2)]
          [else
           ;; when both fail to try-trunk
           ;;   still have chance to syntax equal
           (match {t1 t2}
             [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}}
              (match {(vector-ref k1 0) (vector-ref k2 0)}
                [{{'todo b1 dl1} {'todo b2 dl2}}
                 (cond [(equal? {t1 i1 b1} {t2 i2 b2})
                        (push gs {0 cover gs/exit {dl1 dl2}})
                        (gs/next)]
                       [else #f])])])])))

(define (unify)
  (: -> bool)
  (match (pop gs)
    [{c ex end {dl1 dl2}}
     (cond [(>= c (length dl1))
            (end)
            #t]
           [else
            (let ([d1 (list-ref dl1 c)]
                  [d2 (list-ref dl2 c)])
              (push gs {(+ 1 c) ex end {dl1 dl2}})
              (if (unify/data/data d1 d2)
                (gs/next)
                #f))])]))

(define (unify/data/data d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check
      [{{'bind d v} __} (unify/data/data d d2)]
      [{__ {'bind d v}} (unify/data/data d1 d)]
      ;; var is the hero
      ;; this should pass occur-check
      [{{'var id1 level1} {'var id2 level2}}
       (cond [(var/eq? d1 d2) #t] ;; no self-unify
             [else (unify/var/data d1 d2)])]
      [{{'var id level} __} (unify/var/data d1 d2)]
      [{__ {'var id level}} (unify/var/data d2 d1)]
      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs {0 unify gs/exit {dl1 dl2}})
              (gs/next)]
             [else #f])]
      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}} (unify/trunk/trunk d1 d2)]
      [{{'trunk t k i} __} (unify/trunk/data d1 d2)]
      [{__ {'trunk t k i}} (unify/data/trunk d1 d2)]
      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

;; ;; the equal? of scheme can handle circle
;; (let ([p1 (cons 1 1)]
;;       [p2 (cons 1 1)])
;;   (set-cdr! p1 p1)
;;   (set-cdr! p2 p2)
;;   (list p1 p2 (equal? p1 p2)))
;; ;; => (#0=(1 . #0#) #1=(1 . #1#) #t)

(define (unify/var/data v d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data v d)
    (bs/extend v d)
    #f))

(define (unify/trunk/data t d)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data result d)
      #f)))

(define (unify/data/trunk d t)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data d result)
      #f)))

(define (unify/trunk/trunk t1 t2)
  (let ([result1 (try-trunk t1)]
        [result2 (try-trunk t2)])
    (cond [result1 (unify/data/trunk result1 t2)]
          [result2 (unify/trunk/data t1 result2)]
          [else
           ;; when both fail to try-trunk
           ;;   still have chance to syntax equal
           (match {t1 t2}
             [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}}
              (match {(vector-ref k1 0) (vector-ref k2 0)}
                [{{'todo b1 dl1} {'todo b2 dl2}}
                 (cond [(equal? {t1 i1 b1} {t2 i2 b2})
                        (push gs {0 unify gs/exit {dl1 dl2}})
                        (gs/next)]
                       [else #f])])])])))

(define (rs/exit) (void))

(define (rs/next)
  (match (tos rs)
    [{c ex end jj}
     (ex)]))

(define (compose)
  (match (pop rs)
    [{c ex end jj}
     (cond [(>= c (length jj))
            (end)]
           [else
            (let ([j (list-ref jj c)])
              (push rs {(+ 1 c) ex end jj})
              (compose/jo j)
              (rs/next))])]))

(define (compose/jo j)
  (case (car j)
    ['var           (compose/var j)]
    ['call          (compose/call j)]
    ['apply         (compose/apply j)]
    ['ex-bind       (compose/ex-bind j)]
    [__             (push ds j)]))

(define (compose/jojo jj) (for-each compose/jo jj))

(define (compose/var j)
  ;; (if (var/fresh? j)
  ;;   (bs/extend-new j))
  (let ([d (bs/deep j)])
    (push ds d)))

(define (type/input-number t)
  (match t
    [{'arrow ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda () (compose/jojo ajj))))]))

(define (type/output-number t)
  (match t
    [{'arrow ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda () (compose/jojo sjj))))]))

(define (compose/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'compose/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type pt n nl}
            (let ([len (type/input-number pt)])
              (push ds {'cons n (fetch ds len)}))]
           [{'meaning-data pt n n0}
            (let ([len (type/input-number pt)])
              (push ds {'cons n (fetch ds len)}))]
           [{'meaning-lambda pt pb}
            (compose/function pt pb)])))]))

(define (compose/function t b)
  ;; note that
  ;;   when create-trunk-list
  ;;   it needs to know the type to get input-number & output-number
  (let ([sjj (compose/try-body b)])
    (if sjj
      (push rs {0 compose rs/next sjj})
      (let ([dl (fetch ds (type/input-number t))])
        (put ds (create-trunk-list t b dl))))))

(define (compose/try-body b)
  (: body -> (or #f sjj))
  ;; return #f on fail
  ;; return sjj on success with commit
  (match b
    [{} #f]
    [({'arrow ajj sjj} . r)
     (let* ([ds0 ds]
            [bs0 bs]
            [gs0 gs])
       (let* ([dl1 (call-with-output-to-new-ds
                    (lambda ()
                      (push rs {0 compose rs/exit ajj})
                      (rs/next)))]
              [dl2 (fetch ds (length dl1))])
         (push bs '(commit-point))
         (push gs {0 cover bs/commit {dl1 dl2}})
         (if (gs/next)
           sjj
           (let ()
             (set! ds ds0)
             (set! bs bs0)
             (set! gs gs0)
             (compose/try-body r)))))]))

(define (create-trunk-list t b dl)
  (let ([k (vector {'todo b dl})])
    (reverse
     (map (lambda (i) {'trunk t k i})
       (genlist (type/output-number pt))))))

(define (compose/apply j)
  (match (bs/walk (pop ds))
    [{'lambda t b}
     (compose/function t b)]
    [__ (orz 'compose/apply
          ("can not handle jo : ~a~%" j))]))

(define (compose/ex-bind j)
  (match j
    [{'ex-bind j vl}
     (let* ([dl (call-with-output-to-new-ds
                 (lambda ()
                   (compose/jo j)))]
            [d (car dl)])
       (if (not (eq? (length dl) 1))
         (orz 'compose/ex-bind
           ("jo should return one data~%")
           ("but this jo does not : ~a~%" j))
         (for-each (lambda (v)
                     (bs/extend-up v d)
                     (push ds {'bind d v}))
                   vl)))]))

(define (cut)
  (match (pop rs)
    [{c ex end jj}
     (cond [(>= c (length jj))
            (end)]
           [else
            (let ([j (list-ref jj c)])
              (push rs {(+ 1 c) ex end jj})
              (cut/jo j)
              (rs/next))])]))

(define (cut/jo j)
  (case (car j)
    ['var           (cut/var j)]
    ['call          (cut/call j)]
    ['apply         (cut/apply j)]
    ['arrow         (cut/arrow j)]
    ['lambda        (cut/lambda j)]
    ['ex-bind       (cut/ex-bind j)]))

(define (cut/var j)
  ;; (if (var/fresh? j)
  ;;   (bs/extend-new j))
  (let ([d (bs/deep j)])
    (let ([found-d (bs/find-up j)])
      (if found-d
        (push ds found-d)
        (match j
          [{'var id level}
           (push ds {'var id (+ 1 level)})])))))

(define (cut/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'cut/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type pt n nl}
            (cut/type pt)]
           [{'meaning-data pt n n0}
            (cut/type pt)]
           [{'meaning-lambda pt pb}
            (cut/type pt)])))]))

(define (cut/type a)
  (: arrow -> !)
  (match a
    [{'arrow ajj sjj}
     (let* ([dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs {0 compose rs/exit ajj})
                    (rs/next)))]
            [dl2 (fetch ds (length dl1))])
       (push bs '(commit-point))
       (push gs {0 unify bs/commit {dl1 dl2}})
       (if (gs/next)
         (compose/jojo sjj)
         (orz 'cut/type
           ("fail on unify~%"))))]))

(define (cut/apply j)
  (match (bs/walk (pop ds))
    [{'arrow ajj sjj}
     (cut/type {'arrow ajj sjj})]
    [__ (orz 'cut/apply
          ("can not handle jo : ~a~%" j))]))

(define (cut/arrow j)
  (orz 'cut/arrow
    ("can not handle arrow as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define (cut/lambda j)
  (match j
    [{'lambda {'arrow ajj sjj} b}
     (push ds {'arrow ajj sjj})]
    [__
     (orz 'cut/lambda
       ("can not handle jo : ~a~%" j)
       ("for it is meaning less to write a lambda without local-vars~%"))]))

(define (cut/ex-bind j)
  (orz 'cut/ex-bind
    ("can not handle ex-bind as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define (call-with-output-to-new-ds f)
  (: function -> new-ds)
  (let ([ds-backup ds])
    (set! ds '())
    (f)
    (let ([new-ds ds])
      (set! ds ds-backup)
      new-ds)))

(define print-define-flag #f)
(define (print-define+) (set! print-define-flag #t))
(define (print-define-) (set! print-define-flag #f))

(define type-check-flag #f)
(define (type-check+) (set! type-check-flag #t))
(define (type-check-) (set! type-check-flag #f))

(define-macro (def name body)
  `($def (quote ,name) (quote ,body)))

(define ($def name body)
  (let ([key (car body)])
    ((find-key key) name body)))

(define key-record '())

(define (new-key key fun)
  (set! key-record
        (cons (cons key fun)
              key-record)))

(define (find-key key)
  (let ([found (assq key key-record)])
    (if found
      (cdr found)
      (orz 'find-key
        ("can not find key : ~a~%" key)))))

(define (def-lambda n body)
  (let* ([a (compile-arrow (cadr body))]
         [al (compile-body (cddr body))]
         [meaning (list 'meaning-lambda a al)])
    (push ns (cons n meaning))
    (if type-check-flag
      (type-check/function a al))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-lambda>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-lambda>\n")
        (display "\n")))))

(new-key 'lambda def-lambda)

(define (pair-even-list l)
  (match l
    [{} '()]
    [{x} (orz 'pair-even-list
           ("meet uneven list with ending : ~a~%" x))]
    [(x y . z) (cons (cons x y)
                     (pair-even-list z))]))

(define (def-type n body)
  (let* ([a (compile-arrow (cadr body))]
         [pl (pair-even-list (cddr body))]
         [nl (map car pl)]
         [meaning (list 'meaning-type a n nl)])
    (push ns (cons n meaning ))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-type>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-type>\n")
        (display "\n")))
    (for-each (lambda (p) (def-data n p))
              pl)))

(new-key 'type def-type)

(define (def-data n0 p)
  (let* ([n (car p)]
         [a (compile-arrow (cdr p))]
         [meaning (list 'meaning-data a n n0)])
    (push ns (cons n meaning))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-data>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-data>\n")
        (display "\n")))))

(define (compile-jo jo)
  (define (var? v)
    (and (symbol? v)
         (equal? ":" (substring (symbol->string v) 0 1))))
  (define (call? v)
    (and (symbol? v)
         (not (eq? ":" (substring (symbol->string v) 0 1)))))
  (define (apply? v)
    (eq? v 'apply))
  (define (arrow? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) '->)))
  (define (lambda? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) 'lambda)))
  (define (ex-bind? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) ':)))
  (cond [(var? jo)                (list 'var jo)]
        [(call? jo)               (list 'call jo)]
        [(apply? jo)              (list 'apply)]
        [(arrow? jo)              (compile-arrow jo)]
        [(lambda? jo)             (compile-lambda jo)]
        [(ex-bind? jo)            (compile-ex-bind jo)]))

(define (compile-jojo jojo)
  (map compile-jo jojo))

(define (compile-ex-bind jo)
  (list 'ex-bind
        (compile-jo (cadr jo))
        (compile-jojo (cddr jo))))

(define (compile-arrow a)
  (match a
    [{'-> ac sc}
     {'arrow (compile-jojo ac) (compile-jojo sc)}]))

(define (compile-body b)
  (map compile-arrow b))

(define (compile-lambda l)
  (list 'lambda
        (compile-arrow (cadr l))
        (compile-body (cddr l))))

(define id/counter 0)

(define (id/new n ls)
  (set! id/counter (+ 1 id/counter))
  (vector (cons n id/counter) ls))

(define-macro (app s)
  `($app (quote ,s)))

(define ($app s)
  (compose/jojo (compile/jojo s)))

(define (type-check/function t b)
  (: type body -> bool)
  (match t
    [{'arrow tajj tsjj}
     (for-each (lambda (a) (type-check/arrow t a))
               b)]
    [__ (orz 'type-check/function
          ("type of function must be arrow~%")
          ("type : ~a~%" t))]))

(define (type-check/arrow ta a)
  (: type-arrow arrow -> bool)
  (match {ta a}
    [{{'arrow tajj tsjj} {'arrow ajj sjj}}
     (let* ([dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs {0 compose rs/exit tajj})
                    (rs/next)))]
            [dl2 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs {0 cut rs/exit ajj})
                    (rs/next)))])
       (push gs {0 unify gs/exit {dl1 dl2}})
       (cond [(gs/next)
              (let* ([dl3 (call-with-output-to-new-ds
                           (lambda ()
                             (push rs {0 compose rs/exit tsjj})
                             (rs/next)))]
                     [dl4 (call-with-output-to-new-ds
                           (lambda ()
                             (push rs {0 cut rs/exit sjj})
                             (rs/next)))])
                (push gs {0 cover gs/exit {dl3 dl4}})
                (cond [(gs/exit)
                       #t]
                      [else (orz 'type-check/arrow
                              ("cover fail~%"))]))]
             [else (orz 'type-check/arrow
                     ("unify fail~%"))]))]))
