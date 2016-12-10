(: syntax
   var                :name
   fvar               ::name
   bind               %:name
   call               name
   arrow              (-> [syntax ...] [syntax ...])
   lambda             (lambda arrow arrow ...)
   apply              @)

(: jo
   var                name
   fvar               name
   bind               name
   call               name
   arrow              {var-name ...} {fvar-name ...} {jo ...} {jo ...}
   lambda             arrow {arrow ...}
   apply)

(: data
   uni-var            id level
   uni-bind           uni-var data
   cons               name {data ...}
   uni-arrow          {var-name ...} {(fvar-name . uni-var) ...} {jo ...} {jo ...}
   uni-lambda         uni-arrow {uni-arrow ...}
   trunk              adl sdl (vector trunky) index)

(: id                 (vector (name . counter) ls))
(: ls                 {(level . data) ...})
;; vector thus unique
;; ls (level binding-list) in id for commit

(: trunky
   todo               {uni-arrow ...} {data ...}
   kvar               kv {data ...}
   done               {data ...})

(: kv
   uni-var ~
   uni-lambda ~)

(define-syntax debug0
  (syntax-rules ()
    [(debug0 who c ...)
     (let ()
       (print-ds)
       (print-rs)
       (print-gs)
       (print-bs)
       (error who (cating ("~%") c ...)))]))

(define-macro (push s v) `(set! ,s (cons ,v ,s)))

(define-macro (push-list s l)
  `(set! ,s (append ,l ,s)))

(define-macro (tos s)
  `(if (null? ,s)
     (debug0 'tos
       ("stack is empty : ~a~%" (quote ,s)))
     (car ,s)))

(define-macro (pop s)
  (let ([v (gensym "pop/v")])
    `(if (null? ,s)
       (debug0 'pop
         ("stack is empty : ~a~%" (quote ,s)))
       (let ([,v (car ,s)])
         (set! ,s (cdr ,s))
         ,v))))

(define-macro (pop-list s n)
  (let ([v (gensym "fetch/v")])
    `(if (< (length ,s) ,n)
       (debug0 'pop-list
         ("stack is not long enough : ~a~%" (quote ,s))
         ("stack length : ~a~%" (length ,s))
         ("need length : ~a~%" ,n))
       (let ([,v (take ,s ,n)])
         (set! ,s (drop ,s ,n))
         ,v))))

(: ns {(name . meaning) ...})
(: ds {data ...})
(: bs {(id . ls) ...})
(: rs {rsp ...})
(: gs {gsp ...})

(define (print-jo j)
  ;; (match j
  ;;   []
  ;;   [])
  (display j)
  (display "\n"))

(define (print-data d)
  (match d
    [('uni-var . __)
     (cat ("~a " d))]
    [('uni-bind . __)
     (cat ("~a " d))]
    [{'cons n dl}
     (if3 [(null? dl)]
          [(cat ("~a " n))]
          [(cat ("[ ~a " n))
           (map print-data dl)
           (cat ("] "))])]
    [('uni-arrow . __)
     (cat ("~a " d))]
    [('uni-lambda . __)
     (cat ("~a " d))]
    [('trunk . __)
     (cat ("~a " d))]))

(define (print-bsp bsp)
  (display bsp)
  (display "\n"))

(define (print-nsp nsp)
  (display nsp)
  (display "\n"))

(define (print-ds) (map print-data ds) (display "\n"))
(define (print-bs) (map print-bsp  bs) (display "\n"))
(define (print-ns) (map print-nsp  ns) (display "\n"))

(define (print-rs)
  (cat ("~%")
       ("<rs>~%"))
  (map (lambda (o)
         (@ o 'print))
    rs)
  (cat ("</rs>~%")
       ("~%")))

(define (print-gs)
  (cat ("~%")
       ("<gs>~%"))
  (map (lambda (o)
         (@ o 'print))
    gs)
  (cat ("</gs>~%")
       ("~%")))

(define (print-env)
  (print-ds)
  (print-rs)
  (print-gs)
  (print-bs))

(define (clear-env)
  (set! ds '())
  (set! rs '())
  (set! gs '())
  (set! bs '()))

(define (clear-world)
  (clear-env)
  (set! ns '()))

;; name-stack
(define ns '())
(: ns {(name . meaning) ...})

(: meaning
   meaning-type       uni-arrow name {name ...}
   meaning-data       uni-arrow name name
   meaning-lambda     uni-arrow {uni-arrow ...})

(define (compile-arrow a)
  (pass2-arrow (pass1-arrow a)))

(define (compile-uni-arrow a)
  (match (compile-arrow a)
    [{'arrow nl fnl ajj sjj}
     (if (null? fnl)
       {'uni-arrow nl '() ajj sjj}
       (debug0 'compile-uni-arrow
         ("the free-var-name-list of arrow is not empty~%")
         ("free-var-name-list : ~a~%" fnl)
         ("arrow : ~a~%" a)))]))

(define (compile-jo j)
  (pass2-jo (pass1-jo j)))

(define (pass1-jo jo)
  (define (var? v)
    (and (symbol? v)
         (eq? ': (symbol-car v))
         (not (eq? ': (symbol-car (symbol-cdr v))))))
  (define (fvar? v)
    (and (symbol? v)
         (eq? ': (symbol-car v))
         (eq? ': (symbol-car (symbol-cdr v)))))
  (define (bind? v)
    (and (symbol? v)
         (eq? '% (symbol-car v))
         (eq? ': (symbol-car (symbol-cdr v)))))
  (define (apply? v)
    (eq? v '@))
  (define (call? v)
    (and (symbol? v)
         (not (eq? ': (symbol-car v)))
         (not (eq? '% (symbol-car v)))))
  (define (arrow? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) '->)))
  (define (lambda? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) 'lambda)))
  (cond [(var? jo)                (list 'var jo)]
        [(fvar? jo)               (list 'fvar (symbol-cdr jo))]
        [(bind? jo)               (list 'bind (symbol-cdr jo))]
        [(apply? jo)              (list 'apply)]
        [(call? jo)               (list 'call jo)]
        [(arrow? jo)              (pass1-arrow jo)]
        [(lambda? jo)             (list 'lambda
                                        (pass1-arrow (car (cdr jo)))
                                        (map pass1-arrow (cdr (cdr jo))))]))

(define (pass1-arrow a)
  (match a
    [{'-> ac sc}
     {'arrow (map pass1-jo ac) (map pass1-jo sc)}]))

(define (pass2-jo jo)
  (match jo
    [{'arrow ac sc} (pass2-arrow jo)]
    [{'lambda a al} {'lambda (pass2-arrow a) (map pass2-arrow al)}]
    [__ jo]))

(define (pass2-arrow a)
  (match a
    [{'arrow ac sc}
     {'arrow (jojo->var-list (append ac sc))
             (jojo->fvar-list (append ac sc))
             (map pass2-jo ac) (map pass2-jo sc)}]))

(define (jojo->var-list l)
  (define (one vl n)
    (if (member n vl)
      vl
      (cons n vl)))
  (define (more vl jo)
    (match jo
      [{'var n}         (one vl n)]
      [{'fvar n}        vl]
      [{'bind n}        (one vl n)]
      [{'call n}        vl]
      [{'apply}         vl]
      [{'arrow ac sc}   (loop vl (append ac sc))]
      [{'lambda a al}   (arrow-loop vl (cons a al))]))
  (define (arrow-loop vl l)
    (if (null? l)
      vl
      (match (car l)
        [{'arrow ac sc}
         (arrow-loop (loop vl (append ac sc)) (cdr l))])))
  (define (loop vl l)
    (if (null? l)
      vl
      (loop (more vl (car l)) (cdr l))))
  (loop '() l))

(define (jojo->fvar-list l)
  (define (one vl n)
    (if (member n vl)
      vl
      (cons n vl)))
  (define (more vl jo)
    (match jo
      [{'var n}         vl]
      [{'fvar n}        (one vl n)]
      [{'bind n}        vl]
      [{'call n}        vl]
      [{'apply}         vl]
      [{'arrow ac sc}   (loop vl (append ac sc))]
      [{'lambda a al}   (arrow-loop vl (cons a al))]))
  (define (arrow-loop vl l)
    (if (null? l)
      vl
      (match (car l)
        [{'arrow ac sc}
         (arrow-loop (loop vl (append ac sc)) (cdr l))])))
  (define (loop vl l)
    (if (null? l)
      vl
      (loop (more vl (car l)) (cdr l))))
  (loop '() l))

;; data-stack
(define ds '())
(: ds {data ...})

(define (call-with-output-to-new-ds f)
  (: function -> new-ds)
  (let ([ds-backup ds])
    (set! ds '())
    (f)
    (let ([new-ds ds])
      (set! ds ds-backup)
      new-ds)))

;; binding-stack
(define bs '())
(: bs {(id . ls) ...})

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

(define (bs/extend uv d)
  (: uni-var data -> !)
  (match uv
    [{'uni-var id level}
     (let ([found/ls (assq id bs)])
       (if found/ls
         (set! bs (substitute `(,id . ((,level . ,d) . ,(cdr found/ls)))
                              (lambda (pair) (eq? (car pair) id))
                              bs))
         (push bs `(,id . ((,level . ,d))))))]))

(define (bs/extend-up uv d)
  (: uni-var data -> !)
  (match uv
    [{'uni-var id level}
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
(define (bs/extend-new uv d)
  (: uni-var data -> !)
  (match uv
    [{'uni-var id level}
     (push bs `(,id . ((,level . ,d))))]))

;; (define (bs/extend-new v d)
;;   (: var data -> !)
;;   (match v
;;     [{'uni-var id level}
;;      (let ([found/ls (assq id bs)])
;;        (if found/ls
;;          (void)
;;          (push bs `(,id . ()))))]))

(define (id->ls id)
  (vector-ref id 1))

(define (bs/find uv)
  (: uni-var -> (or data #f))
  (match uv
    [{'uni-var id level}
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

(define (bs/find-up uv)
  (: uni-var -> (or data #f))
  (match uv
    [{'uni-var id level}
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

(define (bs/walk d)
  (: data -> data)
  (match d
    [{'uni-var id level}
     (let ([found (bs/find d)])
       (if found
         (bs/walk found)
         d))]
    [__ d]))

(define (bs/deep d)
  (: data -> data)
  (let ([d (bs/walk d)])
    (match d
      ;; a uni-var is fresh after bs/walk
      [{'cons n dl}          {'cons n (bs/deep-list dl)}]
      [{'uni-bind uv d}      {'bind (bs/deep uv) (bs/deep d)}]
      [{'trunk adl sdl k i}  {'trunk (bs/deep-list adl)
                                     (bs/deep-list sdl)
                                     (bs/deep-trunky k) i}]
      [__                    d])))

(define (bs/deep-list dl)
  (map (lambda (x) (bs/deep x)) dl))

(define (bs/deep-trunky k)
  (vector-set!
    k 0
    (match (vector-ref k 0)
      [{'todo al dl} {'todo al (bs/deep-list dl)}]
      [{'kvar kv dl} {'kvar (bs/deep kv) (bs/deep-list dl)}]
      [{'done dl}    {'done (bs/deep-list dl)}]))
  k)

(define (uni-var/fresh? uv)
  (: uni-var -> bool)
  (equal? (bs/walk uv)
          uv))

(define (uni-var/eq? v1 v2)
  (match {v1 v2}
    [{{'uni-var id1 level1} {'uni-var id2 level2}}
     (and (eq? id1 id2)
          (eq? level1 level2))]))

;; return-stack
(define rs '())

(define (rs/exit) (void))

(define (rs/next)
  ((^ (tos rs) 'ex)))

(define rsp-proto
  (new-object
   (pair-list
    'c      0
    'ex     '(explainer)
    'end    rs/exit
    'vrc    '(var record)
    'jj     '(jojo))
   (pair-list
    'print
    (lambda (o)
      (cat ("  <rsp>~%")
           ("    :counter: ~a~%"       (^ o 'c))
           ("    :explainer: ~a~%"     (^ o 'ex))
           ("    :ender: ~a~%"         (^ o 'end))
           ("    :var-record:~%~a~%"   (^ o 'vrc))
           ("    :jojo:~%~a~%"         (^ o 'jj))
           ("  </rsp>~%"))))))

(define id/counter 0)

;; (define (id/new n ls)
;;   (set! id/counter (+ 1 id/counter))
;;   (vector (cons n id/counter) ls))

(define (id/new n)
  (set! id/counter (+ 1 id/counter))
  (vector (cons n id/counter) '()))

(define (nl->vrc nl)
  (map (lambda (n)
         (cons n (list 'uni-var (id/new n) 0)))
    nl))

(define (name->uni-var n)
  (let* ([rsp (tos rs)]
         [found (assq n (^ rsp 'vrc))])
    (if found
      (cdr found)
      (debug0 'name->uni-var
        ("can not find name : ~a~%" n)
        ("rsp var record : ~a~%" (^ rsp 'vrc))))))

(define (name->fvar-record n)
  (let* ([rsp (tos rs)]
         [found (assq n (^ rsp 'vrc))])
    (if found
      (cons n (cdr found))
      (debug0 'name->fvar-record
        ("can not find name : ~a~%" n)
        ("rsp var record : ~a~%" (^ rsp 'vrc))))))

(define (fnl->frc fnl)
  (map name->fvar-record fnl))

(define (compose)
  (let* ([rsp (pop rs)]
         [c   (^ rsp 'c)]
         [ex  (^ rsp 'ex)]
         [end (^ rsp 'end)]
         [jj  (^ rsp 'jj)])
    (if3 [(>= c (length jj))]
         [(end)]
         [(push rs (% rsp 'c (+ 1 c)))
          (compose/jo (list-ref jj c))
          (rs/next)])))

(define (compose/jo j)
  (case (car j)
    ['var           (compose/var j)]
    ['fvar          (compose/var j)]
    ['bind          (compose/bind j)]
    ['call          (compose/call j)]
    ['arrow         (compose/arrow j)]
    ['lambda        (compose/lambda j)]
    ['apply         (compose/apply j)]))

(define (compose/var j)
  ;; (if (uni-var/fresh? j)
  ;;   (bs/extend-new j))
  (let* ([n (match j
              [{'var n} n]
              [{'fvar n} n])]
         [uv (name->uni-var n)]
         [d (bs/deep uv)])
    (push ds d)))

(define (compose/bind j)
  (match j
    [{'bind n}
     (let* ([uv (name->uni-var n)]
            [d (pop ds)])
            (bs/extend-up uv d)
       (push ds {'uni-bind uv d}))]))

(define (compose/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (debug0 'compose/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type a n nl}
            (let ([len (type/input-number a)])
              (push ds {'cons n (pop-list ds len)}))]
           [{'meaning-data a n n0}
            (let ([len (type/input-number a)])
              (push ds {'cons n (pop-list ds len)}))]
           [{'meaning-lambda a al}
            (compose/body a al)])))]))

(: [for the first covering arrow]
   (push gs {cover commit
                   <data-on-the-stack>
                   (push rs {compose exit <ac>})})
   (cond [(succ?)
          (commit)
          (push rs {compose exit <sc>})
          (exit)]
         [(fail?)
          (undo)
          (loop)])
   (if (all-fail?) (form-trunk)))

(define (compose/body t b)
  ;; note that
  ;;   when create-trunk-list
  ;;   it needs to know the type to get input-number & output-numbe
  (: type body -> [:ds (or [result of body]
                           [trunk generated by body])])
  (match (compose/try-body b)
    [{sjj vrc}
     (push rs (% rsp-proto
                 'ex   compose
                 'end  rs/exit
                 'vrc  vrc
                 'jj   sjj))
     (rs/next)]
    [#f (let ([dl (pop-list ds (type/input-number t))])
          (push-list ds (create-trunk-list t b dl)))]))

(define (compose/try-body b)
  (: body -> (or #f {sjj vrc}))
  ;; return #f on fail
  ;; return sjj on success with commit
  (match b
    [{} #f]
    [({'uni-arrow nl frc ajj sjj} . r)
     (let* ([vrc (append frc (nl->vrc nl))]
            [ds0 ds]
            [bs0 bs]
            [gs0 gs]
            [dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'ex   compose
                                'end  rs/exit
                                'vrc  vrc
                                'jj   ajj))
                    (rs/next)))]
            [dl2 (pop-list ds (length dl1))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex   (unify 'cover)
                         'end  bs/commit
                         'dl+  dl1
                         'dl-  dl2))
             (gs/next)]
            [{sjj vrc}]
            [(set! ds ds0)
             (set! bs bs0)
             (set! gs gs0)
             (compose/try-body r)]))]))

;; ><><><
;; need after-d2t-unify for adl and dl
(define (create-trunk-list t b dl)
  (match t
    [{'uni-arrow nl frc ajj sjj}
     (let* ([vrc (append frc (nl->vrc nl))]
            [adl (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'ex   compose
                                'end  rs/exit
                                'vrc  vrc
                                'jj   sjj))
                    (rs/next)))]
            [sdl (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'ex   compose
                                'end  rs/exit
                                'vrc  vrc
                                'jj   sjj))
                    (rs/next)))]
            [k (match b
                 [('uni-var . __)
                  (vector {'kvar b dl})]
                 [__
                  (vector {'todo b dl})])])
       (reverse
        (map (lambda (i) {'trunk adl sdl k i})
          (genlist (length sdl)))))]))

(define (type/input-number t)
  (match t
    [{'uni-arrow nl frc ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda ()
                (push rs (% rsp-proto
                            'ex   compose
                            'end  rs/exit
                            'vrc  (append frc (nl->vrc nl))
                            'jj  ajj))
                (rs/next))))]))

(define (type/output-number t)
  (match t
    [{'uni-arrow nl frc ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda ()
                (push rs (% rsp-proto
                            'ex   compose
                            'end  rs/exit
                            'vrc  (append frc (nl->vrc nl))
                            'jj  sjj))
                (rs/next))))]))

(define (arrow->uni-arrow a)
  (match a
    [{'arrow nl fnl ajj sjj}
     {'uni-arrow nl (fnl->frc fnl) ajj sjj}]))

(define (compose/arrow j)
  (push ds (arrow->uni-arrow j)))

(define (compose/lambda j)
  (match j
    [{'lambda a al}
     (push ds {'uni-lambda (arrow->uni-arrow a)
                           (map arrow->uni-arrow al)})]))

;; note that
;;   compose/apply can form trunk too
;;   the body of trunk formed by apply is uni-var
(define (compose/apply j)
  (let ([d (bs/walk (pop ds))])
    (match d
      [{'uni-lambda t b}
       (compose/body t b)]
      [{'uni-var id level}
       (let* ([t (d2t d)]
              [b d])
         (match t
           [{'uni-arrow nl frc ajj sjj}
            (let ([dl (pop-list ds (type/input-number t))])
              (push-list ds (create-trunk-list t b dl)))]
           [__ (debug0 'compose/apply
                 ("compose/apply meet uni-var whoes type is not uni-arrow~%")
                 ("uni-var : ~a~%" d)
                 ("type of uni-var : ~a~%" t))]))]
      [__
       (debug0 'compose/apply
         ("compose/apply can not apply data~%")
         ("data : ~a~%" d))])))

(define (cut)
  (let* ([rsp (pop rs)]
         [c   (^ rsp 'c)]
         [ex  (^ rsp 'ex)]
         [end (^ rsp 'end)]
         [jj  (^ rsp 'jj)])
    (if3 [(>= c (length jj))]
         [(end)]
         [(push rs (% rsp 'c (+ 1 c)))
          (cut/jo (list-ref jj c))
          (rs/next)])))

(define (cut/jo j)
  (case (car j)
    ['var           (cut/var j)]
    ['fvar          (cut/var j)]
    ['bind          (cut/bind j)]
    ['call          (cut/call j)]
    ['arrow         (cut/arrow j)]
    ['lambda        (cut/lambda j)]
    ['apply         (cut/apply j)]))

(define (cut/var j)
  ;; (if (uni-var/fresh? j)
  ;;   (bs/extend-new j))
  (let* ([n (match j
              [{'var n} n]
              [{'fvar n} n])]
         [uv (name->uni-var n)]
         [d (bs/deep uv)])
    (let ([found-d (bs/find-up uv)])
      (if found-d
        (push ds found-d)
        (match uv
          [{'uni-var id level}
           (push ds {'uni-var id (+ 1 level)})])))))

(define (cut/bind j)
  (debug0 'cut/bind
    ("bind can not occur in body-arrow~%")
    ("bind : ~a~%" j)))

(define (cut/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (debug0 'cut/call
           ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type a n nl} (cut/type a)]
           [{'meaning-data a n n0} (cut/type a)]
           [{'meaning-lambda a al} (cut/type a)])))]))

(define (cut/type a)
  (match a
    [{'uni-arrow nl frc ajj sjj}
     (let* ([vrc (append frc (nl->vrc nl))]
            [dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'ex   compose
                                'end  rs/exit
                                'vrc  vrc
                                'jj   ajj))
                    (rs/next)))]
            [dl2 (pop-list ds (length dl1))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex   (unify 'unify)
                         'end  bs/commit
                         'dl+  dl1
                         'dl-  dl2))
             (gs/next)]
            [(push rs (% rsp-proto
                         'ex   compose
                         'end  rs/exit
                         'vrc  vrc
                         'jj  sjj))
             (rs/next)]
            [(debug0 'cut/type
               ("fail on unify~%"))]))]))

(define (cut/arrow j)
  (debug0 'cut/arrow
    ("arrow can not occur in body-arrow~%")
    ("arrow : ~a~%" j)))

(define (cut/lambda j)
  (match j
    [{'lambda a al}
     (compose/arrow a)]))

(define (cut/apply j)
  (let ([d (bs/walk (pop ds))])
    (match d
      [{'uni-arrow vnl fvnl ajj sjj}
       (cut/type {'uni-arrow vnl fvnl ajj sjj})]
      [__
       (debug0 'cut/apply
         ("cut/apply can not apply data~%")
         ("data : ~a~%" d)
         ("jo : ~a~%" j))])))

;; goal-stack
;;   binding-stack is to record solution of equations in goal-stack
(define gs '())

(define (gs/exit) (void))

(define (gs/next)
  (: -> bool)
  ((^ (tos gs) 'ex)))

(define gsp-proto
  (new-object
   (pair-list
    'c      0
    'ex     '(explainer)
    'end    gs/exit
    'dl+    '(data-list)
    'dl-    '(data-list))
   (pair-list
    'print
    (lambda (o)
      (cat ("  <gsp>~%")
           ("    :counter: ~a~%"        (^ o 'c))
           ("    :explainer: ~a~%"      (^ o 'ex))
           ("    :ender: ~a~%"          (^ o 'end))
           ("    :data-list+: ~%~a~%"   (^ o 'dl+))
           ("    :data-list-: ~%~a~%"   (^ o 'dl-))
           ("  </gsp>~%"))))))

(: (let ([p1 (cons 1 1)]
         [p2 (cons 1 1)])
     (set-cdr! p1 p1)
     (set-cdr! p2 p2)
     (list p1 p2 (equal? p1 p2))))
(: (#0=(1 . #0#) #1=(1 . #1#) #t))

(define (unify m)
  (: method -> (-> bool))
  (lambda ()
    (let* ([gsp (pop gs)]
           [c   (^ gsp 'c)]
           [ex  (^ gsp 'ex)]
           [end (^ gsp 'end)]
           [dl1 (^ gsp 'dl+)]
           [dl2 (^ gsp 'dl-)])
      (if3 [(>= c (length dl1))]
           [(end)
            #t]
           [(push gs (% gsp 'c (+ 1 c)))
            (if (unify/data/data m
                                 (list-ref dl1 c)
                                 (list-ref dl2 c))
              (gs/next)
              #f)]))))

(define (unify/data/data m d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check
      [{{'uni-bind uv d} __} (unify/data/data m d d2)]
      [{__ {'uni-bind uv d}} (unify/data/data m d1 d)]

      ;; var is the hero
      ;; this should pass occur-check
      [{{'uni-var id1 level1} {'uni-var id2 level2}}
       (cond [(uni-var/eq? d1 d2) #t] ;; no self-unify
             [else (unify/uni-var/data m d1 d2)])]

      [{{'trunk adl sdl k i} {'uni-var id level}} (unify/trunk/uni-var m d1 d2)]
      [{{'uni-var id level} {'trunk adl sdl k i}} (unify/uni-var/trunk m d1 d2)]

      [{{'uni-var id level} __} (unify/uni-var/data m d1 d2)]
      [{__ {'uni-var id level}}
       (case m
         ['cover #f]
         ['unify (unify/uni-var/data m d2 d1)])]

      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs (% gsp-proto
                          'ex (unify 'unify)
                          'end gs/exit
                          'dl+ dl1
                          'dl- dl2))
              (gs/next)]
             [else #f])]

      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk adl1 sdl1 k1 i1} {'trunk adl2 sdl2 k2 i2}}
       (unify/trunk/trunk m d1 d2)]
      [{{'trunk adl sdl k i} __} (unify/trunk/data m d1 d2)]
      [{__ {'trunk adl sdl k i}} (unify/data/trunk m d1 d2)]

      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

(define (unify/uni-var/data m uv d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data uv d)
    (bs/extend uv d)
    #f))

(define (unify/trunk/uni-var m t uv)
  (: trunk fresh-uni-var -> bool)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data m result uv)
      (case m
        ['cover #f]
        ['unify (unify/data/uni-var m t uv)]))))

(define (unify/uni-var/trunk m uv t)
  (: fresh-uni-var trunk -> bool)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data m uv result)
      (unify/uni-var/data m uv t))))

(define (unify/trunk/data m t d)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data m result d)
      #f)))

(define (unify/data/trunk m d t)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data m d result)
      #f)))

(define (unify/trunk/trunk m t1 t2)
  (let ([result1 (try-trunk t1)]
        [result2 (try-trunk t2)])
    (cond [result1 (unify/data/trunk m result1 t2)]
          [result2 (unify/trunk/data m t1 result2)]
          [else
           ;; when both fail to try-trunk
           ;;   still have chance to syntax equal
           (match {t1 t2}
             [{{'trunk adl1 sdl1 k1 i1} {'trunk adl2 sdl2 k2 i2}}
              (match {(vector-ref k1 0) (vector-ref k2 0)}
                [{{'todo b1 dl1} {'todo b2 dl2}}
                 (if3 [(equal? {adl1 sdl1 i1 b1}
                               {adl2 sdl2 i2 b2})]
                      [(push gs (% gsp-proto
                                   'ex (unify 'unify)
                                   'end gs/exit
                                   'dl+ dl1
                                   'dl- dl2))
                       (gs/next)]
                      [#f])]
                ;; maybe ><><>< handle 'kvar here
                ;; [{{'kvar kv1 dl1} {'kvar kv2 dl2}}
                ;;  ]
                [__ #f])])])))

;; ><><><
;; need bind-unify for adl of cons and dl
(define (d2t d)
  (define (a->sdl a)
    (match a
      [{'uni-arrow nl frc ajj sjj}
       (let* ([vrc (append frc (nl->vrc nl))]
              [adl (call-with-output-to-new-ds
                    (lambda ()
                      (push rs (% rsp-proto
                                  'ex   compose
                                  'end  rs/exit
                                  'vrc  vrc
                                  'jj   sjj))
                      (rs/next)))]
              [sdl (call-with-output-to-new-ds
                    (lambda ()
                      (push rs (% rsp-proto
                                  'ex   compose
                                  'end  rs/exit
                                  'vrc  vrc
                                  'jj   sjj))
                      (rs/next)))])
         sdl)]))
  (match d
    [{'uni-var id level} (bs/walk {'uni-var id (+ 1 level)})]
    [{'uni-bind uv d1} d1]
    [{'cons n dl}
     (let ([found (assq n ns)])
       (if (not found)
         (debug0 'd2t ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type a n nl}
            (car (a->sdl a))]
           [{'meaning-data a n n0}
            (car (a->sdl a))]
           [{'meaning-lambda a al}
            (debug0 'd2t
              ("found a lambda from cons name : ~a~%" n)
              ("lambda type : ~a~%" a)
              ("lambda body : ~a~%" al))])))]
    [('uni-arrow . __)
     (debug0 'd2t
       ("can not infer type from uni-arrow : ~a~%" d))]
    [{'uni-lambda a al} a]
    [{'trunk adl sdl k i}
     ;; info about special branch is not needed
     ;;   thus no need to try-trunk
     ;; info about the dl is needed
     ;;   it is already handled when creating the trunk
     (list-ref sdl i)]))

(define (up-unify m)
  (: method -> (-> bool))
  (lambda ()
    (let* ([gsp (pop gs)]
           [c   (^ gsp 'c)]
           [ex  (^ gsp 'ex)]
           [end (^ gsp 'end)]
           [dl1 (^ gsp 'dl+)]
           [dl2 (^ gsp 'dl-)])
      (if3 [(>= c (length dl1))]
           [(end)
            #t]
           [(push gs (% gsp 'c (+ 1 c)))
            (if (up-unify/data/data m
                                 (list-ref dl1 c)
                                 (list-ref dl2 c))
              (gs/next)
              #f)]))))

;; note that
;;   up-unify vs unify
;;   need not to be passed to nested structure
;;   thus we can simply call unify in up-unify

(define (up-unify/data/data m d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check

      ;; [{{'uni-bind uv d} __} (unify/data/data m (d2t d) d2)]
      ;; [{__ {'uni-bind uv d}} (unify/data/data m (d2t d1) d)]

      [{__ __} (unify/data/data m (d2t d1) d2)])))

;; although we can handle multi-return-value
;;   but one trunk only return one value
;;   a multi-return-value function will return many trunks

(define (update-trunky! k0 k)
  (vector-set! k0 0 k))

(define (try-trunk t)
  (: trunk -> (or #f data))
  (match t
    [{'trunk adl sdl k i}
     (match (vector-ref k 0)
       [{'done dl} (list-ref dl i)]
       [{'kvar kv dl}
        (match (bs/deep kv)
          [{'uni-lambda a al}
           ;; not check for type-arrow here
           (update-trunky! k {'todo al dl})
           (try-trunk t)]
          [__ #f])]
       [{'todo b dl}
        (let* ([ds0 ds]
               [bs0 bs]
               [gs0 gs]
               [result
                (let ()
                  (push-list ds dl)
                  (compose/try-body b))])
          (match result
            [{sjj vrc}
             (list-ref (update-trunky! k (call-with-output-to-new-ds
                                          (lambda ()
                                            (push rs (% rsp-proto
                                                        'ex   compose
                                                        'end  rs/exit
                                                        'vrc  vrc
                                                        'jj   sjj))
                                            (rs/next))))
                       i)]
            [#f
             (set! ds ds0)
             (set! bs bs0)
             (set! gs gs0)
             #f]))])]))

(define (occur-check/data uv d)
  (: fresh-uni-var data -> bool)
  (match (bs/deep d)
    [{'uni-var id level} (not (uni-var/eq? uv d))]
    [{'cons n dl}        (occur-check/data-list uv dl)]
    [{'uni-bind v d}     (occur-check/data-list uv {v d})]
    [{'trunk t k i}      (occur-check/trunk uv d)]
    [__                  #t]))

(define (occur-check/data-list uv dl)
  (: fresh-uni-var {data ...} -> bool)
  (match dl
    [{} #t]
    [(d . r)
     (if (occur-check/data uv d)
       (occur-check/data-list uv r)
       #f)]))

(define (occur-check/trunk uv t)
  (: fresh-uni-var trunk -> bool)
  (match t
    [{'trunk t k i}
     (match (vector-ref k 0)
       [{'todo b dl} (occur-check/data-list uv dl)]
       [{'kvar kv1 dl} (occur-check/data-list uv (cons kv1 dl))]
       [{'done dl}   (occur-check/data-list uv dl)])]))

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
      (debug0 'find-key
        ("can not find key : ~a~%" key)))))

(define (def-lambda n body)
  (let* ([a (compile-uni-arrow (cadr body))]
         [al (map compile-uni-arrow (cddr body))]
         [meaning (list 'meaning-lambda a al)])
    (push ns (cons n meaning))
    (if type-check-flag
      (type-check a al))
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

(define (def-type n body)
  (let* ([a (compile-uni-arrow (cadr body))]
         [pl (apply pair-list (cddr body))]
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
         [a (compile-uni-arrow (cdr p))]
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

(define-macro (run . s)
  `($run (quote ,s)))

(define ($run s)
  (for-each compose/jo (map compile-jo s))
  (print-ds))

(:

  (define (type-check ta al)
    (: uni-arrow {uni-arrow ...} -> bool)
    (match ta
      [('uni-arrow . __)
       (for-each (lambda (a) (type-check/arrow ta a))
                 al)]
      [__ (debug0 'type-check
            ("type of function must be arrow~%")
            ("type : ~a~%" ta))]))

  (define (type-check/arrow ta a)
    (: type-arrow arrow -> bool)
    (match {ta a}
      [{{'uni-arrow tnl tfrc tajj tsjj}
        {'uni-arrow nl frc ajj sjj}}
       (let* ([ds0 ds]
              [bs0 bs]
              [gs0 gs]
              [tvrc (append tfrc (nl->vrc tnl))]
              [vrc (append frc (nl->vrc nl))]
              [dl-tajj (call-with-output-to-new-ds
                        (lambda ()
                          (push rs (% rsp-proto
                                      'ex  compose
                                      'vrc tvrc
                                      'jj  tajj))
                          (rs/next)))]
              [dl-ajj (call-with-output-to-new-ds
                       (lambda ()
                         (push rs (% rsp-proto
                                     'ex  cut
                                     'vrc vrc
                                     'jj  ajj))
                         (rs/next)))])
         (: ><><><
            in lack of bind-unify
            (push rs {compose <type-antecedent>})
            (push rs {compose <antecedent>})
            (push gs {bind-unify <gathered>}))
         (if3 [(push gs (% gsp-proto
                           'ex     (unify 'unify)
                           'dl+    dl-ajj
                           'dl-    dl-tajj))
               (gs/next)]
              [(let* ([dl-tsjj (call-with-output-to-new-ds
                                (lambda ()
                                  (push rs (% rsp-proto
                                              'ex  compose
                                              'vrc tvrc
                                              'jj  tsjj))
                                  (rs/next)))]
                      [dl-sjj (call-with-output-to-new-ds
                               (lambda ()
                                 (push rs (% rsp-proto
                                             'ex  cut
                                             'vrc vrc
                                             'jj  sjj))
                                 (rs/next)))])
                 (if3 [(push gs (% gsp-proto
                                   'ex     (unify 'cover)
                                   'dl+    dl-sjj
                                   'dl-    dl-tsjj))
                       (gs/next)]
                      [(set! ds ds0)
                       (set! bs bs0)
                       (set! gs gs0)
                       #t]
                      [(debug0 'type-check/arrow
                         ("cover fail~%")
                         ("tsjj : ~a~%" tsjj)
                         ("dl-tsjj : ~a~%" dl-tsjj)
                         ("sjj : ~a~%" sjj)
                         ("dl-sjj : ~a~%" dl-sjj))]))]
              [(debug0 'type-check/arrow
                 ("unify fail~%")
                 ("tajj : ~a~%" tajj)
                 ("dl-tajj : ~a~%" dl-tajj)
                 ("ajj : ~a~%" ajj)
                 ("ajj : ~a~%" dl-ajj))]))])))

(define (type-check ta al)
  (: uni-arrow {uni-arrow ...} -> bool)
  (match ta
    [('uni-arrow . __)
     (for-each (lambda (a) (type-check/arrow ta a))
               al)]
    [__ (debug0 'type-check
          ("type of function must be arrow~%")
          ("type : ~a~%" ta))]))

(define (type-check/arrow ta a)
  (: type-arrow arrow -> bool)
  (match {ta a}
    [{{'uni-arrow tnl tfrc tajj tsjj}
      {'uni-arrow nl frc ajj sjj}}
     (let* ([ds0 ds]
            [bs0 bs]
            [gs0 gs]
            [tvrc (append tfrc (nl->vrc tnl))]
            [vrc (append frc (nl->vrc nl))]
            [dl-tajj (call-with-output-to-new-ds
                      (lambda ()
                        (push rs (% rsp-proto
                                    'ex  compose
                                    'vrc tvrc
                                    'jj  tajj))
                        (rs/next)))]
            [dl-ajj (call-with-output-to-new-ds
                     (lambda ()
                       (push rs (% rsp-proto
                                   'ex  compose
                                   'vrc vrc
                                   'jj  ajj))
                       (rs/next)))])
       (if3 [(push gs (% gsp-proto
                         'ex     (up-unify 'up-unify)
                         'dl+    dl-ajj
                         'dl-    dl-tajj))
             (gs/next)]
            [(let* ([dl-tsjj (call-with-output-to-new-ds
                              (lambda ()
                                (push rs (% rsp-proto
                                            'ex  compose
                                            'vrc tvrc
                                            'jj  tsjj))
                                (rs/next)))]
                    [dl-sjj (call-with-output-to-new-ds
                             (lambda ()
                               (push rs (% rsp-proto
                                           'ex  compose
                                           'vrc vrc
                                           'jj  sjj))
                               (rs/next)))])
               (if3 [(push gs (% gsp-proto
                                 'ex     (up-unify 'up-cover)
                                 'dl+    dl-sjj
                                 'dl-    dl-tsjj))
                     (gs/next)]
                    [(set! ds ds0)
                     (set! bs bs0)
                     (set! gs gs0)
                     #t]
                    [(debug0 'type-check/arrow
                       ("cover fail~%")
                       ("tsjj : ~a~%" tsjj)
                       ("dl-tsjj : ~a~%" dl-tsjj)
                       ("sjj : ~a~%" sjj)
                       ("dl-sjj : ~a~%" dl-sjj))]))]
            [(debug0 'type-check/arrow
               ("unify fail~%")
               ("tajj : ~a~%" tajj)
               ("dl-tajj : ~a~%" dl-tajj)
               ("ajj : ~a~%" ajj)
               ("ajj : ~a~%" dl-ajj))]))]))
