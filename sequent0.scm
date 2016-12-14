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


(define-macro (tos s)
  `(if (null? ,s)
     (debug0 'tos
       ("stack is empty : ~a~%" (quote ,s)))
     (car ,s)))

(define-macro (tos-list s n)
  `(if (< (length ,s) ,n)
     (debug0 'tos-list
       ("stack is not long enough : ~a~%" (quote ,s))
       ("stack length : ~a~%" (length ,s))
       ("need length : ~a~%" ,n))
     (take ,s ,n)))

(: ns {(name . meaning) ...})
(: ds {data ...})
(: bs {(id . ls) ...})
(: rs {rsp ...})
(: gs {gsp ...})

(define (print-jo j)
  (match j
    [{'var n}  (cat ("~a " n))]
    [{'fvar n} (cat (":~a " n))]
    [{'bind n} (cat ("%~a " n))]
    [{'call n} (cat ("~a " n))]
    [{'arrow nl fnl ajj sjj}
     (cat ("(-> "))
     (cat ("[ ")) (map print-jo ajj) (cat ("] "))
     (cat ("[ ")) (map print-jo sjj) (cat ("]) "))]
    [{'uni-arrow nl frc ajj sjj}
     (cat ("(-> "))
     (cat ("[ ")) (map print-jo ajj) (cat ("] "))
     (cat ("[ ")) (map print-jo sjj) (cat ("]) "))]
    [{'lambda a al}
     (cat ("(lambda "))
     (print-jo a)
     (map print-jo al)
     (cat (") "))]
    [{'uni-lambda a al}
     (cat ("(lambda "))
     (print-jo a)
     (map print-jo al)
     (cat (") "))]
    [{'apply}
     (cat ("@ "))]))

(define (print-data d)
  (match d
    [{'uni-var id level}
     (let* ([p (vector-ref id 0)]
            [n (car p)]
            [c (cdr p)]
            [ls (vector-ref id 1)])
       (cat ("(~a #~a ^~a" n c level))
       (print-ls ls)
       (cat (") ")))]
    [{'uni-bind uv d}
     (cat ("(:%: "))
     (print-data uv)
     (print-data d)
     (cat (") "))]
    [{'cons n dl}
     (if3 [(null? dl)]
          [(cat ("~a " n))]
          [(cat ("[ ~a " n))
           (map print-data dl)
           (cat ("] "))])]
    [('uni-arrow . __)
     (print-jo d)]
    [('uni-lambda . __)
     (print-jo d)]
    [{'trunk adl sdl k i}
     (cat ("(:trunk: #~a " i))
     (map print-data adl)
     (map print-data sdl)
     (cat ("~a) " k))]))

(: bs {(id . ls) ...})
(: id (vector (name . counter) ls))
(: ls {(level . data) ...})

(define (print-bsp bsp)
  ;; note that
  ;;   bsp can be '(commit-point)
  (if3 [(equal? bsp '(commit-point))]
       [(cat ("~%")
             ("  (commit-point)~%")
             ("~%"))]
       [(print-id (car bsp))
        (cat ("~%"))
        (cat ("  ")) (print-ls (cdr bsp))
        (cat ("~%"))]))

(define (print-id id)
  (let* ([p (vector-ref id 0)]
         [n (car p)]
         [c (cdr p)]
         [ls (vector-ref id 1)])
    (cat ("~a #~a " n c)) (print-ls ls)))

(define (print-lsp lsp)
  (let ([level (car lsp)]
        [d (cdr lsp)])
    (cat (":~a: " level))
    (print-data d)))

(define (print-ls ls)
  (map print-lsp ls))

(define (print-nsp nsp)
  (let ([n0 (car nsp)]
        [meaning (cdr nsp)])
    (cat ("~a~%" n0))
    (match meaning
      [{'meaning-type a n nl}
       (cat ("  :type: ")) (print-jo a) (cat ("~%"))
       (cat ("  :constructor: ~a~%" nl))]
      [{'meaning-data a n n0}
       (cat ("  :type: ")) (print-jo a) (cat ("~%"))
       (cat ("  :belong-to: ~a~%" n0)) ]
      [{'meaning-lambda a al}
       (cat ("  :type: ")) (print-jo a) (cat ("~%"))
       (cat ("  :lambda: ")) (map print-jo al) (cat ("~%"))])
    (cat ("~%"))))

(define (print-ds)
  (if3 [(null? ds)]
       [(cat ("~%<ds>~%</ds>~%~%"))]
       [(cat ("~%<ds>~%"))
        (cat ("  ")) (map print-data ds)
        (cat ("~%</ds>~%~%"))]))

(define (print-bs)
  (cat ("~%<bs>~%"))
  (map print-bsp bs)
  (cat ("</bs>~%~%")))

(define (print-ns)
  (cat ("~%<ns>~%"))
  (map print-nsp ns)
  (cat ("</ns>~%~%")))

(define (print-rs)
  (cat ("~%<rs>~%"))
  (map (lambda (o) (@ o 'print)) rs)
  (cat ("</rs>~%~%")))

(define (print-gs)
  (cat ("~%<gs>~%"))
  (map (lambda (o) (@ o 'print)) gs)
  (cat ("</gs>~%~%")))

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
      ;; arrow and lambda block the search of ::name
      [{'arrow ac sc}   vl]
      [{'lambda a al}   vl]))
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

(define (bs/commit idl)
  (define (recur bs0)
    (cond [(equal? '(commit-point) (car bs0))
           (set! bs (cdr bs0))]
          [(let ([id (car (car bs0))])
             (member (car bs0) idl))
           (recur (cdr bs0))]
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

;; in compose/var
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

(define steper-flag #f)
(define (steper+) (set! steper-flag #t))
(define (steper-) (set! steper-flag #f))

(define steper-counter 0)

(define (steper)
  (cat ("step> "))
  (cond [(> steper-counter 0)
         (set! steper-counter (- steper-counter 1))
         (cat (":~a:~%" steper-counter))
         (print-rs)]
        [else
         (let ([user-input (read)])
           (cond [(number? user-input)
                  (set! steper-counter user-input)
                  (cat (":~a:~%" steper-counter))
                  (print-rs)]
                 [(eq? user-input 'n)
                  (cat (":~a:~%" steper-counter))
                  (print-rs)]
                 [(eq? user-input 'q)
                  (cat ("steper: quit~%"))
                  (steper-)]
                 [(eq? user-input 'rs)
                  (map (lambda (who) (cat ("  ~a~%" who)))
                    rs/next/who-list)]
                 [(eq? user-input 'gs)
                  (map (lambda (who) (cat ("  ~a~%" who)))
                    gs/next/who-list)]
                 [else
                  (cat ("steper: unknown command :: ~a~%" user-input))
                  (steper)]))]))

(define (rs/next who)
  (when (and steper-flag
             (member who rs/next/who-list))
    (cat ("~a:" who))
    (steper))
  (rs/next/call-ex))

(define (rs/next/call-ex)
  (let ([ex (^ (tos rs) 'ex)])
    (ex)))

(define rs/next/who-list
  (list
   'compose
   ;; 'compose/cons
   ;; 'compose/body:ajj
   ;; 'compose/body:sjj
   ;; 'compose/try-body
   ;; 'create-trunk-list:ajj
   ;; 'create-trunk-list:sjj
   ;; 'type/input-number
   ;; 'type/output-number
   ;; 'd2t:a->sdl:ajj
   ;; 'd2t:a->sdl:sjj
   ;; 'unify/arrow/arrow:dl-ajj1
   ;; 'unify/arrow/arrow:dl-ajj2
   ;; 'unify/arrow/arrow:dl-sjj1
   ;; 'unify/arrow/arrow:dl-sjj2
   ;; 'try-trunk
   ;; 'type-check/arrow:tajj
   ;; 'type-check/arrow:ajj
   ;; 'type-check/arrow:tsjj
   ;; 'type-check/arrow:sjj
   ))

(define (gs/next who)
  (: -> bool)
  (when (and steper-flag
             (member who gs/next/who-list))
    (cat ("~a:" who))
    (steper))
  (gs/next/call-ex))

(define (gs/next/call-ex)
  (: -> bool)
  (let* ([p (^ (tos gs) 'ex)]
         [ex (cdr p)])
    (ex)))

(define gs/next/who-list
  (list
   ;; 'compose/cons
   ;; 'compose/body
   ;; 'compose/try-body
   'unify
   'unify/data/data:cons
   'unify/trunk/trunk
   'unify/arrow/arrow:ajj1&ajj2
   'unify/arrow/arrow:sjj1&sjj2
   'up-unify
   'type-check/arrow:ajj&tajj
   'type-check/arrow:sjj&tsjj))

;; return-stack
(define rs '())

(define (print-vrcp vrcp)
  (let* ([n (car vrcp)]
         [v (cdr vrcp)])
    (cat ("      ~a " n))
    (print-data v)
    (cat ("~%"))))

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

(define (vrc->idl vrc)
  (map (lambda (x)
         (match (cdr x)
           [{'uni-var id level}
            id]))
    vrc))

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
         [jj  (^ rsp 'jj)])
    (if3 [(>= c (length jj))]
         []
         [(push rs (% rsp 'c (+ 1 c)))
          (compose/jo (list-ref jj c))
          (rs/next 'compose)])))

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
           [{'meaning-type a n nl} (compose/cons n a)]
           [{'meaning-data a n n0} (compose/cons n a)]
           [{'meaning-lambda a al} (compose/body a al)])))]))

(define (compose/cons n a)
  (match a
    [{'uni-arrow nl frc ajj sjj}
     (let* ([tdl (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'vrc  (append frc (nl->vrc nl))
                                'jj   ajj))
                    (rs/next 'compose/cons)))]
            [idl (vrc->idl frc)]
            [dl (pop-list ds (length tdl))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex *up-unify*
                         'dl+ (reverse dl)
                         'dl- (reverse tdl)))
             (gs/next 'compose/cons)]
            [(bs/commit idl)
             (push ds (list 'cons n dl))]
            [(debug0 'compose/cons
               ("unify fail~%")
               ("dl : ~a~%" dl)
               ("tdl : ~a~%" tdl))]))]))

(: [for the first covering arrow]
   (push gs {cover commit
                   <data-on-the-stack>
                   (push rs {compose <ac>})})
   (cond [(succ?)
          (commit)
          (push rs {compose <sc>})
          (exit)]
         [(fail?)
          (undo)
          (loop)])
   (if (all-fail?) (form-trunk)))

(define (compose/body t b)
  ;; note that
  ;;   when create-trunk-list
  ;;   it needs to know the type to get input-number & output-numbe
  ;; note that
  ;;   compose/body can not fail
  ;;   at least trunk is created
  (: type body -> [:ds (or [result of body]
                           [trunk generated by body])])
  (match t
    [{'uni-arrow nl frc ajj sjj}
     (let* ([tvrc (append frc (nl->vrc nl))]
            [idl (vrc->idl frc)]
            [tdl (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'vrc  tvrc
                                'jj   ajj))
                    (rs/next 'compose/body:ajj)))]
            [dl (tos-list ds (length tdl))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex *up-unify*
                         'dl+ (reverse dl)
                         'dl- (reverse tdl)))
             (gs/next 'compose/body)]
            [(bs/commit idl)
             (match (compose/try-body b)
               [{sjj vrc}
                (push rs (% rsp-proto
                            'vrc  vrc
                            'jj   sjj))
                (rs/next 'compose/body:sjj)]
               [#f
                (let ([dl (pop-list ds (length tdl))])
                  (push-list ds (create-trunk-list t b dl)))])]
            [(debug0 'compose/body
               ("up-unify fail~%")
               ("dl  : ~a~%" dl)
               ("tdl : ~a~%" tdl))]))]))

(define (compose/try-body b)
  (: body -> (or #f {sjj vrc}))
  ;; return #f on fail
  ;; return sjj on success with commit
  (match b
    [{} #f]
    [({'uni-arrow nl frc ajj sjj} . r)
     (let* ([vrc (append frc (nl->vrc nl))]
            [idl (vrc->idl frc)]
            [ds0 ds]
            [bs0 bs]
            [gs0 gs]
            [dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'vrc  vrc
                                'jj   ajj))
                    (rs/next 'compose/try-body)))]
            [dl2 (pop-list ds (length dl1))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex   *cover*
                         'dl+  (reverse dl1)
                         'dl-  (reverse dl2)))
             (gs/next 'compose/try-body)]
            ;; commit or undo
            [(bs/commit idl)
             {sjj vrc}]
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
                                'vrc  vrc
                                'jj   ajj))
                    (rs/next 'create-trunk-list:ajj)))]
            [sdl (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'vrc  vrc
                                'jj   sjj))
                    (rs/next 'create-trunk-list:sjj)))]
            [k (match b
                 [('uni-var . __)
                  (vector {'kvar b dl})]
                 [__
                  (vector {'todo b dl})])])
       (reverse
        (map (lambda (i) {'trunk adl sdl k i})
          (genlist (length sdl)))))]))

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

(define (type/input-number t)
  (match t
    [{'uni-arrow nl frc ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda ()
                (push rs (% rsp-proto
                            'vrc  (append frc (nl->vrc nl))
                            'jj  ajj))
                (rs/next 'type/input-number))))]))

;; (define (type/output-number t)
;;   (match t
;;     [{'uni-arrow nl frc ajj sjj}
;;      (length (call-with-output-to-new-ds
;;               (lambda ()
;;                 (push rs (% rsp-proto
;;                             'vrc  (append frc (nl->vrc nl))
;;                             'jj  sjj))
;;                 (rs/next 'type/output-number))))]))

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
      [__ (debug0 'compose/apply
            ("compose/apply can not apply data~%")
            ("data : ~a~%" d))])))

(define rsp-proto
  (new-object
   (pair-list
    'c      0
    'ex     compose
    'vrc    '(var record)
    'jj     '(jojo))
   (pair-list
    'print
    (lambda (o)
      (cat ("  <rsp>~%")
           ("    :counter: ~a~%" (^ o 'c))
           ("    :var-record:~%"))
      (map print-vrcp (^ o 'vrc))
      (cat ("    :jojo: "))
      (map print-jo (^ o 'jj))
      (cat ("~%"))
      (cat ("  </rsp>~%"))))))

;; goal-stack
;;   binding-stack is to record solution of equations in goal-stack
(define gs '())

(define gsp-proto
  (new-object
   (pair-list
    'c      0
    'ex     '(explainer)
    'dl+    '(data-list)
    'dl-    '(data-list))
   (pair-list
    'print
    (lambda (o)
      (cat ("  <gsp>~%")
           ("    :counter: ~a~%"   (^ o 'c))
           ("    :explainer: ~a~%" (car (^ o 'ex))))
      (cat ("    :double-data-list:~%"))
      (map (lambda (d+ d-)
             (cat ("      :+: "))
             (print-data d+)
             (cat (":-: "))
             (print-data d-)
             (cat ("~%")))
        (^ o 'dl+) (^ o 'dl-))
      (cat ("  </gsp>~%"))))))

(define (d2t d)
  (define (a->sdl a)
    (match a
      [{'uni-arrow nl frc ajj sjj}
       (let* ([vrc (append frc (nl->vrc nl))]
              [adl (call-with-output-to-new-ds
                    (lambda ()
                      (push rs (% rsp-proto
                                  'vrc  vrc
                                  'jj   ajj))
                      (rs/next 'd2t:a->sdl:ajj)))]
              [sdl (call-with-output-to-new-ds
                    (lambda ()
                      (push rs (% rsp-proto
                                  'vrc  vrc
                                  'jj   sjj))
                      (rs/next 'd2t:a->sdl:sjj)))])
         sdl)]))
  (match d
    [{'uni-var id level} (bs/walk {'uni-var id (+ 1 level)})]
    [{'uni-bind uv d1} d1]
    [{'cons n dl}
     (let ([found (assq n ns)])
       (if (not found)
         (debug0 'd2t ("unknow name : ~a~%" n))
         (match (cdr found)
           ;; ><><><
           ;; need bind-unify for adl of cons and dl
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
           [dl1 (^ gsp 'dl+)]
           [dl2 (^ gsp 'dl-)])
      (if3 [(not (= (length dl1) (length dl2)))]
           [(debug0 'unify
              ("unify fail~%")
              ("length of dl+ is ~a~%" (length dl1))
              ("length of dl- is ~a~%" (length dl2))
              ("dl+ : ~a~%" dl1)
              ("dl- : ~a~%" dl2))]
           [(if3 [(>= c (length dl1))]
                 [#t]
                 [(push gs (% gsp 'c (+ 1 c)))
                  (if (unify/data/data m
                                       (list-ref dl1 c)
                                       (list-ref dl2 c))
                    (gs/next 'unify)
                    #f)])]))))

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
      [{__ {'uni-var id level}} (unify/data/uni-var m d1 d2)]

      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs (% gsp-proto
                          'ex *unify*
                          'dl+ (reverse dl1)
                          'dl- (reverse dl2)))
              (gs/next 'unify/data/data:cons)]
             [else #f])]

      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk adl1 sdl1 k1 i1} {'trunk adl2 sdl2 k2 i2}}
       (unify/trunk/trunk m d1 d2)]
      [{{'trunk adl sdl k i} __} (unify/trunk/data m d1 d2)]
      [{__ {'trunk adl sdl k i}} (unify/data/trunk m d1 d2)]

      [{{'uni-arrow nl1 frc1 ajj1 sjj1}
        {'uni-arrow nl2 frc2 ajj2 sjj2}}
       (unify/arrow/arrow m d1 d2)]

      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

(define (unify/uni-var/data m uv d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data uv d)
    (bs/extend uv d)
    #f))

(define (unify/data/uni-var m d uv)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (case m
    ['cover #f]
    ['unify (if (occur-check/data uv d)
              (bs/extend uv d)
              #f)]))

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
                                   'ex *unify*
                                   'dl+ (reverse dl1)
                                   'dl- (reverse dl2)))
                       (gs/next 'unify/trunk/trunk)]
                      [#f])]
                [{{'kvar kv1 dl1} {'kvar kv2 dl2}}
                 (if3 [(equal? {adl1 sdl1 i1}
                               {adl2 sdl2 i2})]
                      [(push gs (% gsp-proto
                                   'ex *unify*
                                   'dl+ (reverse (cons kv1 dl1))
                                   'dl- (reverse (cons kv2 dl2))))
                       (gs/next 'unify/trunk/trunk)]
                      [#f])]
                [__ #f])])])))

(define (unify/arrow/arrow m d1 d2)
  (match {d1 d2}
    [{{'uni-arrow nl1 frc1 ajj1 sjj1}
      {'uni-arrow nl2 frc2 ajj2 sjj2}}
     (let* ([vrc1 (append frc1 (nl->vrc nl1))]
            [vrc2 (append frc2 (nl->vrc nl2))]
            [dl-ajj1 (call-with-output-to-new-ds
                      (lambda ()
                        (push rs (% rsp-proto
                                    'vrc  vrc1
                                    'jj   ajj1))
                        (rs/next 'unify/arrow/arrow:dl-ajj1)))]
            [dl-ajj2 (call-with-output-to-new-ds
                      (lambda ()
                        (push rs (% rsp-proto
                                    'vrc  vrc2
                                    'jj   ajj2))
                        (rs/next 'unify/arrow/arrow:dl-ajj2)))])
       (if3 [(push gs (% gsp-proto
                         'ex *unify*
                         'dl+ (reverse dl-ajj1)
                         'dl- (reverse dl-ajj2)))
             (gs/next 'unify/arrow/arrow:ajj1&ajj2)]
            [(let* ([dl-sjj1 (call-with-output-to-new-ds
                              (lambda ()
                                (push rs (% rsp-proto
                                            'vrc  vrc1
                                            'jj   sjj1))
                                (rs/next 'unify/arrow/arrow:dl-sjj1)))]
                    [dl-sjj2 (call-with-output-to-new-ds
                              (lambda ()
                                (push rs (% rsp-proto
                                            'vrc  vrc2
                                            'jj   sjj2))
                                (rs/next 'unify/arrow/arrow:dl-sjj2)))])
               (push gs (% gsp-proto
                           'ex (cons `(unify ,m) (unify m))
                           'dl+ (reverse dl-sjj1)
                           'dl- (reverse dl-sjj2)))
               (gs/next 'unify/arrow/arrow:sjj1&sjj2))]
            [(debug0 'unify/arrow/arrow
               ("unify fail~%")
               ("ajj1 : ~a~%" ajj1)
               ("ajj2 : ~a~%" ajj2)
               ("dl-ajj1 : ~a~%" dl-ajj1)
               ("dl-ajj2 : ~a~%" dl-ajj2))]))]))

(define (up-unify m)
  (: method -> (-> bool))
  (lambda ()
    (let* ([gsp (pop gs)]
           [c   (^ gsp 'c)]
           [ex  (^ gsp 'ex)]
           [dl1 (^ gsp 'dl+)]
           [dl2 (^ gsp 'dl-)])
      (if3 [(not (= (length dl1) (length dl2)))]
           [(debug0 'up-unify
              ("up-unify fail~%")
              ("length of dl+ is ~a~%" (length dl1))
              ("length of dl- is ~a~%" (length dl2))
              ("dl+ : ~a~%" dl1)
              ("dl- : ~a~%" dl2))]
           [(if3 [(>= c (length dl1))]
                 [#t]
                 [(push gs (% gsp 'c (+ 1 c)))
                  (if (up-unify/data/data m
                                          (list-ref dl1 c)
                                          (list-ref dl2 c))
                    (gs/next 'up-unify)
                    #f)])]))))

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

      [{{'uni-bind uv d} __}
       (unify/data/data m (d2t d) d2)]
      [{__ {'uni-bind uv d}}
       (and (unify/data/data m (d2t d1) d)
            (unify/data/data m d1 uv))]

      [{__ __} (unify/data/data m (d2t d1) d2)])))

(define *unify* (cons '(unify 'unify) (unify 'unify)))
(define *cover* (cons '(unify 'cover) (unify 'cover)))
(define *up-unify* (cons '(up-unify 'unify) (up-unify 'unify)))
(define *up-cover* (cons '(up-unify 'cover) (up-unify 'cover)))

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
               [gs0 gs])
          (match (let ()
                   (push-list ds dl)
                   (compose/try-body b))
            [{sjj vrc}
             (list-ref (update-trunky! k (call-with-output-to-new-ds
                                          (lambda ()
                                            (push rs (% rsp-proto
                                                        'vrc  vrc
                                                        'jj   sjj))
                                            (rs/next 'try-trunk))))
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

(define (print-def n meaning)
  (cat ("~%")
       ("<def>~%")
       ("  :name: ~a~%" n)
       ("  :meaning:~%~a~%" meaning)
       ("</def>~%")
       ("~%")))

(define (def-lambda n body)
  (let* ([a (compile-uni-arrow (cadr body))]
         [al (map compile-uni-arrow (cddr body))]
         [meaning (list 'meaning-lambda a al)])
    (push ns (cons n meaning))
    (if type-check-flag (type-check a al))
    (if print-define-flag (print-def n meaning))))

(new-key 'lambda def-lambda)

(define (def-type n body)
  (let* ([a (compile-uni-arrow (cadr body))]
         [pl (apply pair-list (cddr body))]
         [nl (map car pl)]
         [meaning (list 'meaning-type a n nl)])
    (push ns (cons n meaning ))
    (if print-define-flag (print-def n meaning))
    (for-each (lambda (p) (def-data n p)) pl)))

(new-key 'type def-type)

(define (def-data n0 p)
  (let* ([n (car p)]
         [a (compile-uni-arrow (cdr p))]
         [meaning (list 'meaning-data a n n0)])
    (push ns (cons n meaning))
    (if print-define-flag (print-def n meaning))))

(define-macro (run . s)
  `($run (quote ,s)))

(define ($run s)
  (for-each compose/jo (map compile-jo s))
  (print-ds))

(define (type-check ta al)
  (: uni-arrow {uni-arrow ...} -> bool)
  (match ta
    [('uni-arrow . __)
     (for-each (lambda (a) (type-check/arrow ta a))
               al)]
    [__ (debug0 'type-check
          ("type of function must be uni-arrow~%")
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
                                    'vrc tvrc
                                    'jj  tajj))
                        (rs/next 'type-check/arrow:tajj)))]
            [dl-ajj (call-with-output-to-new-ds
                     (lambda ()
                       (push rs (% rsp-proto
                                   'vrc vrc
                                   'jj  ajj))
                       (rs/next 'type-check/arrow:ajj)))])
       (if3 [(push gs (% gsp-proto
                         'ex     *up-unify*
                         'dl+    (reverse dl-ajj)
                         'dl-    (reverse dl-tajj)))
             (gs/next 'type-check/arrow:ajj&tajj)]
            [(let* ([dl-tsjj (call-with-output-to-new-ds
                              (lambda ()
                                (push rs (% rsp-proto
                                            'vrc tvrc
                                            'jj  tsjj))
                                (rs/next 'type-check/arrow:tsjj)))]
                    [dl-sjj (call-with-output-to-new-ds
                             (lambda ()
                               (push rs (% rsp-proto
                                           'vrc vrc
                                           'jj  sjj))
                               (rs/next 'type-check/arrow:sjj)))])
               (if3 [(push gs (% gsp-proto
                                 'ex     *up-cover*
                                 'dl+    (reverse dl-sjj)
                                 'dl-    (reverse dl-tsjj)))
                     (gs/next 'type-check/arrow:sjj&tsjj)]
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
