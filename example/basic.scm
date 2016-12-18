(def type
  (type (-> [] [type])))

(def nat
  (type (-> [] [type])
    zero (-> [] [nat])
    succ (-> [nat] [nat])))

(def add
  (lambda (-> [nat nat] [nat])
    (-> [:m zero] [:m])
    (-> [:m :n succ] [:m :n add succ])))

(def mul
  (lambda (-> [nat nat] [nat])
    (-> [:m zero] [zero])
    (-> [:m :n succ] [:m :n mul :m add])))

(def factorial
  (lambda (-> [nat] [nat])
    (-> [zero] [zero succ])
    (-> [:n succ] [:n factorial :n succ mul])))

(def nat-induction
  (lambda (-> [(-> [nat] [type]) %:p
               zero :p @
               (-> [nat %:k :k ::p @] [:k succ ::p @])
               nat %:x]
              [:x :p @])
    (-> [:p :p/z :p/s zero] [:p/z])
    (-> [:p :p/z :p/s :n succ]
        [:n
         :p :p/z :p/s :n nat-induction
         :p/s @])))

;; (def add-ind
;;   (lambda (-> [nat nat] [nat])
;;     (-> [:m :n]
;;         [(lambda (-> [nat] [type])
;;            (-> [:x] [nat]))
;;          :m
;;          (lambda (-> [nat %:k :k :p @])
;;            (-> [:k :k-rec] [:k-rec succ]))
;;          :n
;;          nat-induction])))

;; (def nat-induction
;;   (lambda (-> [(-> [nat] [type]) %:p
;;                zero :p @
;;                (-> [nat %:k :k :p @]
;;                    [:k succ :p @])]
;;               [(-> [nat %:x] [:x :p @])])
;;     (-> [:p :p/z :p/s zero] [:p/z])
;;     (-> [:p :p/z :p/s :n succ]
;;         [:n
;;          :p :p/z :p/s :n nat-induction
;;          :p/s @])))

(def drop
  (lambda (-> [:t] [])
    (-> [:d]
        [])))

(def dup
  (lambda (-> [:t] [:t :t])
    (-> [:d]
        [:d :d])))

(def over
  (lambda (-> [:t1 :t2] [:t1 :t2 :t1])
    (-> [:d1 :d2]
        [:d1 :d2 :d1])))

(def tuck
  (lambda (-> [:t1 :t2] [:t2 :t1 :t2])
    (-> [:d1 :d2]
        [:d2 :d1 :d2])))

(def swap
  (lambda (-> [:t1 :t2] [:t2 :t1])
    (-> [:d1 :d2]
        [:d2 :d1])))

(run zero succ
     zero succ succ
     add)

(run drop)

(run zero succ succ succ
     zero succ succ succ
     mul)

(run drop)

(run zero succ succ succ
     factorial)

(run drop)

(def list
  (type (-> [type] [type])
    null (-> [] [:t list])
    cons (-> [:t list :t] [:t list])))

(def append
  (lambda (-> [:t list :t list] [:t list])
    (-> [:l null] [:l])
    (-> [:l :r :e cons] [:l :r append :e cons])))

(def length
  (lambda (-> [:t list] [nat])
    (-> [null] [zero])
    (-> [:l :e cons] [:l length succ])))

(def map
  (lambda (-> [:t1 list (-> [:t1] [:t2])]
              [:t2 list])
    (-> [null :f] [null])
    (-> [:l :e cons :f] [:l :f map :e :f @ cons])))

(run null zero cons)

(run drop)

(run null
     zero cons
     null
     zero cons
     append)

(run drop)

(run null
     zero cons
     zero cons
     null
     zero cons
     zero cons
     append
     length)

(run drop)

(run null
     zero cons
     zero cons
     (lambda (-> [nat] [nat])
       (-> [zero] [zero succ]))
     map)

(run drop)

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append
     (lambda (-> [nat] [nat])
       (-> [zero] [zero succ]))
     map)

(run drop)

(def has-length
  (type (-> [:t list nat] [type])
    null/has-length (-> [] [null zero has-length])
    cons/has-length (-> [:l :n has-length]
                        [:l :a cons :n succ has-length])))

;; (steper+)

(def map/has-length
  (lambda (-> [:l :n has-length]
              [:l :f map :n has-length])
    (-> [null/has-length] [null/has-length])
    (-> [:h cons/has-length] [:h map/has-length cons/has-length])))

;; (def map/has-length
;;   (lambda (-> [:l :n has-length]
;;               [:l (-> [:t1] [:t2]) %:f :f map :n has-length])
;;     (-> [null/has-length] [null/has-length])
;;     (-> [:h cons/has-length] [:h map/has-length cons/has-length])))

(def vector
  (type (-> [nat type] [type])
    null (-> [] [zero :t vector])
    cons (-> [:n :t vector :t]
             [:n succ :t vector])))

(def append
  (lambda (-> [:m :t vector :n :t vector]
              [:m :n add :t vector])
    (-> [:l null] [:l])
    (-> [:l :r :e cons]
        [:l :r append :e cons])))

(def map
  (lambda (-> [:n :t1 vector (-> [:t1] [:t2])]
              [:n :t2 vector])
    (-> [null :f] [null])
    (-> [:l :e cons :f] [:l :f map :e :f @ cons])))

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append)

(run drop)

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append
     (lambda (-> [nat] [nat])
       (-> [zero] [zero succ]))
     map)

(run drop)

(def fraction
  (type (-> [type %:t] [type])
    fline (-> [:t dup] [:t fraction])))
