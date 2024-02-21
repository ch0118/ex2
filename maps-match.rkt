;; Exercise 1
;; CIS 352 -- Fall 2023
#lang racket

(provide (all-defined-out))

;;
;; practice with matches and quasiquotes
;;

;; Define using match patterns: (filter f l) selects the elements in
;; the list l which satisfy the predicate f, omitting those elements x
;; for which (f x) is false
(define (filter f lst)
  'todo)

;; select the maximum element from the list lst, assume lst is a list
;; of numbers.
(define (maxof lst)
  'todo)

;; andmap test cases
;; > (andmap list? ‘((1 2) () (3)))
;; #t
;; > (andmap list? ‘((1 . 2) ()))
;; #f
;; > (andmap list? ‘(1 2 3))
;; #f
(define (andmap f lst)
  'todo)

;;
;; practice with hashes and sets
;;

;; hashes in racket are key / value dictionaries
;; add / lookup / etc... are amortized O(1)
(define hsh (hash 'x 1 'y 2 'z 4))

;; important functions on hashes
(hash-keys (hash 'x 1 'y 2 'z 3))
;; returns a list of the keys of the hash
;; in *no specific order*

;; pass in a list of elements
(define (my-set lst)
  ;; make a hash of key/value associations from
  ;; each element of lst (which will be keys in our hash)
  ;; to the dummy value "dummy"
  (define (h lst)
    (match lst
      ['() (hash)]
      [`(,hd . ,tl) (hash-set (h tl) hd "dummy")]))
  (h lst))

;; hashes are generalized *sets*
;; a hash-set (degenerate hash) can be constructed using
(define st (set 1 2 3))

;; print out a hash in a canonical order
(define (render-hash h)
  (map (lambda (key)
         (let ([val (hash-ref h key)])
           (displayln (format "~a ~a" key val))))
       (hash-keys h))
  (void))

;; produce a map from each element in lst to the number zero
;; (zeroes '(x y)) = (hash-set (hash-set 'y 0) 'x 0)
;; note that hashes are *unordered*
;; aka (hash 'x 0 'y 0)
(define (zeroes lst)
  (match lst
    ['() 'todo]
    ;; hint: use hash-set and recursion
    [`(,hd . ,tl)
     'todo]))

(define (sum-list l)
  (match l
    ['() 0]
    [`(,hd . ,tl) (+ hd (sum-list tl))]))

(define (sum-set st)
  (sum-list (set->list st)))

;; Set data structure in Racket
;; Use `set` as a constructor
;; - (set) or (set 'x 'y ...) or (set e0 e1 ...)
;;
;; (set-add st element) O(1) -- returns a new set
;; that includes everything in st and also element
;;
;; (set-union st0 st1) O(n) or faster -- return
;; a new set that includes everything in st0 and st1
;;
;; (set-remove st element) O(1) -- return a set
;; containing everything in st minus element


;; add n to each value of the hash, for each of its keys. Hint: write
;; a recursive function (or foldl, if you know / would like to use it)
;; to walk over each of the keys in `hash-keys`.
(define (add-to-each h n)
  (define (f keys)
    (match keys
      ['() 'todo]
      [`(,first-key . ,rest-keys) 'todo]))
  (f (hash-keys h)))

(define (hash-map hsh f)
  (define (h keys)
    (match keys
      ['() (hash)]
      [`(,hd . ,tl) (hash-set (h tl) hd (f (hash-ref hsh hd)))]))
  (h (hash-keys hsh)))

(define (command? c)
  (match c
    ;; swap x and y's value
    [`(swap ,(? symbol? x) ,(? symbol? y)) #t]
    ;; assign x to the value of y
    [`(assign ,(? symbol? x) ,(? number? y)) #t]
    ;; assign x to the value of y+z
    [`(add ,(? symbol? x) ,(? symbol? y) ,(? symbol? z)) #t]
    [_ #f]))

;; "interpret" a single command: return the resulting hash. Swap
;; should swap the values in x and y. Assign should assign to the
;; value x. Add should assign to the value x with the value of y plus
;; the value of z.
(define (interpret-command e h)
  (match e
    [`(swap ,x ,y)
     'todo]
    [`(assign ,x ,y)
     'todo]
    [`(add ,x ,y ,z)
     'todo]))

;; interpret each command one at a time
(define (interpret-commands es)
  (define (h hsh lst)
    (match lst
      ['() hsh]
      [`(,hd . ,tl) (h (interpret-command hd hsh) tl)]))
  (h (hash) es))
