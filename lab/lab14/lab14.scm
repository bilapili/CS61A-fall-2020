(define (split-at lst n)
  (cond 
    ((zero? n) (cons nil lst))
    ((> n (length lst)) (cons lst nil))
    (else (cons (cons (car lst) (car (split-at (cdr lst) (- n 1)))) (cdr (split-at (cdr lst) (- n 1))))))
)


(define (compose-all funcs)
  (define (helper x)
      ( if (null? funcs) x
            ((compose-all (cdr funcs)) ((car funcs) x))
      )
  )
  helper
)

