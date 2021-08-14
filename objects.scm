;;; stack
(define (make-stack)
    (let ((my-stack '())
          (depth 0)
          (max-depth 0))

    (define (push item)
        (set! my-stack (cons item my-stack))
        (set! depth (+ depth 1))
        (if (> depth max-depth)
            (set! max-depth depth))
        'done)
    (define (pop)
        (let ((popped-item (car my-stack)))
            (set! my-stack (cdr my-stack))
            (set! depth (- depth 1))
            popped-item))
    (define (empty?)
        (null? my-stack))

    (define (dispatch m)
        (cond ((eq? 'push m) push)
              ((eq? 'pop m) pop)
              ((eq? 'empty? m) empty?)
              ((eq? 'get-max-depth m) max-depth))
    )


    dispatch
    ))
(define (push s item)
    ((s 'push) item))
(define (pop s)
    ((s 'pop)))


;;; register
(define (make-register)
    (let ((my-contents 'unspecified))

    (define (dispatch m)
        (cond ((eq? 'set-contents m) (lambda (x) (set! my-contents x) 'done))
              ((eq? 'get-contents m) (lambda () my-contents)))
    )
    dispatch))
(define (set-reg-contents reg item) ((reg 'set-contents) item))
(define (get-reg-contents reg) ((reg 'get-contents)))



;;; table
(define (make-table)
    (let ((my-table '()))

    (define (set-table key item)
        (set! my-table (cons (cons key item) my-table)) my-table)

    (define (get-table key local-table)
        (cond ((null? local-table) (display "failed table lookup: ") (display key) (print) #f)
              ((eq? key (caar local-table)) (car local-table))
              (else (get-table key (cdr local-table)))))
    (define (print)
        (display my-table))

    (define (dispatch m)
        (cond ((eq? 'set-table m) set-table)
              ((eq? 'get-table m) (lambda (key) (get-table key my-table)))
              ((eq? 'print m) print)))
    dispatch))
(define (set-table t key item) ((t 'set-table) key item))
(define (get-table t key) ((t 'get-table) key))
(define (print-table t) ((t 'print)))


(define s (make-stack))
(push s 4)
(push s 6)
(pop s)
(push s 5)
(s 'get-max-depth)
