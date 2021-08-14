(load "RML_helper.scm")

;;; HELPER FUNCTIONS

(define MAX-MEMORY-SIZE 500)

(define (make-pointer address)
  (cons 'p address))

(define (pointer? op)
  (pair? op))

(define (address pointer)
  (cdr pointer))

;; Convert a nested list of list of... into vector format.
;; Input f is the current free index.
;; Returns the next free index.
(define (lisp-to-vectors obj f v-cars v-cdrs)
    (define (setup-cars obj f)
        (cond ((not (pair? (car obj)))
                (vector-set! v-cars f (car obj))
                (+ 1 f))
              (else
                (vector-set! v-cars f (make-pointer (+ 1 f)))
                (lisp-to-vectors (car obj) (+ 1 f) v-cars v-cdrs))))

    (cond ((not (pair? (cdr obj)))
            (vector-set! v-cdrs f (cdr obj))
            (setup-cars obj f))
          (else
            (let ((new-free (setup-cars obj f))) ; do cars before cdrs
            (vector-set! v-cdrs f (make-pointer new-free))
            (lisp-to-vectors (cdr obj) new-free v-cars v-cdrs)))))

;;; frame procedures
(define glob-mac 0)
(define glob-mac-cars 0)
(define glob-mac-cdrs 0)
(define free-reg 0)

(define (make-environment)
    (let ((f (address (get-register-contents glob-mac 'free))))
    (vector-set! glob-mac-cars f '())
    (vector-set! glob-mac-cdrs f '())
    (vector-set! glob-mac-cars (+ f 1) (make-pointer f))
    (vector-set! glob-mac-cdrs (+ f 1) '())
    (set-register-contents glob-mac 'free (make-pointer (+ f 2)))
    (make-pointer (+ f 1))))

;; Helper function. Scans a table of vars and values, looking for an instance of var. Returns the corresponding sublist of values, with the desired value on top. Returns #f if var isn't found. Note that if the variable is present, this won't return #f, even if the value of that variable is #f!
(define (scan-frame var vars values)
    (cond ((null? vars)
            #f)
          ((eq? var (vector-ref glob-mac-cars (address vars)))
            values)
          (else (scan-frame var (vector-ref glob-mac-cdrs (address vars)) (vector-ref glob-mac-cdrs (address values))))))

;; Helper function. Adds a var and value to a frame. Any pointers to the frame will still be valid, but pointers to the "head" of the var and value lists will be stale.
(define (add-binding var value frame)
    (let ((f (address (get-register-contents glob-mac 'free))))
    (vector-set! glob-mac-cars f var)
    (vector-set! glob-mac-cdrs f (frame-vars frame))
    (vector-set! glob-mac-cars (+ 1 f) value)
    (vector-set! glob-mac-cdrs (+ 1 f) (frame-values frame))
    (vector-set! glob-mac-cars (address frame) (make-pointer f))
    (vector-set! glob-mac-cdrs (address frame) (make-pointer (+ 1 f)))
    (set-register-contents glob-mac 'free (make-pointer (+ 2 f))))
    var)

;; Our standard API.
(define (lookup-var-value var env)
    (if (null? env)
        (error "Variable not found by lookup-var-value!")
        (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                     (frame-values (top-frame env)))))
            (if value-list
                (vector-ref glob-mac-cars (address value-list))
                (lookup-var-value var (enclosing-env env))))))


(define (set-var-value! var value env)
    (if (null? env)
        (error "Variable not found by set-var-value!")
        (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                     (frame-values (top-frame env)))))
            (if value-list
                (let ((old-value (vector-ref glob-mac-cars (address value-list))))
                     (vector-set! glob-mac-cars (address value-list) value)
                     old-value)
                (set-var-value! var value (enclosing-env env))))))

(define (define-var! var value env)
    (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                      (frame-values (top-frame env)))))
        (if value-list
            (vector-set! glob-mac-cars (address value-list) value)
            (add-binding var value (top-frame env)))))

(define (extend-environment vars args base-env)
    ; used to be a one-liner: (cons (cons vars args) base-env)
    (let ((f (address (get-register-contents glob-mac 'free))))
    (set-register-contents glob-mac 'free (make-pointer (+ f 2)))
    (vector-set! glob-mac-cars f vars)
    (vector-set! glob-mac-cdrs f args)
    (vector-set! glob-mac-cars (+ f 1) (make-pointer f))
    (vector-set! glob-mac-cdrs (+ f 1) base-env)
    (make-pointer (+ f 1))))

;; table of primitive procedures

(define primitive-table
  (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /) (cons '= (lambda (a b) (if (= a b) 1 0)))))

;;; MAIN FUNCTION

(define (make-special-machine)
  (define (get-op-list)
    (define (vector-ref-2 vec index)
      (vector-ref vec (address index)))

    (define (vector-set-2! vec index value)
      (vector-set! vec (address index) value))

    (define (increment-pointer pointer)
      (cons (car pointer) (+ 1 (cdr pointer))))

    (define (convert-args-to-list args)
      (cond ((not (pointer? args)) args)
            ((null? (vector-ref-2 glob-mac-cdrs args)) (list (convert-args-to-list (vector-ref-2 glob-mac-cars args))))
            (else
            (cons (convert-args-to-list (vector-ref-2 glob-mac-cars args)) (convert-args-to-list (vector-ref-2 glob-mac-cdrs args))))))

    (define (find-primitive-proc proc)
      (define (find-proc-iter curr-table)
        (cond ((null? curr-table) 'undefined)
              ((eq? (caar curr-table) proc) (cdar curr-table))
              (else (find-proc-iter (cdr curr-table)))))
      (find-proc-iter primitive-table))

    (define (apply-primitive proc args)
      (apply (find-primitive-proc proc) (convert-args-to-list args)))

    (define (make-null)
      '())

    (define (eqls a b)
      (if (= a b) 1 0))

    (list (cons 'vector-ref vector-ref-2) (cons 'vector-set! vector-set-2!) (cons 'make-pointer make-pointer)
    (cons 'increment-pointer increment-pointer) (cons 'make-null make-null) (cons 'eq? eq?) (cons '+ +)
    (cons 'null? null?) (cons 'make-environment make-environment) (cons 'lookup-var-value lookup-var-value)
    (cons 'set-var-value! set-var-value!) (cons 'define-var! define-var!) (cons 'extend-environment extend-environment)
    (cons 'number? number?) (cons 'symbol? symbol?) (cons 'apply-primitive apply-primitive) (cons '= eqls)))

  (define m (make-machine '(the-cars the-cdrs free a b temp-1 temp-2 temp-val glob-env null exp env proc val unev argl continue program p-1 p-2 p-val p-cont) (get-op-list) (get-RML)))

  (define (post-process machine)
    (set-register-contents machine 'the-cars (make-vector MAX-MEMORY-SIZE 'undefined))
    (set-register-contents machine 'the-cdrs (make-vector MAX-MEMORY-SIZE 'undefined))
    (set! glob-mac-cars (get-register-contents m 'the-cars))
    (set! glob-mac-cdrs (get-register-contents m 'the-cdrs))
    (set-register-contents machine 'free (make-pointer (lisp-to-vectors (the-program) 0 glob-mac-cars glob-mac-cdrs)))
    (set-register-contents machine 'program (make-pointer 0))
    (set! glob-mac m))

  (post-process m)
  m)

(define (top-frame env) (vector-ref glob-mac-cars (address env)))
(define (enclosing-env env) (vector-ref glob-mac-cdrs (address env)))
(define (frame-vars frame) (vector-ref glob-mac-cars (address frame)))
(define (frame-values frame) (vector-ref glob-mac-cdrs (address frame)))

;;; testing

;;;(define m (make-special-machine))
;;;(run m)
;;;(get-register-contents m 'val)
;;;(print-vector (get-register-contents m 'the-cars) 0 50)
;;;(print-vector (get-register-contents m 'the-cdrs) 0 50)
;;;max-depth
