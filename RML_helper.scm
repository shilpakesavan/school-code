(load "machine.scm")

;;; Precompilation for handling cons, car, cdr.
;;;
;;; Takes sugared RML with CONS, CAR, CDR as ops
;;; in the form of a list of lists;
;;; returns expanded RML that uses subroutines.
;;;
;;; For example:
;;; '((assign (reg a) (const 3))
;;; (assign (reg val) CONS (const 5) (reg a)))
;;;
;;; returns:
;;; '((assign (reg a) (const 3))
;;; (assign (reg p-1) (const 5))
;;; (assign (reg p-2) (reg a))
;;; (assign (reg p-cont) (label precompile-1))
;;; (goto cons)
;;; precompile-1
;;; (assign (reg val) (reg p-val)))
(define label-index 0)

(define (main-map line)
    (cond ((or (not (pair? line)) (< (length line) 3))
          (list line))
          ((eq? 'CONS (caddr line))
          (make-cons line))
          ((eq? 'CAR (caddr line))
          (make-car line))
          ((eq? 'CDR (caddr line))
          (make-cdr line))
          ((eq? 'CADR (caddr line))
          (make-cadr line))
          ((eq? 'CADDR (caddr line))
          (make-caddr line))
          (else
          (list line))))

(define (make-cons line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
          (source-1 (cadddr line))
          (source-2 (cadddr (cdr line)))
          (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source-1)
          (list 'assign '(reg p-2) source-2)
          (list 'assign '(reg p-cont) (list 'label label))
          '(goto (label cons))
          label
          (list 'assign dest '(reg p-val)))))


; CAR:
; (assign (reg a) CAR (reg b))
; turns into:
; (assign (reg p-1) (reg b))
; (assign (reg p-cont) (label precompile-x))
; (goto (label car))
; precompile-x
; (assign (reg a) (reg p-val))
(define (make-car line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
        (source (cadddr line))
        (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source)
          (list 'assign '(reg p-cont) (list 'label label))
          (list 'goto '(label car))
          label
          (list 'assign dest '(reg p-val)))))


(define (make-cdr line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
        (source (cadddr line))
        (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source)
          (list 'assign '(reg p-cont) (list 'label label))
          (list 'goto '(label cdr))
          label
          (list 'assign dest '(reg p-val)))))

;;; oh, I'm so clever...
(define (make-cadr line)
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CAR dest))))

(define (make-caddr line)
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CDR dest)
          (list 'assign dest 'CAR dest))))

(define (loop-once RML)
    (fold-left append '() (map main-map RML)))

(define (precompile-RML RML)
    (loop-once (loop-once RML)))


;;; printing vectors
(define (print-vector v start end)
    (if (= start end)
        '.
        (begin (display start) (display ":") (display (vector-ref v start)) (display "  ") (print-vector v (+ 1 start) end))))

;;; returns lisp program we want to run
(define (the-program)
  '((define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
    (fact 20)))

(define (get-RML)
  (precompile-RML
  '(begin
      (assign (reg env) (op make-environment))
      (assign (reg null) (op make-null))
      (define (reg a) CONS (const +) (reg null))
      (define (reg a) CONS (const primitive) (reg a))
      (perform (op define-var!) (const +) (reg a) (reg env))
      (define (reg a) CONS (const -) (reg null))
      (define (reg a) CONS (const primitive) (reg a))
      (perform (op define-var!) (const -) (reg a) (reg env))
      (define (reg a) CONS (const *) (reg null))
      (define (reg a) CONS (const primitive) (reg a))
      (perform (op define-var!) (const *) (reg a) (reg env))
      (define (reg a) CONS (const /) (reg null))
      (define (reg a) CONS (const primitive) (reg a))
      (perform (op define-var!) (const /) (reg a) (reg env))
      (define (reg a) CONS (const =) (reg null))
      (define (reg a) CONS (const primitive) (reg a))
      (perform (op define-var!) (const =) (reg a) (reg env))
      (assign (reg glob-env) (reg env))
      (assign (reg continue) (label done))
    eval-loop
      (assign (reg env) (reg glob-env))
      (test (op null?) (reg program))
      (branch (label done))
      (assign (reg exp) CAR (reg program))
      (assign (reg program) CDR (reg program))
      (assign (reg continue) (label eval-loop))
    eval
      (test (op number?) (reg exp))
      (branch (label ev-self-evaluating))
      (test (op symbol?) (reg exp))
      (branch (label ev-variable))
      (assign (reg a) CAR (reg exp))
      (test (op eq?) (const define) (reg a))
      (branch (label ev-define))
      (test (op eq?) (const lambda) (reg a))
      (branch (label ev-lambda))
      (test (op eq?) (const if) (reg a))
      (branch (label ev-if))
    ev-apply
      (push (reg exp))
      (push (reg continue))
      (assign (reg exp) CAR (reg exp))
      (assign (reg continue) (label ev-apply-2))
      (goto (label eval))
    ev-apply-2
      (pop (reg continue))
      (pop (reg exp))
      (assign (reg proc) (reg val))
      (assign (reg unev) CDR (reg exp))
      (assign (reg argl) (reg null))
    ev-apply-3
      (test (op null?) (reg unev))
      (branch (label apply))
      (push (reg argl))
      (push (reg unev))
      (push (reg exp))
      (push (reg continue))
      (push (reg proc))
      (assign (reg exp) CAR (reg unev))
      (assign (reg continue) (label ev-apply-4))
      (goto (label eval))
    ev-apply-4
      (pop (reg proc))
      (pop (reg continue))
      (pop (reg exp))
      (pop (reg unev))
      (pop (reg argl))
      (assign (reg unev) CDR (reg unev))
      (assign (reg temp-1) (reg argl))
      (assign (reg temp-2) CONS (reg val) (reg null))
      (push (reg continue))
      (assign (reg continue) (label ev-apply-5))
      (goto (label append))
    ev-apply-5
      (pop (reg continue))
      (goto (label ev-apply-3))
    apply
      (assign (reg temp-1) CAR (reg proc))
      (test (op eq?) (reg temp-1) (const primitive))
      (branch (label apply-primitive))
      (assign (reg env) CDR (reg proc))
      (assign (reg env) CADDR (reg env))
      (assign (reg temp-1) CADR (reg proc))
      (assign (reg env) (op extend-environment) (reg temp-1) (reg argl) (reg env))
      (assign (reg exp) CADDR (reg proc))
      (assign (reg temp-1) CAR (reg exp))
      (assign (reg exp) CDR (reg exp))
      (test (op eq?) (reg exp) (reg null))
      (branch (label apply-4))
      (push (reg continue))
      (push (reg exp))
      (assign (reg exp) (reg temp-1))
      (assign (reg continue) (label apply-2))
      (goto (label eval))
    apply-2
      (pop (reg exp))
      (test (op null?) (reg exp))
      (branch (label apply-3))
      (assign (reg temp-1) CAR (reg exp))
      (assign (reg exp) CDR (reg exp))
      (push (reg exp))
      (assign (reg exp) (reg temp-1))
      (goto (label eval))
    apply-3
      (pop (reg continue))
      (goto (reg continue))
    apply-4
      (assign (reg exp) (reg temp-1))
      (goto (label eval))
    apply-primitive
      (assign (reg temp-1) CADR (reg proc))
      (assign (reg val) (op apply-primitive) (reg temp-1) (reg argl))
      (goto (reg continue))
    ev-self-evaluating
      (assign (reg val) (reg exp))
      (goto (reg continue))
    ev-variable
      (assign (reg val) (op lookup-var-value) (reg exp) (reg env))
      (goto (reg continue))
    ev-define
      (assign (reg b) CADR (reg exp))
      (assign (reg exp) CADDR (reg exp))
      (push (reg continue))
      (push (reg b))
      (assign (reg continue) (label ev-define-2))
      (goto (label eval))
    ev-define-2
      (pop (reg b))
      (pop (reg continue))
      (perform (op define-var!) (reg b) (reg val) (reg env))
      (assign (reg val) (reg b))
      (goto (reg continue))
    ev-lambda
      (assign (reg val) CONS (reg env) (reg null))
      (assign (reg temp-1) CDR (reg exp))
      (assign (reg temp-1) CDR (reg temp-1))
      (assign (reg val) CONS (reg temp-1) (reg val))
      (assign (reg temp-1) CADR (reg exp))
      (assign (reg val) CONS (reg temp-1) (reg val))
      (assign (reg val) CONS (const procedure) (reg val))
      (goto (reg continue))
    ev-if
      (push (reg exp))
      (push (reg env))
      (push (reg continue))
      (assign (reg exp) CADR (reg exp))
      (assign (reg continue) (label ev-if-2))
      (goto (label eval))
    ev-if-2
      (pop (reg continue))
      (pop (reg env))
      (pop (reg exp))
      (test (op eq?) (reg val) (const 1))
      (branch (label ev-if-3))
      (assign (reg exp) CDR (reg exp))
      (assign (reg exp) CADDR (reg exp))
      (goto (label eval))
    ev-if-3
      (assign (reg exp) CADDR (reg exp))
      (goto (label eval))
    append
      (test (op null?) (reg argl))
      (branch (label append-base))
      (assign (reg temp-val) (op vector-ref) (reg the-cdrs) (reg temp-1))
      (test (op null?) (reg temp-val))
      (branch (label append-2))
      (assign (reg temp-1) CDR (reg temp-1))
      (goto (label append))
    append-2
      (perform (op vector-set!) (reg the-cdrs) (reg temp-1) (reg temp-2))
      (goto (reg continue))
    append-base
      (assign (reg argl) (reg temp-2))
      (goto (reg continue))
    car
      (assign (reg p-val) (op vector-ref) (reg the-cars) (reg p-1))
      (goto (reg p-cont))
    cdr
      (assign (reg p-val) (op vector-ref) (reg the-cdrs) (reg p-1))
      (goto (reg p-cont))
    cons
      (assign (reg p-val) (reg free))
      (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
      (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
      (assign (reg free) (op increment-pointer) (reg free))
      (goto (reg p-cont))
    done)))
