(load "objects.scm")

;;; ******************************
;;; globals and machine components
;;; ******************************

;; a special label for the start of the RML, so all the code will appear in
;; the code table
(define start-label '--START-LABEL)

(define (label? line) (not (pair? line)))

;; getters for the four components of a machine
(define (machine-registers m) (car m))
(define (machine-operators m) (cadr m))
(define (machine-stack m) (caddr m))
(define (machine-code m) (cadddr m))

(define (set-register-contents machine reg-name value)
    (set-reg-contents (cdr (get-table (machine-registers machine) reg-name)) value))
(define (get-register-contents machine reg-name)
    (get-reg-contents (cdr (get-table (machine-registers machine) reg-name))))
(define (get-operator machine op-name)
    (cdr (get-table (machine-operators machine) op-name)))



;;; ************
;;; make machine
;;; ************

;; a machine is a list of length 4:
;; * reg-table is a reg-name -> register lookup table
;; * op-table is an op-name -> operator  lookup table
;; * stack is a stack object
;; * code-table is a label -> code lookup table; the code
;;   is the final segment of raw code, starting just after the label.
(define (make-machine reg-names op-list instructions)
    (let ((reg-table (make-table))
          (op-table (make-table))
          (stack (make-stack))
          (code-table (make-table)))

    (define (populate-reg-table reg-names)
        (if (null? reg-names)
            'populated-reg-table
            (begin (set-table reg-table (car reg-names) (make-register))
                   (populate-reg-table (cdr reg-names)))))

    (define (populate-op-table op-list)
        (if (null? op-list)
            'populated-op-table
            (begin (set-table op-table (caar op-list) (cdar op-list))
                   (populate-op-table (cdr op-list)))))

    (define (populate-code-table instructions)
        (if (null? instructions)
            'populated-code-table
            (begin
                (if (label? (car instructions))
                  (set-table code-table (car instructions) (cdr instructions)))
            (populate-code-table (cdr instructions)))))

    (populate-reg-table reg-names)
    (populate-op-table op-list)
    (populate-code-table (cons start-label instructions)) ; add the start label
    (list reg-table op-table stack code-table)))


;;; ***********
;;; run machine
;;; ***********

(define stack-depth 0)
(define max-depth 0)

(define (run machine)

    ;; runs one line of code. Returns the rest of the code to be run,
    ;; which will usually be (cdr code), but not always (e.g. test, goto).
    ;; Each process-X routine is responsible for returning the correct
    ;; rest of code to be run.
    (define (process-line code)
        ; display line
        (if (not (null? code)) (begin (newline) (display '...) (display (car code))

            ))

        (cond ((null? code) 'done!)
              ((label? (car code)) (process-line (process-label code)))
              ((goto? (car code)) (process-line (process-goto code)))
              ((assign? (car code)) (process-line (process-assign code)))
              ((perform? (car code)) (process-line (process-perform code)))
              ((test? (car code)) (process-line (process-test code)))
              ((push? (car code)) (process-line (process-push code)))
              ((pop? (car code)) (process-line (process-pop code)))))


    ;;; **********************
    ;;; processing expressions
    ;;; **********************

    ;;; each test takes one line as input,
    ;;; and assumes it is a pair (i.e. not a label).
    ;;;
    ;;; each process-X routine takes all remaining code as input,
    ;;; and is responsible for returning the correct rest of code.


    ;; label
    (define (process-label code) (cdr code))

    ;; goto and branch
    (define (goto? line) (or (eq? 'goto (car line)) (eq? 'branch (car line))))
    (define (process-goto code)
        (let ((destination (cadr (car code))))
        (cdr (get-table (machine-code machine) (get-RML-value destination)))))

    ;; assign
    ;; source is the name of the register assigned into.
    ;; destination is the list of everything after; e.g.
    ;; ((op +) (const 1) (const 1)) or ((reg b)).
    (define (assign? line) (eq? 'assign (car line)))
    (define (process-assign code)
        (let ((source (cddr (car code)))
              (destination (cadr (cadr (car code)))))
        (set-register-contents
            machine
            destination
            (get-RML-value (if (eq? 'op (caar source))
                                source
                                (car source)))))
        (cdr code))


    ;; exp is something like (reg a), (const 3),
    ;; or ((op +) (const 1) (const 2)).
    ;; It is always a pair; its car is a pair iff we're doing op.
    (define (get-RML-value exp)
        (cond ((eq? 'reg (car exp)) (get-register-contents machine (cadr exp)))
              ((pair? (car exp)) ; if exp is an operator expression
                (apply (get-operator machine (cadar exp))
                       (map get-RML-value (cdr exp))))
              (else (cadr exp))))

    ;; perform
    (define (perform? line) (eq? 'perform (car line)))
    (define (process-perform code)
        (get-RML-value (cdr (car code)))
        (cdr code))

    ;; test
    (define (test? line) (eq? 'test (car line)))
    (define (process-test code)
        (if (get-RML-value (cdr (car code)))
            (cdr code)
            (cddr code))) ; if test is false, skip the next line.


    ;; push and pop
    (define (push? line) (eq? 'push (car line)))
    (define (process-push code)
        (set! stack-depth (+ stack-depth 1))
        (update-max-depth)
        (push (machine-stack machine) (get-RML-value (cadr (car code))))
        (cdr code))
    (define (pop? line) (eq? 'pop (car line)))
    (define (process-pop code)
        (set! stack-depth (- stack-depth 1))
        (update-max-depth)
        (let ((destination (cadadr (car code))))
        (set-register-contents machine destination (pop (machine-stack machine))))
        (cdr code))

    ;; max-depth

    (define (update-max-depth)
      (if (> stack-depth max-depth)
          (set! max-depth stack-depth)))

    ;; start!
    (process-line (cdr (get-table (machine-code machine) start-label)))
)


;;; *******
;;; testing
;;; *******
