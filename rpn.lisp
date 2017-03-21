;; rpn.lisp
;; RPN (Reverse Polish Notation)
;; 逆波兰表示法


(in-package #:calculator)

;; (defun operatorp (str)
;;   (member str '("+" "-" "*" "/")
;;                :test #'string=))

;; 将中缀表示法转化为逆波兰表示法
;; 调度场算法, Shunting Yard Algorithm

;; prototype
(defun infix2rpn (stack)
  (reverse (in2rpn stack nil nil)))

;; Auxilary function of infix2rpn
;; Three stacks: input operators output
(defun in2rpn (input operators output)
  (cond ((and (null input) (null operators)) output)
        ((null input) (let ((op (car operators)))
                        (in2rpn input
                                (cdr operators)
                                (cons op output))))
        (t (let ((op (car input)))
             (cond ((numberp op) (in2rpn (cdr input)
                                         operators
                                         (cons op output)))
                   ((null operators) (in2rpn (cdr input)
                                             (cons op operators)
                                             output))
                   ((>= (get-weight (car operators)) (get-weight op))
                    (in2rpn input
                            (cdr operators)
                            (cons (car operators) output)))
                   (t (cond
                        ((string= op ")")
                         (cond
                           ((string= (car operators) "(")
                            (in2rpn (cdr input) (cdr operators) output))
                           (t (in2rpn input
                                      (cdr operators)
                                      (cons (car operators) output)))))
                        (t (in2rpn (cdr input) (cons op operators) output)))))))))
;; ((>= (get-weight (car operators)) (get-weight op))
;;      (let ((op (car operators)))
;;        (in2rpn input (cdr operators) (cons op output))))
;; (t (in2rpn (cdr input) (cons op operators) output)))))))




;; Get the corresponding operator/function from the operation string.
(defun get-operator (str)
  (cond ((string= str "+") '+)
        ((string= str "-") '-)
        ((string= str "*") '*)
        ((string= str "/") '/)
        ((string= str "%") 'rem)
        ((string= str "!") 'fact)
        ((string= str "fib") 'fib)
        ((string= str "sqrt") 'sqrt)
        ((string= str "cbrt") 'cbrt)
        (t nil)))

(defun get-weight (str)
  (cond ((member str '("(") :test #'string=) 1)
        ((member str '("+" "-") :test #'string=) 2)
        ((member str '("*" "/" "%") :test #'string=) 3)
        ((member str '("!" "fib" "sqrt" "cbrt") :test #'string=) 4)
        ((member str '(")") :test #'string=) 5)
        (t (error "Operation ~A is not supported!" str))))

(defun get-operand-num (str)
  (cond ((member str '("+" "-" "*" "/" "%") :test #'string=) 2)
        ((member str '("!" "fib" "sqrt" "cbrt") :test #'string=) 1)
        (t 0)))

;; Calculate the given rpn expression.
(defun calc-rpn (expr)
  (rpn-iter expr nil))

(defun rpn-iter (expr operands)
  (if (null expr)
      (car operands)
      (let ((op (car expr)))
        (if (numberp op)
            (rpn-iter (cdr expr) (cons op operands))
            (let ((num (get-operand-num op)))
              (case num
                (1 (rpn-iter (cdr expr)
                             (cons (unary-op op operands)
                                   (cdr operands))))
                (2 (rpn-iter (cdr expr)
                             (cons (binary-op op operands)
                                   (cddr operands))))
                (t (error "Unsupport number of operands."))))))))


;; The operators that accepts one argument.
(defun unary-op (op operands)
  (if (< (length operands) 1)
      (error "Operator ~A needs one operand, but there is none." op)
      (funcall (get-operator op) (car operands))))

;; The operators that accepts two arguments.
(defun binary-op (op operands)
  (if (< (length operands) 2)
      (error "Operator ~A needs two operands, but there is only ~A."
             op (length operands))
      (let* ((op2 (pop operands))
             (op1 (pop operands)))
        (funcall (get-operator op) op1 op2))))
