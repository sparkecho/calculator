;;;; calculator.lisp

(in-package #:calculator)


;; Provide the basic control of this program.
(defun menu ()
  (welcome-prompt)
  (prompt)
  (do ((expr (expr-reader) (expr-reader)))
      (nil)
    (cond ((string-equal expr "help") (help))
          ((string-equal expr "exit") (return (princ "Bye.")))
          (t (print-result (calculate expr))
             (prompt)))))


;; Calculate: calculate the prefix expression of Lisp to a result.
(defun calculate (expr)
  (calc-rpn (infix2rpn (expr-parser expr))))


;; Print the result in a pretty style. 
(defun print-result (result)
  (format t "~&~A" result))


;; Print the basic command of this program.
(defun welcome-prompt ()
  (format t "~&Welcome to Calculator v1")
  (format t "~&Commands:")
  (format t "~&Input \'help;\' to get help.")
  (format t "~&Input \'exit;\' or \'quit\' to exit from this program.")
  (force-output))

;; Print the prompt of the repl of calculator.
(defun prompt ()
  (format t "~&>> ")
  (force-output))

;; Print the help message.
(defun help ()
  (format t "~&Help:
        +                         加法
        -                         减法
        *                         乘法
        /                         除法
        ^                         乘方
        %                         取模运算
        !                         阶乘
        =                         判等
        &                         按位与运算
        |                         按位或运算
        ~a                         按位取反
        &&                        逻辑与运算
        ||                        逻辑或运算
        !                         逻辑非运算
        ()                        括号
        fib(n)                    计算第 n 个斐波那契数
        sqrt(n)                   计算 n 的平方根
        cbrt(n)                   计算 n 的立方根
" #\~)
  (force-output))
