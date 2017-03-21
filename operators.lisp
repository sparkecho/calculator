;;; operands.lisp

(in-package #:calculator)

;; ^ = expt (expt base power)

;; % = mod  (mod number divisor)

;; ! on the right
(defun fact (n)
  (if (not (integerp n))
      (format t "~&Operand must be an integer!") 
      (labels ((factorial (n)
                 (cond ((< n 0) (format t "~&Operand should be a positive number."))
                       ((< n 2) 1)
                       (t (* (factorial (- n 1)) n)))))
        (factorial n))))

;; fib(n) calculate the nth fibonacci number
(defun fib (n)
  (cond ((< n 2) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))


;; cbrt(n) calculate the cube root of number n
(defun cbrt (n)
  (expt n 1/3))
