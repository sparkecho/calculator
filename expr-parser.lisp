;; expr-parser.lisp

;; The parser of the expression with string format.
;; Turn the infix expression string to prefix expression of Lisp.

(in-package #:calculator)

(defun expr-parser (str)
  (reduce-expr nil str 0 (length str)))

;; Adjust the order of each function, due to functions are
;; right to left combined operation.
;; Every function call is a function name followed with a
;; parameter list.
;; Change this form to a parameter list followed with the
;; function name.
;; (defun adjust4func (expr)
;;   (let ((element (car expr)))
;;      (cond ((null expr) nil)
;;                ((function-p element)
;;                 (multiple-value-bind (processed rest-expr)
;;                         (reverse-notation (adjust4func (cdr expr)) element))


;; Find the position of the corresponding close parenthesis
;; of the given open parenthesis.
;; Then put the given function behind the close parenthesis.
(defun reverse-notation (expr func)
  (multiple-value-bind (processed rest-expr)
      ;; when expr = (a...b), give reverse-func a...b)
      (reverse-func (cdr expr) func 1)
    (values (cons "(" processed) rest-expr)))

;; The auxilary function of reverse-notation
;; Variable `cnt' is the number of open parenthesis.
(defun reverse-func (expr func cnt)
  (let ((element (car expr)))
    ;; when number of open parenthesis is zero
    ;; it turns that parentesises are all closed.
    (cond ((zerop cnt) (values (cons func expr) expr))
          ((numberp element)
           (cons element (reverse-func (cdr expr) func cnt)))
          (t (cond ((string= element "(")
                    (cons element (reverse-func (cdr expr) func (+ cnt 1))))
                   ((string= element ")")
                    (cons element (reverse-func (cdr expr) func (- cnt 1))))
                   (t (cons element (reverse-func (cdr expr) func cnt))))))))


;; The reduction of the raw expression string.
(defun reduce-expr (stack str pos len)
  (if (>= pos len)
      (nreverse stack)
      (let* ((c   (char str pos))
             (n   (digit-char-p c))             ;judge if c is a digit, if it is not `n<-nil'
             (top (car stack)))
        (if n                                                   ;if n is a number goto then clause.
            (if (numberp top)
                (reduce-expr (cons (+ (* top 10) n) (cdr stack)) str (+ pos 1) len)
                (reduce-expr (cons n stack) str (+ pos 1) len))
            (if (numberp top)
                (reduce-expr (cons (string c) stack) str (+ pos 1) len)
                (let ((con-op (concatenate 'simple-string
                                           top
                                           (string c)))) ;conbined operator
                  (if (and (leagal-operator-p top)
                           (not (leagal-operator-p con-op)))
                      (reduce-expr (cons (string c) stack) str (+ pos 1) len)
                      (reduce-expr (cons con-op (cdr stack))
                                   str
                                   (+ pos 1)
                                   len))))))))
;; (reduce-expr (cons (concatenate 'simple-string
;;                                                              top
;;                                                              (string c))
;;                                 (cdr stack))
;;                       str (+ pos 1) len))))))




;; Expresion Reader
(defun expr-reader ()
  (remove-blank (read-string)))

;; Read string untile the character #\; occures
(defun read-string ()
  (let ((str (read-line)))
    (let ((pos (position #\; str)))
      (if pos
          (subseq str 0 pos)
          (concatenate 'simple-string str (read-string))))))


;; Remove all blank characters of the string str.
(defun remove-blank (str)
  (remove #\Tab
          (remove #\Newline
                  (remove #\Space str))))


;; Judge if a given string stands for a leagal operator/function.
(defun leagal-operator-p (str)
  (or (operator-p str)
      (function-p str)))

;; Judge if a given string stands for a leagal operator.
(defun operator-p (str)
  (member str '("+" "-" "*" "/" "^" "%" "!" "=" "&" "|"
                "~" "&&" "||" "!" "(" ")")
          :test #'string=))

;; Judge if a given string stands for a function.
(defun function-p (str)
  (member str '("fib" "sqrt" "cbrt")
          :test #'string=))
