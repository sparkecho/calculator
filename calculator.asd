;;;; calculator.asd

(asdf:defsystem #:calculator
  :description "A simple calculator application writen with Common Lisp"
  :author "Sparkecho <echozhz@126.com>"
  :license "GPL"
  :serial t
  :components ((:file "package")
               (:file "operators"  :depends-on ("package"))
               (:file "rpn" :depends-on ("operators" "package"))
               (:file "expr-parser" :depends-on ("package"))
               (:file "calculator" :depends-on ("expr-parser" "rpn" "package"))))

