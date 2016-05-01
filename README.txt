Calculator

	This project is writen just for practise. It's a simple calculator

application. This calculator implemented functions below:
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
	~                         按位取反
	&&                        逻辑与运算
	||                        逻辑或运算
	!                         逻辑非运算
	()                        括号
	fib(n)                    计算第 n 个斐波那契数
	sqrt(n)                   计算 n 的平方根
	cbrt(n)                   计算 n 的立方根



	+                         Addition
	-                         Subtraction
	*                         Multiplication
	/                         Division
	^                         Exp
	%                         Mod
	!                         Factorial
	=                         Judge if equal
	&                         按位与运算
	|                         按位或运算
	~                         按位取反
	&&                        逻辑与运算
	||                        逻辑或运算
	!                         逻辑非运算
	()                        括号
	fib(n)                    Calculate the nth fibonacci number
	sqrt(n)                   Calculate the square root of n
	cbrt(n)                   Calculate the cube root of n

    运行方式:
    方法一: 使用 quicklisp 加载
        1. 将源代码文件置于 ~/quicklisp/local-project/ 目录下
	2. 在 Common Lisp 的 REPL 中执行 (quickload :calculator)
	3. 执行函数 (calculator:menu) 即可运行该计算器的 repl
	4. 每个表达式以分号 ; 结尾, 如果要退出, 输入 exit; 然后回车

    方法二: 使用 asdf 加载
	1. 在 Common Lisp 的 REPL 中执行 (load "path/of/calculator.asd")
	2. 然后执行 (asdf:load-system :calculator)
	3. 同方法一
	4. 同方法一
