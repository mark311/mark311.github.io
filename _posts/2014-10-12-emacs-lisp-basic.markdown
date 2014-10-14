---
layout: post
title:  "学习Emacs Lisp - 基础篇"
date:   2014-10-12 00:19:54
---

## 一切都是List

Lisp是LISt Processing的缩写，它是一门处理list的语言。

在Lisp语言中，数据和代码都是用list表示的。List由单词、数字或者其他list组合，并由一对圆括号左右包裹，元素之间用空白字符隔开。

这是一个list的例子：

    '(this list has (a list inside of it))

字符串这么表示：

    '(this list includes "text between quotation markds.")

## 执行程序

在Lisp中，list就是可以被执行的表达式。Lisp解释器这样执行一个list：

1. 如果list冠以单引号，则返回list本身
2. 否则将list中第一个元素视为command/function，返回运行结果。
3. 发送错误消息

执行表达式的方法是`C-x C-e`，调用函数`eval-last-sexp`。

例如，这个表达式执行后返回4：

    (+ 2 2)
    4

但是它执行后返回list本身：

    '(+ 2 2)
    (+ 2 2)

## 变量

一个symbol可以同时代表一个函数定义和一个值。例如，buffer-name是个builtin函数，它返回当前buffer的名称，给变量buffer-name赋值并不会影响buffer-name函数的执行，因为他们是相互独立的。

    (setq buffer-name 1)
    1

    (buffer-name)
    "*scratch*"

变量本身也是表达式，可以通过evaluate它来查看值，例如：

    fill-column
    80

设置变量的值用`set`或`setq`函数。例如：

    (set 'a 4)
    4

    (set 'flowers '(rose violet daisy buttercup))
    (rose violet daisy buttercup)

为什么是a和flowers前面各有一个单引号？

因为，譬如在第一个set的例子中，我们希望传入的第一个参数是*a*本身，而不是*a*执行之后的值。单引号传递的意思是*不要执行而直接返回它本身*，可以理解成C++里面的*传递引用*而非*传递值*。

`setq`和`set`基本相同，除了下面两点：

1. 第一个参数不用单引号也能表达传递引用的意思
2. 可以在同一个setq中给多个变量赋值

例如：

    (setq carnivores '(lion tiger leopard))

    (setq trees '(pine fir oak maple)
          herbivores '(gazelle antelope zebra))

虽然我这里用了“赋值”这个词语，但是准确的理解应该是“指向”，trees指向了(pine fir oak maple)。

## 与Buffer相关的函数

|Function|Description|
|---|---|
|buffer-name|返回当前buffer的名称|
|buffer-file-name|返回当前buffer对应的file的名称|
|current-buffer|返回当前buffer|
|other-buffer|返回除当前buffer外最近使用过的buffer|
|switch-to-buffer|切换buffer|


## 定义函数

定义函数的语法：

{% highlight lisp %}
(defun function-name (arguments...)
  "optional-documentation..."
  (interactive argument-passing-info)    ; optional
  body...)
{% endhighlight %}

一个函数定义有5个部分：

1. **函数名** - 函数的符号名称
2. **参数列表** - 没有参数也要用空列表`()`表示
3. **函数文档** - 可选，但强烈建议添加
4. **交互表达式** - 让你可以用M-x执行函数
5. **函数体** - 函数的代码主体

例子：
{% highlight lisp %}
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (* 7 number))
{% endhighlight %}


定义Interactive的函数

{% highlight lisp %}
(defun multiply-by-seven (number)       ; Interactive version.
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))
{% endhighlight %}


(interactive "p")的意思是*交互参数*作为函数的第一个参数传递，交互参数通过`C-u <number>`来指定。

例如：`C-u 8 M-x multiply-by-seven RET`等价于执行`(multiply-by-seven 8)`

函数体可以包含一条或者多条表达式，函数的返回值等于最后一条表达式的返回值。

## let

let用来定义局部变量，变量的作用于仅限于let调用的body部分，语法如下：

{% highlight lisp %}
(let ((variable value)
      (variable value)
      ...)
  body...)
{% endhighlight %}

例如：

(let ((zebra 'stripes)
      (tiger 'fierce))
  (message "One kind of animal has %s and another is %s."
           zebra tiger))

同setq一样，let的变量不需要加单引号。

## if

if的语法：

{% highlight lisp %}
(if true-or-false-test
     action-to-carry-out-if-the-test-returns-true
   action-to-carry-out-if-the-test-returns-false)
{% endhighlight %}

if的条件测试规则：`nil`为false，其余所有为true。

`nil`和`()`是完全相同的东西。

## car, cdr

* `car` 返回list的第一个元素（等同与`first`函数）
* `cdr` 返回list除第一个外的剩余元素（等同于`rest`函数）

{% highlight lisp %}
(car '(rose violet daisy buttercup))
rose

(cdr '(rose violet daisy buttercup))
(violet daisy buttercup)

(first '(rose violet daisy buttercup))
rose

(rest '(rose violet daisy buttercup))
(violet daisy buttercup)
{% endhighlight %}

学习car和cdr的目的是能够读懂先前的代码，但是自己在写代码的时候应当使用first和rest。

car和cdr都是非破坏性的，他们不会改变list的结构，只是返回其中的部分元素。

## cons

`cons`把一个元素和一个list拼接成一个新的list：
{% highlight lisp %}
(cons 'pine '())
(pine)

(cons 'pine '(fir oak maple))
(pine fir oak maple)

(cons '(pine fir) '(oak maple))
((pine fir) oak maple)
{% endhighlight %}

cons不改变原来的list，而是创建一个新的list。

## length

计算list的长度：
{% highlight lisp %}
(length '(pine fir oak maple))
4
{% endhighlight %}

## List的实现

List的实现是一个**单项链表**。

这里定义了一个名为bouquet的list，其中包括三个元素：

{% highlight lisp %}
(setq bouquet '(rose violet buttercup))
{% endhighlight %}

把它表示成链表的形式，如下图所示：

     bouquet
      |
      |    --------------       ---------------       ----------------
      |   | car   | cdr  |     | car    | cdr  |     | car     | cdr  |
       -->| rose  |   o------->| violet |   o------->| butter- |  nil |
          |       |      |     |        |      |     | cup     |      |
           --------------       ---------------       ----------------


## Review

[总结了一些基本的函数用法](http://www.gnu.org/software/emacs/manual/html_node/eintr/Review.html#Review)




## References

[An Introduction to Programming in Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)

[Emacs Lisp Reference Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)
