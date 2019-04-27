---
layout: page
lang: en
ref: faq
title: FAQ
permalink: /en/faq.html
---

### Q. I found a terrible bug: TRegExpr raises Access Violation exception!

#### A.
You must create the object before usage. So, after You declared something like:

    r : TRegExpr
    
do not forget to create the object instance: 

    r := TRegExpr.Create. 
    
### Q. How can I use TRegExpr with Borland C++ Builder?

I have a problem since no header file (.h or .hpp) is available.

#### A.
* Add RegExpr.pas to bcb project.
* Compile project. This generates the header file RegExpr.hpp.
* Now one can write code which uses the RegExpr unit.
* Don`t forget to add  \#include "RegExpr.hpp" where needed.
* Don`t forget to replace all `\\` in regular expressions with `\\\\` or
redefined [EscChar](tregexpr_interface.html#escchar) const.

### Q. Why many r.e. (including r.e. from TRegExpr help and demo) work wrong in Borland C++ Builder?

#### A.
Please, reread answer to previous question ;) Symbol `\\` has special
treting in C++, so You have to `escape` it (as described in
prev.answer). But if You don`t like r.e. like
`\\\\w+\\\\\\\\\\\\w+\\\\.\\\\w+` You can redefine constant EscChar
(RegExpr.pas), for example EscChar=`/` - then r.e. will be
`/w+\\/w+/./w+`, sligtly unusual but more readable..

### Q. Why does TRegExpr return more then one line?

For example, r.e. `<font .\*>` returns the first `<font`, then the
rest of the file including last `</html>`.

#### A.
For backward compatibility, [modifier
/s](regexp_syntax.html#modifier_s) is `On` by default.

Switch it Off and `.` will match any but [Line
separators](regexp_syntax.html#syntax_line_separators) - as you wish.

BTW I suggest you `<font (\[^\\n>\]\*)>`, in Match\[1\] will be
URL.

### Q. Why does TRegExpr return more then I expect?

For example r.e. `<p>(.+)</p>` applyed to string
`<p>a</p><p>b</p>` returns
`a</p><p>b` but not `a` as I expected.

#### A.
By default all operators works in `greedy` mode, so they match as more
as it possible.

If You want `non-greedy` mode You can use `non-greedy` operators like
`+?` and so on (new in v. 0.940) or switch all operators into
`non-greedy` mode with help of modifier `g` (use appropriate TRegExpr
properties or constractions like `?(-g)` in r.e.).

### Q. How to parse sources like HTML with help of TRegExpr

#### A.
Sorry folks, but it`s nearly impossible!

Of course, You can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if You want accurate parsing You
have to use real parser, not r.e.!

You can read full explanation in Tom Christiansen and Nathan Torkington
`Perl Cookbook`, for example. In short - there are many constractions
that can be easy parsed by real parser but cannot at all by r.e., and
real parser is MUCH faster do the parsing, because r.e. doesn`t simply
scan input stream, it performes optimization search that can take a lot
of time.

### Q. Is there a way to get multiple matchs of a pattern on TRegExpr?

#### A.
You can make loop and iterate match by match with ExecNext method.

It cannot be done more easily becase of Dalphi isn`t interpretator as
Perl (and it`s benefit - interpretators work very slow!).

If You want some example, please take a look at TRegExpr.Replace method
implementation. or at the examples in
[HyperLinksDecorator.pas](#hyperlinksdecorator.html)

### Q. I am checking user input. Why does TRegExpr return `True` for wrong input strings?

#### A.
In many cases TRegExpr users forget that regular expression is for
SEARCH in input string. So, if You want to make user to enter only 4
digits and using for it `\\d{4,4}` expression, You can skip wrong user
input like `12345` or `any letters 1234 and anything else`. You have to
add checking for line start and line end to ensure there are not
anything else around: `^\\d{4,4}$`.

### Q.
Why does non-greedy iterators sometimes work as in greedy mode?

For example, the r.e. `a+?,b+?` applied to string `aaa,bbb` matches
`aaa,b`, but should it not match `a,b` because of non-greediness of
first iterator?

#### A.
This is the limitation of used by TRegExpr (and Perl`s and many Unix`s
regular expressions) mathematics - r.e. performe only `simple` search
optimization, and do not try to do the best optimization. In some cases
it`s bad, but in common it`s rather advantage then limitation - because
of perfomance and predictability reasons.

The main rule - r.e. first of all try to match from current place and
only if it`s completely impossible move forward by one char and try
again from that place. So, if You use `a,b+?` it match `a,b`, but in
case of `a+?,b+?` it`s `not recommended` (due to non-greediness) but
possible to match more then one `a`, so TRegExpr do it and at last
obtaines correct (but non optimum) match. TRegExpr like Perl`s or Unix`s
r.e. doesn`t attempt to move forward and check - would it be `better`
match. Moreover, it cannot be compared in terms `more or less good
match` at all..

Please, read [Syntax](regexp_syntax.html) for more explanation.
