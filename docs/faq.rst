FAQ
===

I found a terrible bug: TRegExpr raises Access Violation exception!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

You must create the object before usage. So, after you declared
something like:

.. code-block:: pascal

    r : TRegExpr

do not forget to create the object instance:

.. code-block:: pascal

    r := TRegExpr.Create. 

Regular expressions with (?=...) do not work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Look ahead is not implemented in the TRegExpr. But in many cases you can
easily `replace it with simple subexpressions <regular_expressions.html#lookahead>`_.


Does it support Unicode?
~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

`How to use Unicode <tregexpr.html#unicode>`__

Why does TRegExpr return more then one line?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example, r.e. ``<font .\*>`` returns the first ``<font``, then the
rest of the file including last ``</html>``.

**Answer**

For backward compatibility, `modifier
/s <regular_expressions.html#modifier_s>`__ is ``On`` by default.

Switch it Off and ``.`` will match any but `Line
separators <regular_expressions.html#syntax_line_separators>`__ - exactly as you wish.

BTW I suggest ``<font ([^\n>]*)>``, in ``Match[1]`` will be the URL.

Why does TRegExpr return more then I expect?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example r.e. ``<p>(.+)</p>`` applyed to string ``<p>a</p><p>b</p>``
returns ``a</p><p>b`` but not ``a`` as I expected.

**Answer**

By default all operators works in ``greedy`` mode, so they match as more
as it possible.

If you want ``non-greedy`` mode you can use ``non-greedy`` operators
like ``+?`` and so on or switch all operators into
``non-greedy`` mode with help of modifier ``g`` (use appropriate
TRegExpr properties or operator ``?(-g)`` in r.e.).

How to parse sources like HTML with help of TRegExpr
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

Sorry folks, but it's nearly impossible!

Of course, you can easily use TRegExpr for extracting some information
from HTML, as shown in my examples, but if you want accurate parsing you
have to use real parser, not r.e.

You can read full explanation in Tom Christiansen and Nathan Torkington
``Perl Cookbook``, for example.

In short - there are many structures
that can be easy parsed by real parser but cannot at all by r.e., and
real parser is much faster to do the parsing, because r.e. doesn't simply
scan input stream, it performs optimization search that can take a lot
of time.

Is there a way to get multiple matches of a pattern on TRegExpr?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

You can iterate matches with ExecNext method.

If you want some example, please take a look at ``TRegExpr.Replace`` method
implementation or at the examples for
`HyperLinksDecorator <demos.html>`_

I am checking user input. Why does TRegExpr return ``True`` for wrong input strings?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

In many cases TRegExpr users forget that regular expression is for
**search** in input string.

So, for example if you use ``\d{4,4}`` expression, you will get success for
wrong user inputs like ``12345`` or ``any letters 1234``.

You have to check from line start to line end to ensure there are no
anything else around: ``^\d{4,4}$``.

.. _nongreedyoptimization:

Why does non-greedy iterators sometimes work as in greedy mode?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For example, the r.e. ``a+?,b+?`` applied to string ``aaa,bbb`` matches
``aaa,b``, but should it not match ``a,b`` because of non-greediness of
first iterator?

**Answer**

This is because of TRegExpr way to work. In fact many others r.e. engines
work exactly the same: they performe only ``simple`` search optimization,
and do not try to do the best optimization.

In some cases it's bad, but in common it's rather advantage then limitation,
because of performance and predictability reasons.

The main rule - r.e. first of all try to match from current place and
only if that's completely impossible move forward by one char and try again
from next position in the text.

So, if you use ``a,b+?`` it'll match ``a,b``. In case of ``a+?,b+?`` it's
now not recommended (we add non-greedy modifyer) but still possible to match
more then one ``a``, so TRegExpr will do it.

TRegExpr like Perl's or Unix's r.e. doesn't attempt to move forward and
check - would it will be "better" match.
Fisrt of all, just because there is no way to say it's more or less
good match.

How can I use TRegExpr with Borland C++ Builder?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I have a problem since no header file (``.h`` or ``.hpp``) is available.

**Answer**

-  Add ``RegExpr.pas`` to ``bcb`` project.
-  Compile project. This generates the header file ``RegExpr.hpp``.
-  Now you can write code which uses the ``RegExpr`` unit.
-  Don't forget to add  ``#include “RegExpr.hpp”`` where needed.
-  Don't forget to replace all ``\`` in regular expressions with ``\\``
   or redefined `EscChar <tregexpr.html#escchar>`__ const.

Why many r.e. (including r.e. from TRegExpr help and demo) work wrong in Borland C++ Builder?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Answer**

The hint is in the previous question ;) Symbol ``\`` has special
meaning in ``C++``, so you have to ``escape`` it (as described in
previous answer). But if you
don't like r.e. like ``\\w+\\\\w+\\.\\w+`` you can redefine the constant ``EscChar``
(in ``RegExpr.pas``).
For example ``EscChar = "/"``. Then you can write ``/w+/w+/./w+``,
looks unusual but more readable.

