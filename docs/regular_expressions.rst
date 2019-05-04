Regular expressions
===================

Introduction
------------

Regular expressions are a handy way to specify patterns of
text.

So you can validate user input, search for some patterns like
emails of phone numbers on web pages or in some documents and so on.

Below is complete regular expressions cheat sheet just on one page.

Simple matches
--------------

Any single character matches itself.

A series of characters matches that series of characters in the target
string, so the pattern ``bluh`` would match ``bluh`` in the target
string.

If you want to use some symbol with special meaning (see below) as plain
symbol you have to "escape" it with backslash ``\``:

=============== ======================
``foobar``      matches ``foobar``
``\^FooBarPtr`` matches ``^FooBarPtr``
=============== ======================

Non-Printable Characters
------------------------

============== ====================================================================================
``\xnn``       character with hex code ``nn``
``\x{nnnn}``   character with hex code ``nnnn`` (one byte for plain text and two bytes for Unicode)
``foo\x20bar`` ``foo bar`` (note space in the middle)
``\tfoobar``   ``foobar`` preceded by TAB
============== ====================================================================================

There are a number of predefined non-printable character classes
just like in ``C`` language:

======== ==========================================================================
``\t``   tab (HT/TAB), same as ``\x09``
``\n``   newline (NL), same as ``\x0a``
``\r``   car.return (CR), same as ``\x0d``
``\f``   form feed (FF), same as ``\x0c``
``\a``   alarm (BEL), same as ``\x07``
``\e``   escape (ESC), same as ``\x1b``
======== ==========================================================================

User Character Classes
----------------------

You can specify character class, by enclosing a list of characters in
``[]``. This class will match any **one** character listed inside ``[]``.

If the first character after the ``[`` is ``^``, the class matches any
character **but** characters listed in the class.

================= =============================================================
``foob[aeiou]r``  ``foobar``, ``foober`` etc but not ``foobbr``, ``foobcr`` etc
``foob[^aeiou]r`` ``foobbr``, ``foobcr`` etc but not ``foobar``, ``foober`` etc
================= =============================================================

Within a list, the ``-`` character is used to specify a range, so that
``a-z`` represents all characters between ``a`` and ``z``, inclusive.

If you want ``-`` itself to be a member of a class, put it at the start
or end of the list, or escape it with a backslash. If you want ``]`` you
may place it at the start of list or escape it with a backslash.

========= =========================================
[-az]     ``a``, ``z`` and ``-``
[az-]     ``a``, ``z`` and ``-``
[a\-z]    ``a``, ``z`` and ``-``
[a-z]     all characters from ``a`` to ``z``
[\n-\x0D] any of ``#10``, ``#11``, ``#12``, ``#13``
========= =========================================

Predefined Character classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

======     =========================================
``\w``     an alphanumeric character (including "_")
``\W``     a nonalphanumeric
``\d``     a numeric character
``\D``     a non-numeric
``\s``     any space (same as [ \t\n\r\f])
``\S``     a non space
======     =========================================

You may use ``\w``, ``\d`` and ``\s`` within `user character classes <User Character Classes_>`_.

=============== =====================================================================================
``foob\dr``     ``foob1r``, ``foob6r`` and so on but not ``foobar``, ``foobbr`` and so on
``foob[\w\s]r`` ``foobar``, ``foob r``, ``foobbr`` and so on but not ``foob1r``, ``foob=r`` and so on
=============== =====================================================================================

TRegExpr uses properties
`SpaceChars <tregexpr.html#spacechars>`_ and
`WordChars <tregexpr.html#wordchars>`_ to define
character classes ``\w``, ``\W``, ``\s``, ``\S``, so you can easely
redefine it.

Metacharacters
--------------

Metacharacters are special characters which are the essence of Regular
Expressions. There are different types of metacharacters, described
below.

Line separators
~~~~~~~~~~~~~~~

============= ================================================
``^``         start of line
``$``         end of line
``\A``        start of text
``\Z``        end of text
``.``         any character in line
``^foobar``   ``foobar`` only if it's at the beginning of line
``foobar$``   ``foobar`` only if it's at the end of line
``^foobar$``  ``foobar`` only if it's the only string in line
``foob.r``    ``foobar``, ``foobbr``, ``foob1r`` and so on
============= ================================================

The ``^`` metacharacter by default is matchthe
beginning of the input string, the ``$`` at the
end.

You may, however, wish to treat a string as a multi-line text,
so ``^`` will match after any line separator within the string,
and ``$`` will match before any line separator. You can do this by
switching ``On`` the `modifier /m <#m>`_.

The ``\A`` and ``\Z`` are just like ``^`` and ``$``, except that they
won’t match multiple times when the `modifier
/m <#m>`_ is used.

The ``.`` metacharacter by default matches any character, but if you
switch ``Off`` the `modifier /s <#s>`_, then
``.`` won’t match embedded line separators.

Tech details
^^^^^^^^^^^^

`TRegExpr <tregexpr.html>`_ works with line separators as recommended at
`www.unicode.org <http://www.unicode.org/unicode/reports/tr18/>`__:

``^`` is at the beginning of a input string, and, if `modifier
/m <#m>`_ is On, also immediately following
any occurrence of ``\x0D\x0A`` or ``\x0A`` or ``\x0D`` (if you are using
`Unicode version <tregexpr.html#unicode>`__ of TRegExpr, then
also ``\x2028`` or  ``\x2029`` or ``\x0B`` or ``\x0C`` or ``\x85``).
Note that there is no empty line within the sequence ``\x0D\x0A``.

``$`` is at the end of a input string, and, if `modifier
/m <#m>`_ is On, also immediately preceding
any occurrence of  ``\x0D\x0A`` or ``\x0A`` or ``\x0D`` (if you are
using `Unicode version <tregexpr.html#unicode>`__ of TRegExpr,
then also ``\x2028`` or  ``\x2029`` or ``\x0B`` or ``\x0C`` or
``\x85``). Note that there is no empty line within the sequence
``\x0D\x0A``.

``.`` matchs any character, but if you switch Off `modifier
/s <#s>`_ then ``.`` doesn’t match
``\x0D\x0A`` and ``\x0A`` and ``\x0D`` (if you are using `Unicode
version <tregexpr.html#unicode>`__ of TRegExpr, then also
``\x2028`` and  ``\x2029`` and ``\x0B`` and ``\x0C`` and ``\x85``).

Note that ``^.*$`` (an empty line pattern) does not match the empty
string within the sequence ``\x0D\x0A``, but matchs the empty string
within the sequence ``\x0A\x0D``.

Multiline processing can be easely tuned for your own purpose with help
of TRegExpr properties
`LineSeparators <tregexpr.html#lineseparators>`_ and
`LinePairedSeparator <tregexpr.html#linepairedseparator>`_,
you can use only Unix style separators ``\n`` or only DOS/Windows style
``\r\n`` or mix them together (as described above and used by default)
or define your own line separators!

Word boundaries
~~~~~~~~~~~~~~~

====== ===================
``\b`` a word boundary
``\B`` a non-word boundary
====== ===================

A word boundary ``\b`` is a spot between two characters that has a
``\w`` on one side of it and a ``\W`` on the other side of it (in either
order), counting the imaginary characters off the beginning and end of
the string as matching a ``\W``.

.. _iterator:

Iterators
~~~~~~~~~

Any item of a regular expression may be followed by iterator.
Iterator specify number of repetition of the item.

========== ============================================================
``{n}``    exactly ``n`` times
``{n,}``   at least ``n`` times
``{n,m}``  at least ``n`` but not more than ``m`` times
``*``      zero or more, similar to ``{0,}``
``+``      one or more, similar to ``{1,}``
``?``      zero or one, similar to ``{0,1}``
``{n}?``   exactly ``n`` times, ``non-greedy``
``{n,}?``  at least ``n`` times, ``non-greedy``
``{n,m}?`` at least ``n`` but not more than ``m`` times, ``non-greedy``
``*?``     zero or more, ``non-greedy``, similar to ``{0,}?``
``+?``     one or more, ``non-greedy``, similar to ``{1,}?``
``??``     zero or one, ``non-greedy``, similar to ``{0,1}?``
========== ============================================================

So, digits in curly brackets ``{n,m}``, specify the minimum
number of times to match ``n`` and the maximum ``m``.

The ``{n}`` is equivalent to ``{n,n}`` and matches exactly ``n`` times.

The ``{n,}`` matches ``n`` or more times.

There is no limit to the size
of ``n`` or ``m``, but large numbers will chew up more memory and slow
down r.e. execution.

If a curly bracket occurs in any other context, it is treated as a
regular character.

================== ========================================================================
``foob.*r``        ``foobar``,  ``foobalkjdflkj9r`` and ``foobr``
``foob.+r``        ``foobar``, ``foobalkjdflkj9r`` but not ``foobr``
``foob.?r``        ``foobar``, ``foobbr`` and ``foobr`` but not ``foobalkj9r``
``fooba{2}r``      ``foobaar``
``fooba{2,}r``     ``foobaar'``, ``foobaaar``, ``foobaaaar`` etc.
``fooba{2,3}r``    ``foobaar``, or ``foobaaar``  but not ``foobaaaar``
``(foobar){8,10}`` strings which contain ``8``, ``9`` or ``10`` instances of the ``foobar``
================== ========================================================================

Greediness
~~~~~~~~~~

`Iterators <#iterator>`_ in ``greedy`` mode takes as many as possible,
in ``non-greedy`` mode - as few as possible.

By default all iterators are ``greedy``.
Use ``?`` to make any iterator ``non-greedy``.

For string ``abbbbc``:

=========== ============
``b+``      ``bbbb``
``b+?``     ``b``
``b*?``     empty string
``b{2,3}?`` ``bb``
``b{2,3}``  ``bbb``
=========== ============

You can switch all iterators into ``non-greedy`` mode (`modifier /g <#g>`_,
below we use `in-line modifier change <#inlinemodifiers>`_).

============ ============
``(?-g)b+``  ``b``
============ ============

Alternatives
~~~~~~~~~~~~

Series of alternatives are separated by ``|``.

So ``fee|fie|foe`` will match any of ``fee``, ``fie``,
or ``foe`` in the target string (as would ``f(e|i|o)e``).

The first alternative includes everything from the last pattern delimiter (``(``,
``[``, or the beginning of the pattern) up to the first ``|``, and the
last alternative contains everything from the last ``|`` to the next
pattern delimiter.

Sounds a little complicated, so it’s common practice to include
alternatives in parentheses, to minimize confusion about where they
start and end.

Alternatives are tried from left to right, so the first alternative
found for which the entire expression matches, is the one that is
chosen.

This means that alternatives are not necessarily ``greedy``. For
example, regular expression ``foo|foot`` in string ``barefoot`` will match ``foo``.
Just a first alternative that's match.

Also remember that ``|`` is interpreted as a literal within square
brackets, so if you write ``[fee|fie|foe]`` you’re really only matching
``[feio|]``.

================ ========================
``foo(bar|foo)`` ``foobar`` or ``foofoo``
================ ========================

.. _subexpression:

Subexpressions
~~~~~~~~~~~~~~

The brackets ``( ... )`` may also be used to define regular expression
subexpressions.

Subexpression positions, lengths and actual values will be in
`MatchPos <tregexpr.html#matchpos>`_,
`MatchLen <tregexpr.html#matchlen>`_ and
`Match <tregexpr.html#match>`_.

You can substitute them with
`Substitute <tregexpr.html#substitute>`_).

Subexpressions are numbered from left to right by their
opening parenthesis (including nested subexpressions).

First subexpression has number ``1``. Whole regular expression match has number ``0``.

Expression ``(foo(bar))`` for string ``foobar``: ``subexpression 1``
match ``foobar``, ``2`` - ``bar`` and ``0`` - ``foobar``

Backreferences
~~~~~~~~~~~~~~

Metacharacters ``\1`` through ``\9`` are interpreted as backreferences.
``\n`` matches previously matched subexpression ``n``.

=========== ============================
``(.)\1+``  ``aaaa`` and ``cc``
``(.+)\1+`` also ``abab`` and ``123123``
=========== ============================

 ``(['"]?)(\d+)\1`` matchs ``"13"`` (in double quotes), or ``'4'`` (in
single quotes) or ``77`` (without quotes) etc

Modifiers
---------

Modifiers are for changing behaviour of ``TRegExpr``.

There are two ways to set up modifiers:

1)
Embed within the regular expression using
the `(?imsxr-imsxr) <#inlinemodifiers>`_.

2)
Assign to appropriate ``TRegExpr`` property
(`Modifier* <tregexpr.html#modifierstr>`__. The
default values for new instances of TRegExpr object defined in `global
variables <tregexpr.html#global-constants>`_. For example global variable
``RegExprModifierX`` defines default value for ``ModifierX`` property.

i
~

Case-insensitive pattern matching (using installed in you system
locale settings), see also
`InvertCase <tregexpr.html#invertcase>`__.

m
~

Treat string as multiple lines. So ``^`` and ``$`` matches the start or end
of any line anywhere within the string.

See also `Line
separators <tregexpr.html#lineseparators>`_.

s
~

Treat string as single line. So ``.`` matches any
character whatsoever, even a line separators.

See also `Line
separators <tregexpr.html#lineseparators>`_, which it
normally would not match.

g
~

Non standard modifier.

Switching it ``Off`` you’ll switch all following
operators into non-greedy mode. So, if
modifier ``/g`` is ``Off`` then ``+`` works as ``+?``, ``*`` as ``*?`` and
so on.

By default this modifier is ``On``.

x
~

Tells the ``TRegExpr`` to ignore whitespace that
is neither backslashed nor within a character class. You can use this to
break up your regular expression into more readable parts.

The ``#`` character is also treated as a metacharacter introducing a
comment.

.. code-block:: text

    (
    (abc) \# comment 1
      |   \# you can use spaces to format r.e. - TRegExpr ignores it
    (efg) \# comment 2
    )

This also means that if you want real whitespace or ``#`` characters in
the pattern (outside a character class, where they are unaffected by
``/x``), you’ll either have to escape them or encode them using
octal or hex escapes.

r
~

Non-standard modifier.

If is set then range ``а-я`` includes
also ``ё``. And ``А-Я`` includes also ``Ё``. And ``а-Я``
includes all russian symbols.

The modifier is set `On` by default.

Extensions
----------

.. _lookahead:

(?=<lookahead>)
~~~~~~~~~~~~~~~

Look ahead assertion. It checks input for the regular expression ``<look-ahead>``,
but do not capture it. In many cases you can replace it with simple
`Sub-expression <#subexpression>`_ but you have to ignore what will be captured in this subexpression.

So ``(blah)(?=foobar)(blah)`` is the same as ``(blah)(foobar)(blah)`` but in
the latter version you have to exclude the middle manually - use
``Match[1] + Match[3]`` and ignore ``Match[2]``.
This is just not so convenient as in the former version where you can use
whole ``Match[0]`` because captured by ``(?=...)`` part would not be included
in the regular expression match.


.. _inlinemodifiers:

(?imsxr-imsxr)
~~~~~~~~~~~~~~

You may use it into r.e. for modifying modifiers by the fly. If this
construction inlined into subexpression, then it effects only into this
subexpression

============================= ==================================================
``(?i)Saint-Petersburg``      ``Saint-petersburg`` and ``Saint-Petersburg``
``(?i)Saint-(?-i)Petersburg`` ``Saint-Petersburg`` but not ``Saint-petersburg``
``(?i)(Saint-)?Petersburg``   ``Saint-petersburg`` and ``saint-petersburg``
``((?i)Saint-)?Petersburg``   ``saint-Petersburg``, but not ``saint-petersburg``
============================= ==================================================

(?#text)
~~~~~~~~

A comment, the text is ignored. Note that TRegExpr closes the comment as
soon as it sees a ``)``, so there is no way to put a literal ``)`` in
the comment.

Just now don’t forget to read the `FAQ <faq.html>`_ (expecially
‘non-greediness’ optimization
`question <faq.html#nongreedyoptimization>`_).

Play ground
-----------

You can play with regular expressions using Windows
`REStudio <https://github.com/masterandrey/TRegExpr/releases/download/0.952b/REStudio.exe>`_.


