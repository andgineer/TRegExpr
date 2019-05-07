.. include:: translations_list.rst

Regular expressions
===================

Introduction
------------

Regular expressions are a handy way to specify patterns of
text.

With regular expressions you can validate user input, search for some
patterns like emails of phone numbers on web pages or in some documents
and so on.

Below is complete regular expressions cheat sheet just on one page.

Characters
----------

Simple matches
~~~~~~~~~~~~~~

Any single character matches itself.

A series of characters matches that series of characters in the input
string.

================== ======================
Regular expression Matches
================== ======================
``foobar``         ``foobar``
================== ======================

Non-Printable Characters
~~~~~~~~~~~~~~~~~~~~~~~~

To represent non-printable character in regular expression you use ``\x..``:

============== ====================================================================================
``\xnn``       character with hex code ``nn``
``\x{nnnn}``   character with hex code ``nnnn`` (one byte for plain text and two bytes for Unicode)
``foo\x20bar`` ``foo bar`` (note space in the middle)
``\tfoobar``   ``foobar`` preceded by TAB
============== ====================================================================================

There are a number of one-character codes for non-printable characters
just like in ``C`` language, they are prefixed by ``\``:

======== ==========================================================================
``\t``   tab (HT/TAB), same as ``\x09``
``\n``   newline (NL), same as ``\x0a``
``\r``   car.return (CR), same as ``\x0d``
``\f``   form feed (FF), same as ``\x0c``
``\a``   alarm (BEL), same as ``\x07``
``\e``   escape (ESC), same as ``\x1b``
======== ==========================================================================

.. _escape:

Escaping
~~~~~~~~

If you want to use character ``\`` in regular expression just
prefix it with ``\``, like that: ``\\``.

In fact you can ``escape`` with ``\`` any character that has special meaning
in regular expressions.

=============== ====================================================
``\^FooBarPtr`` ``^FooBarPtr``
``\[a\]``       ``[a]`` this is not `character class <#userclass>`__
=============== ====================================================

Character Classes
-----------------

.. _userclass:

User Character Classes
~~~~~~~~~~~~~~~~~~~~~~

Character class is a list of characters inside ``[]``.
The class matches any **one** character listed in this class.

You can ``invert`` the class - if the first character after the ``[`` is
``^``, then the class matches any character **but** characters listed
in the class.

================= =============================================================
``foob[aeiou]r``  ``foobar``, ``foober`` etc but not ``foobbr``, ``foobcr`` etc
``foob[^aeiou]r`` ``foobbr``, ``foobcr`` etc but not ``foobar``, ``foober`` etc
================= =============================================================

Within a list, the ``-`` character is used to specify a range, so that
``a-z`` represents all characters between ``a`` and ``z``, inclusive.

If you want ``-`` itself to be a member of a class, put it at the start
or end of the list, or `escape <#escape>`__ it with a backslash.

If you want ``]`` or ``[`` you may place it at the start of list or escape it
with a backslash.

============= ==================================
``[-az]``     ``a``, ``z`` and ``-``
``[az-]``     ``a``, ``z`` and ``-``
``[a\-z]``    ``a``, ``z`` and ``-``
``[a-z]``     characters from ``a`` to ``z``
``[\n-\x0D]`` characters from ``#10`` to ``#13``
============= ==================================

Predefined Character Classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a number of predefined character classes that save your
typing - you do not specify them by hand and can just use them.

======     =========================================
``\w``     an alphanumeric character (including ``_``)
``\W``     a nonalphanumeric
``\d``     a numeric character
``\D``     a non-numeric
``\s``     any space (same as ``[ \t\n\r\f]``)
``\S``     a non space
======     =========================================

You may use ``\w``, ``\d`` and ``\s`` within
`user character classes <User Character Classes_>`_.

=============== =====================================================================================
``foob\dr``     ``foob1r``, ``foob6r`` and so on but not ``foobar``, ``foobbr`` and so on
``foob[\w\s]r`` ``foobar``, ``foob r``, ``foobbr`` and so on but not ``foob1r``, ``foob=r`` and so on
=============== =====================================================================================

.. note::
    `TRegExpr <tregexpr.html>`__

    Properties
    `SpaceChars <tregexpr.html#spacechars>`_ and
    `WordChars <tregexpr.html#wordchars>`_ define
    character classes ``\w``, ``\W``, ``\s``, ``\S``.

    So you can redefine this classes.

Boundaries
----------

.. _lineseparators:

Line Boundaries
~~~~~~~~~~~~~~~

If you want mathematically correct description look at
`www.unicode.org <http://www.unicode.org/unicode/reports/tr18/>`__.

Here is simplified version.

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

``^`` metacharacter by default match the
beginning of the input string. ``$`` - the end.

You may, however, wish to treat a string as a multi-line text,
so ``^`` will match after any line separator within the string,
and ``$`` will match before any line separator. You can do this by
switching `modifier /m <#m>`_.

Note that there is no empty line within the sequence ``\x0D\x0A``.

.. note::
    `TRegExpr <tregexpr.html>`__

    If you are using
    `Unicode version <tregexpr.html#unicode>`__, then ``^``/``$``
    also matches ``\x2028``, ``\x2029``, ``\x0B``, ``\x0C`` or ``\x85``.

The ``\A`` and ``\Z`` are just like ``^`` and ``$``, except that they
won’t match multiple times when the `modifier
/m <#m>`_ is used.

The ``.`` metacharacter by default matches any character, but if you
switch ``Off`` the `modifier /s <#s>`_, then
``.`` won’t match line separators inside the string.

Note that ``^.*$`` does not match a string between ``\x0D\x0A``,
because this is unbreakable line separator.
But it matches the empty string within the sequence ``\x0A\x0D`` because
this is just wrong order to be treated as line separator.

.. note::
    `TRegExpr <tregexpr.html>`__

    Multiline processing can be tuned with of properties
    `LineSeparators <tregexpr.html#lineseparators>`__ and
    `LinePairedSeparator <tregexpr.html#linepairedseparator>`_.

    So you can use Unix style separators ``\n`` or DOS/Windows style
    ``\r\n`` or mix them together (as in described above default behaviour).

Word Boundaries
~~~~~~~~~~~~~~~

====== ===================
``\b`` a word boundary
``\B`` a non-word boundary
====== ===================

A word boundary ``\b`` is a spot between two characters that has a
``\w`` on one side of it and a ``\W`` on the other side of it (in either
order).

.. _iterator:

Quantification
--------------

Quantifier
~~~~~~~~~~

Any item of a regular expression may be followed by quantifier.
Quantifier specifies number of repetition of the item.

========== ============================================================
``{n}``    exactly ``n`` times
``{n,}``   at least ``n`` times
``{n,m}``  at least ``n`` but not more than ``m`` times
``*``      zero or more, similar to ``{0,}``
``+``      one or more, similar to ``{1,}``
``?``      zero or one, similar to ``{0,1}``
========== ============================================================

So, digits in curly brackets ``{n,m}``, specify the minimum
number of times to match ``n`` and the maximum ``m``.

The ``{n}`` is equivalent to ``{n,n}`` and matches exactly ``n`` times.

The ``{n,}`` matches ``n`` or more times.

There is no limit to the size of ``n`` or ``m``.

If a curly bracket occurs in any other context, it is treated as a
regular character.

================== ========================================================================
``foob.*r``        ``foobar``,  ``foobalkjdflkj9r`` and ``foobr``
``foob.+r``        ``foobar``, ``foobalkjdflkj9r`` but not ``foobr``
``foob.?r``        ``foobar``, ``foobbr`` and ``foobr`` but not ``foobalkj9r``
``fooba{2}r``      ``foobaar``
``fooba{2,}r``     ``foobaar'``, ``foobaaar``, ``foobaaaar`` etc.
``fooba{2,3}r``    ``foobaar``, or ``foobaaar``  but not ``foobaaaar``
``(foobar){8,10}`` ``8``, ``9`` or ``10`` instances of the ``foobar`` (``()`` is `Subexpression <#subexpression>`__)
================== ========================================================================

.. _greedy:

Greediness
~~~~~~~~~~

`Quantifiers <#iterator>`_ in ``greedy`` mode takes as many as possible,
in ``non-greedy`` mode - as few as possible.

By default all quantifiers are ``greedy``.
Use ``?`` to make any quantifier ``non-greedy``.

For string ``abbbbc``:

=========== ============
``b+``      ``bbbb``
``b+?``     ``b``
``b*?``     empty string
``b{2,3}?`` ``bb``
``b{2,3}``  ``bbb``
=========== ============

You can switch all quantifiers into ``non-greedy`` mode (`modifier /g <#g>`_,
below we use `in-line modifier change <#inlinemodifiers>`_).

============ ============
``(?-g)b+``  ``b``
============ ============

The choice
----------

Expressions in the choice are separated by ``|``.

So ``fee|fie|foe`` will match any of ``fee``, ``fie``,
or ``foe`` in the target string (as would ``f(e|i|o)e``).

The first expression includes everything from the last pattern delimiter (``(``,
``[``, or the beginning of the pattern) up to the first ``|``, and the
last expression contains everything from the last ``|`` to the next
pattern delimiter.

Sounds a little complicated, so it’s common practice to include
the choice in parentheses, to minimize confusion about where it
starts and ends.

Expressions in the choice are tried from left to right, so the first expression
that matches, is the one that is chosen.

For example, regular expression ``foo|foot`` in string ``barefoot`` will match ``foo``.
Just a first expression that matches.

Also remember that ``|`` is interpreted as a literal within square
brackets, so if you write ``[fee|fie|foe]`` you’re really only matching
``[feio|]``.

================ ========================
``foo(bar|foo)`` ``foobar`` or ``foofoo``
================ ========================

.. _subexpression:

Subexpressions
--------------

The brackets ``( ... )`` may also be used to define regular expression
subexpressions.

.. note::
    `TRegExpr <tregexpr.html>`__

    Subexpression positions, lengths and actual values will be in
    `MatchPos <tregexpr.html#matchpos>`_,
    `MatchLen <tregexpr.html#matchlen>`_ and
    `Match <tregexpr.html#match>`_.

    You can substitute them with
    `Substitute <tregexpr.html#substitute>`_.

Subexpressions are numbered from left to right by their
opening parenthesis (including nested subexpressions).

First subexpression has number ``1``.
Whole regular expression has number ``0``.

.. note::

    regular expression ``(foo(bar))``

    for input string ``foobar``:

    =================== ==========
    ``subexpression 0`` ``foobar``
    ``subexpression 1`` ``foobar``
    ``subexpression 2`` ``bar``
    =================== ==========

Backreferences
--------------

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

Modifiers are for changing behaviour of regular expressions.

You can set modifiers globally in your system or change inside the the
regular expression using the `(?imsxr-imsxr) <#inlinemodifiers>`_.

.. note::
    `TRegExpr <tregexpr.html>`__

    To change modifiers use
    `ModifierStr <tregexpr.html#modifierstr>`__
    or appropriate ``TRegExpr`` properties
    `Modifier* <tregexpr.html#modifieri>`__.

    The default values are defined in `global
    variables <tregexpr.html#global-constants>`_. For example global variable
    ``RegExprModifierX`` defines default value for ``ModifierX`` property.

.. _i:

i, case-insensitive
~~~~~~~~~~~~~~~~~~~

Case-insensitive. Use installed in you system
locale settings, see also
`InvertCase <tregexpr.html#invertcase>`__.

.. _m:

m, multi-line strings
~~~~~~~~~~~~~~~~~~~~~

Treat string as multiple lines. So ``^`` and ``$`` matches the start or end
of any line anywhere within the string.

See also `Line Boundaries <tregexpr.html#lineseparators>`__.

.. _s:

s, single line strings
~~~~~~~~~~~~~~~~~~~~~~

Treat string as single line. So ``.`` matches any
character whatsoever, even a line separators.

See also `Line Boundaries <tregexpr.html#lineseparators>`__, which it
normally would not match.

.. _g:

g, greediness
~~~~~~~~~~~~~

`TRegExpr <index.html>`__ only modifier.

Switching it ``Off`` you’ll switch
`quantifiers <#iterator>`__ into `non-greedy <#greedy>`__ mode.

So, if modifier ``/g`` is ``Off`` then ``+`` works as ``+?``,
``*`` as ``*?`` and so on.

By default this modifier is ``On``.

.. _x:

x, eXtended syntax
~~~~~~~~~~~~~~~~~~

Allows to comment regular expression and break them up into
multiple lines.

If the modifier is ``On`` we ignore all whitespaces that
is neither backslashed nor within a character class.

And the ``#`` character separates comments.

Notice that you can use empty lines to format regular expression for
better readability:

.. code-block:: text

    (
    (abc) # comment 1
    #
    (efg) # comment 2
    )

This also means that if you want real whitespace or ``#`` characters in
the pattern (outside a character class, where they are unaffected by
``/x``), you’ll either have to escape them or encode them using
octal or hex escapes.

.. _r:

r, Russian ranges
~~~~~~~~~~~~~~~~~

`TRegExpr <index.html>`__ only modifier.

In Russian ASCII table characters ``ё``/``Ё`` are placed separately
from others.

Big and small Russian characters are in separated ranges, this is the same
as with English characters but nevertheless I wanted some short form.

With this modifier instead of ``[а-яА-ЯёЁ]`` you can write ``[а-Я]`` if
you need all Russian characters.

When the modifier is ``On``:

======= =======================================
``а-я`` chars from ``а`` to ``я`` and ``ё``
``А-Я`` chars from ``А`` to ``Я`` and ``Ё``
``а-Я`` all russian symbols
======= =======================================

The modifier is set `On` by default.

Extensions
----------

.. _lookahead:

(?=<lookahead>)
~~~~~~~~~~~~~~~

``Look ahead`` assertion. It checks input for the regular expression
``<look-ahead>``, but do not capture it.

.. note::
    `TRegExpr <tregexpr.html>`__

    Look-ahead is not implemented in TRegExpr.

    In many cases you can replace ``look ahead`` with
    `Sub-expression <#subexpression>`_ and just ignore what will be
    captured in this subexpression.

    For example ``(blah)(?=foobar)(blah)`` is the same as ``(blah)(foobar)(blah)``.
    But in the latter version you have to exclude the middle sub-expression
    manually - use ``Match[1] + Match[3]`` and ignore ``Match[2]``.

    This is just not so convenient as in the former version where you can use
    whole ``Match[0]`` because captured by ``look ahead`` part would not be
    included in the regular expression match.

.. _inlinemodifiers:

(?imsgxr-imsgxr)
~~~~~~~~~~~~~~~~

You may use it inside regular expression for modifying modifiers by the fly.

This can be especially handy because it has local scope in a regular
expression. It affects only that part of regular expression that follows
``(?imsgxr-imsgxr)`` operator.

And if it's inside subexpression it will
affect only this subexpression - specifically the part of the subexpression
that follows after the operator. So in ``((?i)Saint)-Petersburg`` it affects
only subexpression ``((?i)Saint)`` so it will match ``saint-Petersburg``
but not ``saint-petersburg``.

============================= ==================================================
``(?i)Saint-Petersburg``      ``Saint-petersburg`` and ``Saint-Petersburg``
``(?i)Saint-(?-i)Petersburg`` ``Saint-Petersburg`` but not ``Saint-petersburg``
``(?i)(Saint-)?Petersburg``   ``Saint-petersburg`` and ``saint-petersburg``
``((?i)Saint-)?Petersburg``   ``saint-Petersburg``, but not ``saint-petersburg``
============================= ==================================================

(?#text)
~~~~~~~~

A comment, the text is ignored.

Note that the comment is closed by
the nearest ``)``, so there is no way to put a literal ``)`` in
the comment.

Afterword
---------

In this ancient blog post from previous century I illustrate some usages of
regular expressions: `Text processing from bird's eye
view <https://masterandrey.com/posts/en/text_processing_from_birds_eye_view.html>`__.

