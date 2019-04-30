TRegExpr interface
==================

TRegExpr class
--------------

VersionMajor, VersionMinor
~~~~~~~~~~~~~~~~~~~~~~~~~~

Return major and minor version, for example, for v. 0.944 VersionMajor =
0 and VersionMinor = 944

Expression
~~~~~~~~~~

Regular expression.

For optimization, TRegExpr will automatically compiles it into ``P-code``
(you can see it with help of Dump_ method) and stores in internal
structures.

Regular expresion is re-compiled only if you call Exec_, ExecNext_,
Substitute_, Dump_, etc and only if Expression_
was changed after last compilation.

In case of any errors in compilation, ``Error`` method is called (by
default ``Error`` raises exception ERegExpr_)

ModifierStr
~~~~~~~~~~~

Set or get default values of
`r.e.modifiers <regexp_syntax.html#modifiers>`__.

Format of the string is similar as in
`(?ismx-ismx) <regexp_syntax.html#inlinemodifiers>`__. For example
``ModifierStr := ‘i-x’`` will switch on modifier `/i <regexp_syntax.html#i>`_,
switch off `/x <regexp_syntax.html#x>`_ and leave unchanged others.

If you try to set unsupported modifier, ``Error`` will be called.

ModifierI
~~~~~~~~~

Modifier /i - (“caseinsensitive”), initialized with
`RegExprModifierI <#modifier_defs>`__ value.

ModifierR
~~~~~~~~~

Modifier /r - (“Russian syntax extensions), initialized with
`RegExprModifierR <#modifier_defs>`__ value.

ModifierS
~~~~~~~~~

`Modifier /s <regexp_syntax.html#modifier_s>`__ - ‘.’ works as any char
(else doesn’t match LineSeparators_ and
`LinePairedSeparator <tregexpr_interface.html#linepairedseparator>`__),
initialized with `RegExprModifierS <#modifier_defs>`__ value.

ModifierG
~~~~~~~~~

`Modifier /g <regexp_syntax.html#modifier_g>`__ Switching off modifier
/g switchs all operators in non-greedy style, so if ModifierG = False,
then all ‘\*’ works as ‘\*?’, all ‘+’ as ‘+?’ and so on, initialized
with `RegExprModifierG <#modifier_defs>`__ value.

ModifierM
~~~~~~~~~

`Modifier /m <regexp_syntax.html#modifier_m>`__ Treat string as multiple
lines. That is, change \`^‘and \`$’ from matching at only the very start
or end of the string to the start or end of any line anywhere within the
string, initialized with `RegExprModifierM <#modifier_defs>`__ value.

ModifierX
~~~~~~~~~

`Modifier /x <regexp_syntax.html#modifier_x>`__ - (“eXtended syntax”),
initialized with `RegExprModifierX <#modifier_defs>`__ value.

Exec
~~~~

match a programm against a string AInputString

!!! Exec store AInputString into InputString property

For Delphi 5 and higher available overloaded versions:

without parameter already assigned to InputString property value

ExecNext
~~~~~~~~

Find next match:

Without parameter works the same as

::

    if MatchLen \[0\] = 0 then ExecPos (MatchPos \[0\] + 1)
      else ExecPos (MatchPos \[0\] + MatchLen \[0\]);

but it’s more simpler !

Raises exception if used without preceeding successful call to Exec\*
(Exec, ExecPos, ExecNext).

So you always must use something like

::

    if Exec (InputString) then repeat { proceed results} until not ExecNext;

.

ExecPos
~~~~~~~

finds match for InputString starting from AOffset position

::

    AOffset=1 - first char of InputString

InputString
~~~~~~~~~~~

returns current input string (from last Exec call or last assign to this
property).

Any assignment to this property clear ``Match*`` properties !

Substitute
~~~~~~~~~~

Returns ATemplate with ``$&`` or ``$0`` replaced by whole r.e. occurence
and ``$n`` replaced by occurence of subexpression number ``n``.

If you want place into template raw ``$`` or ``\\``, use prefix ``\\``.

Special symbols:

====== ===============================
symbol replaced with
``\\`` ``\``
``\n`` ``\r\n`` end of line in Windows
``\l`` lowcase one next char
``\L`` lowercase all chars after that
``\u`` uppcase one next char
``\U`` uppercase all chars after that
====== ===============================

::

     '1\$ is $2\\rub\\' -> '1$ is <Match[2]>\rub\'
     '\U$1\\r' transforms into '<Match[1] in uppercase>\r'

If you want to place raw digit after ‘$n’ you must delimit n with curly
braces ``{}``.

::

     'a$12bc' -> 'a<Match[12]>bc'
     'a${1}2bc' -> 'a<Match[1]>2bc'.

Split
~~~~~

Split AInputStr into APieces by r.e. occurencies

Internally calls Exec_ / ExecNext_

Replace, ReplaceEx
~~~~~~~~~~~~~~~~~~

Returns the string with r.e. occurencies replaced by the replace string.

If ``AUseSubstitution`` is true, then ``AReplaceStr`` will be used
as template for Substitution methods.

::

    Expression := '({-i}block|var)\\s\*\\(\\s\*(\[^ \]\*)\\s\*\\)\\s\*';
    Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);

    will return:  def ‘BLOCK’ value ‘test1’

::

    Replace ('BLOCK( test1)', 'def "$1" value "$2"', False)

    will return:  def “$1” value “$2”

Internally calls Exec_ / ExecNext_

Overloaded version and ReplaceEx operate with call-back function,

so you can implement really complex functionality.

SubExprMatchCount
~~~~~~~~~~~~~~~~~

Number of subexpressions has been found in last Exec_ / ExecNext_ call.

If there are no subexpr. but whole expr was found (Exec\* returned
True), then ``SubExprMatchCount=0``, if no subexpressions nor whole r.e.
found (Exec_ / ExecNext_ returned false) then ``SubExprMatchCount=-1``.

Note, that some subexpr. may be not found and for such subexpr.
``MathPos=MatchLen=-1`` and ``Match=’’``.

::

    Expression := '(1)?2(3)?';
    Exec ('123'): SubExprMatchCount=2, Match[0]='123', [1]='1', [2]='3'

    Exec ('12'): SubExprMatchCount=1, Match[0]='12', [1]='1'

    Exec ('23'): SubExprMatchCount=2, Match[0]='23', [1]='', [2]='3'

    Exec ('2'): SubExprMatchCount=0, Match[0]='2'

    Exec ('7') - return False: SubExprMatchCount=-1


MatchPos
~~~~~~~~

pos of entrance subexpr. ``#Idx`` into tested in last ``Exec*`` string.
First subexpr. have ``Idx=1``, last - ``MatchCount``, whole r.e. have
``Idx=0``.

Returns ``-1`` if in r.e. no such subexpr. or this subexpr. not found in
input string.

MatchLen
~~~~~~~~

len of entrance subexpr. ``#Idx`` r.e. into tested in last ``Exec*``
string. First subexpr. have ``Idx=1``, last - MatchCount, whole r.e.
have ``Idx=0``.

Returns -1 if in r.e. no such subexpr. or this subexpr. not found in
input string.

Match
~~~~~

Returns ``’’`` if in r.e. no such subexpression or this subexpression
was not found in the input string.

LastError
~~~~~~~~~

Returns ``ID`` of last error, ``0`` if no errors (unusable if ``Error`` method
raises exception) and clear internal status into ``0`` (no errors).

ErrorMsg
~~~~~~~~

Returns ``Error`` message for error with ``ID = AErrorID``.

CompilerErrorPos
~~~~~~~~~~~~~~~~

Returns pos in r.e. there compiler stopped.

Useful for error diagnostics

SpaceChars
~~~~~~~~~~

Contains chars, treated as ``\s`` (initially filled with RegExprSpaceChars_
global constant)

WordChars
~~~~~~~~~

Contains chars, treated as ``\w`` (initially filled with RegExprWordChars_
global constant)


LineSeparators
~~~~~~~~~~~~~~

line separators (like ``\n`` in Unix), initially filled with
RegExprLineSeparators_ global constant)

see also `about line
separators <regexp_syntax.html#line-separators>`__

LinePairedSeparator
~~~~~~~~~~~~~~~~~~~

paired line separator (like ``\r\n`` in DOS and Windows).

must contain exactly two chars or no chars at all, initially filled with
RegExprLinePairedSeparator global constant)

see also `about line
separators <regexp_syntax.html#line-separators>`__

For example, if you need Unix-style behaviour, assign
``LineSeparators := #$a`` and ``LinePairedSeparator := ''`` (empty string).

If you want to accept as line separators only ``\x0D\x0A`` but not ``\x0D``
or ``\x0A`` alone, then assign ``LineSeparators := ''`` (empty string) and
``LinePairedSeparator := #$d#$a``.

By default ‘mixed’ mode is used (defined in
RegExprLine[Paired]Separator[s] global constants):

::

    LineSeparators := #$d#$a; 
    LinePairedSeparator := #$d#$a

Behaviour of this mode is detailed described in the `syntax
section <regexp_syntax.html#syntax_line_separators>`__.

InvertCase
~~~~~~~~~~

Set this property if you want to override case-insensitive
functionality.

Create set it to RegExprInvertCaseFunction (InvertCaseFunction by
default)

Compile
~~~~~~~

[Re]compile r.e. Useful for example for GUI r.e. editors (to check all
properties validity).

Dump
~~~~

dump a compiled regexp in vaguely comprehensible form

Global constants
----------------

EscChar
~~~~~~~

Escape-char, by default ``\``.

RegExprModifierI
~~~~~~~~~~~~~~~~

`Modifier i <regexp_syntax.html#i>`_ default value

RegExprModifierR
~~~~~~~~~~~~~~~~

`Modifier r <regexp_syntax.html#r>`_ default value

RegExprModifierS
~~~~~~~~~~~~~~~~

`Modifier s <regexp_syntax.html#s>`_ default value

RegExprModifierG
~~~~~~~~~~~~~~~~

`Modifier g <regexp_syntax.html#g>`_ default value

RegExprModifierM
~~~~~~~~~~~~~~~~

`Modifier m <regexp_syntax.html#m>`_ default value

RegExprModifierX
~~~~~~~~~~~~~~~~

`Modifier x <regexp_syntax.html#x>`_ default value

RegExprSpaceChars
~~~~~~~~~~~~~~~~~

Default for SpaceChars_ property
 

RegExprWordChars
~~~~~~~~~~~~~~~~

Default value for WordChars_ property

 
RegExprLineSeparators
~~~~~~~~~~~~~~~~~~~~~

Default value for LineSeparators_ property

RegExprLinePairedSeparator
~~~~~~~~~~~~~~~~~~~~~~~~~~

Default value for LinePairedSeparator_ property


RegExprInvertCaseFunction
~~~~~~~~~~~~~~~~~~~~~~~~~

Default for InvertCase_ property

Global functions
----------------

ExecRegExpr
~~~~~~~~~~~

true if the string matches the regular expression

SplitRegExpr
~~~~~~~~~~~~

Splits the string by r.e. occurencies

ReplaceRegExpr
~~~~~~~~~~~~~~

Returns the string with r.e. occurencies replaced by the ``AReplaceStr``.

If ``AUseSubstitution` is true, then ``AReplaceStr`` will be used as template
for ``Substitution methods``.

::

    ReplaceRegExpr ('({-i}block|var)\\s\*\\(\\s\*(\[^ \]\*)\\s\*\\)\\s\*',
      'BLOCK( test1)', 'def "$1" value "$2"', True)

    return  def ‘BLOCK’ value ‘test1’

::

    ReplaceRegExpr ('({-i}block|var)\\s\*\\(\\s\*(\[^ \]\*)\\s\*\\)\\s\*',
      'BLOCK( test1)', 'def "$1" value "$2"')

    return  def “$1” value “$2”

QuoteRegExprMetaChars
~~~~~~~~~~~~~~~~~~~~~

Replace all metachars with its safe representation, for example
``abc'cd.(`` converts into ``abc\'cd\.\(``

This function usefull for r.e. autogeneration from user input

RegExprSubExpressions
~~~~~~~~~~~~~~~~~~~~~

Makes list of subexpressions found in ``ARegExpr``

In ``ASubExps`` every item represent subexpression, from first to last, in
format:

 String - subexpression text (without ‘()’)

 low word of Object - starting position in ARegExpr, including ‘(’ if
exists! (first position is 1)

 high word of Object - length, including starting ‘(’ and ending ‘)’ if
exist!

``AExtendedSyntax`` - must be ``True`` if modifier ``/x`` will be ``On`` while
using the r.e.

Usefull for GUI editors of r.e. etc (you can find example of using in
`REStudioMain.pas <https://github.com/masterandrey/TRegExpr/blob/74ab342b639fc51941a4eea9c7aa53dcdf783592/restudio/REStudioMain.pas#L474>`_)

=========== =======
Result code Meaning
=========== =======
0           Success. No unbalanced brackets was found
-1          there are not enough closing brackets ``)``
-(n+1)      at position n was found opening ``[`` without corresponding closing ``]``
n           at position n was found closing bracket ``)`` without corresponding opening ``(``
=========== ======= 

If ``Result <> 0``, then ``ASubExprs`` can contain empty items or illegal ones

ERegExpr
--------

::

    ERegExpr = class (Exception)
      public
       ErrorCode : integer; // error code. Compilation error codes are before 1000
       CompilerErrorPos : integer; // Position in r.e. where compilation error occured
     end;

Unicode
-------

TRegExpr now supports UniCode, but it works very slow :(

Who want to optimize it ? ;)

Use it only if you really need Unicode support !

Remove ``.`` in ``{.$DEFINE UniCode}`` in regexpr.pas. After that all
strings will be treated as WideString.

 
