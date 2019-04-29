Demos
=====

Introduction
------------

If you don't familiar with regular expression, please, take a look at
the `r.e.syntax <regexp_syntax.html>`__.

TRegExpr interface described in `TRegExpr
interface <tregexpr_interface.html>`__.

Some of demo-projects use extended VCL properties which exists only in
Delphi 4 or higher. While compiling in Delphi 3 or Delphi 2 you'll
receive some error messages about unknown properties. You may ignore it
- this properties is needed only for resizing and justification of
components then form change it's size.

Windows compiled
----------------

First of all I recommend You to read `Text processing from bird's eye
view <https://masterandrey.com/posts/en/text_processing_from_birds_eye_view.html>`__.

Ready to run Windows application
`REStudio <https://github.com/masterandrey/TRegExpr/releases/download/0.952b/REStudio.exe>`__
to learn and debug regular expressions.

Please, note that there are localized demos available (with comments in
source code on national languages).

This localized versions distributed in localized full TRegExpr packages,
and in separate localized documentation packages (when you unpack this
documentation package in TRegExpr directory the localized demos
overwrite English ones).

Text2HTML
---------

`Text2HTML sources <https://github.com/masterandrey/TRegExpr/tree/master/examples/Text2HTML>`_

Publish plain text as HTML

Uses unit `HyperLinksDecorator <https://github.com/masterandrey/TRegExpr/blob/master/src/HyperLinksDecorator.pas>`__
that is based on TRegExpr.
 
This unit contains functions to decorate hyper-links.

For example, replaces ``www.masterAndrey.com`` with
``<a href="http://www.masterAndrey.com">www.masterAndrey.com</a>``
or ``anso@mail.ru`` with ``<a href="mailto:filbert@yandex.ru">filbert@yandex.ru</a>``.
 
.. code-block:: pascal

    function DecorateURLs (
        const AText : string;
        AFlags : TDecorateURLsFlagSet = [durlAddr, durlPath]
    ) : string;

    type
    TDecorateURLsFlags = (
        durlProto, durlAddr, durlPort, durlPath, durlBMark, durlParam);

    TDecorateURLsFlagSet = set of TDecorateURLsFlags;

    function DecorateEMails (const AText : string) : string;  

========= ====================================================
  Value   Meaning
========= ====================================================
durlProto Protocol (like ``ftp://`` or ``http://``)
durlAddr  TCP address or domain name (like ``masterAndrey.com``)
durlPort  Port number if specified (like ``:8080``)
durlPath  Path to document (like ``index.html``)
durlBMark Book mark (like ``#mark``)
durlParam URL params (like ``?ID=2&User=13``)
========= ====================================================

Returns input text ``AText`` with decorated hyper links.

``AFlags`` describes, which parts of hyper-link must be included into
visible part of the link.

For example, if `AFlags` is ``[durlAddr]`` then hyper link
``www.masterAndrey.com/contacts.htm`` will be decorated as
``<a href="www.masterAndrey.com/contacts.htm">www.masterAndrey.com</a>``.

`TRegExprRoutines <https://github.com/masterandrey/TRegExpr/tree/master/examples/TRegExprRoutines>`_
----------------------------------------------------------------------------------------------------

Very simple examples, see comments inside the unit

`TRegExprClass <https://github.com/masterandrey/TRegExpr/tree/master/examples/TRegExprClass>`_
----------------------------------------------------------------------------------------------

Slightly more complex examples, see comments inside the unit
