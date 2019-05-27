.. list-table::
   :widths: 40 10 10 10 10 10 10
   :header-rows: 0

   * -
     - English
     - `Русский <https://regex.sorokin.engineer/ru/latest/demos.html>`__
     - `Deutsch <https://regex.sorokin.engineer/de/latest/demos.html>`__
     - `Български <https://regex.sorokin.engineer/bg/latest/demos.html>`__
     - `Français <https://regex.sorokin.engineer/fr/latest/demos.html>`__
     - `Español <https://regex.sorokin.engineer/es/latest/demos.html>`__

Demos
=====

Demo code for `TRegExpr <index.html>`__

Introduction
------------

If you don't familiar with regular expression, please, take a look at
the `r.e.syntax <regular_expressions.html>`__.

TRegExpr interface described in `TRegExpr
interface <tregexpr.html>`__.

Text2HTML
---------

`Text2HTML sources <https://github.com/andgineer/TRegExpr/tree/master/examples/Text2HTML>`_

Publish plain text as HTML

Uses unit `HyperLinksDecorator <https://github.com/andgineer/TRegExpr/blob/master/src/HyperLinksDecorator.pas>`__
that is based on TRegExpr.
 
This unit contains functions to decorate hyper-links.

For example, replaces ``www.masterAndrey.com`` with
``<a href="http://www.masterAndrey.com">www.masterAndrey.com</a>``
or ``filbert@yandex.ru`` with ``<a href="mailto:filbert@yandex.ru">filbert@yandex.ru</a>``.
 
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

`TRegExprRoutines <https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprRoutines>`_
----------------------------------------------------------------------------------------------------

Very simple examples, see comments inside the unit

`TRegExprClass <https://github.com/andgineer/TRegExpr/tree/master/examples/TRegExprClass>`_
----------------------------------------------------------------------------------------------

Slightly more complex examples, see comments inside the unit
