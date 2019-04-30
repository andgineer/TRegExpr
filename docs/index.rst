Introduction
~~~~~~~~~~~~

TRegExpr library implements `regular
expressions <regexp_syntax.html>`_ in pure Delphi.
TRegExpr is part of `Lazarus (Free Pascal) <http://wiki.freepascal.org/Regexpr>`_.

TRegExpr is easy to use and powerful tool for sophisticated search and
substitution and for template-based text check.

It is especially useful for user input validation in input forms - to
validate e-mail addresses and so on.

Also you can extract phone numbers, ZIP-codes etc from web-pages or documents,
search for complex patterns in log files and all you can imagine.
Rules (templates) can be changed without your
program recompilation.

The library source code is compatible with Delphi 2-7, Borland C++
Builder 3-6, FreePascal.

Reviews
~~~~~~~

`How good the library was met <https://masterandrey.com/posts/en/regexpstudio_site_is_lunched.html>`_.

Quick start
~~~~~~~~~~~

To use the library just add `the sources <https://github.com/masterandrey/TRegExpr/blob/master/src/RegExpr.pas>`_
to you project and use the class `TRegExpr <tregexpr_interface.html>`_.

`Demo projects <https://github.com/masterandrey/TRegExpr/tree/master/examples>`__
and `usage blog post <https://masterandrey.com/posts/en/text_processing_from_birds_eye_view.html>`__
illustrate the simplicity and power of text processing with the library.

If you need Unicode (so called ‘WideString’ in Delphi) - see `How to use
unicode <tregexpr_interface.html#unicode>`__.

Feedback
~~~~~~~~

If you see any problems,
please `create the bug <https://github.com/masterandrey/TRegExpr/issues>`_.

Documentation
~~~~~~~~~~~~~

.. toctree::
   :glob:
   :maxdepth: 2

   regexp_syntax
   tregexpr_interface
   faq
   demos

Translations
~~~~~~~~~~~~

`Documentation <https://regexpr.masterandrey.com/en/latest/>`_ is available
in English and Russian.

There are also old translations to German, Bulgarian, French and Spanish.
If you want to help to update the translation please `contact me <https://github.com/masterandrey>`_.
New translation is based on `GetText <https://en.wikipedia.org/wiki/Gettext>`_
and can be edited with `transifex.com <https://www.transifex.com/masterAndrey/tregexpr/dashboard/>`_.
All this translations are already machine-translated and need only proof-reading
and may be some copy-pasting from old translations.

Source code
~~~~~~~~~~~
Pure Object Pascal.

- `FPC fork (GitHub mirror of the SubVersion) <https://github.com/graemeg/freepascal/blob/master/packages/regexpr/src/regexpr.pas>`_
- `Original version <https://github.com/masterandrey/TRegExpr>`_

Gratitude
~~~~~~~~~

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them (actually I kept listing only on very
early stage of development), but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from
you.

-  Guido Muehlwitz - found and fixed ugly bug in big string processing
-  Stephan Klimek - testing in CPPB and suggesting/implementing many
   features
-  Steve Mudford - implemented Offset parameter
-  Martin Baur (`www.mindpower.com <http://www.mindpower.com>`__) -
   German translation, usefull suggestions
-  Yury Finkel - implemented UniCode support, found and fixed some bugs
-  Ralf Junker - Implemented some features, many optimization
   suggestions
-  Simeon Lilov - Bulgarian translation
-  Filip Jirsбk and Matthew Winter - help in Implementation non-greedy
   mode
-  Kit Eason many examples for introduction help section
-  Juergen Schroth - bug hunting and useful suggestions
-  Martin Ledoux - French translation
-  Diego Calp, Argentina - Spanish translation

And many others - for hard work in bug hunting!

