.. list-table::
   :widths: 40 10 10 10 10 10 10
   :header-rows: 0

   * -
     - English
     - `Русский <https://regex.sorokin.engineer/ru/latest/index.html>`__
     - `Deutsch <https://regex.sorokin.engineer/de/latest/index.html>`__
     - `Български <https://regex.sorokin.engineer/bg/latest/index.html>`__
     - `Français <https://regex.sorokin.engineer/fr/latest/index.html>`__
     - `Español <https://regex.sorokin.engineer/es/latest/index.html>`__

Introduction
~~~~~~~~~~~~

TRegExpr library implements `regular
expressions <regular_expressions.html>`_.

Regular expressions are easy to use and powerful tool for sophisticated
search and substitution and for template based text check.

It is especially useful for user input validation in input forms - to
validate e-mail addresses and so on.

Also you can extract phone numbers, ZIP-codes etc from web-pages or documents,
search for complex patterns in log files and all you can imagine.
Rules (templates) can be changed without your program recompilation.

TRegExpr is implemented in pure Pascal. It's included into
`Lazarus (Free Pascal) <http://wiki.freepascal.org/Regexpr>`_ project.
But also it exists as separate library and can be compiled by Delphi 2-7,
Borland C++ Builder 3-6.

Reviews
~~~~~~~

`How good the library was met <https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html>`_.

Quick start
~~~~~~~~~~~

To use the library just add `the sources <https://github.com/andgineer/TRegExpr/blob/master/src/RegExpr.pas>`_
to you project and use the class `TRegExpr <tregexpr.html>`_.

In the `FAQ <faq.html>`_ you can learn from others users problems.

Ready to run Windows application
`REStudio <https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip>`__
will help you learn and debug regular expressions.

Feedback
~~~~~~~~

If you see any problems,
please `create the bug <https://github.com/andgineer/TRegExpr/issues>`_.

Source code
~~~~~~~~~~~
Pure Object Pascal.

- `Original version <https://github.com/andgineer/TRegExpr>`_
- `FreePascal fork (GitHub mirror of the SubVersion) <https://github.com/graemeg/freepascal/blob/master/packages/regexpr/src/regexpr.pas>`_

Documentation
~~~~~~~~~~~~~

.. toctree::
   :glob:
   :maxdepth: 2

   regular_expressions
   tregexpr
   faq
   demos

Translations
~~~~~~~~~~~~
The documentation is available in English and
`Russian <https://regexpr.sorokin.engineer/ru/latest/>`_.

There are also old translations to German, Bulgarian, French and Spanish.
If you want to help to update this old translations please `contact me <https://github.com/andgineer>`_.

New translations are based on `GetText <https://en.wikipedia.org/wiki/Gettext>`_
and can be edited with `Weblate <https://hosted.weblate.org/projects/tregexpr/>`_.

They are already machine-translated and need only proof-reading
and may be some copy-pasting from old translations.

Gratitude
~~~~~~~~~

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
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
