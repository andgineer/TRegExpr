TRegExpr library implements regular expressions in pure Delphi.

Github repo

Now it’s included into `Lazarus (Free
Pascal) <http://wiki.freepascal.org/Regexpr>`__.

TRegExpr is easy to use and powerfull tool for sophisticated search and
substitutioning and for template-based text checking (especially usefull
for user input validation in DBMS and web projects).

You can validate e-mail adresses, extract phone numbers or ZIP-codes
from web-pages or documents, search for complex patterns in log files
and all You can imagine! Rules (templates) can be changed without Your
program recompilation!

As a language for rules used subset of Perl’s `regular
expressions </regexp_syntax>`__ (regexp).

Full source code included, pure Object Pascal. Thus, You need no DLL!
The library source code is compatible with Delphi 2-7, Borland C++
Builder 3-6, Kylix, FreePascal (if You see any incompatibility problems,
please. drop the bug-report to `author </about>`__).

Documentation in English, Russian, German, Bulgarian, French and Spanish
available at TRegExpr home page

Installation is very simple, the implementation encapsulated completely
into class `TRegExpr </tregexpr_interface/>`__.

`Demos
projects <https://github.com/masterandrey/TRegExpr/tree/master/examples>`__
and `usage
articles <http://masterandrey.com/posts/en/text_processing_from_birds_eye_view.html>`__
illustrate simplicity and power of text processing with the library.

If You need Unicode (so called ‘WideString’ in Delphi) - see `How to use
unicode <tregexpr_interface#unicode>`__.

Gratitudes
~~~~~~~~~~

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them (actually I kept listing only on very
early stage of development), but I do appreciate all

bug-reports, features suggestions and questions that I am receiving from
You.

-  Guido Muehlwitz - found and fixed ugly bug in big string processing
-  Stephan Klimek - testing in CPPB and suggesting/implementing many
   features
-  Steve Mudford - implemented Offset parameter
-  Martin Baur (`www.mindpower.com <http://www.mindpower.com>`__) -
   German help, usefull suggetions
-  Yury Finkel - implemented UniCode support, found and fixed some bugs
-  Ralf Junker - Implemented some features, many optimization
   suggestions
-  Simeon Lilov - Bulgarian help
-  Filip Jirsбk and Matthew Winter - help in Implementation non-greedy
   mode
-  Kit Eason many examples for introduction help section
-  Juergen Schroth - bug hunting and usefull suggestions
-  Martin Ledoux - French help
-  Diego Calp, Argentina -Spanish help

And many others - for big work in bug hunting!

Documentation
-------------

.. toctree::
   :glob:
   :maxdepth: 2

   regexp_syntax.rst
   tregexpr_interface.rst
   faq
   demos
