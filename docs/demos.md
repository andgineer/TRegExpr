---
layout: page
lang: en
ref: demos
title: Demos
permalink: /en/demos.html
---

First of all I recommend You to read 
[article with usage illustrations](http://masterandrey.com/posts/en/text_processing_from_birds_eye_view.html).

Ready to run Windows application
[REStudio](https://github.com/masterandrey/TRegExpr/releases/download/0.952b/REStudio.exe)
to learn and debug regular expressions.

Please, note that there are localized demos available (with comments in
source code on national languages). 

This localized versions distributed
in localized full TRegExpr packages, and in separate localized
documentation packages (when You unpack this documentation package in
TRegExpr directory the localized demos overwrite English ones).

Demos\TRegExprRoutines

very simple examples, see comments inside the unit

Demos\TRegExprClass

slightly more complex examples, see comments inside the unit

Demos\Text2HTML

see [description](#text2html.html)

If You don't familiar with regular expression, please, take a look at
the [r.e.syntax](regexp_syntax.html) topic.

TRegExpr interface described in [TRegExpr
interface](tregexpr_interface.html).

Note

Some of demo-projects use extended VCL properties which exists only in
Delphi 4 or higher. While compiling in Delphi 3 or Delphi 2 you'll
receive some error messages about unknown properties. You may ignore it
- this properties is needed only for resizing and justification of
components then form change it's size.

Text2HTML
=========

Very simple utility, that helps publish plain text as HTML

Uses unit [HyperLinksDecorator](#hyperlinksdecorator.html) that is based
on TRegExpr.

 

Specially written as a demonstration of TRegExpr usage.

 

Unit HyperLinksDecorator
========================

[DecorateURLs](#hyperlinksdecorator.html#decorateurls)   [DecorateEMails](#hyperlinksdecorator.html#decorateemails)
===================================================================================================================

This unit contains functions to decorate hyper-links (see
[Text2Html](#text2html.html) demo-project for usage example).

 

For example, replaces 'www.RegExpStudio.com' with '<a
href="http://www.RegExpStudio.com">www.RegExpStudio.com</a>' or
'anso@mail.ru' with '<a
href="mailto:anso@mail.ru">anso@mail.ru</a>'.

 

function DecorateURLs

 

Finds and replaces hyper links like 'http://...' or 'ftp://..' as well
as links without protocol, but start with 'www.' If You want to decorate
emails as well, You have to use function
[DecorateEMails](#hyperlinksdecorator.html#decorateemails) instead.

 

function DecorateURLs (const AText : string; AFlags :
TDecorateURLsFlagSet = \[durlAddr, durlPath\]) : string;

 

Description

 

Returns input text AText with decorated hyper links.

 

AFlags describes, which parts of hyper-link must be included into
VISIBLE part of the link:

For example, if \[durlAddr\] then hyper link
'www.RegExpStudio.com/contacts.htm' will be decorated as '<a
href="http://www.RegExpStudio.com/contacts.htm">www.RegExpStudio.com</a>'

 

type

 TDecorateURLsFlags = (durlProto, durlAddr, durlPort, durlPath,
durlBMark, durlParam);

 TDecorateURLsFlagSet = set of TDecorateURLsFlags;

 

Description

 

These are the possible values:

 

Value                Meaning

------------------------------------------------------------------------

durlProto        Protocol (like 'ftp://' or 'http://')

durlAddr        TCP address or domain name (like 'RegExpStudio.com')

durlPort                Port number if specified (like ':8080')

durlPath        Path to document (like 'index.html')

durlBMark        Book mark (like '\#mark')

durlParam        URL params (like '?ID=2&User=13')

 

 

 

 

function DecorateEMails

 

Replaces all syntax correct e-mails with '<a
href="mailto:ADDR">ADDR</a>'. For example, replaces
'anso@mail.ru' with '<a
href="mailto:anso@mail.ru">anso@mail.ru</a>'.

 

function DecorateEMails (const AText : string) : string;

 

Description

 

Returns input text AText with decorated e-mails

 

Usage illustrations
===================

•[Text processing from bird's eye view](#article_bird_eye_view.html)

•[MrDecorator](#article_mrdecorator.html)
