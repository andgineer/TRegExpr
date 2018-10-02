program TestRegExpr;

{
  Very simple test of Delphi Regular Expression library
  (www.RegExpStudio.com) for Delphi/FreePascal compilers.
  This is also self-test (very basic one) for the library.
  Adapted from FreePascal test application
  \source\packages\base\regexpr\testreg1.pp
  by Andrey V. Sorokin.

  2004.01.04 v.1.0
  -=- Just first version

}

{$IFDEF FPC}
 {$MODE DELPHI} // Delphi-compatible mode in FreePascal
{$ENDIF}

{$IFDEF FPC}
 Please, do not forget to set correct path to RegExpr.pas unit
 (because embeded into FreePascal simple r.e. unit has the same
  name as one from www.RegExpStudio.com).
 The simplest way - just place RegExpr.pas into the same directory
 with TestRegExpr.dpr.
 And remove this comment ;)
{$ENDIF}


uses
  RegExpr,
  tests;

begin
  tests.tests();
end.
