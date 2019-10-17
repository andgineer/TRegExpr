// include file to determine Delphi features by version

// Determine version
{$IFNDEF VER80}         { Delphi 1.0}
 {$IFNDEF VER90}        { Delphi 2.0}
  {$IFNDEF VER93}       { C++Builder 1.0}
   {$IFNDEF VER100}     { Borland Delphi 3.0}
     {$DEFINE D4_}      { -- Remember - we are in Delphi 4.0 or higher}
     {$IFNDEF VER110}   { Borland C++Builder 3.0}
      {$IFNDEF VER120}  { Borland Delphi 4.0}
       {$DEFINE D5_}    { -- Remember - we are in Delphi 5.0 or higher}
      {$ENDIF}
     {$ENDIF}
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF D4_}
 {$DEFINE DefParam} // use 'subroutine parameter value by default' feature
{$ENDIF}

{$IFDEF D5_}
 {$DEFINE OverMeth} // use 'overloaded methods' feature
{$ENDIF}
