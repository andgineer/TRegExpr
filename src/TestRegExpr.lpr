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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  RegExpr,
  tests;

type

  { TestApplication }

  TestApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TestApplication }

procedure TestApplication.DoRun;
begin
  tests.tests();
  // stop program loop
  readln ();
  Terminate;
end;

constructor TestApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TestApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TestApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TestApplication;
begin
  Application:=TestApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

