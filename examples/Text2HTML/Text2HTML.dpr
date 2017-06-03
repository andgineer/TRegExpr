program Text2HTML;

uses
  Forms,
  Text2HTMLMain in 'Text2HTMLMain.pas' {fmText2HTMLMain},
  HyperLinksDecorator in '..\..\Src\HyperLinksDecorator.pas',
  RegExpr in '..\..\src\RegExpr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmText2HTMLMain, fmText2HTMLMain);
  Application.Run;
end.
