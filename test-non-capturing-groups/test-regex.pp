{$mode objfpc}
uses
  Classes, SysUtils, RegExpr;

procedure Test(const ARegex, AStr: string);
var
  Obj: TRegExpr;
  i: integer;
begin
  Writeln('Regex: ', ARegex);
  Writeln('Subj: ', AStr);
  
  if ARegex='' then exit;
  if AStr='' then exit;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= false;
    Obj.ModifierM:= true;
    Obj.ModifierI:= false;

    Obj.Expression:= ARegex;
    Obj.InputString:= AStr;
    Obj.ExecPos(1);
    if Obj.MatchPos[0]<=0 then
    begin
      WriteLn('No match');
      exit;
    end;  

    Writeln('Sub-expressions:');
    for i:= 0 to Obj.SubExprMatchCount do
      Writeln('  #', i, ': ', Obj.Match[i]);
  finally
    FreeAndNil(Obj);
  end;
  Writeln;  
end;

begin
  Test(
    '(?:\w+)=(\w+);(?:\w+)=(\w+);(\w+=(.+?);)?',
    'module=mmm;func=fff;info="iii:<function>";'
    );
  Test(
    '(?:https?|ftp)://(.+)\.(?:\w+)',
    'http://url.org'
    );

end.
