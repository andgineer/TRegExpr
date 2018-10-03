{$B-}
unit RegularExpressionList;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TSubjectType = class()
  protected
    function Get_Text: WideString;
  public
    property Text: WideString read Get_Text;
  end;

implementation

function TSubjectType.Get_Text: WideString;
 begin
 end;

end.
