{$B-}
unit RegularExpressionList;

interface

uses
 RegularExpressionsXMLBind;

type
  TSubjectType = class(TXMLSubjectType)
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
