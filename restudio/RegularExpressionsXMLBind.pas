
{*****************************************************************************}
{                                                                             }
{                              XML Data Binding                               }
{                                                                             }
{         Generated on: 25.02.2006 11:51:20                                   }
{       Generated from: Q:\RegExpr\Test\Tests\RegularExpressionsXMLBind.xml   }
{   Settings stored in: Q:\RegExpr\Test\Tests\RegularExpressionsXMLBind.xdb   }
{                                                                             }
{*****************************************************************************}

unit RegularExpressionsXMLBind;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLRegularExpressionsType = interface;
  IXMLRegularExpressionType = interface;
  IXMLTestCaseType = interface;
  IXMLTestCaseTypeList = interface;
  IXMLSubjectType = interface;
  IXMLSubstringType = interface;
  IXMLMatchType = interface;
  IXMLMatchTypeList = interface;
  IXMLSubstitutionType = interface;
  IXMLSubstitutionTypeList = interface;
  IXMLReplaceType = interface;
  IXMLReplaceTypeList = interface;
  IXMLString_List = interface;

{ IXMLRegularExpressionsType }

  IXMLRegularExpressionsType = interface(IXMLNodeCollection)
    ['{A7D5B1B3-C40D-4D63-8270-146EE0667E7F}']
    { Property Accessors }
    function Get_RegularExpression(Index: Integer): IXMLRegularExpressionType;
    { Methods & Properties }
    function Add: IXMLRegularExpressionType;
    function Insert(const Index: Integer): IXMLRegularExpressionType;
    property RegularExpression[Index: Integer]: IXMLRegularExpressionType read Get_RegularExpression; default;
  end;

{ IXMLRegularExpressionType }

  IXMLRegularExpressionType = interface(IXMLNode)
    ['{6CD4EDE6-B46D-4764-BD0E-61660204612A}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_Name: WideString;
    function Get_Description: WideString;
    function Get_Comment: WideString;
    function Get_Expression: WideString;
    function Get_Category: IXMLString_List;
    function Get_TestCase: IXMLTestCaseTypeList;
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_Description(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Expression(Value: WideString);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property Comment: WideString read Get_Comment write Set_Comment;
    property Expression: WideString read Get_Expression write Set_Expression;
    property Category: IXMLString_List read Get_Category;
    property TestCase: IXMLTestCaseTypeList read Get_TestCase;
  end;

{ IXMLTestCaseType }

  IXMLTestCaseType = interface(IXMLNode)
    ['{A087D83F-28E1-486A-AE80-CA8AE4CFE8A8}']
    { Property Accessors }
    function Get_Modifiers: WideString;
    function Get_Subject: IXMLSubjectType;
    function Get_Match: IXMLMatchTypeList;
    function Get_Substitution: IXMLSubstitutionTypeList;
    function Get_Replace: IXMLReplaceTypeList;
    procedure Set_Modifiers(Value: WideString);
    { Methods & Properties }
    property Modifiers: WideString read Get_Modifiers write Set_Modifiers;
    property Subject: IXMLSubjectType read Get_Subject;
    property Match: IXMLMatchTypeList read Get_Match;
    property Substitution: IXMLSubstitutionTypeList read Get_Substitution;
    property Replace: IXMLReplaceTypeList read Get_Replace;
  end;

{ IXMLTestCaseTypeList }

  IXMLTestCaseTypeList = interface(IXMLNodeCollection)
    ['{FFE9C8E6-06FA-42E8-B469-CF4B75B9CAFE}']
    { Methods & Properties }
    function Add: IXMLTestCaseType;
    function Insert(const Index: Integer): IXMLTestCaseType;
    function Get_Item(Index: Integer): IXMLTestCaseType;
    property Items[Index: Integer]: IXMLTestCaseType read Get_Item; default;
  end;

{ IXMLSubjectType }

  IXMLSubjectType = interface(IXMLNodeCollection)
    ['{B8582A18-07B0-4A22-8916-FDCC7CB7137A}']
    { Property Accessors }
    function Get_Substring(Index: Integer): IXMLSubstringType;
    { Methods & Properties }
    function Add: IXMLSubstringType;
    function Insert(const Index: Integer): IXMLSubstringType;
    property Substring[Index: Integer]: IXMLSubstringType read Get_Substring; default;
  end;

{ IXMLSubstringType }

  IXMLSubstringType = interface(IXMLNode)
    ['{F263D564-1EC6-4FCA-BEBB-4390B9E50734}']
    { Property Accessors }
    function Get_RepeatCount: Integer;
    function Get_FileName: WideString;
    procedure Set_RepeatCount(Value: Integer);
    procedure Set_FileName(Value: WideString);
    { Methods & Properties }
    property RepeatCount: Integer read Get_RepeatCount write Set_RepeatCount;
    property FileName: WideString read Get_FileName write Set_FileName;
  end;

{ IXMLMatchType }

  IXMLMatchType = interface(IXMLNodeCollection)
    ['{C484715D-A39C-49AF-85B5-C97E16022397}']
    { Property Accessors }
    function Get_Position: Integer;
    function Get_Length: Integer;
    function Get_CapturedSubstring(Index: Integer): WideString;
    procedure Set_Position(Value: Integer);
    procedure Set_Length(Value: Integer);
    { Methods & Properties }
    function Add(const CapturedSubstring: WideString): IXMLNode;
    function Insert(const Index: Integer; const CapturedSubstring: WideString): IXMLNode;
    property Position: Integer read Get_Position write Set_Position;
    property Length: Integer read Get_Length write Set_Length;
    property CapturedSubstring[Index: Integer]: WideString read Get_CapturedSubstring; default;
  end;

{ IXMLMatchTypeList }

  IXMLMatchTypeList = interface(IXMLNodeCollection)
    ['{9699FB75-7D5C-4906-9218-7CA89995B208}']
    { Methods & Properties }
    function Add: IXMLMatchType;
    function Insert(const Index: Integer): IXMLMatchType;
    function Get_Item(Index: Integer): IXMLMatchType;
    property Items[Index: Integer]: IXMLMatchType read Get_Item; default;
  end;

{ IXMLSubstitutionType }

  IXMLSubstitutionType = interface(IXMLNode)
    ['{B0E59E06-3FDB-48D7-857A-169599156F93}']
    { Property Accessors }
    function Get_Template: WideString;
    function Get_Result: WideString;
    procedure Set_Template(Value: WideString);
    procedure Set_Result(Value: WideString);
    { Methods & Properties }
    property Template: WideString read Get_Template write Set_Template;
    property Result: WideString read Get_Result write Set_Result;
  end;

{ IXMLSubstitutionTypeList }

  IXMLSubstitutionTypeList = interface(IXMLNodeCollection)
    ['{4389D23C-1983-42D4-B37A-2DAD2CD8E05E}']
    { Methods & Properties }
    function Add: IXMLSubstitutionType;
    function Insert(const Index: Integer): IXMLSubstitutionType;
    function Get_Item(Index: Integer): IXMLSubstitutionType;
    property Items[Index: Integer]: IXMLSubstitutionType read Get_Item; default;
  end;

{ IXMLReplaceType }

  IXMLReplaceType = interface(IXMLNode)
    ['{20CBB304-7D53-40B9-87D2-5CB502AFE298}']
    { Property Accessors }
    function Get_Template: WideString;
    function Get_Result: WideString;
    procedure Set_Template(Value: WideString);
    procedure Set_Result(Value: WideString);
    { Methods & Properties }
    property Template: WideString read Get_Template write Set_Template;
    property Result: WideString read Get_Result write Set_Result;
  end;

{ IXMLReplaceTypeList }

  IXMLReplaceTypeList = interface(IXMLNodeCollection)
    ['{845B6020-3A75-42D3-8BC4-EF7F9E6A34FF}']
    { Methods & Properties }
    function Add: IXMLReplaceType;
    function Insert(const Index: Integer): IXMLReplaceType;
    function Get_Item(Index: Integer): IXMLReplaceType;
    property Items[Index: Integer]: IXMLReplaceType read Get_Item; default;
  end;

{ IXMLString_List }

  IXMLString_List = interface(IXMLNodeCollection)
    ['{174F014A-A1B9-4FDC-BF2B-1D21F3D6E41A}']
    { Methods & Properties }
    function Add(const Value: WideString): IXMLNode;
    function Insert(const Index: Integer; const Value: WideString): IXMLNode;
    function Get_Item(Index: Integer): WideString;
    property Items[Index: Integer]: WideString read Get_Item; default;
  end;

{ Forward Decls }

  TXMLRegularExpressionsType = class;
  TXMLRegularExpressionType = class;
  TXMLTestCaseType = class;
  TXMLTestCaseTypeList = class;
  TXMLSubjectType = class;
  TXMLSubstringType = class;
  TXMLMatchType = class;
  TXMLMatchTypeList = class;
  TXMLSubstitutionType = class;
  TXMLSubstitutionTypeList = class;
  TXMLReplaceType = class;
  TXMLReplaceTypeList = class;
  TXMLString_List = class;

{ TXMLRegularExpressionsType }

  TXMLRegularExpressionsType = class(TXMLNodeCollection, IXMLRegularExpressionsType)
  protected
    { IXMLRegularExpressionsType }
    function Get_RegularExpression(Index: Integer): IXMLRegularExpressionType;
    function Add: IXMLRegularExpressionType;
    function Insert(const Index: Integer): IXMLRegularExpressionType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRegularExpressionType }

  TXMLRegularExpressionType = class(TXMLNode, IXMLRegularExpressionType)
  private
    FCategory: IXMLString_List;
    FTestCase: IXMLTestCaseTypeList;
  protected
    { IXMLRegularExpressionType }
    function Get_Id: Integer;
    function Get_Name: WideString;
    function Get_Description: WideString;
    function Get_Comment: WideString;
    function Get_Expression: WideString;
    function Get_Category: IXMLString_List;
    function Get_TestCase: IXMLTestCaseTypeList;
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_Description(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Expression(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTestCaseType }

  TXMLTestCaseType = class(TXMLNode, IXMLTestCaseType)
  private
    FMatch: IXMLMatchTypeList;
    FSubstitution: IXMLSubstitutionTypeList;
    FReplace: IXMLReplaceTypeList;
  protected
    { IXMLTestCaseType }
    function Get_Modifiers: WideString;
    function Get_Subject: IXMLSubjectType;
    function Get_Match: IXMLMatchTypeList;
    function Get_Substitution: IXMLSubstitutionTypeList;
    function Get_Replace: IXMLReplaceTypeList;
    procedure Set_Modifiers(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTestCaseTypeList }

  TXMLTestCaseTypeList = class(TXMLNodeCollection, IXMLTestCaseTypeList)
  protected
    { IXMLTestCaseTypeList }
    function Add: IXMLTestCaseType;
    function Insert(const Index: Integer): IXMLTestCaseType;
    function Get_Item(Index: Integer): IXMLTestCaseType;
  end;

{ TXMLSubjectType }

  TXMLSubjectType = class(TXMLNodeCollection, IXMLSubjectType)
  protected
    { IXMLSubjectType }
    function Get_Substring(Index: Integer): IXMLSubstringType;
    function Add: IXMLSubstringType;
    function Insert(const Index: Integer): IXMLSubstringType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSubstringType }

  TXMLSubstringType = class(TXMLNode, IXMLSubstringType)
  protected
    { IXMLSubstringType }
    function Get_RepeatCount: Integer;
    function Get_FileName: WideString;
    procedure Set_RepeatCount(Value: Integer);
    procedure Set_FileName(Value: WideString);
  end;

{ TXMLMatchType }

  TXMLMatchType = class(TXMLNodeCollection, IXMLMatchType)
  protected
    { IXMLMatchType }
    function Get_Position: Integer;
    function Get_Length: Integer;
    function Get_CapturedSubstring(Index: Integer): WideString;
    procedure Set_Position(Value: Integer);
    procedure Set_Length(Value: Integer);
    function Add(const CapturedSubstring: WideString): IXMLNode;
    function Insert(const Index: Integer; const CapturedSubstring: WideString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMatchTypeList }

  TXMLMatchTypeList = class(TXMLNodeCollection, IXMLMatchTypeList)
  protected
    { IXMLMatchTypeList }
    function Add: IXMLMatchType;
    function Insert(const Index: Integer): IXMLMatchType;
    function Get_Item(Index: Integer): IXMLMatchType;
  end;

{ TXMLSubstitutionType }

  TXMLSubstitutionType = class(TXMLNode, IXMLSubstitutionType)
  protected
    { IXMLSubstitutionType }
    function Get_Template: WideString;
    function Get_Result: WideString;
    procedure Set_Template(Value: WideString);
    procedure Set_Result(Value: WideString);
  end;

{ TXMLSubstitutionTypeList }

  TXMLSubstitutionTypeList = class(TXMLNodeCollection, IXMLSubstitutionTypeList)
  protected
    { IXMLSubstitutionTypeList }
    function Add: IXMLSubstitutionType;
    function Insert(const Index: Integer): IXMLSubstitutionType;
    function Get_Item(Index: Integer): IXMLSubstitutionType;
  end;

{ TXMLReplaceType }

  TXMLReplaceType = class(TXMLNode, IXMLReplaceType)
  protected
    { IXMLReplaceType }
    function Get_Template: WideString;
    function Get_Result: WideString;
    procedure Set_Template(Value: WideString);
    procedure Set_Result(Value: WideString);
  end;

{ TXMLReplaceTypeList }

  TXMLReplaceTypeList = class(TXMLNodeCollection, IXMLReplaceTypeList)
  protected
    { IXMLReplaceTypeList }
    function Add: IXMLReplaceType;
    function Insert(const Index: Integer): IXMLReplaceType;
    function Get_Item(Index: Integer): IXMLReplaceType;
  end;

{ TXMLString_List }

  TXMLString_List = class(TXMLNodeCollection, IXMLString_List)
  protected
    { IXMLString_List }
    function Add(const Value: WideString): IXMLNode;
    function Insert(const Index: Integer; const Value: WideString): IXMLNode;
    function Get_Item(Index: Integer): WideString;
  end;

{ Global Functions }

function GetregularExpressions(Doc: IXMLDocument): IXMLRegularExpressionsType;
function LoadregularExpressions(const FileName: WideString): IXMLRegularExpressionsType;
function NewregularExpressions: IXMLRegularExpressionsType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetregularExpressions(Doc: IXMLDocument): IXMLRegularExpressionsType;
begin
  Result := Doc.GetDocBinding('regularExpressions', TXMLRegularExpressionsType, TargetNamespace) as IXMLRegularExpressionsType;
end;

function LoadregularExpressions(const FileName: WideString): IXMLRegularExpressionsType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('regularExpressions', TXMLRegularExpressionsType, TargetNamespace) as IXMLRegularExpressionsType;
end;

function NewregularExpressions: IXMLRegularExpressionsType;
begin
  Result := NewXMLDocument.GetDocBinding('regularExpressions', TXMLRegularExpressionsType, TargetNamespace) as IXMLRegularExpressionsType;
end;

{ TXMLRegularExpressionsType }

procedure TXMLRegularExpressionsType.AfterConstruction;
begin
  RegisterChildNode('regularExpression', TXMLRegularExpressionType);
  ItemTag := 'regularExpression';
  ItemInterface := IXMLRegularExpressionType;
  inherited;
end;

function TXMLRegularExpressionsType.Get_RegularExpression(Index: Integer): IXMLRegularExpressionType;
begin
  Result := List[Index] as IXMLRegularExpressionType;
end;

function TXMLRegularExpressionsType.Add: IXMLRegularExpressionType;
begin
  Result := AddItem(-1) as IXMLRegularExpressionType;
end;

function TXMLRegularExpressionsType.Insert(const Index: Integer): IXMLRegularExpressionType;
begin
  Result := AddItem(Index) as IXMLRegularExpressionType;
end;

{ TXMLRegularExpressionType }

procedure TXMLRegularExpressionType.AfterConstruction;
begin
  RegisterChildNode('testCase', TXMLTestCaseType);
  FCategory := CreateCollection(TXMLString_List, IXMLNode, 'category') as IXMLString_List;
  FTestCase := CreateCollection(TXMLTestCaseTypeList, IXMLTestCaseType, 'testCase') as IXMLTestCaseTypeList;
  inherited;
end;

function TXMLRegularExpressionType.Get_Id: Integer;
begin
  Result := AttributeNodes['id'].NodeValue;
end;

procedure TXMLRegularExpressionType.Set_Id(Value: Integer);
begin
  SetAttribute('id', Value);
end;

function TXMLRegularExpressionType.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLRegularExpressionType.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TXMLRegularExpressionType.Get_Description: WideString;
begin
  Result := ChildNodes['description'].Text;
end;

procedure TXMLRegularExpressionType.Set_Description(Value: WideString);
begin
  ChildNodes['description'].NodeValue := Value;
end;

function TXMLRegularExpressionType.Get_Comment: WideString;
begin
  Result := ChildNodes['comment'].Text;
end;

procedure TXMLRegularExpressionType.Set_Comment(Value: WideString);
begin
  ChildNodes['comment'].NodeValue := Value;
end;

function TXMLRegularExpressionType.Get_Expression: WideString;
begin
  Result := ChildNodes['expression'].Text;
end;

procedure TXMLRegularExpressionType.Set_Expression(Value: WideString);
begin
  ChildNodes['expression'].NodeValue := Value;
end;

function TXMLRegularExpressionType.Get_Category: IXMLString_List;
begin
  Result := FCategory;
end;

function TXMLRegularExpressionType.Get_TestCase: IXMLTestCaseTypeList;
begin
  Result := FTestCase;
end;

{ TXMLTestCaseType }

procedure TXMLTestCaseType.AfterConstruction;
begin
  RegisterChildNode('subject', TXMLSubjectType);
  RegisterChildNode('match', TXMLMatchType);
  RegisterChildNode('substitution', TXMLSubstitutionType);
  RegisterChildNode('replace', TXMLReplaceType);
  FMatch := CreateCollection(TXMLMatchTypeList, IXMLMatchType, 'match') as IXMLMatchTypeList;
  FSubstitution := CreateCollection(TXMLSubstitutionTypeList, IXMLSubstitutionType, 'substitution') as IXMLSubstitutionTypeList;
  FReplace := CreateCollection(TXMLReplaceTypeList, IXMLReplaceType, 'replace') as IXMLReplaceTypeList;
  inherited;
end;

function TXMLTestCaseType.Get_Modifiers: WideString;
begin
  Result := AttributeNodes['Modifiers'].Text;
end;

procedure TXMLTestCaseType.Set_Modifiers(Value: WideString);
begin
  SetAttribute('Modifiers', Value);
end;

function TXMLTestCaseType.Get_Subject: IXMLSubjectType;
begin
  Result := ChildNodes['subject'] as IXMLSubjectType;
end;

function TXMLTestCaseType.Get_Match: IXMLMatchTypeList;
begin
  Result := FMatch;
end;

function TXMLTestCaseType.Get_Substitution: IXMLSubstitutionTypeList;
begin
  Result := FSubstitution;
end;

function TXMLTestCaseType.Get_Replace: IXMLReplaceTypeList;
begin
  Result := FReplace;
end;

{ TXMLTestCaseTypeList }

function TXMLTestCaseTypeList.Add: IXMLTestCaseType;
begin
  Result := AddItem(-1) as IXMLTestCaseType;
end;

function TXMLTestCaseTypeList.Insert(const Index: Integer): IXMLTestCaseType;
begin
  Result := AddItem(Index) as IXMLTestCaseType;
end;
function TXMLTestCaseTypeList.Get_Item(Index: Integer): IXMLTestCaseType;
begin
  Result := List[Index] as IXMLTestCaseType;
end;

{ TXMLSubjectType }

procedure TXMLSubjectType.AfterConstruction;
begin
  RegisterChildNode('substring', TXMLSubstringType);
  ItemTag := 'substring';
  ItemInterface := IXMLSubstringType;
  inherited;
end;

function TXMLSubjectType.Get_Substring(Index: Integer): IXMLSubstringType;
begin
  Result := List[Index] as IXMLSubstringType;
end;

function TXMLSubjectType.Add: IXMLSubstringType;
begin
  Result := AddItem(-1) as IXMLSubstringType;
end;

function TXMLSubjectType.Insert(const Index: Integer): IXMLSubstringType;
begin
  Result := AddItem(Index) as IXMLSubstringType;
end;

{ TXMLSubstringType }

function TXMLSubstringType.Get_RepeatCount: Integer;
begin
  Result := AttributeNodes['repeatCount'].NodeValue;
end;

procedure TXMLSubstringType.Set_RepeatCount(Value: Integer);
begin
  SetAttribute('repeatCount', Value);
end;

function TXMLSubstringType.Get_FileName: WideString;
begin
  Result := AttributeNodes['fileName'].Text;
end;

procedure TXMLSubstringType.Set_FileName(Value: WideString);
begin
  SetAttribute('fileName', Value);
end;

{ TXMLMatchType }

procedure TXMLMatchType.AfterConstruction;
begin
  ItemTag := 'capturedSubstring';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLMatchType.Get_Position: Integer;
begin
  Result := AttributeNodes['position'].NodeValue;
end;

procedure TXMLMatchType.Set_Position(Value: Integer);
begin
  SetAttribute('position', Value);
end;

function TXMLMatchType.Get_Length: Integer;
begin
  Result := AttributeNodes['length'].NodeValue;
end;

procedure TXMLMatchType.Set_Length(Value: Integer);
begin
  SetAttribute('length', Value);
end;

function TXMLMatchType.Get_CapturedSubstring(Index: Integer): WideString;
begin
  Result := List[Index].Text;
end;

function TXMLMatchType.Add(const CapturedSubstring: WideString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := CapturedSubstring;
end;

function TXMLMatchType.Insert(const Index: Integer; const CapturedSubstring: WideString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := CapturedSubstring;
end;

{ TXMLMatchTypeList }

function TXMLMatchTypeList.Add: IXMLMatchType;
begin
  Result := AddItem(-1) as IXMLMatchType;
end;

function TXMLMatchTypeList.Insert(const Index: Integer): IXMLMatchType;
begin
  Result := AddItem(Index) as IXMLMatchType;
end;
function TXMLMatchTypeList.Get_Item(Index: Integer): IXMLMatchType;
begin
  Result := List[Index] as IXMLMatchType;
end;

{ TXMLSubstitutionType }

function TXMLSubstitutionType.Get_Template: WideString;
begin
  Result := ChildNodes['template'].Text;
end;

procedure TXMLSubstitutionType.Set_Template(Value: WideString);
begin
  ChildNodes['template'].NodeValue := Value;
end;

function TXMLSubstitutionType.Get_Result: WideString;
begin
  Result := ChildNodes['result'].Text;
end;

procedure TXMLSubstitutionType.Set_Result(Value: WideString);
begin
  ChildNodes['result'].NodeValue := Value;
end;

{ TXMLSubstitutionTypeList }

function TXMLSubstitutionTypeList.Add: IXMLSubstitutionType;
begin
  Result := AddItem(-1) as IXMLSubstitutionType;
end;

function TXMLSubstitutionTypeList.Insert(const Index: Integer): IXMLSubstitutionType;
begin
  Result := AddItem(Index) as IXMLSubstitutionType;
end;
function TXMLSubstitutionTypeList.Get_Item(Index: Integer): IXMLSubstitutionType;
begin
  Result := List[Index] as IXMLSubstitutionType;
end;

{ TXMLReplaceType }

function TXMLReplaceType.Get_Template: WideString;
begin
  Result := ChildNodes['template'].Text;
end;

procedure TXMLReplaceType.Set_Template(Value: WideString);
begin
  ChildNodes['template'].NodeValue := Value;
end;

function TXMLReplaceType.Get_Result: WideString;
begin
  Result := ChildNodes['result'].Text;
end;

procedure TXMLReplaceType.Set_Result(Value: WideString);
begin
  ChildNodes['result'].NodeValue := Value;
end;

{ TXMLReplaceTypeList }

function TXMLReplaceTypeList.Add: IXMLReplaceType;
begin
  Result := AddItem(-1) as IXMLReplaceType;
end;

function TXMLReplaceTypeList.Insert(const Index: Integer): IXMLReplaceType;
begin
  Result := AddItem(Index) as IXMLReplaceType;
end;
function TXMLReplaceTypeList.Get_Item(Index: Integer): IXMLReplaceType;
begin
  Result := List[Index] as IXMLReplaceType;
end;

{ TXMLString_List }

function TXMLString_List.Add(const Value: WideString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := Value;
end;

function TXMLString_List.Insert(const Index: Integer; const Value: WideString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := Value;
end;
function TXMLString_List.Get_Item(Index: Integer): WideString;
begin
  Result := List[Index].NodeValue;
end;

end. 