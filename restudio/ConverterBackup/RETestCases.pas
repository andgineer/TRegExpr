{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
unit RETestCases;

{
 Container classes for
 regular expressions test cases.

 Stores regular expression, modifiers, input
 examples (with exec results) and so on.

 (c) 2002 Andrey V. Sorokin
  St-Petersburg, Russia
  anso@mail.ru, anso@paycash.ru
  http://anso.virtualave.net
  http://anso.da.ru
}

interface

uses
 Classes,
 RegExpr,
 tynList;

type
 TMatchRec = packed record
   StartPos : integer;
   EndPos : integer;
  end;

 TRETestCase = class (TtynListItem)
   private
    fName : string;
    fGroup : string;
    fComment : string;
    fDescription : string;
    fExpression : RegExprString;
    fInputStringBeg, fInputStringBody, fInputStringEnd : RegExprString;
    fInputStringBodyRepN : integer;
    fInputFileName : string;
    fModifierStr : string;
    fSubstitutionTemplate : RegExprString;
    fString4Replace : RegExprString;
    fMatchList : array of TMatchRec;

    function GetInputString : RegExprString;
    procedure SetInputString (AInputString : RegExprString);
    procedure SetInputFileName (AInputFileName : string);

    function GetSubExprMatchCount : integer;
    function GetMatchPos (Idx : integer) : integer;
    function GetMatchLen (Idx : integer) : integer;

    function GetMatchList : string;
    procedure SetMatchList (AStr : string);

   public

    property InputString : RegExprString read GetInputString write SetInputString;
    // -- compounded, so we must not save it directly

   published // -- I use RTTI to store fields into persistent storage
    property Name : string read fName write fName;
    property Group : string read fGroup write fGroup;
    property Comment : string read fComment write fComment;
    property Description : string read fDescription write fDescription;
    property InputStringBeg : RegExprString read fInputStringBeg write fInputStringBeg;
    property InputStringBody : RegExprString read fInputStringBody write fInputStringBody;
    property InputStringBodyRepN : integer read fInputStringBodyRepN write fInputStringBodyRepN;
    property InputStringEnd : RegExprString read fInputStringEnd write fInputStringEnd;
    property Expression : RegExprString read fExpression write fExpression;
    property InputFileName : string read fInputFileName write SetInputFileName;
    property ModifierStr : string read fModifierStr write fModifierStr;

    property SubstitutionTemplate : RegExprString read fSubstitutionTemplate write fSubstitutionTemplate;
    property String4Replace : RegExprString read fString4Replace write fString4Replace;

    property MatchList : string read GetMatchList write SetMatchList;

   public
    // --- result of Exec
    property SubExprMatchCount : integer read GetSubExprMatchCount;
    property MatchPos [Idx : integer] : integer read GetMatchPos;
    property MatchLen [Idx : integer] : integer read GetMatchLen;

    procedure AssignToTRegExpr (ARegExpr : TRegExpr);
    // Assignes values from the test case to TRegExpr object

    procedure AssignMatchList (ARegExpr : TRegExpr);
    // Assign Exec results (MatchPos etc) from the ARegExpr

  end;

 TRETestCases = class (TtynList)
   private
    function GetItem (AIdx : integer) : TRETestCase;
   public
    constructor Create;

    property Items [AIdx : integer] : TRETestCase read GetItem;
     DEFAULT;
    function IndexByName (const AName : string) : integer;

  end;


implementation

uses
 SysUtils;

{====================== TRETestCase ==========================}

function TRETestCase.GetInputString : RegExprString;
 var
  i : integer;
  Stream : TFileStream;
 begin
  if (length (fInputFileName) > 0) then begin
     Stream := TFileStream.Create (fInputFileName, fmOpenRead or fmShareDenyWrite);
     try
        if Stream.Size > 0 then begin
           SetString (Result, nil, Stream.Size);
           Stream.Read (PChar (Result)^, Stream.Size);
          end
         else Result := '';
       finally Stream.Free;
      end;
    end
  else if (fInputStringBodyRepN > 0)
          and (length (fInputStringBody) > 0) then begin
     Result := fInputStringBody;
     for i := 1 to fInputStringBodyRepN
      do Result := Result + Result;
     Result := fInputStringBeg + Result + fInputStringEnd;
    end
   else Result := fInputStringBeg + fInputStringBody + fInputStringEnd;
 end; { of function TRETestCase.GetInputString
--------------------------------------------------------------}

procedure TRETestCase.SetInputString (AInputString : RegExprString);
 begin
  fInputStringBeg := '';
  fInputStringBody := AInputString;
  fInputStringBodyRepN := 0;
  fInputStringEnd := '';
 end; { of procedure TRETestCase.SetInputString
--------------------------------------------------------------}

procedure TRETestCase.SetInputFileName (AInputFileName : string);
 begin
  fInputFileName := AInputFileName;
 end; { of procedure TRETestCase.SetInputFileName
--------------------------------------------------------------}

function TRETestCase.GetSubExprMatchCount : integer;
 begin
  Result := High (fMatchList) - Low (fMatchList); // we do not count last item
 end; { of function TRETestCase.GetSubExprMatchCount
--------------------------------------------------------------}

function TRETestCase.GetMatchPos (Idx : integer) : integer;
 begin
  Result := fMatchList [Idx].StartPos;
 end; { of function TRETestCase.GetMatchPos
--------------------------------------------------------------}

function TRETestCase.GetMatchLen (Idx : integer) : integer;
 begin
  with fMatchList [Idx] do
    Result := EndPos - StartPos + 1;
 end; { of function TRETestCase.GetMatchLen
--------------------------------------------------------------}

procedure TRETestCase.AssignToTRegExpr (ARegExpr : TRegExpr);
 begin
  ARegExpr.Expression := Expression;
  ARegExpr.InputString := InputString;
  ARegExpr.ModifierStr := ModifierStr;
 end; { of procedure TRETestCase.AssignToTRegExpr
--------------------------------------------------------------}

const
 remlDelim   = '/'; // delimiters inside MatchList between Start/End pos

function TRETestCase.GetMatchList : string;
 var
  i : integer;
 begin
  Result := '';
  with TStringList.Create do try
     for i := Low (fMatchList) to High (fMatchList) do
       Add (IntToStr (fMatchList [i].StartPos)
            + remlDelim + IntToStr (fMatchList [i].EndPos));
     Result := CommaText;
    finally Free;
   end;
 end; { of function TRETestCase.GetMatchList
--------------------------------------------------------------}

procedure TRETestCase.SetMatchList (AStr : string);
 var
  i, n : integer;
 begin
  with TStringList.Create do try
     CommaText := AStr;
     SetLength (fMatchList, Count);
     for i := 0 to Count - 1 do begin
       n := pos (remlDelim, Strings [i]);
       fMatchList [i].StartPos := StrToIntDef (copy (Strings [i], 1, n - 1), -1);
       fMatchList [i].EndPos := StrToIntDef (copy (Strings [i], n + 1, MaxInt), -1);
      end;
    finally Free;
   end;
 end; { of procedure TRETestCase.SetMatchList
--------------------------------------------------------------}

procedure TRETestCase.AssignMatchList (ARegExpr : TRegExpr);
 var
  i : integer;
 begin
  SetLength (fMatchList, ARegExpr.SubExprMatchCount + 1);
  for i := 0 to ARegExpr.SubExprMatchCount do
   with fMatchList [i] do begin
     StartPos := ARegExpr.MatchPos [i];
     EndPos := ARegExpr.MatchPos [i] + ARegExpr.MatchLen [i] - 1;
     if EndPos = -1
      then StartPos := -1;
     if StartPos = -1
      then EndPos := -1;
    end;
 end; { of procedure TRETestCase.AssignMatchList
--------------------------------------------------------------}


{====================== TRETestCases =========================}

constructor TRETestCases.Create;
 begin
  inherited Create (TRETestCase);
  StorageIDPrefix := 're_';
 end; { of constructor TRETestCases.Create
--------------------------------------------------------------}

function TRETestCases.GetItem (AIdx : integer) : TRETestCase;
 begin
  Result := TRETestCase (inherited Get (AIdx));
 end; { of function TRETestCases.GetItem
--------------------------------------------------------------}

function TRETestCases.IndexByName (const AName : string) : integer;
 begin
  Result := 0;
  while (Result < Count) and (CompareText (Items [Result].Name, AName) <> 0)
   do inc (Result);
  if Result >= Count
   then Result := -1;
 end; { of function TRETestCases.IndexByName
--------------------------------------------------------------}

initialization
 tynList.RegisterItemClass (TRETestCase);

end.

