{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
unit tynList;

{
  Simple persistence framework

  (c) 2002 Andrey V. Sorokin

  -= Brief description =-
  Object inherited from TtynListItem can be
  stored with help of TtynList descendants.
  In current version supported only .ini-file
  storage and only plain objects (without
  aggregations (except TStrings) and references).

  -= History =-

  2002.07.28 v.0.02
  -=- Polished up before release

  2002.05.09 v.0.01
  -=- Just first version
}

interface

uses
 Classes, // TStream
 contnrs; // TObjectList

type
 TtynItemID = integer;
 TtynList = class;

{$TYPEINFO ON}
 TtynListItem = class (TObject)
   protected
    fStorageRef : TtynList;
    fInStorageID : TtynItemID;
  end;

 CtynListItem = class of TtynListItem;

 TtynList = class (TObjectList)
   private
    fNextItemID : TtynItemID;
    fStorageIDPrefix : string;
    fDefaultItemClass : CtynListItem;
    fEncodedValues : boolean;
    procedure IncorporateItem (AItem : TtynListItem);
   public
    constructor Create (ADefaultItemClass : CtynListItem = nil);

    property StorageIDPrefix : string read fStorageIDPrefix write fStorageIDPrefix;
    property DefaultItemClass : CtynListItem read fDefaultItemClass write fDefaultItemClass;

    procedure Add (AItem : TtynListItem);
    procedure Insert (AIndex: integer; AItem: TtynListItem);

    procedure SaveToFile (const AFileName : string);
    procedure SaveToStream (AStream : TStream);

    procedure LoadFromFile (const AFileName : string);
    procedure LoadFromStream (AStream : TStream);

    property EncodedValues : boolean read fEncodedValues write fEncodedValues;
    // If true then 'escapes' characters < #$20
    // (see En/DecodeProfileValue)
  end;

procedure RegisterItemClass (AClass : CtynListItem; const AClassID : string = '');

implementation

uses
 SysUtils,
 ansoStrings,
 ansoRTTIHook;

const
 ItemClassIDValueName = '_ItemClassID';

type
 TtynItemClassDescr = class
   public
    ItemClass : CtynListItem;
    ItemClassID : string;
  end;

var
 tynItemClasses : TObjectList = nil;

function GettynListItemClassID (AClass : CtynListItem) : string;
 var
  i : integer;
 begin
  Result := '';
  for i := 0 to tynItemClasses.Count - 1 do
   with tynItemClasses [i] as TtynItemClassDescr do
   if AClass = ItemClass then begin
     Result := ItemClassID;
     BREAK;
    end;
 end;

function GettynListItemClass (const AClassID : string) : CtynListItem;
 var
  i : integer;
 begin
  Result := nil;
  for i := 0 to tynItemClasses.Count - 1 do
   with tynItemClasses [i] as TtynItemClassDescr do
   if AClassID = ItemClassID then begin
     Result := ItemClass;
     BREAK;
    end;
 end;

{
constructor TtynListItem.Create (AList : TtynList);
 begin
  inherited Create;
  fOwnerRef := AList;
 end;
}

constructor TtynList.Create (ADefaultItemClass : CtynListItem = nil);
 begin
  inherited Create;
  fNextItemID := 1;
  fEncodedValues := True;
  fDefaultItemClass := ADefaultItemClass
 end;

procedure TtynList.IncorporateItem (AItem : TtynListItem);
 begin
  AItem.fStorageRef := Self;
  AItem.fInStorageID := fNextItemID;
  inc (fNextItemID);
 end;

procedure TtynList.Add (AItem : TtynListItem);
 begin
  IncorporateItem (AItem);
  inherited Add (AItem);
 end;

procedure TtynList.Insert (AIndex: integer; AItem: TtynListItem);
 begin
  IncorporateItem (AItem);
  inherited Insert (AIndex, AItem);
 end;

procedure TtynList.SaveToFile (const AFileName : string);
 var
  TheStream : TFileStream;
 begin
  TheStream := TFileStream.Create (AFileName, fmCreate or fmShareExclusive);
  try
     SaveToStream (TheStream);
    finally TheStream.Free;
   end;
 end;

procedure TtynList.SaveToStream (AStream : TStream);
 procedure WriteStrings (AStrings : TStrings);
  var
   Buf : string;
  begin
   Buf := AStrings.Text;
   AStream.Write (PChar (Buf)^, length (Buf));
  end;
 var
  ItemIdx, PropIdx : integer;
  Strs : TStrings;
 begin
  Strs := TStringList.Create;
  try
    // Write section for each item
    for ItemIdx := 0 to Count - 1 do begin
      Strs.Clear;
      Strs.Add (
          '['
        + fStorageIDPrefix
        + IntToStr ((Items [ItemIdx] as TtynListItem).fInStorageID)
        + ']'
       );
      Strs.Add (ItemClassIDValueName + '='
       + GettynListItemClassID (CtynListItem (Items [ItemIdx].ClassType)));
      with TansoRTTIHook.Create (Items [ItemIdx]) do
       try
         for PropIdx := 0 to Count - 1 do
          if Readable [PropIdx]
           then
            if EncodedValues
             then Strs.Add (MakeProfileString (
                Names [PropIdx],
                EncodeProfileValue (AsStrings [PropIdx])))
             else Strs.Add (MakeProfileString (
                Names [PropIdx], AsStrings [PropIdx]));
         finally Free;
        end;
//      ObjectProps2Strings (Items [ItemIdx], Strs);
      Strs.Add (''); // just a separator for redability sake only
      WriteStrings (Strs);
     end;
    finally Strs.Free;
   end;
 end;

procedure TtynList.LoadFromFile (const AFileName : string);
 var
  TheStream : TFileStream;
 begin
  TheStream := TFileStream.Create (AFileName, fmOpenRead or fmShareDenyWrite);
  try
     LoadFromStream (TheStream);
    finally TheStream.Free;
   end;
 end;

procedure TtynList.LoadFromStream (AStream : TStream);
 var
  i : integer;
  Lines, PropList : TStrings;
  StreamBuf : string;
  Line : string;
  InSection : boolean;
  ItemID : TtynItemID;
  ItemClass : CtynListItem;
  TheName, TheValue : string;
 function FormatSectionHeaderLine (const AStr : string) : string;
  var
   n : integer;
  begin
   n := pos (']', AStr);
   if n > 0
    then Result := Trim (copy (AStr, 2, n - 2))
    else Result := Trim (copy (AStr, 2, MaxInt));
  end;

 procedure ProceedLoadedSection;
  var
   TheItem : TtynListItem;
   PropIdx : integer;
  begin
   if InSection then begin
     InSection := False;
     if not Assigned (ItemClass)
      then ItemClass := DefaultItemClass;
     if Assigned (ItemClass) then begin
       TheItem := ItemClass.Create;
       with TansoRTTIHook.Create (TheItem) do
        try
           for PropIdx := 0 to Count - 1 do
            if Writeable [PropIdx]
             then try
               AsStrings [PropIdx] := PropList.Values [Names [PropIdx]]
              except on ERTTIHookConvertError do
               ; // Clear (PropIdx); -- We just created it, no need to clear!
             end;
          finally Free;
         end;
//       Strings2ObjectProps (TheItem, PropList);
       Add (TheItem);
       if ItemID >= fNextItemID
        then fNextItemID := ItemID + 1;
      end;
    end;

   PropList.Clear;
   ItemClass := nil;
  end;
 begin
  fNextItemID := 1;

  Lines := nil;
  PropList := nil;
  try
    Lines := TStringList.Create;
    PropList := TStringList.Create;

    SetString (StreamBuf, nil, AStream.Size);
    AStream.Read (PChar (StreamBuf)^, length (StreamBuf));
    Lines.Text := StreamBuf;
    for i := 0 to Lines.Count - 1 do begin
     Line := Trim (Lines [i]);
     if length (Line) <= 0
      then CONTINUE;
     if Line [1] = '[' then begin // section header line?
        ProceedLoadedSection;
        Line := FormatSectionHeaderLine (Line);
        if CompareText (fStorageIDPrefix,
            copy (Line, 1, length (fStorageIDPrefix))) = 0 then begin
          ItemID := StrToIntDef (copy (Line, length (fStorageIDPrefix) + 1, MaxInt), -1);
          InSection := ItemID <> -1;
         end;
       end
      else begin // value line?
        if ParseProfileString (Line, TheName, TheValue) then begin
          if CompareText (ItemClassIDValueName, TheName) = 0
           then
            if EncodedValues
             then ItemClass := GettynListItemClass (DecodeProfileValue (TheValue))
             else ItemClass := GettynListItemClass (TheValue)
           else PropList.Add (TheName + '=' + DecodeProfileValue (TheValue));
         end;
       end;
     end;
    ProceedLoadedSection;
    finally begin
      FreeAndNil (PropList);
      FreeAndNil (Lines);
     end;
   end;
 end;

procedure RegisterItemClass (AClass : CtynListItem; const AClassID : string = '');
 var
  ClassDescr : TtynItemClassDescr;
 begin
//  if Assigned (AClass.PropInfos [ItemClassIDValueName])
// raise
  ClassDescr := TtynItemClassDescr.Create;
  ClassDescr.ItemClass := AClass;
  if AClassID = ''
   then ClassDescr.ItemClassID := AClass.ClassName
   else ClassDescr.ItemClassID := AClassID;
  tynItemClasses.Add (ClassDescr);
 end;

initialization
  tynItemClasses := TObjectList.Create;

finalization
  FreeAndNil (tynItemClasses);
end.

