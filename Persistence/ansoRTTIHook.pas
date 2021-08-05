{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
{$IFDEF FPC}
{$DEFINE NODEREF}
{$ENDIF}
unit ansoRTTIHook;

{
  Helper class for RTTI usage

  (c) 2002 Andrey V. Sorokin

  -= Brief description =-
  Provides easy and fast access to RTTI of
  specified ('hooked') object.

  -= Limitations =-
  Supports only 'simple types' (all subtypes of
   integer, float, string and char) and TStrings (accessed
   as CommaText).

  -= History =-

  2002.07.28 v.0.01
  -=- Just first version
}

interface

{$I DelphiVers.pas}
{$DEFINE TStringsRTTISupport}
 // define if TRTTIHook must support convertion of TStrings to/from string

uses
 SysUtils, // Exceptions
 TypInfo;

type
 TansoRTTIHook = class
   private
    fHookedObject : TObject;
    fCount : integer;
    fPropList : PPropList;
    procedure SetHookedObject (const AObjectToHook : TObject);
    function GetExists (const APropertyName : string) : boolean;
    function GetReadable (const AIdx : integer) : boolean;
    function GetWriteable (const AIdx : integer) : boolean;
    function GetNames (const AIdx : integer) : string;
    function GetIndexes (const APropertyName : string) : integer;
    function GetValues (const APropertyName : string) : Variant;
    procedure SetValues (const APropertyName : string; const AValue : Variant);
    function GetAsVariants (const AIdx : integer) : Variant;
    procedure SetAsVariants (const AIdx : integer; const AValue : Variant);
    function GetAsStrings (const AIdx : integer) : string;
    procedure SetAsStrings (const AIdx : integer; AValue : string);
   public
    constructor Create (AObjectToHook : TObject);
    destructor Destroy; override;

    property HookedObject : TObject read fHookedObject write SetHookedObject;
    // Object to which RTTI access provided
    property Count : integer read fCount;
    // number of RTTI properties

    property Exists [const APropertyName : string] : boolean read GetExists;
    // False if there is no RTTI property with such a name
    property Readable [const AIdx : integer] : boolean read GetReadable;
    // True if the RTTI property is readable
    property Writeable [const AIdx : integer] : boolean read GetWriteable;
    // True if the RTTI property is writeable
    property Names [const AIdx : integer] : string read GetNames;
    // names of RTTI properties by index
    property Indexes [const APropertyName : string] : integer read GetIndexes;
    // Indices of properties by names (-1 if no such property)
    property Values [const APropertyName : string] : Variant
     read GetValues write SetValues; DEFAULT;
    // values of RTTI properties (by property name)
    property AsVariants [const AIdx : integer] : Variant
     read GetAsVariants write SetAsVariants;
    // values of RTTI properties (by property index)
    property AsStrings [const AIdx : integer] : string
     read GetAsStrings write SetAsStrings;
    // values of RTTI properties (by property index) as strings
    // Attension! May raise ERTTIHookConvertError while assigning
    // unconvertable values.

    procedure Clear (const AIdx : integer);
    // set 'empty' value (0 to numerics, '' to strings,
    // TStrings.Clear)
  end;

ERTTIHook = class (Exception)
  private
   fObjectClassName : string;
   fObjectPropName : string;
  public
   property ObjectClassName : string read fObjectClassName;
   property ObjectPropName : string read fObjectPropName;
   // name of the object processed and of property
   // that was beeing converting when exception raised.
   constructor Create (const AMsg: string;
    const AObjectClassName, AObjectPropName : string);
 end;

ERTTIHookConvertError = class (ERTTIHook)
 end;

ERTTIHookNoSuchProperty = class (ERTTIHook)
 end;

implementation

uses
 {$IFDEF TStringsRTTISupport} Classes {$ENDIF}
 ;

constructor ERTTIHook.Create (const AMsg: string;
    const AObjectClassName, AObjectPropName : string);
 begin
  inherited Create (AMsg);
  fObjectClassName := AObjectClassName;
  fObjectPropName := AObjectPropName
 end;

constructor TansoRTTIHook.Create (AObjectToHook : TObject);
 begin
  inherited Create;
  HookedObject := AObjectToHook;
 end;

destructor TansoRTTIHook.Destroy;
 begin
  if Assigned (fPropList) then begin
    FreeMem (fPropList);
    fPropList := nil;
   end;
  inherited;
 end;

procedure TansoRTTIHook.SetHookedObject (const AObjectToHook : TObject);
 begin
  fHookedObject := AObjectToHook;

  Assert (fHookedObject.ClassInfo <> nil,
   Format ('There is no RTTI in the object "%s"',
    [fHookedObject.ClassName]));

  if Assigned (fPropList) then begin
    FreeMem (fPropList);
    fPropList := nil;
   end;

  fCount:= GetTypeData(fHookedObject.ClassInfo)^.PropCount;
  if fCount > 0 then begin
    GetMem (fPropList, fCount * SizeOf (Pointer));
    GetPropInfos (fHookedObject.ClassInfo, fPropList);
    // SortPropList
   end;
 end;

function TansoRTTIHook.GetExists (const APropertyName : string) : boolean;
 begin
  Result := Indexes [APropertyName] <> -1;
 end;

function TansoRTTIHook.GetReadable (const AIdx : integer) : boolean;
 var
  Info : PPropInfo;
 begin
  if (AIdx >= 0) and (AIdx < fCount)
      and (Assigned (fPropList [AIdx])) then begin
     Info := fPropList [AIdx];
     Result := Assigned (Info.GetProc);
    end
   else Result := False;
 end;

function TansoRTTIHook.GetWriteable (const AIdx : integer) : boolean;
 var
  Info : PPropInfo;
 begin
  if (AIdx >= 0) and (AIdx < fCount)
      and (Assigned (fPropList [AIdx])) then begin
     Info := fPropList [AIdx];
     Result := Assigned (Info.SetProc);
    end
   else Result := False;
 end;

function TansoRTTIHook.GetNames (const AIdx : integer) : string;
 var
  Info : PPropInfo;
 begin
  if (AIdx >= 0) and (AIdx < fCount)
      and (Assigned (fPropList [AIdx])) then begin
     Info := fPropList [AIdx];
     Result := Info.Name;
    end
   else Result := '';
 end;

function TansoRTTIHook.GetIndexes (const APropertyName : string) : integer;
 var
  Idx : integer;
 begin
  for Idx := 0 to Count - 1 do
   if CompareText (APropertyName, PPropInfo (fPropList [Idx]).Name) = 0 then begin
     Result := Idx;
     EXIT;
    end;
  Result := -1;
 end;

function TansoRTTIHook.GetAsVariants (const AIdx : integer) : Variant;
 var
  Info : PPropInfo;
 begin
  if (AIdx >= 0) and (AIdx < fCount)
      and Assigned (fPropList [AIdx]) then begin
    Info := fPropList [AIdx];
    Result := GetPropValue (fHookedObject, Info.Name);
   end;
 end;

procedure TansoRTTIHook.SetAsVariants (const AIdx : integer; const AValue : Variant);
 var
  Info : PPropInfo;
 begin
  if (AIdx >= 0) and (AIdx < fCount)
      and Assigned (fPropList [AIdx]) then begin
    Info := fPropList [AIdx];
    SetPropValue (fHookedObject, Info.Name, AValue);
   end;
 end;

function TansoRTTIHook.GetValues (const APropertyName : string) : Variant;
 var
  Idx : integer;
 begin
  Idx := Indexes [APropertyName];
  if Idx <> - 1
   then Result := AsVariants [Idx]
   else
    raise ERTTIHookNoSuchProperty.Create (
     Format ('Unknown property "%s.%s"', [fHookedObject.ClassName, APropertyName]),
     fHookedObject.ClassName, APropertyName);
 end;

procedure TansoRTTIHook.SetValues (const APropertyName : string; const AValue : Variant);
 var
  Idx : integer;
 begin
  Idx := Indexes [APropertyName];
  if Idx <> - 1
   then AsVariants [Idx] := AValue
   else
    raise ERTTIHookNoSuchProperty.Create (
     Format ('Unknown property "%s.%s"', [fHookedObject.ClassName, APropertyName]),
     fHookedObject.ClassName, APropertyName);
 end;

function TansoRTTIHook.GetAsStrings (const AIdx : integer) : string;
 var
  PropInfo : PPropInfo;
  PropKind : TTypeKind;
  ObjProp : TObject;
 begin
  PropInfo := fPropList [AIdx];
  PropKind := PropInfo^.PropType^{$IFNDEF NODEREF}^{$ENDIF}.Kind;
  case PropKind of
    tkInteger : Result := IntToStr ((GetOrdProp (fHookedObject, PropInfo)));
    tkChar :    Result := Char ((GetOrdProp (fHookedObject, PropInfo)));
    tkFloat :   Result := FloatToStr ((GetFloatProp (fHookedObject, PropInfo)));
    tkString,
    tkLString,
    tkWString : Result := GetStrProp (fHookedObject, PropInfo);
    tkInt64 :   Result := IntToStr (GetInt64Prop (fHookedObject, PropInfo));
    tkClass : begin
      ObjProp := TObject (GetOrdProp (fHookedObject, PropInfo));
      {$IFDEF TStringsRTTISupport}
      if ObjProp is TStrings then begin
        with TStrings (ObjProp) do
         Result := CommaText;
       end
      {$ENDIF}
     end; { of tkClass}
   end; { of case PropKind}
 end;

procedure TansoRTTIHook.SetAsStrings (const AIdx : integer; AValue : string);
 var
  PropInfo : PPropInfo;
  PropKind : TTypeKind;
  ObjProp : TObject;
 begin
  PropInfo := fPropList [AIdx];
  PropKind := PropInfo^.PropType^{$IFNDEF NODEREF}^{$ENDIF}.Kind;
  try
    case PropKind of
      tkInteger : SetOrdProp (fHookedObject, PropInfo, StrToInt (AValue));
      tkChar    : if length (AValue) <> 1
                   then raise EConvertError.Create ('Must be one char value')
                   else SetOrdProp (fHookedObject, PropInfo, ord (AValue [1]));
      tkFloat   : SetFloatProp (fHookedObject, PropInfo, StrToFloat (AValue));
      tkString,
      tkLString,
      tkWString : SetStrProp (fHookedObject, PropInfo, AValue);
      tkInt64   : SetInt64Prop (fHookedObject, PropInfo, StrToInt64 (AValue));
      tkClass : begin
        ObjProp := TObject (GetOrdProp (fHookedObject, PropInfo));
        {$IFDEF TStringsRTTISupport}
        if ObjProp is TStrings then begin
          with TStrings (ObjProp) do
           CommaText := AValue;
         end
        {$ENDIF}
       end; { of tkClass}
     end; { of case PropKind}
    except on E: EConvertError do
      raise ERTTIHookConvertError.Create (
              Format ('%s (Object:%s.%s)',
               [E.Message, fHookedObject.ClassName, PropInfo.Name]),
               fHookedObject.ClassName, PropInfo.Name);
   end;
 end;

procedure TansoRTTIHook.Clear (const AIdx : integer);
 var
  PropInfo : PPropInfo;
  PropKind : TTypeKind;
  ObjProp : TObject;
 begin
  PropInfo := fPropList [AIdx];
  PropKind := PropInfo^.PropType^{$IFNDEF NODEREF}^{$ENDIF}.Kind;
  case PropKind of
      tkInteger : SetOrdProp (fHookedObject, PropInfo, 0);
      tkChar    : SetOrdProp (fHookedObject, PropInfo, 0);
      tkFloat   : SetFloatProp (fHookedObject, PropInfo, 0);
      tkString,
      tkLString,
      tkWString : SetStrProp (fHookedObject, PropInfo, '');
      tkInt64   : SetInt64Prop (fHookedObject, PropInfo, 0);
      tkClass : begin
        ObjProp := TObject (GetOrdProp (fHookedObject, PropInfo));
        {$IFDEF TStringsRTTISupport}
        if ObjProp is TStrings then begin
          // Current version can handle only TStrings descendants
          with TStrings (ObjProp) do
           Clear;
         end
        {$ENDIF}
       end; { of tkClass}
   end; { of case PropKind}
 end;

end.

