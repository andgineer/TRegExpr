{$B-}
unit ansoRTTI;

{
  Helper routines for RTTI usage

  (c) 2002 Andrey V. Sorokin

  -= Limitations =-
  Supports only 'simple types' (all subtypes of
   integer, float, string and char) and TStrings (accessed
   as CommaText).

  -= History =-

  2002.05.09 v.0.01
  -=- Just first version
}

interface

uses
 SysUtils,
 Classes; // TStrings

procedure ObjectProps2Strings (AObject : TObject; AStrings : TStrings;
 AFillCleanProps : boolean = True);
// Fills AStrings with names and values of published
// properties of AObjects so You can access any property
// of AObject with AStrings.Values [PropName].
// The routine does NOT clear AStrings before processing.
// if AFillCleanProps is False (by default is True) then
// 'clear' properties aren't saved into AStrings.
// Attension!
// If You use AFillCleanProps with False value, then You
// must use routine Strings2ObjectProps with
// AClearUndefinedProps = True or fill all properties
// of AObject before calling Strings2ObjectProps to
// prevent 'trash' in values of undefined in AStrings
// properties.

function Strings2ObjectProps (AObject : TObject; AStrings : TStrings;
 AClearUndefinedProps : boolean = True;
 AConvertExceptions : boolean = False) : boolean;
// Fills all published properties of AObject with
// values from AStrings.
// If AClearUndefinedProps is True (by default) then
// clears properties that is not defined in AStrings.
// If AConvertExceptions is False (by default) then
// suppress all convertion error exceptins (if
// AClearUndefinedProps is True then appropriate
// properties will be cleared). If True, then raise
// EStrings2ObjectPropsConvertError
// Returns False if any convertion error occured.
// Attension!
// Before using with AClearUndefinedProps = False
// read comment for procedure ObjectProps2Strings

procedure ClearObjectProps (AObject : TObject);
// Clears all AObject properties of types supported
// by the library (0 to numerics, '' to strings,
// TStrings.Clear).

type
EStrings2ObjectPropsConvertError = class (EConvertError)
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

implementation

uses
 TypInfo;

constructor EStrings2ObjectPropsConvertError.Create (const AMsg: string;
    const AObjectClassName, AObjectPropName : string);
 begin
  inherited Create (AMsg);
  fObjectClassName := AObjectClassName;
  fObjectPropName := AObjectPropName
 end;


procedure ObjectProps2Strings (AObject : TObject; AStrings : TStrings;
 AFillCleanProps : boolean = True);
 var
  TypeInfo : PTypeInfo;
  PropCount : integer;
  PropList : TPropList; // 16 Kbytes structure!
  i : integer;
  PropInfo : PPropInfo;
  PropKind : TTypeKind;
  ObjProp : TObject;
 begin
//  AStrings.Clear;

  TypeInfo := AObject.ClassInfo;
  Assert (Assigned (TypeInfo), 'There is no RTTI in the object '
   + AObject.ClassName);
  PropCount := GetTypeData (TypeInfo)^.PropCount;
  GetPropInfos (TypeInfo, @PropList);

  for i := 0 to PropCount - 1 do begin
    PropInfo := PropList [i];
    PropKind := PropInfo^.PropType^^.Kind;
    if AFillCleanProps then begin
     // I duplicate the code for speed optimization sake.
     // So do NOT forget to duplicate all changes in both copies
     // (for AFillCleanProps True and False cases).
       case PropKind of
         tkInteger :
          AStrings.Values [PropInfo.Name] := IntToStr ((GetOrdProp (AObject, PropInfo)));
         tkChar :
          AStrings.Values [PropInfo.Name] := Char ((GetOrdProp (AObject, PropInfo)));
         tkFloat :
          AStrings.Values [PropInfo.Name] := FloatToStr ((GetFloatProp (AObject, PropInfo)));
         tkString,
         tkLString,
         tkWString :
          AStrings.Values [PropInfo.Name] := GetStrProp (AObject, PropInfo);
         tkInt64 :
          AStrings.Values [PropInfo.Name] := IntToStr (GetInt64Prop (AObject, PropInfo));
         tkClass : begin
           ObjProp := TObject (GetOrdProp (AObject, PropInfo));
           if ObjProp is TStrings then begin
             // Current version can handle only TStrings descendants
             with TStrings (ObjProp) do
              AStrings.Values [PropInfo.Name] := CommaText;
             end
           end; { of tkClass}
        end; { of case PropKind}
      end
     else begin // not AFillCleanProps
       case PropKind of
         tkInteger :
          if GetOrdProp (AObject, PropInfo) <> 0
           then AStrings.Values [PropInfo.Name] := IntToStr ((GetOrdProp (AObject, PropInfo)));
         tkChar :
          if GetOrdProp (AObject, PropInfo) <> 0
           then AStrings.Values [PropInfo.Name] := Char ((GetOrdProp (AObject, PropInfo)));
         tkFloat :
          if GetFloatProp (AObject, PropInfo) <> 0
           then AStrings.Values [PropInfo.Name] := FloatToStr ((GetFloatProp (AObject, PropInfo)));
         tkString,
         tkLString,
         tkWString :
          if length (GetStrProp (AObject, PropInfo)) > 0
           then AStrings.Values [PropInfo.Name] := GetStrProp (AObject, PropInfo);
         tkInt64 :
          if GetInt64Prop (AObject, PropInfo) <> 0
           then AStrings.Values [PropInfo.Name] := IntToStr (GetInt64Prop (AObject, PropInfo));
         tkClass : begin
           ObjProp := TObject (GetOrdProp (AObject, PropInfo));
           if ObjProp is TStrings then begin
             // Current version can handle only TStrings descendants
             with TStrings (ObjProp) do
              if Count > 0
               then AStrings.Values [PropInfo.Name] := CommaText;
             end
           end; { of tkClass}
        end; { of case PropKind}
      end;
   end; { of for i}
 end;

procedure ClearObjectProp (AObject : TObject; APropInfo : PPropInfo);
 var
  PropKind : TTypeKind;
  ObjProp : TObject;
 begin
  PropKind := APropInfo^.PropType^^.Kind;
  case PropKind of
      tkInteger : SetOrdProp (AObject, APropInfo, 0);
      tkChar    : SetOrdProp (AObject, APropInfo, 0);
      tkFloat   : SetFloatProp (AObject, APropInfo, 0);
      tkString,
      tkLString,
      tkWString : SetStrProp (AObject, APropInfo, '');
      tkInt64   : SetInt64Prop (AObject, APropInfo, 0);
      tkClass : begin
        ObjProp := TObject (GetOrdProp (AObject, APropInfo));
        if ObjProp is TStrings then begin
          // Current version can handle only TStrings descendants
          with TStrings (ObjProp) do
           Clear;
         end
       end; { of tkClass}
   end; { of case PropKind}
 end;

procedure ClearObjectProps (AObject : TObject);
 var
  TypeInfo : PTypeInfo;
  PropCount : integer;
  PropList : TPropList; // 16 Kbytes structure!
  i : integer;
  PropInfo : PPropInfo;
 begin
  TypeInfo := AObject.ClassInfo;
  Assert (Assigned (TypeInfo), 'There is no RTTI in the object '
   + AObject.ClassName);
  PropCount := GetTypeData (TypeInfo)^.PropCount;
  GetPropInfos (TypeInfo, @PropList);

  for i := 0 to PropCount - 1 do begin
    PropInfo := PropList [i];
    ClearObjectProp (AObject, PropInfo);
   end; { of for i}

 end;

function Strings2ObjectProps (AObject : TObject; AStrings : TStrings;
 AClearUndefinedProps : boolean = True;
 AConvertExceptions : boolean = False) : boolean;
 var
  TypeInfo : PTypeInfo;
  PropCount : integer;
  PropList : TPropList; // 16 Kbytes structure!
  i : integer;
  PropInfo : PPropInfo;
  PropKind : TTypeKind;
  ObjProp : TObject;
  TheValue : string;
 begin
  Result := True;

  TypeInfo := AObject.ClassInfo;
  Assert (Assigned (TypeInfo), 'There is no RTTI in the object '
   + AObject.ClassName);
  PropCount := GetTypeData (TypeInfo)^.PropCount;
  GetPropInfos (TypeInfo, @PropList);

  for i := 0 to PropCount - 1 do begin
    PropInfo := PropList [i];
    if AStrings.IndexOfName (PropInfo.Name) >= 0 then begin
       PropKind := PropInfo^.PropType^^.Kind;
       TheValue := AStrings.Values [PropInfo.Name];
       try
         case PropKind of
           tkInteger : SetOrdProp (AObject, PropInfo, StrToInt (TheValue));
           tkChar    : if length (TheValue) <> 1
                        then raise EConvertError.Create ('Must be one char value')
                        else SetOrdProp (AObject, PropInfo, ord (TheValue [1]));
           tkFloat   : SetFloatProp (AObject, PropInfo, StrToFloat (TheValue));
           tkString,
           tkLString,
           tkWString : SetStrProp (AObject, PropInfo, TheValue);
           tkInt64   : SetInt64Prop (AObject, PropInfo, StrToInt64 (TheValue));
           tkClass : begin
             ObjProp := TObject (GetOrdProp (AObject, PropInfo));
             if ObjProp is TStrings then begin
               // Current version can handle only TStrings descendants
               with TStrings (ObjProp) do
                CommaText := TheValue;
              end
            end; { of tkClass}
          end; { of case PropKind}
         except on E: EConvertError do
          if AConvertExceptions
           then raise EStrings2ObjectPropsConvertError.Create (
                        Format ('%s (Object:%s.%s)',
                        [E.Message, AObject.ClassName, PropInfo.Name]),
                       AObject.ClassName, PropInfo.Name)
           else begin
             Result := False;
             if AClearUndefinedProps
              then ClearObjectProp (AObject, PropInfo);
            end;
        end;
      end
     else // No value for the property defined in AStrings
      if AClearUndefinedProps
       then ClearObjectProp (AObject, PropInfo);
  end; { of for i}
 end;


end.
