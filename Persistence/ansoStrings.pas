{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
unit ansoStrings;

interface

function ParseProfileString (const AString : string;
                              out AName, AValue : string
                            ) : boolean;
// Extracts name and value from ini-file line (like 'name=value').
// Removes all spaces (chars <= ' ') around name and value,
// removes quotes (' or ") if exists around value.
// If string starts with ';' (after optional spaces)
// or doesn't contain '=', then the function returns False
// and doesn't fill AName and AValue.

function MakeProfileString (const AName, AValue : string) : string;
// Makes string for ini-file line (like 'name=value').
// If AValue starts or ends with ' ' then it will be
// quoted with characters '"', for example
// MakeProfileString ('Name1', 'Value2 ') = 'Name1="Value2 "'.

function EncodeProfileValue (const AString : string) : string;
// encode all charactes below #$20 to store it properly
// in ini-files.
// all charactes below #$20 and 'escape' character '%'
// encoded as '%nn' where nn is hex-code of encoded char.
// For example:
// EncodeProfileValue (' 1'#$d#$a'2') = ' 1%0D%0A2'

function DecodeProfileValue (const AString : string) : string;
// restores string encoded with function EncodeProfileValue
// if 'escape' character followed by non-hex chars, then
// this sequence is not decoded (so we decrease probability
// to destroy data from standard ini-file that was not encoded)
// For example DecodeProfileValue ('1%202%aw%') = '1 2%aw%'


implementation

uses
 SysUtils;

function ParseProfileString (const AString : string;
                              out AName, AValue : string
                             ) : boolean;
 const
  SpaceChars = [#0 .. ' '];
 var
  iEq, iBeg, iEnd : integer;
  Len : integer;
 begin
  iEq := pos ('=', AString);
  if iEq <= 0 then begin
    AName := '';
    AValue := '';
    Result := False;
    EXIT;
   end;

  Len := length (AString);
  iBeg := 1;
  while (iBeg < iEq) and (AString [iBeg] in SpaceChars)
   do inc (iBeg);

  if AString [iBeg] = ';' then begin
    // Comment line
    AName := '';
    AValue := '';
    Result := False;
    EXIT;
   end;

  iEnd := iEq - 1;
  while (iEnd >= iBeg) and (AString [iEnd] in SpaceChars)
   do dec (iEnd);

  AName := copy (AString, iBeg, iEnd - iBeg + 1);

  iBeg := iEq + 1;
  while (iBeg <= Len) and (AString [iBeg] in SpaceChars)
   do inc (iBeg);
  iEnd := Len;
  while (iEnd >= iBeg) and (AString [iEnd] in SpaceChars)
   do dec (iEnd);
  if (iBeg <= Len) and (AString [iBeg] in ['''', '"'])
     and (iEnd > iBeg) and (AString [iBeg] = AString [iEnd]) then begin
    inc (iBeg);
    dec (iEnd);
   end;

  AValue := copy (AString, iBeg, iEnd - iBeg + 1);

  Result := True;
 end;

function MakeProfileString (const AName, AValue : string) : string;
 var
  Len : integer;
 begin
  Len := length (AValue);
  if (Len > 0) and ((AValue [1] = ' ') or (AValue [Len] = ' '))
   then Result := AName + '="' + AValue + '"'
   else Result := AName + '=' + AValue;
 end;

const
 EscChar = '%';
 Chars2Enc = [#0 .. #$1F, EscChar];

function EncodeProfileValue (const AString : string) : string;
 var
  Len, ResLen : integer;
  iIn, iRes : integer;
  HexStr : string;
 begin
  Len := length (AString);

  ResLen := Len;
  for iIn := 1 to Len do
   if AString [iIn] in Chars2Enc
    then inc (ResLen, 2);

  if ResLen = Len then begin
    Result := AString;
    EXIT;
   end;

  SetString (Result, nil, ResLen);
  iRes := 1;
  for iIn := 1 to Len do
   if AString [iIn] in Chars2Enc then begin
      Result [iRes] := EscChar;
      inc (iRes);
      HexStr := IntToHex (ord (AString [iIn]), 2);
      Result [iRes] := HexStr [1];
      inc (iRes);
      Result [iRes] := HexStr [2];
      inc (iRes);
     end
    else begin
      Result [iRes] := AString [iIn];
      inc (iRes);
     end;
 end;

function DecodeProfileValue (const AString : string) : string;
 const
  HexChars = ['0' .. '9', 'A' .. 'F', 'a' .. 'f'];
 var
  Len, ResLen : integer;
  iIn, iRes : integer;
  HexVal : integer;
 begin
  Len := length (AString);

  ResLen := Len;
  iIn := 1;
  while iIn <= Len - 2 do
   if (AString [iIn] = EscChar)
       and (AString [iIn + 1] in HexChars)
       and (AString [iIn + 2] in HexChars) then begin
      dec (ResLen, 2);
      inc (iIn, 3)
     end
    else inc (iIn);

  if ResLen = Len then begin
    Result := AString;
    EXIT;
   end;

  SetString (Result, nil, ResLen);
  iIn := 1;
  for iRes := 1 to ResLen do
   if (AString [iIn] = EscChar) and (iIn <= Len - 2)
       and (AString [iIn + 1] in HexChars)
       and (AString [iIn + 2] in HexChars) then begin
      HexVal := StrToInt ('$' + copy (AString, iIn + 1, 2));
      Result [iRes] := Char (HexVal);
      inc (iIn, 3);
     end
    else begin
      Result [iRes] := AString [iIn];
      inc (iIn);
     end;
 end;

end.
