program make_unidata;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpwidestring,
  StrUtils,
  unicodedata;

function IsUnicodeWordChar(AChar: WideChar): boolean;
var
  NType: byte;
begin
  if AChar='_' then
    Exit(true);

  if Ord(AChar) >= LOW_SURROGATE_BEGIN then
    Exit(False);

  NType := GetProps(Ord(AChar))^.Category;
  Result := (NType <= UGC_OtherNumber);
end;

function GetCateg(c: word): byte;
begin
  Result:= GetProps(c)^.Category;
end;

var
  i: integer;
  sl: tstringlist;
  bWord: boolean;
  byteVal: byte;
begin
  sl:= tstringlist.create;

  sl.add('unit regexpr_unicodedata;');
  sl.add('interface');
  sl.add('');
  sl.add('const');
  sl.add('  //bit 7: is word char; bits 0..6: category value from UnicodeData unit');
  sl.add('  CharCategoryArray: packed array[0..$FFFF] of byte = (');
  for i:= 0 to $FFFF do
  begin
    bWord:= IsUnicodeWordChar(widechar(i));
    byteVal:= (Ord(bWord) shl 7) + GetCateg(i);
    sl.add('    '+IntToStr(byteVal)+IfThen(i<$ffff, ',')+' // $'+IntToHex(i, 4));
  end;
  sl.add('  );');

  sl.add('');
  sl.add('  CharUpperArray: packed array[0..$FFFF] of WideChar = (');
  for i:= 0 to $FFFF do
    sl.add('    #$'+IntToHex(Ord(WideUpperCase(widechar(i))[1]), 4)+IfThen(i<$ffff, ',')+' // $'+IntToHex(i, 4));
  sl.add('  );');

  sl.add('');
  sl.add('  CharLowerArray: packed array[0..$FFFF] of WideChar = (');
  for i:= 0 to $FFFF do
    sl.add('    #$'+IntToHex(Ord(WideLowerCase(widechar(i))[1]), 4)+IfThen(i<$ffff, ',')+' // $'+IntToHex(i, 4));
  sl.add('  );');

  sl.add('');
  sl.add('implementation');
  sl.add('end.');

  sl.SaveToFile('regexpr_unicodedata.pas');
  sl.free;

end.
