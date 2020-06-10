unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  StrUtils,
  unicodedata;

{$R *.lfm}

function IsUnicodeWordChar(AChar: WideChar): boolean; inline;
var
  NType: byte;
begin
  if Ord(AChar) >= LOW_SURROGATE_BEGIN then
    Exit(False);
  NType := GetProps(Ord(AChar))^.Category;
  Result := (NType <= UGC_OtherNumber);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  sl: tstringlist;
begin
  sl:= tstringlist.create;

  sl.add('unit RegExpr_UnicodeData;');
  sl.add('interface');
  sl.add('');
  sl.add('const');
  sl.add('  WordDetectArray: packed array[0..$FFFF] of boolean = (');
  for i:= 0 to $FFFF do
    sl.add('    '+BoolToStr(IsUnicodeWordChar(widechar(i)), true)+IfThen(i<$ffff, ',')+' // $'+IntToHex(i, 4));
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
end;

end.

