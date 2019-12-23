unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonFile: TButton;
    ButtonFindAll: TButton;
    ButtonSpeed: TButton;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure ButtonFileClick(Sender: TObject);
    procedure ButtonFindAllClick(Sender: TObject);
    procedure ButtonSpeedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    TestStr: UnicodeString;
    procedure DoMsg(const S: string);
    procedure DoTest(const Subj: Unicodestring; AWithLog: boolean);
    procedure UseFile(const fn: string);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  RegExpr in '../src/RegExpr.pas';

{$R *.lfm}

const
  Rules: array[0..9] of string = (
    '//.*',
    '(?s)\{.*?\}',
    '(?s)\(\*.*?\*\)',
    '\d+(\.\d+)?([eE][\-\+]?\d+)?',
    '\w+',
    '\#\$[0-9a-fA-F]+',
    '\$[0-9a-fA-F]+',
    '\#[0-9]+',
    '[\-\+/\*\[\]\(\)\.,:;=<>@\^]+',
    '''.*?'''
    );

function _IsSpace(ch: WideChar): boolean; inline;
begin
  case ch of
    ' ', #9, #10, #13:
      Result:= true
    else
      Result:= false;
  end;
end;

procedure TForm1.DoTest(const Subj: Unicodestring; AWithLog: boolean);
var
  Obj: array[0..Length(Rules)-1] of TRegExpr;
  NPos, NLen: integer;
  IndexRule, TokenNum, i: integer;
  ch: WideChar;
begin
  for i:= 0 to Length(Rules)-1 do
  begin
    Obj[i]:= TRegExpr.Create;
    Obj[i].Expression:= Rules[i];
    Obj[i].ModifierI:= false;
    Obj[i].ModifierS:= false; //don't catch all text by .*
    Obj[i].ModifierM:= true; //allow to work with ^$
    Obj[i].ModifierX:= false; //don't ingore spaces
    Obj[i].Compile;
    Obj[i].InputString:= Subj;
  end;

  NPos:= 1;
  NLen:= 1;
  TokenNum:= 0;

  repeat
    if NPos>Length(Subj) then Break;
    NLen:= 1;

    ch:= Subj[NPos];
    if not _IsSpace(ch) then
      for IndexRule:= 0 to Length(Rules)-1 do
      begin
        if Obj[IndexRule].ExecPos(NPos, true) then
        begin
          NLen:= Obj[IndexRule].MatchLen[0];

          if AWithLog then
          begin
            Inc(TokenNum);
            DoMsg('['+IntToStr(TokenNum)+'] '+Obj[IndexRule].Match[0]);
          end;

          Break;
        end;
      end;

    Inc(NPos, NLen);
  until false;

  for i:= 0 to Length(Rules)-1 do
    Obj[i].Free;
end;

procedure TForm1.DoMsg(const S: string);
begin
  Listbox1.Items.Add(S);
  Listbox1.ItemIndex:= Listbox1.Items.Count-1;
end;

procedure TForm1.ButtonSpeedClick(Sender: TObject);
var
  t: QWord;
begin
  t:= GetTickCount64;
  DoTest(TestStr, false);
  t:= GetTickCount64-t;
  DoMsg(Format('Parsing by TRegExpr: %d ms', [t]));
end;

procedure TForm1.ButtonFileClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      UseFile(FileName);
end;

procedure TForm1.ButtonFindAllClick(Sender: TObject);
begin
  DoTest(TestStr, true);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFileDir(ExtractFileDir(Application.ExeName))+
    DirectorySeparator+'src'+DirectorySeparator+'RegExpr.pas';

  if not FileExists(fn) then
  begin
    Listbox1.Items.Add('Cannot find sample file: '+fn);
    exit;
  end;
  UseFile(fn);
end;

procedure TForm1.UseFile(const fn: string);
var
  L: TStringList;
  i: integer;
begin
  L:= TStringList.Create;
  L.LoadFromFile(fn);
  TestStr:= UTF8Decode(L.Text);
  L.Free;

  //remove non ascii chars
  for i:= 1 to Length(TestStr) do
    if Ord(TestStr[i])>$FF then
      TestStr[i]:= '?';

  ListBox1.Items.Add('Test file: '+fn);
  ListBox1.Items.Add('Length: '+IntToStr(Length(TestStr)));
end;

end.

