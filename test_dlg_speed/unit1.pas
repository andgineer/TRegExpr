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
    SText: string;
    procedure DoMsg(const S: string);
    procedure Test_EC(const Subj: Unicodestring; AWithLog: boolean);
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
  Rules: array[0..7] of string = (
    '//.*',
    '(?s)\{.*?\}',
    '\d+(\.\d+)?',
    '\w+',
    '\#\$[0-9a-fA-F]+',
    '\#[0-9]+',
    '[\-\+/\*\[\]\(\)\.,:=]+',
    '''.*?'''
    );

procedure TForm1.Test_EC(const Subj: Unicodestring; AWithLog: boolean);
var
  Obj: array[0..Length(Rules)-1] of TRegExpr;
  NPos, NLen: integer;
  bRuleFound, bLastFound: boolean;
  IndexRule, TokenNum, i: integer;
  ch: Widechar;
begin
  for i:= 0 to Length(Rules)-1 do
  begin
    Obj[i]:= TRegExpr.Create;
    Obj[i].Expression:= Rules[i];
    Obj[i].InputString:= Subj;
    Obj[i].ModifierI:= false;
    Obj[i].ModifierS:= false; //don't catch all text by .*
    Obj[i].ModifierM:= true; //allow to work with ^$
    Obj[i].ModifierX:= false; //don't ingore spaces
  end;

  NPos:= 1;
  NLen:= 1;
  bLastFound:= false;
  TokenNum:= 0;

  repeat
    if NPos>Length(Subj) then Break;
    bRuleFound:= false;

    ch:= Subj[NPos];
    if ((ch<>' ') and (ch<>#9)) then
      for IndexRule:= 0 to Length(Rules)-1 do
      begin
        if Obj[IndexRule].ExecPos(NPos, true) then
        begin
          //Assert(Obj[IndexRule].MatchPos[0]=NPos, 'Strange MatchPos');
          NLen:= Obj[IndexRule].MatchLen[0];
          bRuleFound:= true;

          if AWithLog then
          begin
            Inc(TokenNum);
            DoMsg('['+IntToStr(TokenNum)+'] '+Obj[IndexRule].Match[0]);
          end;

          Break;
        end;
      end;

    if not bRuleFound then
    begin
      Inc(NPos);
    end
    else
    begin
      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;

  for i:= 0 to Length(Rules)-1 do
    Obj[i].Free;
end;


{ TForm1 }

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
  Test_EC(UTF8Decode(SText), false);
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
  Test_EC(UTF8Decode(SText), true);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFileDir(ExtractFileDir(Application.ExeName))+
    DirectorySeparator+'src'+DirectorySeparator+'RegExpr.pas';

  //fn:= ExtractFilePath(Application.ExeName)+DirectorySeparator+'unit1.pas';

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
begin
  L:= TStringList.Create;
  L.LoadFromFile(fn);
  SText:= L.Text;
  L.Free;

  ListBox1.Items.Add('Test file: '+fn);
  ListBox1.Items.Add('Length: '+IntToStr(Length(SText)));
end;

end.

