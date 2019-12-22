unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  RegExpr in '../src/RegExpr.pas';

type
  { TForm1 }

  TForm1 = class(TForm)
    chk_g: TCheckBox;
    chk_s: TCheckBox;
    chk_i: TCheckBox;
    chk_x: TCheckBox;
    chk_r: TCheckBox;
    EditRegex: TEdit;
    EditText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListDump: TListBox;
    ListRes: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure chk_iChange(Sender: TObject);
    procedure EditRegexChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    reg: TRegExpr;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  reg:= TRegExpr.Create;
  EditRegex.Text:= '(\w+) (\d+)';
  EditText.Text:= '.. test 23';
  EditRegex.OnChange(nil);
  chk_i.Checked:= RegExprModifierI;
  chk_g.Checked:= RegExprModifierG;
  chk_s.Checked:= RegExprModifierS;
  chk_r.Checked:= RegExprModifierR;
  chk_x.Checked:= RegExprModifierX;
end;

procedure TForm1.EditRegexChange(Sender: TObject);
var
  i: integer;
begin
  ListRes.Items.Clear;
  ListDump.Items.Clear;

  try
    reg.Expression:= EditRegex.Text;
    reg.ModifierI:= chk_i.Checked;
    reg.ModifierX:= chk_x.Checked;
    reg.ModifierR:= chk_r.Checked;
    reg.ModifierS:= chk_s.Checked;
    reg.ModifierG:= chk_g.Checked;
    ListDump.Items.AddText(reg.Dump);
  except
    on e: Exception do
    begin
      ListDump.Items.Add(e.Message);
      exit;
    end;
  end;

  try
    if EditText.Text='' then exit;
    if reg.Exec(EditText.Text) then
    begin
      ListRes.Items.Add(
        'Found at pos: '+IntToStr(reg.MatchPos[0])+
        '  Match: '+reg.Match[0]);
      for i:= 1 to reg.SubExprMatchCount do
        ListRes.Items.Add(Format('Group %d: %s', [i, reg.Match[i]]));
    end
    else
      ListRes.Items.Add('Cannot find');
  except
    on e: Exception do
      ListRes.Items.Add(e.Message);
  end;
end;

procedure TForm1.chk_iChange(Sender: TObject);
begin
  EditRegexChange(Self);
end;

end.

