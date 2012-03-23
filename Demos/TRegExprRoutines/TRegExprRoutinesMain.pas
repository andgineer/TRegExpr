unit TRegExprRoutinesMain;

{
  TRegExpr Demo-project

  Using TRegExpr unit global routins:
   -=- Add RegExpr.pas into Your project files list (or just
    place this file in the same directory as other files
    of Your project):
      Delphi Main Menu -> Project -> Add to project..
   -=- Add 'RegExpr' into 'uses' of the unit where You want
    to use the functions:
      Delphi Main Menu -> File -> Use Unit..
   -=- Just use the functions, ExecRegExpr for example (see
    full list in 'TRegExpr interface' topic of TRegExpr help).

  Note:
   That's simple but not very flexible or effective way

}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmTRegExprRoutines = class(TForm)
    grpSearchOrValidate: TGroupBox;
    lblPhone: TLabel;
    edPhone: TComboBox;
    lblValidatePhoneRes: TLabel;
    btnValidatePhone: TBitBtn;
    edTextWithPhone: TComboBox;
    lblTextWithPhone: TLabel;
    lblSearchPhoneRes: TLabel;
    btnSearchPhone: TBitBtn;
    grpReplace: TGroupBox;
    lblSearchIn: TLabel;
    memSearchIn: TMemo;
    lblReplaceWith: TLabel;
    btnReplace: TBitBtn;
    edReplaceWith: TEdit;
    lblSearchFor: TLabel;
    edSearchFor: TEdit;
    memReplaceRes: TMemo;
    procedure btnSearchPhoneClick(Sender: TObject);
    procedure btnValidatePhoneClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmTRegExprRoutines: TfmTRegExprRoutines;

implementation

uses RegExpr;

{$R *.DFM}

procedure TfmTRegExprRoutines.btnSearchPhoneClick(Sender: TObject);
 begin
  if ExecRegExpr ('\d{3}-(\d{2}-\d{2}|\d{4})', edTextWithPhone.Text) then begin
     // This regular expression only check if phone number
     // exists somewhere inside edTextWithPhone string
     lblSearchPhoneRes.Font.Color := clBlue;
     lblSearchPhoneRes.Caption := 'Phone number found';
     lblSearchPhoneRes.Visible := True;
    end
   else begin // Error in test
     lblSearchPhoneRes.Font.Color := clRed;
     lblSearchPhoneRes.Caption := 'There is no phone number in the Phone field';
     lblSearchPhoneRes.Visible := True;
     edPhone.SetFocus;
    end;
 end;

procedure TfmTRegExprRoutines.btnValidatePhoneClick(Sender: TObject);
 begin
  if ExecRegExpr ('^\s*\d{3}-(\d{2}-\d{2}|\d{4})\s*$', edPhone.Text) then begin
     // This regular expression ensure that input string IS
     // phone number. Note 'space-skippers' (\s*) in beginning
     // and end - user very often inser spaces occasionally,
     // so is' wise just to skip them and do not bother user
     // with 'check input field, may be You enter invisible spaces'.
     lblValidatePhoneRes.Font.Color := clBlue;
     lblValidatePhoneRes.Caption := 'Phone number is Ok';
     lblValidatePhoneRes.Visible := True;
    end
   else begin // Error in test
     lblValidatePhoneRes.Font.Color := clRed;
     lblValidatePhoneRes.Caption := 'Error in the Phone field';
     lblValidatePhoneRes.Visible := True;
     edPhone.SetFocus;
    end;
 end;

procedure TfmTRegExprRoutines.btnReplaceClick(Sender: TObject);
 begin
  memReplaceRes.Text := ReplaceRegExpr (edSearchFor.Text, memSearchIn.Text, edReplaceWith.Text);
 end;

end.
