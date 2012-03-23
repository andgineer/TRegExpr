unit TRegExprClassMain;

{

 Using TRegExpr class
 You have all power of the library in Your hands.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfmTRegExprClassMain = class(TForm)
    btnExtractEmails: TBitBtn;
    memExtractEmails: TMemo;
    lbxEMailesExtracted: TListBox;
    lblSearchPhoneIn: TLabel;
    memSearchPhoneIn: TMemo;
    Bevel1: TBevel;
    lblSubstituteTemplate: TLabel;
    memSubstituteTemplate: TMemo;
    btnSubstitutePhone: TBitBtn;
    memSubstitutePhoneRes: TMemo;
    procedure btnExtractEmailsClick(Sender: TObject);
    procedure btnSubstitutePhoneClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmTRegExprClassMain: TfmTRegExprClassMain;

implementation

uses RegExpr;
{$R *.DFM}

// This simple function extracts all emails from input string
// and places list of this emails into result string as CSV (comma separated values)
function ExtractEmails (const AInputString : string) : string;

// Note: compilation of r.e. performed during Expression assignment
// take some time, so if You will use this function many times
// it will be useless overhead.
// You can significant optimize this if You will create TRegExpr
// and precompile expression during programm initialization.

 const
  EmailRE = '[_a-zA-Z\d\-\.]+@[_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+)+';
 var
  r : TRegExpr;
 begin
  Result := '';

  r := TRegExpr.Create;
  // Create the object instance.
  // Please, don't forget that - 10% of all 'bug-reports' to me caused 
  // by attempts to use object without creation of it!

  try // ensure memory clean-up
     r.Expression := EmailRE;
     // Assign r.e. source code. It will be compiled when nessesary
     // (for example when Exec called). If there are errors in r.e.
     // run-time execption will be raised during r.e. compilation
     if r.Exec (AInputString) then
      REPEAT
       Result := Result + r.Match [0] + ',';
      UNTIL not r.ExecNext;
    finally r.Free;
   end;
 end;

procedure TfmTRegExprClassMain.btnExtractEmailsClick(Sender: TObject);
 begin
  lbxEMailesExtracted.Items.CommaText := ExtractEmails (memExtractEmails.Text);
 end;

// This simple function extracts phone number and
// parse it into parts (City and Country code, internal number).
// Then it substitutes this parts into template.
function ParsePhone (const AInputString, ATemplate : string) : string;
 const
  IntPhoneRE = '(\+\d *)?(\(\d+\) *)?\d+(-\d*)*';
 var
  r : TRegExpr;
 begin
  r := TRegExpr.Create; // Create object
  try // ensure memory release
     r.Expression := IntPhoneRE;
     // Assign r.e. source code. It will be compiled when nessesary
     // (for example when Exec called). If there are errors in r.e.
     // run-time execption will be raised during r.e. compilation
     if r.Exec (AInputString)
      then Result := r.Substitute (ATemplate)
      else Result := '';
    finally r.Free;
   end;
 end;

procedure TfmTRegExprClassMain.btnSubstitutePhoneClick(Sender: TObject);
 begin
  memSubstitutePhoneRes.Text := ParsePhone (memSearchPhoneIn.Text, memSubstituteTemplate.Text);
 end;

end.
