{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
unit RETestCasesFm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, RETestCases, StdCtrls, Buttons,
  XMLIntf, XMLDoc,
  RegularExpressionsXMLBind, ExtCtrls;

type
  TfmTestCases = class(TForm)
    pnlBottom: TPanel;
    Panel1: TPanel;
    grbDetails: TGroupBox;
    lblREName: TLabel;
    lblRE: TLabel;
    edREName: TEdit;
    memRE: TMemo;
    grbTestCases: TGroupBox;
    lblModifiers: TLabel;
    edModifiers: TEdit;
    btnSelect: TBitBtn;
    BitBtn2: TBitBtn;
    Panel2: TPanel;
    grbREList: TGroupBox;
    tvCases: TTreeView;
    Splitter1: TSplitter;
    btnSave: TBitBtn;
    lblSubject: TLabel;
    lblDescription: TLabel;
    edDescription: TEdit;
    edComment: TEdit;
    lblComment: TLabel;
    memSubject: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure tvCasesDblClick(Sender: TObject);
    procedure tvCasesChange(Sender: TObject; Node: TTreeNode);
    procedure edRENameChange(Sender: TObject);
    procedure tvCasesChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure btnSaveClick(Sender: TObject);
   private
    isChanged : boolean;
    XMLDocument : IXMLDocument;
    procedure RefreshDetailes;
    function SaveExpression : boolean;
   public
    RegularExpressions : IXMLRegularExpressionsType;
    RegularExpression : IXMLRegularExpressionType;
    procedure LoadREs (const AFileName : string);
  end;

var
  fmTestCases: TfmTestCases;

implementation
{$R *.DFM}

function MakeTestSubject (ATestSubject : IXMLSubjectType) : WideString;
 var
  i, subjIdx, repCount : integer;
  Stream : TFileStream;
  substring : WideStirng;
 begin
  Result := '';
  for subjIdx := 0 to ATestSubject.Count - 1 do begin
    if ATestSubject.Substring [subjIdx].RepeatCount > 0
     then repCount := ATestSubject.Substring [subjIdx].RepeatCount
     else repCount := 1;
    if (length (ATestSubject.FileName) > 0) then begin
       if ATestSubject.Count > 0 then begin
         Application.MessageBox (
          PChar ('Test case subject specifies both file name'#$d#$a
           + '("' + ATestSubject.FileName + '")'#$d#$a' and subject substrings!'),
          'Test subject format error', mb_IconExclamation or mb_Ok);
        end;
       Stream := TFileStream.Create (ATestSubject.FileName, fmOpenRead or fmShareDenyWrite);
       try
          if Stream.Size > 0 then begin
             SetString (substring, nil, Stream.Size);
             Stream.Read (PChar (substring)^, Stream.Size);
            end
           else substring := '';
         finally Stream.Free;
        end;
      end
     else substring := ATestSubject.Substring [subjIdx].Text;

    for i := 1 to repCount do
     Result := Result + substring;
   end;
 end;

procedure TfmTestCases.LoadREs (const AFileName : string);
 var
  i : integer;
 begin
  try
     XMLDocument.LoadFromFile (AFileName);
    except on E:Exception do
     Application.MessageBox (
      PChar ('Cannot load r.e. repository from the file'#$d#$a
       + ' "' + AFileName + '"'#$d#$a#$d#$a
       + 'Error detailes:'#$d#$a + E.Message),
      'Loading error', mb_IconExclamation or mb_Ok);
   end;

  RegularExpressions := XMLDocument.GetDocBinding ('regularExpressions', TXMLRegularExpressionsType,
   'http:/'+'/www.RegExpStudio.com/re') as IXMLRegularExpressionsType;

  for i := 0 to RegularExpressions.Count - 1 do begin
    tvCases.Items.Add (nil, RegularExpressions [i].Name);
//  for i := 0 to REs.Count - 1 do begin
//    tvCases.Items.Add (nil, REs [i].Name);
{
    RegularExpressions.Add;
    RegularExpressions [i].Name := REs [i].Name;
    // REs [i].InputString
    RegularExpressions [i].Category.Add (REs [i].Group);
    RegularExpressions [i].Comment := REs [i].Comment;
    RegularExpressions [i].Description := REs [i].Description;
    RegularExpressions [i].Expression := REs [i].Expression;

    RegularExpressions [i].TestCase.Add;
    with RegularExpressions [i].TestCase [0] do begin
      if REs [i].InputStringBeg <> '' then begin
        Subject.Add;
        Subject [Subject.Count - 1].Text := REs [i].InputStringBeg;
       end;
      if REs [i].InputStringBody <> '' then begin
        Subject.Add;
        Subject [Subject.Count - 1].Text := REs [i].InputStringBody;
        if REs [i].InputStringBodyRepN > 0
         then Subject [Subject.Count - 1].RepeatCount := REs [i].InputStringBodyRepN;
       end;
      if REs [i].InputStringEnd <> '' then begin
        Subject.Add;
        Subject [Subject.Count - 1].Text := REs [i].InputStringEnd;
       end;
      Modifiers := REs [i].ModifierStr;
      if REs [i].SubstitutionTemplate <> '' then begin
        Substitution.Add;
        Substitution [0].Template := REs [i].SubstitutionTemplate;
       end;
      if REs [i].String4Replace <> '' then begin
        Replace.Add;
        Replace [0].Template := REs [i].String4Replace;
       end;
//    REs [i].MatchList
//    REs [i].SubExprMatchCount
//    REs [i].MatchPos
//    REs [i].MatchLen
     end;
}
   end;

//  XMLDocument.SaveToFile (ChangeFileExt (AFileName, '.xml'));

  RefreshDetailes;

 end;

procedure TfmTestCases.RefreshDetailes;
 begin
  if not Assigned (tvCases.Selected) and (tvCases.Items.Count > 0)
   then tvCases.Selected := tvCases.Items [0];
  if Assigned (tvCases.Selected) then begin
     RegularExpression := RegularExpressions [tvCases.Selected.Index];

     grbDetails.Caption := Format (' %s ', [RegularExpression.Name]);
     edREName.Text := RegularExpression.Name;
     memRE.Text := RegularExpression.Expression;
     edDescription.Text := RegularExpression.Description;
     edComment.Text := RegularExpression.Comment;
     if RegularExpression.TestCase.Count > 0 then
      with RegularExpression.TestCase [0] do begin
        edModifiers.Text := Modifiers;
        memSubject.Text := '';

{       if Substitution.Count > 0
        then memSubstitutionTemplate.Text := Substitution [0].Template
        else memSubstitutionTemplate.Text := '';
       if Replace.Count > 0
        then edReplaceString.Text := Replace [0].Template
        else edReplaceString.Text := '';
}
       end
      else begin
//       edInputString.Text := '';
//       memSubstitutionTemplate.Text := '';
//       edReplaceString.Text := '';
      end;
    end
   else begin
     grbDetails.Caption := '';
     edREName.Text := '';
     memRE.Text := '';
     edDescription.Text := '';
     edComment.Text := '';
     memSubject.Text := '';
    end;

  isChanged := False;

 end;

procedure TfmTestCases.FormCreate(Sender: TObject);
 begin
  isChanged := False;
  XMLDocument := TXMLDocument.Create (nil);
  LoadREs (ExtractFilePath (Application.ExeName) + 'DemoREs.xml');
 end;

procedure TfmTestCases.tvCasesDblClick(Sender: TObject);
 begin
  RegularExpression := RegularExpressions [tvCases.Selected.Index];
  ModalResult := mrYes;
 end;

procedure TfmTestCases.tvCasesChange(Sender: TObject; Node: TTreeNode);
 begin
  RefreshDetailes;
 end;

procedure TfmTestCases.edRENameChange(Sender: TObject);
 begin
  isChanged := True;
 end;

function TfmTestCases.SaveExpression : boolean;
 begin
  Result := True;
 end;

procedure TfmTestCases.tvCasesChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
 begin
  if isChanged then begin

    case Application.MessageBox (
     PChar ('Current expression was changed.'#$a#$d#$a#$d'Save the changes?'),
     PChar ('Expression change'),
     MB_YESNOCANCEL or MB_ICONQUESTION) of
      IDYES: AllowChange := SaveExpression;
      IDNO: ;
      else AllowChange := False;
     end;

   end;
 end;

procedure TfmTestCases.btnSaveClick(Sender: TObject);
 begin
  SaveExpression;
 end;

end.

