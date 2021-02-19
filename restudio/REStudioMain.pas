{$I REStudio_inc.pas}
unit REStudioMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
 Main form of
 Visual debugger for regular expressions

 (c) 1999-2004 Andrey V. Sorokin
  Saint Petersburg, Russia
  https://sorokin.engineer/
  andrey@sorokin.engineer
}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
  regexpr, RETestCases
  {$IFDEF UseProfiler}, StopWatch {$ENDIF};

type
  TfmREDebuggerMain = class(TForm)
    btnClose: TBitBtn;
    grpRegExpr: TGroupBox;
    lblWWW: TLabel;
    Bevel1: TBevel;
    PageControl1: TPageControl;
    tabExpression: TTabSheet;
    tabSubstitute: TTabSheet;
    pnlSubstitutionComment: TPanel;
    lblSubstitutionComment: TLabel;
    tabReplace: TTabSheet;
    pnlReplaceComment: TPanel;
    lblReplaceComment: TLabel;
    tabSplit: TTabSheet;
    btnSplit: TBitBtn;
    memSplitResult: TMemo;
    pnlSplitComment: TPanel;
    lblSplitComment: TLabel;
    lblSplitResult: TLabel;
    pnlTopExamples: TPanel;
    pnlReplaceTemplate: TPanel;
    lblReplaceString: TLabel;
    edReplaceString: TMemo;
    Splitter1: TSplitter;
    pnlReplaceResult: TPanel;
    lblReplaceResult: TLabel;
    memReplaceResult: TMemo;
    btnReplace: TBitBtn;
    pnlSubstitutionTemplate: TPanel;
    lblSubstitutionTemplate: TLabel;
    memSubstitutionTemplate: TMemo;
    Splitter2: TSplitter;
    pnlRegExpr: TPanel;
    gbModifiers: TGroupBox;
    chkModifierI: TCheckBox;
    chkModifierR: TCheckBox;
    chkModifierS: TCheckBox;
    chkModifierG: TCheckBox;
    chkModifierM: TCheckBox;
    lblRegExpr: TLabel;
    lblRegExprUnbalancedBrackets: TLabel;
    edRegExpr: TMemo;
    edSubExprs: TLabel;
    cbSubExprs: TComboBox;
    btnViewPCode: TSpeedButton;
    Splitter3: TSplitter;
    pnlSubstitutionResult: TPanel;
    lblSubstitutionResult: TLabel;
    memSubstitutionResult: TMemo;
    pnlInputStrings: TPanel;
    lblInputString: TLabel;
    edInputString: TMemo;
    lblInputStringPos: TLabel;
    edInputStringPos: TEdit;
    btnTestString: TBitBtn;
    btnExecNext: TBitBtn;
    btnFindRegExprInFile: TBitBtn;
    cbSubStrs: TComboBox;
    lblTestResult: TLabel;
    chkModifierX: TCheckBox;
    chkUseSubstitution: TCheckBox;
    btnGetRE: TSpeedButton;
    pnlRepositoryHint: TPanel;
    Label1: TLabel;
    Image1: TImage;
    lblStopWatch: TLabel;
    procedure btnViewPCodeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnTestStringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExecNextClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnSplitClick(Sender: TObject);
    procedure chkModifierIClick(Sender: TObject);
    procedure chkModifierSClick(Sender: TObject);
    procedure chkModifierRClick(Sender: TObject);
    procedure chkModifierGClick(Sender: TObject);
    procedure edRegExprChange(Sender: TObject);
    procedure cbSubExprsClick(Sender: TObject);
    procedure edInputStringClick(Sender: TObject);
    procedure edInputStringKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edInputStringMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edInputStringMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure edRegExprClick(Sender: TObject);
    procedure lblRegExprUnbalancedBracketsDblClick(Sender: TObject);
    procedure edRegExprKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbSubStrsClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure chkModifierMClick(Sender: TObject);
    procedure lblWWWClick(Sender: TObject);
    procedure chkModifierXClick(Sender: TObject);
    procedure btnGetREClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
   private
    r : TRegExpr;
    {$IFDEF UseProfiler}
    sw : TStopWatch;
    {$ENDIF}
    procedure UpdateAndCompileExpression;
    procedure ProtectedCompilation;
    procedure ExecIt (AFindNext : boolean);
    procedure InputStringPosIsChanged;
    procedure RegExprChanged (AShowErrorPos : boolean = false);
    procedure RegExprPosIsChanged;
    procedure SubexprSelected;
    procedure SubStringSelected;
    procedure GoToRegExprHomePage;
    procedure UpdateModifiers;
   protected
    fHelpFolder : string;
    fShowOnce : boolean;
    //procedure AssignTestCase (ARegularExpression : string);
   public
{$IFDEF FILEVIEWER}
    procedure HighlightREInFileViewer (AFileViewer : TfmFileViewer);
{$ENDIF}
    property HelpFolder : string read fHelpFolder;
  end;

var
  fmREDebuggerMain: TfmREDebuggerMain;

implementation
{$R *.dfm}

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
{$ENDIF}
  PCode;

const
 ProductHomePage = 'https://regex.sorokin.engineer';

procedure TfmREDebuggerMain.FormCreate(Sender: TObject);
 begin
  r := TRegExpr.Create;
  {$IFDEF UseProfiler}
  sw := TStopWatch.Create;
  {$ENDIF}

  Caption := Format ('Regular expressions visual debugger (TRegExpr v. %d.%d)',
   [TRegExpr.VersionMajor, TRegExpr.VersionMinor]);
  {$IFDEF UseProfiler}
  if (CPUClockKHz > 0) then begin
    Caption := Caption + ', ' + IntToStr (Round (CPUClockKHz / 1000.0))
    + ' MHz CPU assumed for time measurement';
  end;
  lblStopWatch.Visible := True;
  lblStopWatch.Caption := '';
  {$ENDIF}
  lblWWW.Hint := ProductHomePage;

  lblRegExprUnbalancedBrackets.Caption := '';

  fShowOnce := false;
 end;

procedure TfmREDebuggerMain.FormDestroy(Sender: TObject);
 begin
  {$IFDEF UseProfiler}
  sw.Free;
  {$ENDIF}
  r.Free;
 end;

procedure TfmREDebuggerMain.UpdateModifiers;
 begin
  // show effective values of modifiers
  chkModifierI.Checked := r.ModifierI;
  chkModifierR.Checked := r.ModifierR;
  chkModifierS.Checked := r.ModifierS;
  chkModifierG.Checked := r.ModifierG;
  chkModifierM.Checked := r.ModifierM;
  chkModifierX.Checked := r.ModifierX;
 end;

procedure TfmREDebuggerMain.btnViewPCodeClick(Sender: TObject);
 begin
  UpdateAndCompileExpression;
  with TfmPseudoCodeViewer.Create (Application) do begin
    edSource.Text := r.Expression;
    Memo1.Lines.Text := r.Dump;
    ShowModal;
   end;
 end;

procedure TfmREDebuggerMain.btnCloseClick(Sender: TObject);
 begin
  Close;
 end;

procedure TfmREDebuggerMain.UpdateAndCompileExpression;
 // Syncronize r.Expression with edRegExpr value
 // and force compilation.
 begin
  r.Expression := edRegExpr.Text;
  ProtectedCompilation;
 end;

procedure TfmREDebuggerMain.ProtectedCompilation;
 // Force r.e. [re]compilation, catch exceptions
 // and show error position to user.
 // Exception then reraised.
 begin
  try
    r.Compile;
    except on E:Exception do begin // exception during r.e. compilation or execution
      if E is ERegExpr then
       if (E as ERegExpr).CompilerErrorPos > 0 then begin
         // compilation exception - show place of error
         edRegExpr.SetFocus;
         edRegExpr.SelStart := (E as ERegExpr).CompilerErrorPos - 1;
         edRegExpr.SelLength := 1;
        end;
      raise; // continue exception processing
     end;
   end;
 end;

procedure TfmREDebuggerMain.ExecIt (AFindNext : boolean);
 var
  i : integer;
  res : boolean;
  s : string;
 begin
  try
  if Not aFindNext then
    begin
    r.Expression := edRegExpr.Text;
    {$IFDEF UseProfiler}
    sw.Start;
    {$ENDIF}
    ProtectedCompilation;
    {$IFDEF UseProfiler}
    sw.Stop;
    lblStopWatch.Caption := 'Compile: ' + sw.TimeStr;
    {$ENDIF}
    end;
    {$IFDEF UseProfiler}
    sw.Start;
    {$ENDIF}
    if AFindNext
     then res := r.ExecNext // search next occurence. raise
                            // exception if no Exec call preceded
     else res := r.Exec (edInputString.Text); // search from first position
    {$IFDEF UseProfiler}
    sw.Stop;
    lblStopWatch.Caption := lblStopWatch.Caption
     + ', Exec: ' + sw.TimeStr;
    {$ENDIF}
    if res then begin // r.e. found
       // Show r.e. positions: 0 - whole r.e.,
       // 1 .. SubExprMatchCount - subexpressions.
       lblTestResult.Caption := 'Reg.expr found:';
       lblTestResult.Font.Color := clGreen;

       cbSubStrs.Items.Clear;
       for i := 0 to r.SubExprMatchCount do begin
          s := '$' + IntToStr (i);
          if r.MatchPos [i] > 0
           then s := s + ' [' + IntToStr (r.MatchPos [i]) + ' - '
             + IntToStr (r.MatchPos [i] + r.MatchLen [i] - 1) + ']: '
             + r.Match [i]
           else s := s + ' not found!';
          cbSubStrs.Items.AddObject (s, TObject (r.MatchPos [i]
           + (r.MatchLen [i] ShL 16)));
         end;
       cbSubStrs.Visible := True;
       cbSubStrs.ItemIndex := 0;
       SubStringSelected;

       // Perform substitution - example of fill in template
       memSubstitutionResult.Text :=
        r.Substitute (PChar (memSubstitutionTemplate.Text));
      end
     else begin // r.e. not found
       cbSubStrs.Visible := False;
       lblTestResult.Caption := 'Regexpr. not found in string.';
       lblTestResult.Font.Color := clPurple;
       memSubstitutionResult.Text := 'Substitution is not performed';
      end;
    except on E:Exception do begin // exception during r.e. compilation or execution
      cbSubStrs.Visible := False;
      lblTestResult.Caption := 'Error: "' + E.Message + '"';
      lblTestResult.Font.Color := clRed;
      memSubstitutionResult.Text := 'Substitution is not performed';
     end;
   end;
 end;

procedure TfmREDebuggerMain.btnTestStringClick(Sender: TObject);
 begin
  ExecIt (false);
 end;

procedure TfmREDebuggerMain.btnExecNextClick(Sender: TObject);
 begin
  ExecIt (true);
 end;

{$IFDEF FILEVIEWER}
procedure TfmREDebuggerMain.HighlightREInFileViewer (AFileViewer : TfmFileViewer);
 var
  ExecRes : boolean;
 begin
  with AFileViewer do
  try // exception catcher
    lblMatchs.Visible := False;
    cbMatchs.Visible := False;
    cbMatchs.Items.Clear;

    RichEdit1.SelectAll;
    RichEdit1.Color := clBlack;

    r.Expression := edRegExpr.Text;
    {$IFDEF UseProfiler}
    sw.Start;
    {$ENDIF}
    ProtectedCompilation;
    {$IFDEF UseProfiler}
    sw.Stop;
    lblStat.Caption := 'Compile: ' + sw.TimeStr;
    {$ELSE}
    lblStat.Caption := '';
    {$ENDIF}

    {$IFDEF UseProfiler}
    sw.Start;
    {$ENDIF}
    ExecRes := r.Exec (RichEdit1.Text);
    {$IFDEF UseProfiler}
    sw.Stop;
    {$ENDIF}
    if ExecRes then
      REPEAT
       cbMatchs.Items.AddObject (
        copy (
          copy (r.InputString, r.MatchPos [0], r.MatchLen [0]),
         1, 80),
        TObject (r.MatchPos [0]));
       RichEdit1.SelStart := r.MatchPos [0] - 1;
       RichEdit1.SelLength := r.MatchLen [0];
       RichEdit1.Color := clRed;
       {$IFDEF UseProfiler}
       sw.Start (False);
       {$ENDIF}
       ExecRes := r.ExecNext;
       {$IFDEF UseProfiler}
       sw.Stop;
       {$ENDIF}
      UNTIL not ExecRes;
    RichEdit1.SelLength := 0;
    memExpression.Lines.Text := r.Expression;
    lblModifiers.Caption := r.ModifierStr;
    {$IFDEF UseProfiler}
    lblStat.Caption := lblStat.Caption
     + ', Exec: ' + sw.TimeStr + ', ';
    {$ENDIF}
    lblStat.Caption := lblStat.Caption
     + Format ('%d match(s) found', [cbMatchs.Items.Count]);
    lblMatchs.Visible := True;
    cbMatchs.Visible := True;
    if cbMatchs.Items.Count > 0
     then cbMatchs.ItemIndex := 0;
    cbMatchs.OnChange (nil); 
  except on E:Exception do begin
    lblStat.Caption := E.Message;
    Application.MessageBox (
     PChar ('Operation exception:'#$d#$a#$d#$a + E.Message),
     'Compilation or execution error',
     mb_IconExclamation or mb_Ok);
   end;
  end;
 end;
{$ENDIF}

procedure TfmREDebuggerMain.btnReplaceClick(Sender: TObject);
 begin
  UpdateAndCompileExpression;
  memReplaceResult.Text := r.Replace (edInputString.Text,
   edReplaceString.Text, chkUseSubstitution.Checked); //###0.947
 end;

procedure TfmREDebuggerMain.btnSplitClick(Sender: TObject);
 begin
  UpdateAndCompileExpression;
  memSplitResult.Lines.Clear;
  r.Split (edInputString.Text, memSplitResult.Lines);
 end;

procedure TfmREDebuggerMain.chkModifierIClick(Sender: TObject);
 begin
  r.ModifierI := chkModifierI.Checked;
  // You may use also
  //   r.ModifierStr := 'i';
  // or
  //   r.ModifierStr := '-i';
 end;

procedure TfmREDebuggerMain.chkModifierRClick(Sender: TObject);
 begin
  r.ModifierR := chkModifierR.Checked;
 end;

procedure TfmREDebuggerMain.chkModifierSClick(Sender: TObject);
 begin
  r.ModifierS := chkModifierS.Checked;
 end;

procedure TfmREDebuggerMain.chkModifierGClick(Sender: TObject);
 begin
  r.ModifierG := chkModifierG.Checked;
 end;

procedure TfmREDebuggerMain.chkModifierMClick(Sender: TObject);
 begin
  r.ModifierM := chkModifierM.Checked;
 end;

procedure TfmREDebuggerMain.chkModifierXClick(Sender: TObject);
 begin
  r.ModifierX := chkModifierX.Checked;
 end;

procedure TfmREDebuggerMain.InputStringPosIsChanged;
 begin
  if edInputString.SelLength <= 0
   then edInputStringPos.Text := IntToStr (edInputString.SelStart + 1)
   else edInputStringPos.Text := IntToStr (edInputString.SelStart + 1)
      + ' - ' + IntToStr (edInputString.SelStart + edInputString.SelLength);
 end;

procedure TfmREDebuggerMain.RegExprChanged (AShowErrorPos : boolean = false);
 var
  i : integer;
  n : integer;
  s : string;
 begin
  n := RegExprSubExpressions (edRegExpr.Text, cbSubExprs.Items, False);
  case n of //###0.942
    0: lblRegExprUnbalancedBrackets.Caption := ''; // No errors
   -1: lblRegExprUnbalancedBrackets.Caption := 'Not enough ")"';
    else begin
      if n < 0 then begin
         s := 'No "]" found for "["';
         n := Abs (n) - 1;
        end
       else s := 'Unexpected ")"';
      if AShowErrorPos then begin
         s := s + ' at pos ' + IntToStr (n);
         edRegExpr.SetFocus;
         edRegExpr.SelStart := n - 1;
         edRegExpr.SelLength := 1;
        end
       else s := s + '. Doubleclick here!';
      lblRegExprUnbalancedBrackets.Caption := s;
     end;
   end;
  with cbSubExprs.Items do
   for i := 0 to Count - 1 do
    Strings [i] := '$' + IntToStr (i) + ': ' + Strings [i];

  RegExprPosIsChanged;
 end;

procedure TfmREDebuggerMain.RegExprPosIsChanged;
 var
  i : integer;
  CurrentPos : integer;
  SEStart, SELen : integer;
  MinSEIdx : integer;
  MinSELen : integer;
 begin
  MinSEIdx := -1;
  MinSELen := MaxInt;
  CurrentPos := edRegExpr.SelStart + 1;
  with cbSubExprs.Items do begin
    for i := 0 to Count - 1 do begin
      SEStart := integer (Objects [i]) and $FFFF;
      SELen := (integer (Objects [i]) ShR 16) and $FFFF;
      if (SEStart <= CurrentPos) and (SEStart + SELen > CurrentPos)
        and (MinSELen > SELen)
       then begin
         MinSEIdx := i;
         MinSELen := SELen;
        end;
     end;
    if (MinSEIdx >= 0) and (MinSEIdx < Count)
     then cbSubExprs.ItemIndex := MinSEIdx;
   end;
 end;

procedure TfmREDebuggerMain.SubexprSelected;
 var
  n : integer;
 begin
  if cbSubExprs.ItemIndex < cbSubExprs.Items.Count then begin
    n := integer (cbSubExprs.Items.Objects [cbSubExprs.ItemIndex]);
    edRegExpr.SetFocus;
    edRegExpr.SelStart := n and $FFFF - 1;
    edRegExpr.SelLength := (n ShR 16) and $FFFF;
   end;
 end;

procedure TfmREDebuggerMain.SubStringSelected;
 var
  n : integer;
 begin
  if cbSubStrs.ItemIndex < cbSubStrs.Items.Count then begin
    n := integer (cbSubStrs.Items.Objects [cbSubStrs.ItemIndex]);
    edInputString.SetFocus;
    edInputString.SelStart := n and $FFFF - 1;
    edInputString.SelLength := (n ShR 16) and $FFFF;
    InputStringPosIsChanged;
   end;
 end;

procedure TfmREDebuggerMain.edRegExprChange(Sender: TObject);
 begin
  RegExprChanged;
 end;

procedure TfmREDebuggerMain.cbSubExprsClick(Sender: TObject);
 begin
  SubexprSelected;
 end;

procedure TfmREDebuggerMain.edInputStringClick(Sender: TObject);
 begin
  InputStringPosIsChanged;
 end;

procedure TfmREDebuggerMain.edInputStringKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
 begin
  InputStringPosIsChanged;
 end;

procedure TfmREDebuggerMain.edInputStringMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 begin
  InputStringPosIsChanged;
 end;

procedure TfmREDebuggerMain.edInputStringMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
 begin
  InputStringPosIsChanged;
 end;

procedure TfmREDebuggerMain.edRegExprClick(Sender: TObject);
 begin
  RegExprPosIsChanged;
 end;

procedure TfmREDebuggerMain.lblRegExprUnbalancedBracketsDblClick(Sender: TObject);
 begin
  RegExprChanged (True);
 end;

procedure TfmREDebuggerMain.edRegExprKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
 begin
  RegExprPosIsChanged;
 end;

procedure TfmREDebuggerMain.cbSubStrsClick(Sender: TObject);
 begin
  SubStringSelected;
 end;

procedure TfmREDebuggerMain.GoToRegExprHomePage;
 var
  zFileName, zParams, zDir: array [0 .. MAX_PATH] of Char;
{$IFNDEF FPC}
  URL : String;
{$ENDIF}

 begin
{$IFDEF FPC}
  OpenURL(StrPCopy (zFileName, 'http://' + ProductHomePage)); { *Converted from ShellExecute* }
{$ELSE}
  URL:='http://' + ProductHomePage;
  ShellExecute(0,'OPEN',PChar(URL),Nil,Nil,0); { *Converted from ShellExecute* }
{$ENDIF}
 end;

procedure TfmREDebuggerMain.btnHelpClick(Sender: TObject);
 begin
  if not FileExists (Application.HelpFile) then begin
    case Application.MessageBox (
     PChar ('The help in language You''ve selected is not found'
     + ' at the "' + ExtractFilePath (Application.HelpFile) + '".'#$d#$a#$d#$a
     + 'Press Yes if You want to go to my home page to obtain it,'
     + ' or Press No if it stored in different folder at Your computer,'
     + ' or press Cancel to cancel the action.'),
     PChar ('Cannot find help file "' + Application.HelpFile + '"'),
     MB_YESNOCANCEL or MB_ICONQUESTION) of
      IDYES: begin
        GoToRegExprHomePage;
        EXIT;
       end;
      IDNO:; // just skip it thru windows help system - it will ask user for help file path
      else EXIT; // must be cancel or error in showing of the message box
     end;
   end;
  Application.HelpCommand (HELP_FINDER, 0);
 end;

procedure TfmREDebuggerMain.lblWWWClick(Sender: TObject);
 begin
  GoToRegExprHomePage;
 end;

//procedure TfmREDebuggerMain.AssignTestCase (ARegularExpression : IXMLRegularExpressionType);
// begin
//  if not Assigned (ARegularExpression) then begin
//    Application.MessageBox (
//     'R.e. test case is not loadeded!',
//     'No r.e. is selected', mb_IconExclamation or mb_Ok);
//
//    EXIT;
//
//   end;
//
//  with ARegularExpression do begin
//    edRegExpr.Text := Expression;
//    if TestCase.Count > 0 then
//     with TestCase [0] do begin
//  //    edInputString.Text := (TestCase [0].Subject as ISubjectType).Text;
//      if Substitution.Count > 0
//       then memSubstitutionTemplate.Text := Substitution [0].Template
//       else memSubstitutionTemplate.Text := '';
//      if Replace.Count > 0
//       then edReplaceString.Text := Replace [0].Template
//       else edReplaceString.Text := '';
//      // chkUseSubstitution.Checked := ;
//      r.ModifierStr := Modifiers;
//      UpdateModifiers;
//     end
//     else begin
//       edInputString.Text := '';
//       memSubstitutionTemplate.Text := '';
//       edReplaceString.Text := '';
//      end;
//   end;
// end;

procedure TfmREDebuggerMain.btnGetREClick(Sender: TObject);
 begin
  //with fmTestCases do begin
  //  Caption := 'Select r.e. to load into debugger';
  //  if ShowModal = mrYes
  //   then AssignTestCase (RegularExpression);
  // end;
{
  with fmRETestCasesDlg do begin
    Caption := 'Select r.e. to load into debugger';
    if ShowModal = mrYes
     then AssignTestCase (RE);
   end;
}
 end;

procedure TfmREDebuggerMain.FormShow(Sender: TObject);
 begin
  if fShowOnce
   then EXIT;

  fShowOnce := True;

//  with fmTestCases do
//   if Assigned (RegularExpression)
//     then AssignTestCase (RegularExpression);

  InputStringPosIsChanged;
 end;

end.

