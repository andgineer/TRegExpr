{$B-}
unit RETestCasesDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, RETestCases, StdCtrls, Buttons;

type
  TfmRETestCasesDlg = class(TForm)
    pnlRight: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlClient: TPanel;
    pnlBottom: TPanel;
    memDetailes: TMemo;
    grdREs: TDrawGrid;
    Splitter1: TSplitter;
    btnLoad: TBitBtn;
    LoadDlg: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grdREsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdREsClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure grdREsDblClick(Sender: TObject);
   private
    procedure FocusMoved;
   public
    REs : TRETestCases;
    RE : TRETestCase;
    procedure LoadREs (const AFileName : string);
  end;

var
  fmRETestCasesDlg: TfmRETestCasesDlg;

implementation
{$R *.DFM}

procedure TfmRETestCasesDlg.LoadREs (const AFileName : string);
 begin
  try
    REs.LoadFromFile (AFileName);
    except on E:Exception do
     Application.MessageBox (
      PChar ('Cannot load r.e. repository from the file'#$d#$a
       + ' "' + AFileName + '"'#$d#$a#$d#$a
       + 'Error detailes:'#$d#$a + E.Message),
      'Loading error', mb_IconExclamation or mb_Ok);
   end;
  grdREs.RowCount := REs.Count + 1;
  if grdREs.RowCount > 1
   then grdREs.FixedRows := 1;
  grdREs.ColCount := 2;
  grdREs.ColWidths [0] := 72;
  grdREs.ColWidths [1] := grdREs.Width - 10 - grdREs.ColWidths [0];

  FocusMoved; // Update detailes etc

 end;

procedure TfmRETestCasesDlg.FormCreate(Sender: TObject);
 begin
  REs := TRETestCases.Create;
  LoadREs (ExtractFilePath (Application.ExeName) + 'DemoREs.txt');
 end;

procedure TfmRETestCasesDlg.FormDestroy(Sender: TObject);
 begin
  REs.Free;
 end;

procedure TfmRETestCasesDlg.grdREsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
 var
  s : string;
 begin
  s := '???';
  if ARow = 0 then  { Header}
    case ACol of
      0: s := 'Name';
      1: s := 'Description';
     end
   else if ARow - 1 < REs.Count then begin
     case ACol of
       0: s := REs [ARow - 1].Name;
       1: s := REs [ARow - 1].Description;
      end;
    end;
  with Sender as TDrawGrid do begin
    Canvas.FillRect (Rect);
    Canvas.TextRect (Rect, Rect.Left, Rect.Top, s);
   end;
 end;

procedure TfmRETestCasesDlg.FocusMoved;
 var
  Row : integer;
 begin
  Row := grdREs.Row;
  if (Row > 0) and (Row - 1 < REs.Count) then begin
     RE := REs [Row - 1];
     memDetailes.Text := RE.Expression;
    end
   else begin
     RE := nil;
     memDetailes.Text := '';
    end;
 end;

procedure TfmRETestCasesDlg.grdREsClick(Sender: TObject);
 begin
  FocusMoved;
 end;

procedure TfmRETestCasesDlg.btnLoadClick(Sender: TObject);
 begin
  if LoadDlg.Execute then begin
    LoadREs (LoadDlg.FileName);
   end;
 end;

procedure TfmRETestCasesDlg.grdREsDblClick(Sender: TObject);
 begin
  ModalResult := mrYes;
 end;

end.

