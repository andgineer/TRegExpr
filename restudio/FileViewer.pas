{$I REStudio_inc.pas}
unit FileViewer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  TfmFileViewer = class(TForm)
    RichEdit1: TMemo;
    pnlBottom: TPanel;
    btnRefresh: TSpeedButton;
    btnClose: TSpeedButton;
    lblStat: TLabel;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    lblFileName: TLabel;
    edFileName: TEdit;
    lblExpression: TLabel;
    memExpression: TMemo;
    lblModifiers: TLabel;
    lblMatchs: TLabel;
    cbMatchs: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cbMatchsChange(Sender: TObject);
  end;

implementation
{$R *.dfm}

uses
 REStudioMain;

procedure TfmFileViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  Action := caFree;
 end;

procedure TfmFileViewer.btnCloseClick(Sender: TObject);
 begin
  Close;
 end;

procedure TfmFileViewer.btnRefreshClick(Sender: TObject);
 begin
  fmREDebuggerMain.HighlightREInFileViewer (Self);
 end;

procedure TfmFileViewer.cbMatchsChange(Sender: TObject);
 begin
  if cbMatchs.ItemIndex < cbMatchs.Items.Count then begin
    RichEdit1.SelStart := integer (cbMatchs.Items.Objects [cbMatchs.ItemIndex]) - 1;
    SendMessage (RichEdit1.Handle, EM_SCROLLCARET, 0, 0);
    RichEdit1.SetFocus;
   end;
 end;

end.

