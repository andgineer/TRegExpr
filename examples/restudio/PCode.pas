{$B-}
unit PCode;

{

 View internal pseudo-code of compiled regular expression

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmPseudoCodeViewer = class(TForm)
    Memo1: TMemo;
    edSource: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btnClose: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

implementation
{$R *.DFM}

procedure TfmPseudoCodeViewer.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  Action := caFree;
 end;

end.

