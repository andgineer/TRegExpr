unit AblePersistencedTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.DFM}

uses
 IniFiles,
 ansoStrings,
 tynList, ansoRTTIHook, RETestCases;

type


 TRETestCase1 = class (TtynListItem)
   protected
    fInputStringBodyRepN : integer;
    fInputFileName : string;
   published
    property InputStringBodyRepN : integer read fInputStringBodyRepN write fInputStringBodyRepN;
    property InputFileName : string read fInputFileName write fInputFileName;
  end;

 TRETestCaseList = class (TtynList)
   protected
    function GetItems (AIndex: integer) : TRETestCase1;
    procedure SetItems (AIndex: integer; AItem : TRETestCase1);
   public
    property Items [AIndex: integer]: TRETestCase1
     read GetItems write SetItems; default;
  end;

function TRETestCaseList.GetItems (AIndex: integer) : TRETestCase1;
 begin
  Result := inherited Items [AIndex] as TRETestCase1;
 end;

procedure TRETestCaseList.SetItems (AIndex: integer; AItem : TRETestCase1);
 begin
  inherited Items [AIndex] := AItem;
 end;


procedure TForm1.FormCreate(Sender: TObject);
 var
  TheList : TRETestCaseList;
  TheCase : TRETestCase1;
  i : integer;
  RTTIHook : TansoRTTIHook;
  f : TIniFile;
  s, n, v : string;
  Cases : TRETestCases;
  ACase : TRETestCase;
 begin
  ACase := TRETestCase.Create;
  Cases := TRETestCases.Create;
  TheList := TRETestCaseList.Create;

//  TheList.StorageIDPrefix := '_';
{
  TheCase := TRETestCase1.Create;
  with TheCase do begin
    InputStringBodyRepN := 3;
    InputFileName := 'TheInputFileName';
   end;
  TheList.Add (TheCase);
  TheCase := TRETestCase1.Create;
  with TheCase do begin
    InputStringBodyRepN := 77;
    InputFileName := 'AnotherInputFileName';
   end;
  TheList.Add (TheCase);
  TheList.SaveToFile ('c:\1.str');
}

  Cases.LoadFromFile ('D:\Andrey\RegExpr\Demo\DemoREs.txt');

  TheList.LoadFromFile ('c:\1.str');
  for i := 0 to TheList.Count - 1 do
   with TheList [i] do
    ListBox1.Items.Add (Format ('RepN=%d, File:%s',
     [InputStringBodyRepN, InputFileName]));

  RTTIHook := TansoRTTIHook.Create (Cases [0]);
  for i := 0 to RTTIHook.Count - 1 do
   ListBox1.Items.Add (RTTIHook.Names [i] + '=' + RTTIHook.AsStrings [i]);
{
  f := TIniFile.Create ('c:\1.str');
  ListBox1.Items.Add ('-=-');
  ListBox1.Items.Add (f.ReadString ('Ugly', 'Ugly', ''));
//  f.WriteString ('Ugly', 'Ugly', '!!!'#$d#$a'???');
  s := 'efbgebg=34564536';
  ParseProfileString (s, n, v);
  ListBox1.Items.Add (Format ('%s: >%s<=>%s<', [s,n,v]));
  s := 'efbgebg = "34564536;"    ';
  ParseProfileString (s, n, v);
  ListBox1.Items.Add (Format ('%s: >%s<=>%s<', [s,n,v]));
  f.Free;
}  
 end;

initialization
 RegisterItemClass (TRETestCase1);
end.
