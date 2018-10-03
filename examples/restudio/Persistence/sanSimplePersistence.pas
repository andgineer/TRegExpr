unit sanSimplePersistence;

interface

uses
 classes,
 sanPersistence;

type
 TsanSimpleObjectStorage = class (TsanObjectStorage)
   private
    fList : TList;
   protected
    function GetObject (AID : TsanOID) : isanObject; override;
   public
    constructor Create;
    destructor Destroy; override;

    function AddObject (AObject : isanObject; AObjectClass : TsanClassID = 0) : TsanOID; override;
    procedure DeleteObject (AID : TsanOID); override;
  end;

implementation

uses
 SysUtils; // FreeAndNil

constructor TsanSimpleObjectStorage.Create;
 begin
  inherited;
  fList := TList.Create;
 end;

destructor TsanSimpleObjectStorage.Destroy;
 begin
  FreeAndNil (fList);
  inherited;
 end;

function TsanSimpleObjectStorage.GetObject (AID : TsanOID) : isanObject;
 begin
  Result := TsanObject (fList [AID]);
 end;

function TsanSimpleObjectStorage.AddObject (AObject : isanObject; AObjectClass : TsanClassID = 0) : TsanOID;
 begin
  Result := fList.Add (pointer (AObject));
 end;

procedure TsanSimpleObjectStorage.DeleteObject (AID : TsanOID);
 begin
  fList.Delete (AID);
 end;

end.
