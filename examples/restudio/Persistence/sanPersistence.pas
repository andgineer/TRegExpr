unit sanPersistence;
{

 Abstract persistent object classes
 v. 0.01

 (c) 2002 Andrey V. Sorokin
 St-Petersburg, Russia
 anso@mail.ru, anso@paycash.ru
 http://anso.virtualave.net

}

interface

type
 TsanOID = integer;
 TsanClassID = byte;

 TsanObject = class;
 isanObject = interface;

 TsanObjectStorage = class
   protected
    function GetObject (AID : TsanOID) : isanObject; virtual; ABSTRACT;
   public
    function AddObject (AObject : isanObject; AObjectClass : TsanClassID = 0) : TsanOID; virtual; ABSTRACT;
    procedure DeleteObject (AID : TsanOID); virtual; ABSTRACT;

    property Objects [AID : TsanOID] : isanObject read GetObject;
     DEFAULT;
    // returns object by object ID
  end;

 isanObject = interface (IUnknown)
   procedure Modify;
   // Start modification.
  end;

 TsanObject = class (TObject, isanObject)
   protected
    fTieCount: Integer;
    fStorage : TsanObjectStorage;
    fID : TsanOID;
   private
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
   public
    constructor Create (AStorage : TsanObjectStorage; AID : TsanOID); virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property TieCount: Integer read fTieCount;

    procedure Modify;
  end;

implementation

 uses
  Windows,
  SysUtils,
  Forms;



{============================================================}
{====================== TabObject ===========================}

constructor TsanObject.Create (AStorage : TsanObjectStorage; AID : TsanOID);
 begin
  inherited Create;
  fStorage := AStorage;
  fID := AID;
 end; { of constructor TsanObject.Create
--------------------------------------------------------------}

destructor TsanObject.Destroy;
 begin
  inherited;
 end; { of destructor TsanObject.Destroy
--------------------------------------------------------------}

procedure TsanObject.AfterConstruction;
 begin
  // Release the constructor's implicit refcount
  InterlockedDecrement (fTieCount);
 end; { of procedure TsanObject.AfterConstruction
--------------------------------------------------------------}

procedure TsanObject.BeforeDestruction;
 begin
  if TieCount <> 0
   then raise Exception.Create ('Destruction of untied TsanObject');
 end; { of procedure TsanObject.BeforeDestruction
--------------------------------------------------------------}

// Set an implicit refcount so that tiecounting
// during construction won't destroy the object.
class function TsanObject.NewInstance: TObject;
 begin
  Result := inherited NewInstance;
  TsanObject (Result).fTieCount := 1;
 end; { of function TsanObject.NewInstance
--------------------------------------------------------------}

function TsanObject.QueryInterface (const IID: TGUID; out Obj): HResult;
 const
  E_NOINTERFACE = HResult($80004002);
 begin
  if GetInterface (IID, Obj)
   then Result := 0
   else Result := E_NOINTERFACE;
 end; { of function TsanObject.QueryInterface
--------------------------------------------------------------}

function TsanObject._AddRef: Integer;
 begin
  Result := InterlockedIncrement (fTieCount);
 end; { of function TsanObject._AddRef
--------------------------------------------------------------}

function TsanObject._Release: Integer;
 begin
  Result := InterlockedDecrement (fTieCount);
  if Result = 0
   then Application.MessageBox ('', 'TsanObject.TieCount=0', 0);
//   then Destroy;
 end; { of function TsanObject._Release
--------------------------------------------------------------}



(*
Collection
 is_emty, is_ordered, allows_duplicates
 contains_element (e)
 insert_element (e)
 remove_element (e)
 remove_all
 create_iterator
  begin, end - spec.iterators
 select_element (OQL) : e
 select (OQL) : e
 query (Collection, OQL) : errcode
Collection-List
 retrive_first_element
 retrive_last_element
 retrive_element_at (pos) == op[]
 find_element (e, var pos) : bool
 remove_element_at
 replace_element_at
 insert_element (to end)
 insert_element_first
 insert_element_last
 insert_element_after (e, pos)
 insert_element_before (e, pos)
 concat


Iterator
 reset (first)
 advance, not_done, get_element
 next (element) : bool - while next (e) do with e
 op *
*)

end.
