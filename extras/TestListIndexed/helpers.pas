unit helpers;

interface

uses Vcl.Forms,
  xn.list, xn.list.index;

{$M+}


type
  TMyRecord = record
  private
    fCod: integer;
    procedure CodSet(aCod: integer);
    function CodGet: integer;
  public
    constructor Create(aCod: integer);
    property Cod: integer read CodGet write CodSet;
  end;

  IMyObject = interface
    ['{D7BA6D54-E2D9-492B-81B3-84B737AC4859}']
    procedure CodSet(aCod: integer);
    function CodGet: integer;
    property Cod: integer read CodGet write CodSet;
  end;

  TMyObject = class(TInterfacedObject, IMyObject)
  private
    fCod: integer;
    procedure CodSet(aCod: integer);
    function CodGet: integer;
  published
    constructor Create(aCod: integer);
    property Cod: integer read CodGet write CodSet;
  end;

var
  t0: TDateTime;
  t1: TDateTime;

procedure TimerStart;
procedure TimerStop(aForm: TForm; aCaption: string);

function IntegerComparison(const aLeft, aRight: integer): integer;
function MyRecordComparison(const aLeft, aRight: TMyRecord): integer;
function MyObjectComparison(const aLeft, aRight: IMyObject): integer;

function AsDebug(aList: IxnList<integer>): string; overload;
function AsDebug(aList: IxnList<TMyRecord>): string; overload;
function AsDebug(aList: IxnList<IMyObject>): string; overload;
function AsDebug(aList: IxnListIndex<integer>): string; overload;
function AsDebug(aList: IxnListIndex<TMyRecord>): string; overload;
function AsDebug(aList: IxnListIndex<IMyObject>): string; overload;

implementation

uses System.SysUtils;

// *****************************************************************************
// *****************************************************************************

{ TMyRecord }

function TMyRecord.CodGet: integer;
begin
  Result := fCod
end;

procedure TMyRecord.CodSet(aCod: integer);
begin
  fCod := aCod;
end;

constructor TMyRecord.Create(aCod: integer);
begin
  fCod := aCod;
end;

{ TMyObject }

constructor TMyObject.Create(aCod: integer);
begin
  fCod := aCod;
end;

function TMyObject.CodGet: integer;
begin
  Result := fCod
end;

procedure TMyObject.CodSet(aCod: integer);
begin
  fCod := aCod;
end;

// *****************************************************************************
// *****************************************************************************

procedure TimerStart;
begin
  t0 := Now;
  Application.ProcessMessages;
end;

procedure TimerStop(aForm: TForm; aCaption: string);
begin
  Application.ProcessMessages;
  t1 := Now;
  Application.ProcessMessages;

  aForm.Caption := aCaption + ' elapsed time ' + FormatDateTime('ss:zzz', t1 - t0);
end;

// *****************************************************************************
// *****************************************************************************
function IntegerComparison(const aLeft, aRight: integer): integer;
begin
  Result := aLeft - aRight;
end;

function MyRecordComparison(const aLeft, aRight: TMyRecord): integer;
begin
  Result := aLeft.Cod - aRight.Cod;
end;

function MyObjectComparison(const aLeft, aRight: IMyObject): integer;
begin
  // if aLeft.Cod.ToString = aRight.Cod.ToString then
  // Result := 0
  // else if aLeft.Cod.ToString < aRight.Cod.ToString then
  // Result := -1
  // else
  // Result := +1;

  Result := aLeft.Cod - aRight.Cod;
end;

// *****************************************************************************
// *****************************************************************************
function AsDebug(aList: IxnList<integer>): string; overload;
var
  i: integer;
begin
  Result := '';
  for i in aList do
    Result := Result + i.ToString() + ' ';
end;

function AsDebug(aList: IxnList<TMyRecord>): string; overload;
var
  i: TMyRecord;
begin
  Result := '';
  for i in aList do
    Result := Result + i.Cod.ToString() + ' ';
end;

function AsDebug(aList: IxnList<IMyObject>): string; overload;
var
  i: IMyObject;
begin
  Result := '';
  for i in aList do
    Result := Result + i.Cod.ToString() + ' ';
end;

function AsDebug(aList: IxnListIndex<integer>): string; overload;
var
  i: integer;
begin
  Result := '';
  for i in aList do
    Result := Result + i.ToString() + ' ';
end;

function AsDebug(aList: IxnListIndex<TMyRecord>): string; overload;
var
  i: TMyRecord;
begin
  Result := '';
  for i in aList do
    Result := Result + i.Cod.ToString() + ' ';
end;

function AsDebug(aList: IxnListIndex<IMyObject>): string; overload;
var
  i: IMyObject;
begin
  Result := '';
  for i in aList do
    Result := Result + i.Cod.ToString() + ' ';
end;

end.
