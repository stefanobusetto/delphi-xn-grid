unit xn.dataset.cache;

interface

uses Generics.Collections, System.Classes, System.TypInfo, Vcl.Grids, Data.Db;

{$M+}


type
  TBookmarkHelper = record helper for TBookmark
    function AsString: string;
    function Equals(aBookmark: TBookmark): Boolean;
  end;

  TxnDatasetCache = class
  type
    TxnField = string;

    TxnRecord = class
    strict private
      fBookmark: TBookmark;
      fFields: TList<TxnField>;
      procedure FieldSet(aFieldIndex: integer; aValue: TxnField);
      function FieldGet(aFieldIndex: integer): TxnField;
      procedure FieldsCreate(aFieldCount: integer);
    public
      constructor Create(aFieldCount: integer);
      destructor Destroy; override;
      procedure Load(aDataset: TDataSet);

      property Fields[aFieldIndex: integer]: TxnField read FieldGet write FieldSet;
      property Bookmark: TBookmark read fBookmark write fBookmark;
    end;

    TxnRecords = class(TObjectList<TxnRecord>)
    protected
      function Find(aBookmark: TBookmark): TxnRecord;
    end;

    TxnDataLink = class(TDataLink)
    strict private
      fOwner: TxnDatasetCache;
    strict protected
      procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    public
      constructor Create(aOwner: TxnDatasetCache);
      destructor Destroy; override;
    end;

  strict private
    fGrid: TStringGrid;
    fStrings: TStrings;

    fFieldsCount: integer;

    fLogNo: integer;

    fBookmark: TBookmark;
    fBookmarkPrev: TBookmark;
    fRecNo: integer;
    fRecNoPrev: integer;

    fRecords: TxnRecords;
    fDataLink: TxnDataLink;

    procedure GridSet(const aGrid: TStringGrid);
    function GridGet: TStringGrid;
    procedure StringsSet(const aStrings: TStrings);
    function StringsGet: TStrings;

    procedure FieldSet(aFieldIndex, aRecordIndex: integer; aValue: TxnField);
    function FieldGet(aFieldIndex, aRecordIndex: integer): TxnField;

    function RecordGet(aRecordIndex: integer): TxnRecord;
    procedure RecordsCreate(aFieldCount: integer; aRecordCount: integer);
  strict protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt);
    procedure Log(aMessage: string);
  public
    constructor Create(aDataSource: TDataSource; aFieldCount: integer); overload;
    destructor Destroy; override;

    procedure OnAfterPost(aDataset: TDataSet);
    procedure OnAfterInsert(aDataset: TDataSet);
    procedure OnNewRecord(aDataset: TDataSet);
    procedure Load(aDataset: TDataSet);

    procedure Clear;
    function Append: TxnRecord;

    function Find(aBookmark: TBookmark): TxnRecord;

    function Get(aCol, aRow: integer): string;
    function GetColCount: integer;
    function GetRowCount: integer;

    function FieldsCount: integer;
    function RecordsCount: integer;
    function RecNo: integer;

    procedure PaintIndicator; overload;
    procedure PaintGrid; overload;

    property Fields[aFieldIndex, aRecordIndex: integer]: TxnField read FieldGet write FieldSet;
    property Records[aRecordIndex: integer]: TxnRecord read RecordGet;

  published
    property Strings: TStrings read StringsGet write StringsSet;
    property Grid: TStringGrid read GridGet write GridSet;
  end;

implementation

uses System.SysUtils;

{ Functions }

function EventToString(aEvent: TDataEvent): string;
begin
  Result := GetEnumName(TypeInfo(TDataEvent), integer(aEvent));
end;

function StateToString(aState: TDataSetState): string;
begin
  Result := GetEnumName(TypeInfo(TDataSetState), integer(aState));
end;

{ TBookmarkHelper }

function TBookmarkHelper.AsString: string;
var
  b: byte;
begin
  for b in Self do
  begin
    if Result <> '' then
      Result := Result + ':';
    Result := Result + IntToHex(b, 2);
  end;
end;

function TBookmarkHelper.Equals(aBookmark: TBookmark): Boolean;
begin
  Result := SameText(Self.AsString, aBookmark.AsString);
end;

{ TxnDatasetCache }

function TxnDatasetCache.Append: TxnRecord;
begin
  Result := TxnRecord.Create(FieldsCount());
  fRecords.Add(Result);
end;

procedure TxnDatasetCache.Clear;
begin
  fRecords.Clear;
end;

constructor TxnDatasetCache.Create(aDataSource: TDataSource; aFieldCount: integer);
begin
  fGrid := nil;
  fFieldsCount := aFieldCount;
  fRecords := TxnRecords.Create;
  fDataLink := TxnDataLink.Create(Self);
  fDataLink.DataSource := aDataSource;

  fLogNo := -1;

  fRecNo := -1;
  fRecNoPrev := -1;

  SetLength(fBookmark, 0);
  SetLength(fBookmarkPrev, 0);

end;

destructor TxnDatasetCache.Destroy;
begin
  fDataLink.Free;
  fRecords.Free;
  inherited;
end;

function TxnDatasetCache.FieldsCount: integer;
begin
  Result := fFieldsCount;
end;

function TxnDatasetCache.FieldGet(aFieldIndex, aRecordIndex: integer): TxnField;
begin
  Result := fRecords[aRecordIndex].Fields[aFieldIndex];
end;

procedure TxnDatasetCache.FieldSet(aFieldIndex, aRecordIndex: integer; aValue: TxnField);
begin
  fRecords[aRecordIndex].Fields[aFieldIndex] := aValue;
end;

function TxnDatasetCache.Find(aBookmark: TBookmark): TxnRecord;
begin
  Result := fRecords.Find(aBookmark);
end;

function TxnDatasetCache.Get(aCol, aRow: integer): string;
begin
  if (aCol < 1) or (aCol > FieldsCount) then
    Exit('');
  if (aRow < 1) or (aRow > RecordsCount) then
    Exit('');

  Result := fRecords[aRow - 1].Fields[aCol - 1]
end;

function TxnDatasetCache.GetColCount: integer;
begin
  Result := FieldsCount()
end;

function TxnDatasetCache.GetRowCount: integer;
begin
  Result := RecordsCount()
end;

function TxnDatasetCache.GridGet: TStringGrid;
begin
  Result := fGrid;
end;

procedure TxnDatasetCache.GridSet(const aGrid: TStringGrid);
begin
  fGrid := aGrid;
end;

procedure TxnDatasetCache.Load(aDataset: TDataSet);
begin
  aDataset.First;
  while not aDataset.Eof do
  begin
    Append.Load(aDataset);
    aDataset.Next;
  end;
end;

procedure TxnDatasetCache.Log(aMessage: string);
begin
  Inc(fLogNo);
  if Strings <> nil then
  begin
    Strings.insert(0, format('%4d %s', [fLogNo, aMessage]));

    while Strings.Count > 40 do
      Strings.Delete(Strings.Count - 1);
  end;
end;

procedure TxnDatasetCache.OnAfterInsert(aDataset: TDataSet);
begin
  Log('OnAfterInsert()');
  Log(aDataset.Bookmark.AsString);
end;

procedure TxnDatasetCache.OnAfterPost(aDataset: TDataSet);
var
  r: TxnRecord;
begin
  Log('OnAfterPost()');
  Log(StateToString(aDataset.State));
  Log(aDataset.Bookmark.AsString);

  exit;
  r := Find(aDataset.Bookmark);
  Assert(r <> nil, 'Record not found!');

  r.Load(aDataset);
  PaintGrid();
end;

procedure TxnDatasetCache.OnNewRecord(aDataset: TDataSet);
begin
  Log('OnNewRecord()');
  Log(StateToString(aDataset.State));

end;

procedure TxnDatasetCache.PaintGrid;
var
  c: integer;
  r: integer;
begin
  Log('PaintGrid()');
  fGrid.ColCount := GetColCount + 1;
  fGrid.RowCount := GetRowCount + 1;

  for c := 1 to GetColCount do
    for r := 1 to GetRowCount do
      fGrid.Cells[c, r] := Get(c, r);
end;

procedure TxnDatasetCache.PaintIndicator;
begin
  Log('PaintIndicator()');

  if fGrid = nil then
    Exit
  else if fGrid.RowCount < 1 then
    Exit
  else if fGrid.ColCount < 2 then
    Exit;

  if (fRecNo >= 0) and (fRecNo <= fGrid.RowCount) then
    fGrid.Cells[0, fRecNo] := '-';

  if (fRecNoPrev >= 0) and (fRecNoPrev <= fGrid.RowCount) then
    fGrid.Cells[0, fRecNoPrev] := '';
end;

procedure TxnDatasetCache.DataEvent(Event: TDataEvent; Info: NativeInt);
// var
// f: TField;
begin
  Log(StateToString(fDataLink.dataset.State));

  // f := TField(Info);
  // if f <> nil then
  // begin
  // UpdateCell(f.Index, fDataLink.dataset.RecNo - 1, f.AsString);
  // end;

  if fDataLink.dataset.RecNo <> fRecNo then
  begin
    fRecNoPrev := fRecNo;
    fRecNo := fDataLink.dataset.RecNo;
    PaintIndicator();
  end;
end;

function TxnDatasetCache.RecordsCount: integer;
begin
  Result := fRecords.Count;
end;

function TxnDatasetCache.RecNo: integer;
begin
  Result := fRecNo;
end;

function TxnDatasetCache.RecordGet(aRecordIndex: integer): TxnRecord;
begin
  Result := fRecords[aRecordIndex];
end;

procedure TxnDatasetCache.RecordsCreate(aFieldCount, aRecordCount: integer);
var
  i: integer;
begin
  for i := 0 to aRecordCount - 1 do
    fRecords.Add(TxnRecord.Create(aFieldCount));
end;

function TxnDatasetCache.StringsGet: TStrings;
begin
  Result := fStrings;
end;

procedure TxnDatasetCache.StringsSet(const aStrings: TStrings);
begin
  fStrings := aStrings;
end;

{ TMemTable.TMemRecord }

constructor TxnDatasetCache.TxnRecord.Create(aFieldCount: integer);
begin
  fFields := TList<TxnField>.Create;
  FieldsCreate(aFieldCount);
end;

destructor TxnDatasetCache.TxnRecord.Destroy;
begin
  fFields.Free;
  inherited;
end;

function TxnDatasetCache.TxnRecord.FieldGet(aFieldIndex: integer): TxnField;
begin
  Result := fFields[aFieldIndex];
end;

procedure TxnDatasetCache.TxnRecord.FieldsCreate(aFieldCount: integer);
var
  i: integer;
begin
  for i := 0 to aFieldCount - 1 do
    fFields.Add('');
end;

procedure TxnDatasetCache.TxnRecord.FieldSet(aFieldIndex: integer; aValue: TxnField);
begin
  fFields[aFieldIndex] := aValue
end;

procedure TxnDatasetCache.TxnRecord.Load(aDataset: TDataSet);
var
  i: integer;
begin
  Bookmark := aDataset.GetBookmark;

  for i := 0 to aDataset.Fields.Count - 1 do
    Fields[i] := aDataset.Fields[i].AsString;
end;

{ TxnDatasetCache.TxnDataLink }

constructor TxnDatasetCache.TxnDataLink.Create(aOwner: TxnDatasetCache);
begin
  fOwner := aOwner;
end;

procedure TxnDatasetCache.TxnDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  fOwner.DataEvent(Event, Info);
  inherited;
end;

destructor TxnDatasetCache.TxnDataLink.Destroy;
begin
  inherited;
end;

{ TxnDatasetCache.TxnRecords }

function TxnDatasetCache.TxnRecords.Find(aBookmark: TBookmark): TxnRecord;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Bookmark.Equals(aBookmark) then
      Exit(Items[i]);
  Exit(nil);
end;

end.
