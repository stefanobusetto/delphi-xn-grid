unit xn.grid.sort;

interface

uses Generics.Collections,
  xn.grid.link, cSampleData;

type
  TxnGridSortItem = record
  type
    TKind = (xstStrAsc, xstStrDesc, xstNumAsc, xstNumDesc);
  private
    fItem: integer;
    fKind: TKind;
  public
    constructor Create(aIndex: integer; aKind: TKind);
  end;

  TxnGridSortItems = class(TList<TxnGridSortItem>)
  end;

  TxnGridLinkSort = class(TInterfacedObject, IxnGridData)
  strict private
    fItems: TxnGridSortItems;
    fIndex: TList<integer>;

    function Comparer(const aLeft, aRight: TArray<variant>): integer;
    function Getter(const aIndex: integer): TArray<variant>;

    procedure QuickSort(aLo, aHi: integer); overload;
    procedure QuickSort; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure sort(aItems: TxnGridSortItems); overload;
    function RowCount: LongInt;

    function AsDebug: string;
    function ValueString(aCol, aRow: LongInt): String;
    function ValueFloat(aCol, aRow: LongInt): Double;
  end;

implementation

uses System.SysUtils;

{ TxnGridLinkSort }

function TxnGridLinkSort.AsDebug: string;
var
  r: integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + ValueString(0, r) + ','
end;

function TxnGridLinkSort.RowCount: LongInt;
begin
  Result := TSampleData.Length
end;

procedure TxnGridLinkSort.QuickSort(aLo, aHi: integer);
var
  iLo: integer;
  iHi: integer;
  iPivot: TArray<variant>;

  function Get(aIndex: integer): TArray<variant>;
  begin
    // Result := fGetter(aData[aIndex])
    Result := Getter(aIndex)
  end;

begin
  iLo := aLo;
  iHi := aHi;
  iPivot := Get((iLo + iHi) div 2);
  repeat
    while (Comparer(Get(iLo), iPivot) < 0) do
      Inc(iLo);
    while (Comparer(Get(iHi), iPivot) > 0) do
      Dec(iHi);

    if iLo <= iHi then
    begin
      fIndex.Exchange(iLo, iHi);
      Inc(iLo);
      Dec(iHi);
    end;
  until iLo > iHi;

  if iHi > aLo then
    QuickSort(aLo, iHi);
  if iLo < aHi then
    QuickSort(iLo, aHi);
end;

procedure TxnGridLinkSort.QuickSort;
begin
  QuickSort(0, fIndex.Count - 1);
end;

function TxnGridLinkSort.Comparer(const aLeft, aRight: TArray<variant>): integer;
var
  t: variant;
  l: variant;
  r: variant;
  i: integer;
begin
  for i := Low(aLeft) to High(aLeft) do
  begin
    l := aLeft[i];
    r := aRight[i];

    if (fItems[i].fKind = xstStrDesc) or
      (fItems[i].fKind = xstNumDesc) then
    begin
      t := l;
      l := r;
      r := t;
    end;

    if l < r then
      exit(-1)
    else if l > r then
      exit(+1);
  end;
  exit(0);
end;

constructor TxnGridLinkSort.Create;
var
  i: integer;
begin
  fIndex := TList<integer>.Create;
  for i := 0 to RowCount - 1 do
    fIndex.Add(i);
end;

destructor TxnGridLinkSort.Destroy;
begin
  fIndex.Free;
  inherited;
end;

function TxnGridLinkSort.Getter(const aIndex: integer): TArray<variant>;
var
  i: integer;
begin
  SetLength(Result, fItems.Count);

  for i := 0 to fItems.Count - 1 do
    if (fItems[i].fKind = xstStrAsc) or
      (fItems[i].fKind = xstStrDesc) then
      Result[i] := ValueString(fItems[i].fItem, aIndex)
    else
      Result[i] := ValueFloat(fItems[i].fItem, aIndex)
end;

procedure TxnGridLinkSort.sort(aItems: TxnGridSortItems);
begin
  fItems := aItems;

  if fIndex.Count > 0 then
    if aItems.Count > 0 then
      QuickSort(0, fIndex.Count - 1);
end;

function TxnGridLinkSort.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TxnGridLinkSort.ValueString(aCol, aRow: integer): String;
begin
  Result := TSampleData.Value(aCol, fIndex[aRow]);
end;

{ TxnGridSortItem }

constructor TxnGridSortItem.Create(aIndex: integer; aKind: TKind);
begin
  fItem := aIndex;
  fKind := aKind;
end;

{ TxnSort }

end.
