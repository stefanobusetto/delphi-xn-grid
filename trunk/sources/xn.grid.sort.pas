unit xn.grid.sort;

interface

uses Generics.Collections, xn.grid.link;

type
  TxnGridSortItem = record
  type
    TKind = (gskStr, gskNum);
    TOrder = (gsoAsc, gsoDesc);
  private
    fIndex: integer;
    fKind: TKind;
    fOrder: TOrder;
  public
    constructor Create(aIndex: integer; aKind: TKind; aOrder: TOrder);
  end;

  TxnGridSortItems = class(TList<TxnGridSortItem>)
  end;

  TxnGridFilterItem = record
  type
    TCriteria = (gfcEquals, gfcStartsWith, gfcEndsWith);
    TCase = (gfcCaseSensitive, gfcCaseInsensitive);
  private
    fIndex: integer;
    fCriteria: TCriteria;
    fCase: TCase;
  public
    constructor Create(aIndex: integer; aCriteria: TCriteria; aCase: TCase);
  end;

  TxnGridFilterItems = class(TList<TxnGridFilterItem>)
  end;

  TxnGridLinkSort = class(TInterfacedObject, IxnGridData)
  strict private
    fGridData: IxnGridData;

    fSortItems: TxnGridSortItems;
    fFilterItems: TxnGridFilterItems;

    fHashTable: TList<integer>;

    function Comparer(const aLeft, aRight: TArray<variant>): integer;
    function Getter(const aIndex: integer): TArray<variant>;

    procedure DoSort(aStart, aStop: integer); overload;
    procedure DoSort; overload;
  public
    constructor Create(aGridData: IxnGridData);
    destructor Destroy; override;
    procedure sort(aSortItems: TxnGridSortItems); overload;
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
  Result := fGridData.RowCount
end;

procedure TxnGridLinkSort.DoSort(aStart, aStop: integer);
var
  iStart: integer;
  iStop: integer;
  iPivot: TArray<variant>;
begin
  iStart := aStart;
  iStop := aStop;
  iPivot := Getter((iStart + iStop) div 2);
  repeat
    while (Comparer(Getter(iStart), iPivot) < 0) do
      Inc(iStart);
    while (Comparer(Getter(iStop), iPivot) > 0) do
      Dec(iStop);

    if iStart <= iStop then
    begin
      fHashTable.Exchange(iStart, iStop);
      Inc(iStart);
      Dec(iStop);
    end;
  until iStart > iStop;

  if iStop > aStart then
    DoSort(aStart, iStop);
  if iStart < aStop then
    DoSort(iStart, aStop);
end;

procedure TxnGridLinkSort.DoSort;
begin
  DoSort(0, fHashTable.Count - 1);
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

    if (fSortItems[i].fOrder = gsoDesc) then
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

constructor TxnGridLinkSort.Create(aGridData: IxnGridData);
var
  i: integer;
begin
  fGridData := aGridData;

  fHashTable := TList<integer>.Create;
  for i := 0 to aGridData.RowCount - 1 do
    fHashTable.Add(i);
end;

destructor TxnGridLinkSort.Destroy;
begin
  fHashTable.Free;
  inherited;
end;

function TxnGridLinkSort.Getter(const aIndex: integer): TArray<variant>;
var
  i: integer;
begin
  SetLength(Result, fSortItems.Count);

  for i := 0 to fSortItems.Count - 1 do
    if (fSortItems[i].fKind = gskStr) then
      Result[i] := ValueString(fSortItems[i].fIndex, aIndex)
    else
      Result[i] := ValueFloat(fSortItems[i].fIndex, aIndex)
end;

procedure TxnGridLinkSort.sort(aSortItems: TxnGridSortItems);
begin
  fSortItems := aSortItems;

  if fHashTable.Count > 0 then
    if aSortItems.Count > 0 then
      DoSort(0, fHashTable.Count - 1);
end;

function TxnGridLinkSort.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := fGridData.ValueFloat(aCol, fHashTable[aRow]);
end;

function TxnGridLinkSort.ValueString(aCol, aRow: integer): String;
begin
  Result := fGridData.ValueString(aCol, fHashTable[aRow]);
end;

{ TxnGridSortItem }

constructor TxnGridSortItem.Create(aIndex: integer; aKind: TKind; aOrder: TOrder);
begin
  fIndex := aIndex;
  fKind := aKind;
  fOrder := aOrder;
end;

{ TxnGridFilterItem }

constructor TxnGridFilterItem.Create(aIndex: integer; aCriteria: TCriteria; aCase: TCase);
begin
  fIndex := aIndex;
  fCriteria := aCriteria;
  fCase := aCase;
end;

end.
