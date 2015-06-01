unit xn.grid.data;

interface

uses Generics.Collections, xn.grid.common, xn.list;

type
  // ********************************************************************************
  // ********************************************************************************
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

  IxnGridSortItems = IxnList<TxnGridSortItem>;
  TxnGridSortItems = TxnList<TxnGridSortItem>;

  // ********************************************************************************
  // ********************************************************************************
  TxnGridFilterItem = record
  type
    TCase = (gfcCaseSensitive, gfcCaseInsensitive);
    TKind = (gfkStr, gfkNum);
  private
    fIndex: integer;
    fKind: TKind;
    fCase: TCase;
    fValue: variant;
  public
    constructor Create(aIndex: integer; aValue: variant; aKind: TKind; aCase: TCase);
  end;

  IxnGridFilterItems = IxnList<TxnGridFilterItem>;
  TxnGridFilterItems = TxnList<TxnGridFilterItem>;

  // ********************************************************************************
  // ********************************************************************************
  TxnGridData = class(TInterfacedObject, IxnGridData)
  private
    fData: IxnGridData;
    fIndex: TList<integer>;
  public
    function RowCount: integer; virtual;
    function AsDebug: String; virtual;
    function ValueString(aCol, aRow: integer): String; virtual;
    function ValueFloat(aCol, aRow: integer): Double; virtual;
  end;

  // ********************************************************************************
  // ********************************************************************************
  TxnGridDataSort = class(TxnGridData)
  strict private
    fItems: IxnGridSortItems;
    function Comparer(const aLeft, aRight: TArray<variant>): integer;
    function Getter(const aIndex: integer): TArray<variant>;
    procedure Sort(aStart, aStop: integer); overload;
  public
    constructor Create(aGridData: IxnGridData; aSortItems: IxnGridSortItems);
    destructor Destroy; override;
    procedure Fill; virtual;
    procedure Sort; overload;
    function Seek1(aKeys: TArray<variant>): integer;
    function Seek2(aKeys: TArray<variant>): integer;
  end;

  TxnGridDataFilter = class(TxnGridData)
  strict private
    fItems: IxnGridFilterItems;
    function Comparer(const aLeft, aRight: TArray<variant>): integer;
    function Getter(const aIndex: integer): TArray<variant>;
  public
    constructor Create(aGridData: IxnGridData; aFilterItems: IxnGridFilterItems);
    destructor Destroy; override;
    procedure Fill; virtual;
  end;

implementation

uses System.SysUtils;

{ TxnGridSortItem }

constructor TxnGridSortItem.Create(aIndex: integer; aKind: TKind; aOrder: TOrder);
begin
  fIndex := aIndex;
  fKind := aKind;
  fOrder := aOrder;
end;

{ TxnGridFilterItem }

constructor TxnGridFilterItem.Create(aIndex: integer; aValue: variant; aKind: TKind; aCase: TCase);
begin
  fIndex := aIndex;
  fKind := aKind;
  fCase := aCase;
  fValue := aValue;
end;

{ TxnGridData }

function TxnGridData.AsDebug: String;
var
  i: integer;
begin
  for i := 0 to fIndex.Count - 1 do
    Result := Result + ValueString(0, i) + ',';
end;

function TxnGridData.RowCount: integer;
begin
  Result := fIndex.Count
end;

function TxnGridData.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := fData.ValueFloat(aCol, fIndex[aRow]);
end;

function TxnGridData.ValueString(aCol, aRow: integer): String;
begin
  Result := fData.ValueString(aCol, fIndex[aRow]);
end;

{ TxnGridDataSort }

procedure TxnGridDataSort.Sort(aStart, aStop: integer);
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
      if iStart <> iStop then
        fIndex.Exchange(iStart, iStop);
      Inc(iStart);
      Dec(iStop);
    end;
  until iStart > iStop;

  if iStop > aStart then
    Sort(aStart, iStop);
  if iStart < aStop then
    Sort(iStart, aStop);
end;

function TxnGridDataSort.Comparer(const aLeft, aRight: TArray<variant>): integer;
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

    if (fItems[i].fOrder = gsoDesc) then
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

constructor TxnGridDataSort.Create(aGridData: IxnGridData; aSortItems: IxnGridSortItems);
begin
  fData := aGridData;
  fItems := aSortItems;

  fIndex := TList<integer>.Create;
  Fill;
end;

destructor TxnGridDataSort.Destroy;
begin
  fIndex.Free;
  inherited;
end;

procedure TxnGridDataSort.Fill;
var
  i: integer;
begin
  fIndex.Clear;
  for i := 0 to fData.RowCount - 1 do
    fIndex.Add(i);
end;

function TxnGridDataSort.Getter(const aIndex: integer): TArray<variant>;
var
  i: integer;
begin
  SetLength(Result, fItems.Count);

  for i := 0 to fItems.Count - 1 do
    if (fItems[i].fKind = gskStr) then
      Result[i] := ValueString(fItems[i].fIndex, aIndex)
    else
      Result[i] := ValueFloat(fItems[i].fIndex, aIndex)
end;

function TxnGridDataSort.Seek1(aKeys: TArray<variant>): integer;
var
  iStart: integer;
  iStop: integer;
  iPivot: integer;
  iComparer: integer;
begin
  // returns the actual index of the item in the list
  // -1 if the item is not found
  iStart := 0;
  iStop := fIndex.Count - 1;

  if fIndex.Count = 0 then
    exit(-1);

  Result := -1;
  while iStart <= iStop do
  begin
    iPivot := (iStart + iStop) div 2;
    iComparer := Comparer(Getter(iPivot), aKeys);

    if iComparer = 0 then
      exit(iPivot)
    else if iComparer > 0 then
      iStop := iPivot - 1
    else
      iStart := iPivot + 1;
  end;
end;

function TxnGridDataSort.Seek2(aKeys: TArray<variant>): integer;
var
  iStart: integer;
  iStop: integer;
  oStart: integer;
  oStop: integer;
  iPivot: integer;
  iComparer: integer;
  iOther: integer;
begin
  // returns the expected index of the item in the list
  // -1 if the item is after the last item of the list
  iStart := 0;
  oStart := 0;
  iStop := fIndex.Count - 1;
  oStop := fIndex.Count - 1;

  if fIndex.Count = 0 then
    exit(0);

  Result := -1;
  while iStart <= iStop do
  begin
    iPivot := (iStart + iStop) div 2;
    iComparer := Comparer(Getter(iPivot), aKeys);

    if iComparer = 0 then
      exit(iPivot)
    else if iComparer > 0 then
    begin
      if iPivot > oStart then
      begin
        iOther := Comparer(Getter(iPivot - 1), aKeys);
        if iOther = 0 then
          exit(iPivot - 1)
        else if iOther < 0 then
          exit(iPivot);
      end;
      iStop := iPivot - 1
    end
    else
    begin
      if iPivot < oStop then
      begin
        iOther := Comparer(Getter(iPivot + 1), aKeys);
        if iOther = 0 then
          exit(iPivot + 1)
        else if iOther > 0 then
          exit(iPivot + 1);
      end;
      iStart := iPivot + 1;
    end;
  end;

  if Comparer(Getter(oStart), aKeys) > 0 then
    exit(oStart);
end;

procedure TxnGridDataSort.Sort;
begin
  if fIndex.Count > 0 then
    if fItems.Count > 0 then
      Sort(0, fIndex.Count - 1);
end;

{ TxnGridDataFilter }

function TxnGridDataFilter.Comparer(const aLeft, aRight: TArray<variant>): integer;
var
  l: variant;
  r: variant;
  i: integer;
begin
  for i := Low(aLeft) to High(aLeft) do
  begin
    l := aLeft[i];
    r := aRight[i];

    if fItems[i].fCase = gfcCaseInsensitive then
    begin
      l := LowerCase(l);
      r := LowerCase(r);
    end;

    if l < r then
      exit(-1)
    else if l > r then
      exit(+1);
  end;
  exit(0);
end;

constructor TxnGridDataFilter.Create(aGridData: IxnGridData; aFilterItems: IxnGridFilterItems);
begin
  fData := aGridData;
  fItems := aFilterItems;

  fIndex := TList<integer>.Create;
  Fill;
end;

destructor TxnGridDataFilter.Destroy;
begin
  fIndex.Free;
  inherited;
end;

procedure TxnGridDataFilter.Fill;
var
  i: integer;
  a: TArray<variant>;
begin
  SetLength(a, fItems.Count);
  for i := 0 to fItems.Count - 1 do
    a[i] := fItems[i].fValue;

  fIndex.Clear;
  for i := 0 to fData.RowCount - 1 do
    if Comparer(a, Getter(i)) = 0 then
      fIndex.Add(i);
end;

function TxnGridDataFilter.Getter(const aIndex: integer): TArray<variant>;
var
  i: integer;
begin
  SetLength(Result, fItems.Count);

  for i := 0 to fItems.Count - 1 do
    if (fItems[i].fKind = gfkStr) then
      Result[i] := fData.ValueString(fItems[i].fIndex, aIndex)
    else
      Result[i] := fData.ValueFloat(fItems[i].fIndex, aIndex)
end;

end.
