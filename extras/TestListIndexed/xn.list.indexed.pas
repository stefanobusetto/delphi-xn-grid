unit xn.list.indexed;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  xn.list;

type
  TxnListIndexedEnumerator<T> = class;

  IxnListIndexed<T> = interface
    ['{7F4738B8-A53D-4EEB-AE16-66C97D62E64A}']
    procedure Add(aItem: T);
    function Count: integer;
    function GetEnumerator: TxnListIndexedEnumerator<T>;
    procedure Fill;
    procedure Sort(aStart, aStop: integer); overload;
    procedure Sort; overload;
    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;

    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnListIndexed<T> = class(TInterfacedObject, IxnListIndexed<T>)
  strict private
    fList: IxnList<T>;
    fIndex: TList<integer>;
    fComparer: IComparer<T>;
  public
    constructor Create(aList: IxnList<T>; aComparer: IComparer<T>);
    destructor Destroy; override;
    procedure Add(aItem: T);
    function Count: integer;
    function GetEnumerator: TxnListIndexedEnumerator<T>;
    procedure Fill;
    procedure Sort(aStart, aStop: integer); overload;
    procedure Sort; overload;
    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;

    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnListIndexedEnumerator<T> = class
  private
    fList: TxnListIndexed<T>;
    fIndex: integer;
  public
    constructor Create(aList: TxnListIndexed<T>);
    function GetCurrent: T;
    function MoveNext: boolean;
    property Current: T read GetCurrent;
  end;

implementation

{ TxnListIndexed<T> }

procedure TxnListIndexed<T>.Add(aItem: T);
var
  s: integer;
  n: integer;
begin
  // posizione nella lista
  fList.Add(aItem);
  n := fList.Count - 1;

  // posizione nell'indice
  s := Seek2(fList.Items[n]);

  if s < 0 then
    fIndex.Add(n)
  else
    fIndex.Insert(s, n)
end;

function TxnListIndexed<T>.Count: integer;
begin
  Result := fList.Count;
end;

constructor TxnListIndexed<T>.Create(aList: IxnList<T>; aComparer: IComparer<T>);
begin
  fList := aList;
  fComparer := aComparer;

  fIndex := TList<integer>.Create;
end;

destructor TxnListIndexed<T>.Destroy;
begin
  fIndex.Free;
  inherited;
end;

procedure TxnListIndexed<T>.Fill;
var
  i: integer;
  a: TArray<variant>;
begin
  // SetLength(a, fItems.Count);
  // for i := 0 to fItems.Count - 1 do
  // a[i] := fItems[i].fValue;

  fIndex.Clear;
  for i := 0 to fList.Count - 1 do
    // if Comparer(a, Getter(i)) = 0 then
    fIndex.Add(i);
end;

function TxnListIndexed<T>.GetEnumerator: TxnListIndexedEnumerator<T>;
begin
  Result := TxnListIndexedEnumerator<T>.Create(Self);
end;

function TxnListIndexed<T>.ItemGet(aIndex: integer): T;
begin
  Result := fList[fIndex[aIndex]]
end;

function TxnListIndexed<T>.Seek1(aItem: T): integer;
var
  iStart: integer;
  iStop: integer;
  iPivot: integer;
  IComparer: integer;
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
    IComparer := fComparer.Compare(Items[iPivot], aItem);

    if IComparer = 0 then
      exit(iPivot)
    else if IComparer > 0 then
      iStop := iPivot - 1
    else
      iStart := iPivot + 1;
  end;
end;

function TxnListIndexed<T>.Seek2(aItem: T): integer;
var
  iStart: integer;
  iStop: integer;
  oStart: integer;
  oStop: integer;
  iPivot: integer;
  IComparer: integer;
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
    IComparer := fComparer.Compare(Items[iPivot], aItem);

    if IComparer = 0 then
      exit(iPivot)
    else if IComparer > 0 then
    begin
      if iPivot > oStart then
      begin
        iOther := fComparer.Compare(Items[iPivot - 1], aItem);
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
        iOther := fComparer.Compare(Items[iPivot + 1], aItem);
        if iOther = 0 then
          exit(iPivot + 1)
        else if iOther > 0 then
          exit(iPivot + 1);
      end;
      iStart := iPivot + 1;
    end;
  end;

  if fComparer.Compare(Items[oStart], aItem) > 0 then
    exit(oStart);
end;

procedure TxnListIndexed<T>.Sort;
begin
  if fIndex.Count > 0 then
    if fList.Count > 0 then
      Sort(0, fIndex.Count - 1);
end;

procedure TxnListIndexed<T>.Sort(aStart, aStop: integer);
var
  iStart: integer;
  iStop: integer;
  iPivot: T;
begin
  iStart := aStart;
  iStop := aStop;
  iPivot := Items[(iStart + iStop) div 2];
  repeat
    while (fComparer.Compare(Items[iStart], iPivot)) < 0 do
      Inc(iStart);
    while (fComparer.Compare(Items[iStop], iPivot)) > 0 do
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

{ TxnListIndexedEnumerator<T> }

constructor TxnListIndexedEnumerator<T>.Create(aList: TxnListIndexed<T>);
begin
  inherited Create;
  fIndex := -1;
  fList := aList;
end;

function TxnListIndexedEnumerator<T>.GetCurrent: T;
begin
  Result := fList.Items[fIndex];
end;

function TxnListIndexedEnumerator<T>.MoveNext: boolean;
begin
  Result := fIndex < fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

end.
