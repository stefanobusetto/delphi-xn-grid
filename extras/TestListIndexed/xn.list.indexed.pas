unit xn.list.indexed;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  xn.list;

type
  TxnListIndexedEnumerator<T> = class;

  IxnListIndexed<T> = interface(IxnListObserver<T>)
    ['{7F4738B8-A53D-4EEB-AE16-66C97D62E64A}']
    function GetEnumerator: TxnListIndexedEnumerator<T>;
    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;

    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnListIndexed<T> = class(TInterfacedObject, IxnListIndexed<T>)
  strict private
    fIndex: TList<integer>;
    fComparer: IComparer<T>;
  private
    fList: IxnList<T>;
  public
    constructor Create(aList: IxnList<T>; aComparer: IComparer<T>);
    destructor Destroy; override;

    function GetEnumerator: TxnListIndexedEnumerator<T>;
    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;

    procedure AfterAdd(aIndex: integer);
    procedure AfterRemove(aIndex: integer);
    procedure AfterClear;

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

procedure TxnListIndexed<T>.AfterAdd(aIndex: integer);
var
  s: integer;
begin
  // posizione nell'indice
  s := Seek2(fList.Items[aIndex]);

  // add a indice
  if s < 0 then
    fIndex.Add(aIndex)
  else
    fIndex.Insert(s, aIndex)
end;

procedure TxnListIndexed<T>.AfterClear;
begin
  fIndex.Clear;
end;

procedure TxnListIndexed<T>.AfterRemove(aIndex: integer);
var
  i: integer;
begin
  for i := fIndex.Count - 1 downto 0 do
    if fIndex[i] > aIndex then
      fIndex[i] := fIndex[i] - 1
    else if fIndex[i] = aIndex then
      fIndex.Delete(i)
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
  Result := fIndex < fList.fList.Count - 1;
  if Result then
    Inc(fIndex);
end;

end.
