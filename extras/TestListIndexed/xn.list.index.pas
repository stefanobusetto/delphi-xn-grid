unit xn.list.index;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  xn.list, xn.Types, xn.list.observer;

type
  TxnListIndexEnumerator<T> = class;

  TxnComparison<T> = reference to function(const aValue: T): Integer;

  IxnComparer<T> = interface
    ['{57923A26-26C6-47AD-AE9F-EE58F0FE0316}']
    function Comparison(const aValue: T): Integer;
  end;

  TxnComparer<T> = class(TInterfacedObject, IxnComparer<T>)
  private
    fComparison: TxnComparison<T>;
  public
    constructor Create(aComparison: TxnComparison<T>);
    class function Construct(aComparison: TxnComparison<T>): IxnComparer<T>;
    function Comparison(const aValue: T): Integer;
  end;

  IxnListIndex<T> = interface(IxnItemsNotify<T>)
    ['{7F4738B8-A53D-4EEB-AE16-66C97D62E64A}']
    function GetEnumerator: TxnListIndexEnumerator<T>;
    function Seek1(aItem: T): Integer;
    function Seek2(aItem: T): Integer;

    function Seek1b(aComparer: IxnComparer<T>): Integer;
    function Count: Integer;

    function ItemGet(aIndex: Integer): T;
    property Items[aIndex: Integer]: T read ItemGet; default;
  end;

  TxnListIndex<T> = class(TInterfacedObject, IxnListIndex<T>)
  strict private
    fComparer: IComparer<T>;
  private
    fList: IxnList<T>;
    fIndex: TList<Integer>;

    procedure NotifyAdd(aIndex: Integer);
    procedure NotifyModify(aIndex: Integer);
    procedure NotifyDelete(aIndex: Integer);
    procedure NotifyClear;
    procedure NotifySort;
  public
    constructor Create(aList: IxnListNotify<T>; aComparer: IComparer<T>);
    destructor Destroy; override;
    class function ConstructIndex(aList: IxnListNotify<T>; aComparer: IComparer<T>): IxnListIndex<T>;

    procedure Notify(aAction: TxnNotifyAction; aIndex: Integer);

    function GetEnumerator: TxnListIndexEnumerator<T>;
    function Seek1(aItem: T): Integer;
    function Seek2(aItem: T): Integer;

    function Seek1b(aComparer: IxnComparer<T>): Integer;
    function Count: Integer;

    function ItemGet(aIndex: Integer): T;
    property Items[aIndex: Integer]: T read ItemGet; default;
  end;

  TxnListIndexEnumerator<T> = class
  private
    fList: TxnListIndex<T>;
    fIndex: Integer;
  public
    constructor Create(aList: TxnListIndex<T>);
    function GetCurrent: T;
    function MoveNext: boolean;
    property Current: T read GetCurrent;
  end;

implementation

{ TxnListIndex<T> }

procedure TxnListIndex<T>.Notify(aAction: TxnNotifyAction; aIndex: Integer);
begin
  case aAction of
    naAdd:
      NotifyAdd(aIndex);
    naModify:
      NotifyModify(aIndex);
    naDelete:
      NotifyDelete(aIndex);
    naClear:
      NotifyClear;
    naSort:
      NotifySort;
  end;
end;

procedure TxnListIndex<T>.NotifyAdd(aIndex: Integer);
var
  s: Integer;
begin
  // posizione nell'indice
  s := Seek2(fList.Items[aIndex]);

  // add a indice
  if s < 0 then
    fIndex.Add(aIndex)
  else
    fIndex.Insert(s, aIndex)
end;

procedure TxnListIndex<T>.NotifyClear;
begin
  fIndex.Clear;
end;

procedure TxnListIndex<T>.NotifyModify(aIndex: Integer);
var
  i: Integer;
begin
  for i := fIndex.Count - 1 downto 0 do
    if fIndex[i] = aIndex then
      fIndex.Delete(i);

  NotifyAdd(aIndex);
end;

procedure TxnListIndex<T>.NotifySort;
var
  i: Integer;
begin
  fIndex.Clear;

  for i := 0 to fList.Count - 1 do
    NotifyAdd(i);
end;

procedure TxnListIndex<T>.NotifyDelete(aIndex: Integer);
var
  i: Integer;
begin
  for i := fIndex.Count - 1 downto 0 do
    if fIndex[i] > aIndex then
      fIndex[i] := fIndex[i] - 1
    else if fIndex[i] = aIndex then
      fIndex.Delete(i)
end;

function TxnListIndex<T>.Count: Integer;
begin
  Result := fIndex.Count
end;

constructor TxnListIndex<T>.Create(aList: IxnListNotify<T>; aComparer: IComparer<T>);
begin
  fList := aList;
  fComparer := aComparer;

  fIndex := TList<Integer>.Create;
end;

destructor TxnListIndex<T>.Destroy;
begin
  fIndex.Free;
  inherited;
end;

class function TxnListIndex<T>.ConstructIndex(aList: IxnListNotify<T>; aComparer: IComparer<T>): IxnListIndex<T>;
begin
  Result := TxnListIndex<T>.Create(aList, aComparer);
  aList.NotifyRegister(Result);
end;

function TxnListIndex<T>.GetEnumerator: TxnListIndexEnumerator<T>;
begin
  Result := TxnListIndexEnumerator<T>.Create(Self);
end;

function TxnListIndex<T>.ItemGet(aIndex: Integer): T;
begin
  Result := fList[fIndex[aIndex]]
end;

function TxnListIndex<T>.Seek1(aItem: T): Integer;
var
  iStart: Integer;
  iStop: Integer;
  iPivot: Integer;
  IComparer: Integer;
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

function TxnListIndex<T>.Seek1b(aComparer: IxnComparer<T>): Integer;
var
  iStart: Integer;
  iStop: Integer;
  iPivot: Integer;
  IComparer: Integer;
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
    IComparer := aComparer.Comparison(Items[iPivot]);

    if IComparer = 0 then
      exit(iPivot)
    else if IComparer > 0 then
      iStop := iPivot - 1
    else
      iStart := iPivot + 1;
  end;
end;

function TxnListIndex<T>.Seek2(aItem: T): Integer;
var
  iStart: Integer;
  iStop: Integer;
  oStart: Integer;
  oStop: Integer;
  iPivot: Integer;
  IComparer: Integer;
  iOther: Integer;
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

{ TxnListIndexEnumerator<T> }

constructor TxnListIndexEnumerator<T>.Create(aList: TxnListIndex<T>);
begin
  inherited Create;
  fIndex := -1;
  fList := aList;
end;

function TxnListIndexEnumerator<T>.GetCurrent: T;
begin
  Result := fList.fList[fList.fIndex[fIndex]]
end;

function TxnListIndexEnumerator<T>.MoveNext: boolean;
begin
  Result := fIndex < fList.fIndex.Count - 1;
  if Result then
    Inc(fIndex);
end;

{ TxnComparer<T> }

function TxnComparer<T>.Comparison(const aValue: T): Integer;
begin
  Result := fComparison(aValue)
end;

class function TxnComparer<T>.Construct(aComparison: TxnComparison<T>): IxnComparer<T>;
begin
  Result := TxnComparer<T>.Create(aComparison);
end;

constructor TxnComparer<T>.Create(aComparison: TxnComparison<T>);
begin
  fComparison := aComparison;
end;

end.
