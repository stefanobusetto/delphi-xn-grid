unit xn.list.indexed;

interface

uses System.Generics.Collections, System.Generics.Defaults,
  xn.types, xn.list, xn.list.observable;

type
  IxnListIndexNT<T> = interface(IxnListObservable<T>)
    ['{01E0A248-E138-4C24-8FBD-80B0E30D5B91}']
    procedure Notify(aAction: TxnAction; aIndex: integer);

    procedure NotifyAdd(aIndex: integer);
    procedure NotifyChange(aIndex: integer);
    procedure NotifyDelete(aIndex: integer);
    procedure NotifyClear;
    procedure NotifySort;

    procedure ObserverRegister(aObserver: IxnActionObserver);
    procedure ObserverUnregister(aObserver: IxnActionObserver);
  end;

  TxnListIndexNT<T> = class(TInterfacedObject, IxnListIndexNT<T>, IxnActionObserver)
  strict private
    fComparer: IComparer<T>;
    fObservers: TList<IxnActionObserver>;
  private
    fList: IxnListObservable<T>;
    fIndex: TList<integer>;
    fReverse: TList<integer>;
  public
    constructor Create(aList: IxnListObservable<T>); overload; virtual;
    constructor Create(aList: IxnListObservable<T>; aComparer: IComparer<T>); overload; virtual;
    destructor Destroy; override;

    procedure Notify(aAction: TxnAction; aIndex: integer); virtual;
    procedure NotifyAdd(aIndex: integer); virtual;
    procedure NotifyChange(aIndex: integer); virtual; abstract;
    procedure NotifyDelete(aIndex: integer); virtual;
    procedure NotifyClear; virtual;
    procedure NotifySort; virtual;

    procedure ObserverRegister(aObserver: IxnActionObserver); virtual;
    procedure ObserverUnregister(aObserver: IxnActionObserver); virtual;

    function Add(aItem: T): integer; virtual;
    function Remove(aItem: T): integer; virtual;
    procedure Delete(aIndex: integer); virtual;
    procedure Clear; virtual;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;

    function IndexOf(aItem: T): integer; virtual;
    function Contains(aItem: T): boolean; virtual;
    function GetEnumerator: TEnumerator<T>; virtual;

    function Seek1(aItem: T): integer;
    function Seek2(aItem: T): integer;
    function Count: integer;

    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

uses System.SysUtils;

{ TxnListIndexed<T> }

// *****************************************************************************
// *****************************************************************************

constructor TxnListIndexNT<T>.Create(aList: IxnListObservable<T>);
begin
  Create(aList, TComparer<T>.Default);
end;

constructor TxnListIndexNT<T>.Create(aList: IxnListObservable<T>; aComparer: IComparer<T>);
var
  i: IxnActionObserver;
begin
  inherited Create;

  fList := aList;
  fComparer := aComparer;

  fIndex := TList<integer>.Create;
  fReverse := TList<integer>.Create;
  fObservers := TList<IxnActionObserver>.Create;

   fList.ObserverRegister(Self);
end;

destructor TxnListIndexNT<T>.Destroy;
var
  i: integer;
begin
fObservers.Clear ;
//  for i := 0 to fObservers.Count - 1 do
//    ObserverUnregister(fObservers[i]);

  fIndex.Free;
  fReverse.Free;
  fObservers.Free;

  inherited;
end;


// *****************************************************************************
// *****************************************************************************

procedure TxnListIndexNT<T>.Notify(aAction: TxnAction; aIndex: integer);
var
  o: IxnActionObserver;
begin
  for o in fObservers do
    o.Notify(aAction, aIndex);
end;

procedure TxnListIndexNT<T>.NotifyAdd(aIndex: integer);
begin
  Notify(naAdd, aIndex);
end;

procedure TxnListIndexNT<T>.NotifyClear;
begin
  Notify(naClear, -1);
end;

procedure TxnListIndexNT<T>.NotifyDelete(aIndex: integer);
begin
  Notify(naDelete, aIndex);
end;

procedure TxnListIndexNT<T>.NotifySort;
begin
  Notify(naSort, -1);
end;

// *****************************************************************************
// *****************************************************************************

procedure TxnListIndexNT<T>.ObserverRegister(aObserver: IxnActionObserver);
begin
  fObservers.Add(aObserver);
  aObserver.Notify(naSort, -1);
end;

procedure TxnListIndexNT<T>.ObserverUnregister(aObserver: IxnActionObserver);
begin
  aObserver.Notify(naClear, -1);
  fObservers.Remove(aObserver);
end;


// *****************************************************************************
// *****************************************************************************

function TxnListIndexNT<T>.Seek1(aItem: T): integer;
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

function TxnListIndexNT<T>.Seek2(aItem: T): integer;
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

function TxnListIndexNT<T>.Count: integer;
begin
  Result := fIndex.Count
end;

function TxnListIndexNT<T>.ItemGet(aIndex: integer): T;
begin
  Result := fList[fIndex[aIndex]]
end;

// *****************************************************************************
// *****************************************************************************

function TxnListIndexNT<T>.Add(aItem: T): integer;
begin

end;

procedure TxnListIndexNT<T>.Clear;
begin
  fList.Clear;
end;

procedure TxnListIndexNT<T>.Delete(aIndex: integer);
begin

end;

function TxnListIndexNT<T>.Remove(aItem: T): integer;
begin

end;

procedure TxnListIndexNT<T>.Sort(const aComparer: IComparer<T>);
begin

end;

procedure TxnListIndexNT<T>.Sort;
begin

end;

function TxnListIndexNT<T>.GetEnumerator: TEnumerator<T>;
begin

end;

function TxnListIndexNT<T>.IndexOf(aItem: T): integer;
begin

end;

function TxnListIndexNT<T>.Contains(aItem: T): boolean;
begin

end;

end.
