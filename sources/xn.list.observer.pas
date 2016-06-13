unit xn.list.observer;

interface

uses System.Generics.Collections, System.Generics.Defaults,
  xn.list;

type
  IxnListNotify<T> = interface
    ['{A1638A29-DC7B-4832-931D-1F73BA208708}']
    procedure NotifyAdd(aIndex: integer);
    procedure NotifyModify(aIndex: integer);
    procedure NotifyDelete(aIndex: integer);
    // procedure NotifyClear;
    // procedure NotifySort;
  end;

  IxnListObserver<T> = interface(IxnList<T>)
    ['{0961E04A-751E-4B97-8820-CF20F375F883}']
    procedure NotifyAdd(aIndex: integer);
    procedure NotifyModify(aIndex: integer);
    procedure NotifyDelete(aIndex: integer);
    // procedure NotifyClear;
    // procedure NotifySort;

    procedure ObserverRegister(aObserver: IxnListNotify<T>);
    procedure ObserverUnregister(aObserver: IxnListNotify<T>);
    procedure ObserversUnregister;
  end;

  TxnListObserver<T> = class(TxnList<T>, IxnListObserver<T>)
  strict private
    fObservers: IxnList<IxnListNotify<T>>;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Add(aItem: T): integer; override;
    function Remove(aItem: T): integer; override;
    procedure Clear; override;
    procedure Delete(aIndex: integer); override;
    procedure Sort; overload; override;
    procedure Sort(const aComparer: IComparer<T>); overload; override;

    procedure NotifyAdd(aIndex: integer); virtual;
    procedure NotifyModify(aIndex: integer); virtual;
    procedure NotifyDelete(aIndex: integer); virtual;
    // procedure NotifyClear; virtual;
    // procedure NotifySort; virtual;

    procedure ObserverRegister(aObserver: IxnListNotify<T>); virtual;
    procedure ObserverUnregister(aObserver: IxnListNotify<T>); virtual;
    procedure ObserversUnregister; virtual;
  end;

implementation

{ TxnListNotify<T> }

constructor TxnListObserver<T>.Create;
begin
  inherited;
  fObservers := TxnList < IxnListNotify < T >>.Create;
end;

destructor TxnListObserver<T>.Destroy;
begin
  inherited;
end;

procedure TxnListObserver<T>.NotifyAdd(aIndex: integer);
var
  o: IxnListNotify<T>;
begin
  for o in fObservers do
    o.NotifyAdd(aIndex);
end;

procedure TxnListObserver<T>.NotifyClear;
var
  o: IxnListNotify<T>;
begin
  for o in fObservers do
    o.NotifyClear;
end;

procedure TxnListObserver<T>.NotifyDelete(aIndex: integer);
var
  o: IxnListNotify<T>;
begin
  for o in fObservers do
    o.NotifyDelete(aIndex);
end;

procedure TxnListObserver<T>.NotifyModify(aIndex: integer);
var
  o: IxnListNotify<T>;
begin
  for o in fObservers do
    o.NotifyModify(aIndex);
end;

procedure TxnListObserver<T>.NotifySort;
var
  o: IxnListNotify<T>;
begin
  for o in fObservers do
    o.NotifySort;
end;

procedure TxnListObserver<T>.ObserverRegister(aObserver: IxnListNotify<T>);
begin
  fObservers.Add(aObserver);
  aObserver.NotifySort;
end;

procedure TxnListObserver<T>.ObserverUnregister(aObserver: IxnListNotify<T>);
begin
  aObserver.NotifyClear;
  fObservers.Remove(aObserver);
end;

procedure TxnListObserver<T>.ObserversUnregister;
var
  i: integer;
begin
  for i := fObservers.Count - 1 downto 0 do
    ObserverUnregister(fObservers[i]);
end;

function TxnListObserver<T>.Add(aItem: T): integer; override;
begin
  result := inherited Add(aItem);
  NotifyAdd(fItems.Count - 1);
end;

procedure TxnListObserver<T>.Clear;
begin
  inherited Clear;
  NotifyClear;
end;

procedure TxnListObserver<T>.Delete(aIndex: integer);
begin
  inherited Delete(aIndex);
  NotifyDelete(aIndex);
end;

function TxnListObserver<T>.Remove(aItem: T): integer;
begin
  result := inherited Remove(aItem);
  NotifyDelete(result);
end;

procedure TxnListObserver<T>.Sort(const aComparer: IComparer<T>);
begin
  inherited Sort(aComparer);
  NotifySort;
end;

procedure TxnListObserver<T>.Sort;
begin
  inherited Sort;
  NotifySort;
end;

end.
