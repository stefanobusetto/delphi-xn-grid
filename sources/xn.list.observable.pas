unit xn.list.observable;

interface

uses System.Generics.Collections, System.Generics.Defaults,
  xn.types, xn.list;

type
  IxnListObservable<T> = interface(IxnList<T>)
    ['{0961E04A-751E-4B97-8820-CF20F375F883}']
    procedure Notify(aAction: TxnAction; aIndex: integer);

    procedure NotifyAdd(aIndex: integer);
    procedure NotifyChange(aIndex: integer);
    procedure NotifyDelete(aIndex: integer);
    procedure NotifyClear;
    procedure NotifySort;

    procedure ObserverRegister(aObserver: IxnActionObserver);
    procedure ObserverUnregister(aObserver: IxnActionObserver);
    // procedure ObserverUnregisterAll;
  end;

  TxnListObservable<T> = class(TxnList<T>, IxnListObservable<T>)
  strict private
    fObservers: TList<IxnActionObserver>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Notify(aAction: TxnAction; aIndex: integer); virtual;
    procedure NotifyAdd(aIndex: integer); virtual;
    procedure NotifyChange(aIndex: integer); virtual; abstract;
    procedure NotifyDelete(aIndex: integer); virtual;
    procedure NotifyClear; virtual;
    procedure NotifySort; virtual;

    procedure ObserverRegister(aObserver: IxnActionObserver); virtual;
    procedure ObserverUnregister(aObserver: IxnActionObserver); virtual;

    function Add(aItem: T): integer; override;
    function Remove(aItem: T): integer; override;
    procedure Delete(aIndex: integer); override;
    procedure Clear; override;
    procedure Sort; overload; override;
    procedure Sort(const aComparer: IComparer<T>); overload; override;
  end;

implementation

{ TxnListObserver<T> }

// *****************************************************************************
// *****************************************************************************
constructor TxnListObservable<T>.Create;
begin
  inherited;
  fObservers := TList<IxnActionObserver>.Create;
end;

destructor TxnListObservable<T>.Destroy;
begin
  fObservers.Free;
  inherited;
end;

// *****************************************************************************
// *****************************************************************************

procedure TxnListObservable<T>.Notify(aAction: TxnAction; aIndex: integer);
var
  o: IxnActionObserver;
begin
  for o in fObservers do
    o.Notify(aAction, aIndex);
end;

procedure TxnListObservable<T>.NotifyAdd(aIndex: integer);
begin
  Notify(naAdd, aIndex);
end;

procedure TxnListObservable<T>.NotifyClear;
begin
  Notify(naClear, -1);
end;

procedure TxnListObservable<T>.NotifyDelete(aIndex: integer);
begin
  Notify(naDelete, aIndex);
end;

procedure TxnListObservable<T>.NotifySort;
begin
  Notify(naSort, -1);
end;

// *****************************************************************************
// *****************************************************************************

procedure TxnListObservable<T>.ObserverRegister(aObserver: IxnActionObserver);
begin
  fObservers.Add(aObserver);
  aObserver.Notify(naSort, -1);
end;

procedure TxnListObservable<T>.ObserverUnregister(aObserver: IxnActionObserver);
begin
  aObserver.Notify(naClear, -1);
  fObservers.Remove(aObserver);
end;

// *****************************************************************************
// *****************************************************************************
function TxnListObservable<T>.Add(aItem: T): integer;
begin
  result := inherited Add(aItem);
  NotifyAdd(fItems.Count - 1);
end;

procedure TxnListObservable<T>.Clear;
begin
  inherited Clear;
  NotifyClear;
end;

procedure TxnListObservable<T>.Delete(aIndex: integer);
begin
  inherited Delete(aIndex);
  NotifyDelete(aIndex);
end;

function TxnListObservable<T>.Remove(aItem: T): integer;
begin
  result := inherited Remove(aItem);
  NotifyDelete(result);
end;

procedure TxnListObservable<T>.Sort(const aComparer: IComparer<T>);
begin
  inherited Sort(aComparer);
  NotifySort;
end;

procedure TxnListObservable<T>.Sort;
begin
  inherited Sort;
  NotifySort;
end;

end.
