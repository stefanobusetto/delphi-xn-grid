unit xn.list.observer;

interface

uses System.Generics.Collections, System.Generics.Defaults,
  xn.types, xn.list;

type
  IxnListNotify<T> = interface(IxnList<T>)
    ['{0961E04A-751E-4B97-8820-CF20F375F883}']
    procedure Notify(aAction: TxnNotifyAction; aIndex: integer);

    procedure NotifyRegister(aListObserver: IxnItemsNotify<T>);
    procedure NotifyUnregister(aListObserver: IxnItemsNotify<T>);
    procedure NotifyUnregisterAll;
  end;

  TxnListNotify<T> = class(TxnList<T>, IxnListNotify<T>)
  strict private
    fNotifyList: IxnList<IxnItemsNotify<T>>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Notify(aAction: TxnNotifyAction; aIndex: integer);

    function Add(aItem: T): integer; override;
    function Remove(aItem: T): integer; override;
    procedure Clear; override;
    procedure Delete(aIndex: integer); override;
    procedure Sort; overload; override;
    procedure Sort(const aComparer: IComparer<T>); overload; override;

    procedure NotifyRegister(aListObserver: IxnItemsNotify<T>); virtual;
    procedure NotifyUnregister(aListObserver: IxnItemsNotify<T>); virtual;
    procedure NotifyUnregisterAll; virtual;
  end;

implementation

{ TxnListNotify<T> }

constructor TxnListNotify<T>.Create;
begin
  inherited;
  fNotifyList := TxnList < IxnItemsNotify < T >>.Create;
end;

destructor TxnListNotify<T>.Destroy;
begin
  inherited;
end;

procedure TxnListNotify<T>.Notify(aAction: TxnNotifyAction; aIndex: integer);
var
  o: IxnItemsNotify<T>;
begin
  for o in fNotifyList do
    o.Notify(aAction, aIndex);
end;

procedure TxnListNotify<T>.NotifyRegister(aListObserver: IxnItemsNotify<T>);
begin
  fNotifyList.Add(aListObserver);
  aListObserver.Notify(naSort, -1);
end;

procedure TxnListNotify<T>.NotifyUnregister(aListObserver: IxnItemsNotify<T>);
begin
  aListObserver.Notify(naClear, -1);
  fNotifyList.Remove(aListObserver);
end;

procedure TxnListNotify<T>.NotifyUnregisterAll;
var
  i: integer;
begin
  for i := fNotifyList.Count - 1 downto 0 do
    NotifyUnregister(fNotifyList[i]);
end;

function TxnListNotify<T>.Add(aItem: T): integer;
begin
  result := inherited Add(aItem);
  Notify(naAdd, fItems.Count - 1);
end;

procedure TxnListNotify<T>.Clear;
begin
  inherited Clear;
  Notify(naClear, -1);
end;

procedure TxnListNotify<T>.Delete(aIndex: integer);
begin
  inherited Delete(aIndex);
  Notify(naDelete, aIndex);
end;

function TxnListNotify<T>.Remove(aItem: T): integer;
begin
  result := inherited Remove(aItem);
  Notify(naDelete, result);
end;

procedure TxnListNotify<T>.Sort(const aComparer: IComparer<T>);
begin
  inherited Sort(aComparer);
  Notify(naSort, -1);
end;

procedure TxnListNotify<T>.Sort;
begin
  inherited Sort;
  Notify(naSort, -1);
end;

end.
