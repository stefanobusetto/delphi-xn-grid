unit xn.list;

interface

uses System.Generics.Collections, System.Generics.Defaults;

type
  IxnListObserver<T> = interface;

  IxnList<T> = interface
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    procedure Add(aItem: T);
    function Remove(aItem: T): integer;
    procedure Clear;
    procedure Delete(aIndex: integer);

    function IndexOf(aItem: T): integer;
    function Contains(aItem: T): boolean;
    function Count: integer;
    function GetEnumerator: TEnumerator<T>;
    procedure Sort; overload;
    procedure Sort(const aComparer: IComparer<T>); overload;

    procedure ObserverRegister(aObserver: IxnListObserver<T>);
    procedure ObserverUnregister(aObserver: IxnListObserver<T>);

    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  IxnListObserver<T> = interface
    ['{F8363706-20CD-4922-BA96-F788D94AA141}']
    procedure AfterAdd(aIndex: integer);
    procedure AfterRemove(aIndex: integer);
    procedure AfterClear;
  end;

  TxnList<T> = class(TInterfacedObject, IxnList<T>)
  strict private
    fItems: TList<T>;
    // procedure Exchange(aIndex1, aIndex2: integer);
    // procedure Delete(aIndex: integer); virtual;
  public
    fObserver: IxnListObserver<T>;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(aItem: T); virtual;
    function Remove(aItem: T): integer; virtual;
    procedure Clear;
    procedure Delete(aIndex: integer);

    function IndexOf(aItem: T): integer; virtual;
    function Contains(aItem: T): boolean; virtual;
    function Count: integer; virtual;
    function GetEnumerator: TEnumerator<T>;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;

    procedure ObserverRegister(aObserver: IxnListObserver<T>);
    procedure ObserverUnregister(aObserver: IxnListObserver<T>);

    function ItemGet(aIndex: integer): T; virtual;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

{ TxnList<T> }

procedure TxnList<T>.Add(aItem: T);
begin
  fItems.Add(aItem);

  if fObserver <> nil then
    fObserver.AfterAdd(fItems.Count - 1);
end;

function TxnList<T>.Remove(aItem: T): integer;
begin
  Result := fItems.Remove(aItem);

  if fObserver <> nil then
    fObserver.AfterRemove(Result);
end;

procedure TxnList<T>.Clear;
begin
  fItems.Clear;

  if fObserver <> nil then
    fObserver.AfterClear;
end;

procedure TxnList<T>.Delete(aIndex: integer);
begin
  fItems.Delete(aIndex);

  if fObserver <> nil then
    fObserver.AfterRemove(aIndex);
end;

function TxnList<T>.Contains(aItem: T): boolean;
begin
  Result := fItems.Contains(aItem)
end;

function TxnList<T>.Count: integer;
begin
  Result := fItems.Count
end;

constructor TxnList<T>.Create;
begin
  fItems := TList<T>.Create;
  fObserver := nil;
end;

// procedure TxnList<T>.Exchange(aIndex1, aIndex2: integer);
// begin
// se si ripristina serve notificare agli indici collegati
// il cambio di ordine degli elementi
// fItems.Exchange(aIndex1, aIndex2);
// end;

// procedure TxnList<T>.Delete(aIndex: integer);
// begin
// se si ripristina serve notificare agli indici collegati
// il la cancellazione dell'elemento
// fItems.Delete(aIndex);
// end;

destructor TxnList<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TxnList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fItems.GetEnumerator;
end;

function TxnList<T>.IndexOf(aItem: T): integer;
begin
  Result := fItems.IndexOf(aItem);
end;

function TxnList<T>.ItemGet(aIndex: integer): T;
begin
  Result := fItems[aIndex];
end;

procedure TxnList<T>.ObserverRegister(aObserver: IxnListObserver<T>);
begin
  fObserver := aObserver;
end;

procedure TxnList<T>.ObserverUnregister(aObserver: IxnListObserver<T>);
begin
  fObserver := nil;
end;

procedure TxnList<T>.Sort;
begin
  fItems.Sort;
end;

procedure TxnList<T>.Sort(const aComparer: IComparer<T>);
begin
  fItems.Sort(aComparer);
end;

end.
