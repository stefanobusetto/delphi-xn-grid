unit xn.list;

interface

uses System.Generics.Collections, System.Generics.Defaults, xn.Items;

type
  IxnList<T> = interface(IxnItems<T>)
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    function GetEnumerator: TEnumerator<T>;

    function Add(aItem: T): integer; overload;
    function Remove(aItem: T): integer; overload;
    procedure Delete(aIndex: integer); overload;
    procedure Clear;
    procedure Sort; overload;
    procedure Sort(const aComparer: IComparer<T>); overload;
    function IndexOf(aItem: T): integer;
    function Contains(aItem: T): boolean;

    procedure Insert(aIndex: integer; aItem: T);
  end;

  TxnList<T> = class(TxnItems<T>, IxnList<T>)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; override;

    function Add(aItem: T): integer; overload; virtual;
    function Remove(aItem: T): integer; overload; virtual;
    procedure Delete(aIndex: integer); overload; virtual;
    procedure Clear; virtual;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;
    function IndexOf(aItem: T): integer; virtual;
    function Contains(aItem: T): boolean; virtual;

    procedure Insert(aIndex: integer; aItem: T);
  end;

implementation

{ TxnList<T> }

function TxnList<T>.Add(aItem: T): integer;
begin
  Result := fItems.Add(aItem);

  // ObserverNotify(naAdd, Result);
end;

function TxnList<T>.Remove(aItem: T): integer;
begin
  Result := fItems.Remove(aItem);
  // ObserverNotify(naDelete, Result);
end;

procedure TxnList<T>.Clear;
begin
  fItems.Clear;
  // ObserverNotify(naClear, -1);
end;

procedure TxnList<T>.Delete(aIndex: integer);
begin
  // ObserverNotify(naDelete, aIndex);
  fItems.Delete(aIndex);
end;

constructor TxnList<T>.Create;
begin
  inherited;
end;

destructor TxnList<T>.Destroy;
begin
  inherited;
end;

function TxnList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fItems.GetEnumerator;
end;

function TxnList<T>.Contains(aItem: T): boolean;
begin
  Result := fItems.Contains(aItem)
end;

function TxnList<T>.IndexOf(aItem: T): integer;
begin
  Result := fItems.IndexOf(aItem);
end;

procedure TxnList<T>.Insert(aIndex: integer; aItem: T);
begin
  fItems.Insert(aIndex, aItem);
end;

procedure TxnList<T>.Sort;
begin
  fItems.Sort;
  // ObserverNotify(naFill, -1);
end;

procedure TxnList<T>.Sort(const aComparer: IComparer<T>);
begin
  fItems.Sort(aComparer);
  // ObserverNotify(naFill, -1);
end;

end.
