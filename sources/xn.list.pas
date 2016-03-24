unit xn.list;

interface

uses System.Generics.Collections, System.Generics.Defaults;

type
  IxnList<T> = interface
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    procedure Add(aItem: T);
    function Remove(aItem: T): integer;
    procedure Clear;
    procedure Delete(aIndex: integer);
    procedure Sort; overload;
    procedure Sort(const aComparer: IComparer<T>); overload;
    function IndexOf(aItem: T): integer;
    function Contains(aItem: T): boolean;
    function Count: integer;
    function GetEnumerator: TEnumerator<T>;
    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnList<T> = class(TInterfacedObject, IxnList<T>)
  strict protected
    fItems: TList<T>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(aItem: T); virtual;
    function Remove(aItem: T): integer; virtual;
    procedure Clear; virtual;
    procedure Delete(aIndex: integer); virtual;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;
    function IndexOf(aItem: T): integer; virtual;
    function Contains(aItem: T): boolean; virtual;
    function Count: integer; virtual;
    function GetEnumerator: TEnumerator<T>;
    function ItemGet(aIndex: integer): T; virtual;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

{ TxnList<T> }

procedure TxnList<T>.Add(aItem: T);
begin
  fItems.Add(aItem);
end;

function TxnList<T>.Remove(aItem: T): integer;
begin
  Result := fItems.Remove(aItem);
end;

procedure TxnList<T>.Clear;
begin
  fItems.Clear;
end;

procedure TxnList<T>.Delete(aIndex: integer);
begin
  fItems.Delete(aIndex);
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
end;

destructor TxnList<T>.Destroy;
begin
  Clear;
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

procedure TxnList<T>.Sort;
begin
  fItems.Sort;
end;

procedure TxnList<T>.Sort(const aComparer: IComparer<T>);
begin
  fItems.Sort(aComparer);
end;

end.
