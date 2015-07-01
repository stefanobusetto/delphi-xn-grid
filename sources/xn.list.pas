unit xn.list;

interface

uses System.Generics.Collections, System.Generics.Defaults;

type
  IxnList<T> = interface
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    procedure Add(aItem: T);
    procedure Remove(aItem: T);
    procedure Clear;
    function Contains(aItem: T): boolean;
    function IndexOf(aItem: T): integer;
    function Count: integer;
    function ItemGet(aIndex: integer): T;
    function GetEnumerator: TEnumerator<T>;
    procedure Sort; overload;
    procedure Sort(const aComparer: IComparer<T>); overload;

    procedure Exchange(aIndex1, aIndex2: integer);
    procedure Delete(aIndex: integer);

    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnList<T> = class(TInterfacedObject, IxnList<T>)
  strict private
    fItems: TList<T>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(aItem: T); virtual;
    procedure Remove(aItem: T); virtual;
    procedure Clear;
    function Contains(aItem: T): boolean; virtual;
    function IndexOf(aItem: T): integer; virtual;
    function Count: integer; virtual;
    function ItemGet(aIndex: integer): T; virtual;
    function GetEnumerator: TEnumerator<T>;
    procedure Sort; overload; virtual;
    procedure Sort(const aComparer: IComparer<T>); overload; virtual;

    procedure Exchange(aIndex1, aIndex2: integer);
    procedure Delete(aIndex: integer);

    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

{ TxnList<T> }

procedure TxnList<T>.Add(aItem: T);
begin
  fItems.Add(aItem);
end;

procedure TxnList<T>.Clear;
begin
  fItems.Clear;
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

procedure TxnList<T>.Delete(aIndex: integer);
begin
  fItems.Delete(aIndex);
end;

destructor TxnList<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TxnList<T>.Exchange(aIndex1, aIndex2: integer);
begin
  fItems.Exchange(aIndex1, aIndex2);
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

procedure TxnList<T>.Remove(aItem: T);
begin
  fItems.Remove(aItem);
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
