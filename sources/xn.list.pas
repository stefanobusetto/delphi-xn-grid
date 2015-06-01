unit xn.list;

interface

uses System.Generics.Collections;

type
  // IxnList<T: record > = interface
  IxnList<T> = interface
    ['{D4CEE49C-1EEE-40BF-A136-C6B074CFA76B}']
    procedure Add(aItem: T);
    procedure Remove(aItem: T);
    function Contains(aItem: T): boolean;
    function Count: integer;
    function ItemGet(aIndex: integer): T;
    function GetEnumerator: TEnumerator<T>;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  // TxnList<T: record > = class(TInterfacedObject, IxnList<T>)
  TxnList<T> = class(TInterfacedObject, IxnList<T>)
  private
    fItems: TList<T>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aItem: T);
    procedure Remove(aItem: T);
    function Contains(aItem: T): boolean;
    function Count: integer;
    function ItemGet(aIndex: integer): T;
    function GetEnumerator: TEnumerator<T>;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

{ TxnList<T> }

procedure TxnList<T>.Add(aItem: T);
begin
  fItems.Add(aItem);
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
  fItems.Free;
  inherited;
end;

function TxnList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fItems.GetEnumerator;
end;

function TxnList<T>.ItemGet(aIndex: integer): T;
begin
  Result := fItems[aIndex];
end;

procedure TxnList<T>.Remove(aItem: T);
begin
  fItems.Remove(aItem);
end;

end.
