unit xn.Items;

interface

uses System.Generics.Collections, System.Generics.Defaults;

type
  IxnItems<T> = interface
    ['{74A8B69A-6A88-4CCF-9B8B-15DDDD05EB6F}']
    function GetEnumerator: TEnumerator<T>;

    function Count: integer;
    function ItemGet(aIndex: integer): T;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

  TxnItems<T> = class(TInterfacedObject, IxnItems<T>)
  strict protected
    fItems: TList<T>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; virtual;

    function Count: integer; virtual;
    function ItemGet(aIndex: integer): T; virtual;
    property Items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

{ TxnItems<T> }

function TxnItems<T>.Count: integer;
begin
  Result := fItems.Count;
end;

constructor TxnItems<T>.Create;
begin
  fItems := TList<T>.Create;
end;

destructor TxnItems<T>.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TxnItems<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fItems.GetEnumerator;
end;

function TxnItems<T>.ItemGet(aIndex: integer): T;
begin
  Result := fItems[aIndex];
end;

end.
