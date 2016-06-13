unit xn.dictionary;

interface

uses System.Generics.Collections;

type
  IxnDictionary<K, V> = interface
    ['{4953A5E2-38BD-4898-B85D-7A1157739D6A}']
    procedure Clear;
    procedure Add(const aKey: K; const aValue: V);

    function Count: integer;
    function ContainsKey(const aKey: K): Boolean;
    function ContainsValue(const aValue: V): Boolean;

    function GetEnumerator: TDictionary<K, V>.TPairEnumerator;

    function GetKeys: TEnumerable<K>;
    property Keys: TEnumerable<K> read GetKeys;

    function GetValues: TEnumerable<V>;
    property Values: TEnumerable<V> read GetValues;

    function GetItem(const aKey: K): V;
    procedure SetItem(const aKey: K; const aValue: V);
    property Items[const aKey: K]: V read GetItem write SetItem; default;
  end;

  TxnDictionary<K, V> = class(TInterfacedObject, IxnDictionary<K, V>)
  strict protected
    fDictionary: TDictionary<K, V>;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const aKey: K; const aValue: V);

    function Count: integer;
    function ContainsKey(const aKey: K): Boolean;
    function ContainsValue(const aValue: V): Boolean;

    function GetEnumerator: TDictionary<K, V>.TPairEnumerator;

    function GetKeys: TEnumerable<K>;
    property Keys: TEnumerable<K> read GetKeys;

    function GetValues: TEnumerable<V>;
    property Values: TEnumerable<V> read GetValues;

    function GetItem(const aKey: K): V;
    procedure SetItem(const aKey: K; const aValue: V);
    property Items[const aKey: K]: V read GetItem write SetItem; default;
  end;

implementation

{ TxnDictionary<K, V> }

procedure TxnDictionary<K, V>.Add(const aKey: K; const aValue: V);
begin
  fDictionary.Add(aKey, aValue);
end;

procedure TxnDictionary<K, V>.Clear;
begin
  fDictionary.Clear;
end;

function TxnDictionary<K, V>.ContainsKey(const aKey: K): Boolean;
begin
  Result := fDictionary.ContainsKey(aKey)
end;

function TxnDictionary<K, V>.ContainsValue(const aValue: V): Boolean;
begin
  Result := fDictionary.ContainsValue(aValue)
end;

function TxnDictionary<K, V>.Count: integer;
begin
  Result := fDictionary.Count;
end;

constructor TxnDictionary<K, V>.Create;
begin
  fDictionary := TDictionary<K, V>.Create;
end;

destructor TxnDictionary<K, V>.Destroy;
begin
  fDictionary.Free;
  inherited;
end;

function TxnDictionary<K, V>.GetEnumerator: TDictionary<K, V>.TPairEnumerator;
begin
  Result := fDictionary.GetEnumerator;
end;

function TxnDictionary<K, V>.GetItem(const aKey: K): V;
begin
  Result := fDictionary.Items[aKey]
end;

function TxnDictionary<K, V>.GetKeys: TEnumerable<K>;
begin
  Result := fDictionary.Keys;
end;

function TxnDictionary<K, V>.GetValues: TEnumerable<V>;
begin
  Result := fDictionary.Values;
end;

procedure TxnDictionary<K, V>.SetItem(const aKey: K; const aValue: V);
begin
  fDictionary.Items[aKey] := aValue;
end;

end.
