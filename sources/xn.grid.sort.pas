unit xn.grid.sort;

interface

uses Generics.Collections, Generics.Defaults,
  xn.grid.link, cSampleData;

type
  TxnGridSortItem = record
  type
    TKind = (xstAsc, xstDesc);
  private
    fIndex: integer;
    fKind: TKind;
  public
    constructor Create(aIndex: integer; aKind: TKind);
  end;

  TxnGridSortItems = class(TList<TxnGridSortItem>)
  end;

  TxnGridLinkSort = class(TInterfacedObject, IxnGridData)
  strict private
    fIndex: TList<integer>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ordina(aItems: TxnGridSortItems);
    function RowCount: LongInt;
    function AsDebug: string;
    function Value(aCol, aRow: LongInt): String;
  end;

implementation

uses System.SysUtils;

{ TxnGridLinkSort }

function TxnGridLinkSort.AsDebug: string;
var
  r: integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + Value(0, r) + ','
end;

function TxnGridLinkSort.RowCount: LongInt;
begin
  Result := TSampleData.Length
end;

constructor TxnGridLinkSort.Create;
var
  i: integer;
begin
  fIndex := TList<integer>.Create;

  for i := 0 to RowCount - 1 do
    fIndex.Add(i);
end;

destructor TxnGridLinkSort.Destroy;
begin
  fIndex.Free;
  inherited;
end;

procedure TxnGridLinkSort.Ordina(aItems: TxnGridSortItems);
var
  c: TComparison<integer>;
begin
  c := function(const Left, Right: integer): integer
    var
      l: string;
      r: string;
    begin
      l := Value(0, Left);
      r := Value(0, Right);

      if l < r then
        Result := -1
      else if r > l then
        Result := +1
      else
        Result := 0;
    end;

  fIndex.sort(TComparer<integer>.Construct(c));
end;

function TxnGridLinkSort.Value(aCol, aRow: integer): String;
begin
  Result := TSampleData.Value(aCol, fIndex[aRow]);
end;

{ TxnGridSortItem }

constructor TxnGridSortItem.Create(aIndex: integer; aKind: TKind);
begin
  fIndex := aIndex;
  fKind := aKind;
end;

end.
