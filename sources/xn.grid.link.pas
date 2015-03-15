unit xn.grid.link;

interface

uses xn.grid.common, xn.grid.data ;

type
  TxnGridLink = class(TInterfacedObject, IxnGridData)
  private
    fDataSort: TxnGridDataSort;
    fDataFilter: TxnGridDataFilter;
  public
    constructor Create(aGridData: IxnGridData; aFilterItems: IxnGridFilterItems; aSortItems: IxnGridSortItems);
    destructor Destroy; override;
    procedure Sort; overload;
    function Seek1(aKeys: TArray<variant>): integer;
    function Seek2(aKeys: TArray<variant>): integer;

    procedure Clear; virtual;
    procedure Fill; virtual;
    function RowCount: LongInt; virtual;
    function AsDebug: string; virtual;
    function ValueString(aCol, aRow: LongInt): String; virtual;
    function ValueFloat(aCol, aRow: LongInt): Double; virtual;
  end;

implementation

function TxnGridLink.AsDebug: string;
begin
  Result := fDataSort.AsDebug;
end;

procedure TxnGridLink.Clear;
begin
  fDataSort.Clear;
  fDataFilter.Clear;
end;

constructor TxnGridLink.Create(aGridData: IxnGridData; aFilterItems: IxnGridFilterItems; aSortItems: IxnGridSortItems);
begin
  fDataFilter := TxnGridDataFilter.Create(aGridData, aFilterItems);
  fDataSort := TxnGridDataSort.Create(fDataFilter, aSortItems);
end;

destructor TxnGridLink.Destroy;
begin
  fDataSort.Free;
  inherited;
end;

procedure TxnGridLink.Fill;
begin
  fDataFilter.Fill;
  fDataSort.Fill;
end;

function TxnGridLink.RowCount: LongInt;
begin
  Result := fDataSort.RowCount;
end;

function TxnGridLink.Seek1(aKeys: TArray<variant>): integer;
begin
  Result := fDataSort.Seek1(aKeys);
end;

function TxnGridLink.Seek2(aKeys: TArray<variant>): integer;
begin
  Result := fDataSort.Seek2(aKeys);
end;

procedure TxnGridLink.Sort;
begin
  fDataSort.Sort;
end;

function TxnGridLink.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := fDataSort.ValueFloat(aCol, aRow)
end;

function TxnGridLink.ValueString(aCol, aRow: integer): String;
begin
  Result := fDataSort.ValueString(aCol, aRow)
end;

end.
