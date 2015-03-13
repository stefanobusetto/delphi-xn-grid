unit xn.grid;

interface

uses Generics.Collections, // Vcl.Dialogs,
  Vcl.Controls, Windows, SysUtils, System.Classes, Vcl.Grids,
  xn.grid.link;

type
  TxnGrid = class;

  TxnGridLink = class(TInterfacedObject, IxnGridLink)
  strict private
    fGrid: TxnGrid;
    fItems: TList<string>;
  public
    constructor Create(aGrid: TxnGrid);
    destructor Destroy; override;
    procedure Append(aString: string);
    procedure Insert(aIndex: integer; aString: string);
    procedure Change(aIndex: integer; aString: string);
    procedure Delete(aIndex: integer);

    function RowCount: LongInt;
    function AsDebug: string;
    function ValueString(aCol, aRow: LongInt): String;
    function ValueFloat(aCol, aRow: LongInt): Double;
  end;

  TxnGridColumnNotify = procedure(aIndex: integer) of object;
  TxnGridRowNotify = procedure(aIndex: integer) of object;

  TxnGridColumn = class(TCollectionItem)
  strict private
    fAlignment: TAlignment;
    fCaption: string;
    fWidth: integer;
  protected
    procedure SetIndex(aIndex: integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read fAlignment write fAlignment;
    property Width: integer read fWidth write fWidth;
    property Caption: String read fCaption write fCaption;
  end;

  TxnGridColumns = class(TOwnedCollection)
  strict private
    function ItemGet(aIndex: integer): TxnGridColumn;
    procedure ItemSet(aIndex: integer; aValue: TxnGridColumn);
  private
    fOnColAdd: TxnGridColumnNotify;
    fOnColDelete: TxnGridColumnNotify;
    fOnColChange: TxnGridColumnNotify;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Items[aIndex: integer]: TxnGridColumn read ItemGet write ItemSet; default;
  end;

  TxnGrid = class(TCustomDrawGrid)
  private
    fLink: IxnGridLink;
    fColumns: TxnGridColumns;
    procedure OptionsEditingSet(aValue: Boolean);
    function OptionsEditingGet: Boolean;
    procedure ColumnsSet(aValue: TxnGridColumns);
    function ColumnsGet: TxnGridColumns;
  protected
    procedure OnColAdd(aIndex: integer);
    procedure OnColDelete(aIndex: integer);
    procedure OnColChange(aIndex: integer);

    procedure OnRowAdd(aIndex: integer);
    procedure OnRowDelete(aIndex: integer);
    procedure OnRowChange(aIndex: integer);

    procedure InvalidateRowsFrom(aIndex: integer);
    procedure InvalidateColsFrom(aIndex: integer);

    procedure DrawCell(aCol, aRow: LongInt; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ColCountGet: integer;
    function RowCountGet: integer;
    procedure ForceInvalidateCell(aCol, aRow: integer);
  published

    property Columns: TxnGridColumns read ColumnsGet write ColumnsSet;
    property OptionsEditing: Boolean read OptionsEditingGet write OptionsEditingSet;

    property link: IxnGridLink read fLink;
  end;

  // TxnStringGrid = class(TStringGrid)
  // private
  // FData: TCustomData;
  // FColsDefaultAlignment: TStringList;
  //
  // function GetColsDefaultAlignment(ACol: integer): TAlignment;
  // procedure SetColsDefaultAlignment(ACol: integer; const Alignment: TAlignment);
  // protected
  // procedure DrawCell(ACol, ARow: Longint; aRect: TRect; aState: TGridDrawState); override;
  // public
  // constructor Create(AOwner: TComponent); override;
  // destructor Destroy; override;
  // property ColsDefaultAlignment[ACol: integer]: TAlignment read GetColsDefaultAlignment write SetColsDefaultAlignment;
  // end;

implementation

// constructor TxnStringGrid.Create(AOwner: TComponent);
// begin
// inherited Create(AOwner);
// FColsDefaultAlignment := TStringList.Create;
// FColsDefaultAlignment.CaseSensitive := True;
// FColsDefaultAlignment.Sorted := True;
// FColsDefaultAlignment.Duplicates := dupIgnore;
// end;
//
// destructor TxnStringGrid.Destroy;
// begin
// FColsDefaultAlignment.Free;
// inherited Destroy;
// end;
//
// procedure TxnStringGrid.SetColsDefaultAlignment(ACol: integer; const Alignment: TAlignment);
// var
// Index: integer;
// begin
// Index := FColsDefaultAlignment.IndexOf(IntToStr(ACol));
// if -1 < Index
// then
// FColsDefaultAlignment.Objects[Index] := TObject(Alignment)
// else
// FColsDefaultAlignment.AddObject(IntToStr(ACol), TObject(Alignment))
// end;
//
// function TxnStringGrid.GetColsDefaultAlignment(ACol: integer): TAlignment;
// var
// Index: integer;
// begin
// Index := FColsDefaultAlignment.IndexOf(IntToStr(ACol));
// if -1 < Index
// then
// GetColsDefaultAlignment := TAlignment(FColsDefaultAlignment.Objects[Index])
// else
// GetColsDefaultAlignment := taLeftJustify;
// end;
//
// procedure TxnStringGrid.DrawCell(ACol, ARow: Longint; aRect: TRect; aState: TGridDrawState);
// var
// d: Boolean;
// v: string;
// f: Cardinal;
// begin
// if DefaultDrawing then
// begin
// v := Cells[ACol, ARow];
//
// if True then
// f := DT_WORDBREAK
// else
// f := DT_SINGLELINE or DT_VCENTER;
//
// case ColsDefaultAlignment[ACol] of
// taLeftJustify:
// f := f or DT_LEFT;
// taRightJustify:
// f := f or DT_RIGHT;
// taCenter:
// f := f or DT_CENTER;
// end;
//
// DrawText(Canvas.Handle, PChar(v), Length(v), aRect, f);
// end;
//
// d := DefaultDrawing;
// DefaultDrawing := False;
// inherited DrawCell(ACol, ARow, aRect, aState);
// DefaultDrawing := d;
// end;

{ TxnStringGrid1 }

function TxnGrid.ColumnsGet: TxnGridColumns;
begin
  Result := fColumns;
end;

procedure TxnGrid.ColumnsSet(aValue: TxnGridColumns);
begin
  fColumns.Assign(aValue);
end;

function TxnGrid.ColCountGet: integer;
begin
  Result := ColCount
end;

constructor TxnGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLink := TxnGridLink.Create(Self);
  fColumns := TxnGridColumns.Create(Self, TxnGridColumn);
  ColCount := 0;
  RowCount := 0;

  RowCount := fLink.RowCount();

  fColumns.fOnColAdd := OnColAdd;
  fColumns.fOnColDelete := OnColDelete;
  fColumns.fOnColChange := OnColChange;
end;

destructor TxnGrid.Destroy;
begin
  fColumns.Free;
  inherited Destroy;
end;

procedure TxnGrid.DrawCell(aCol, aRow: integer; ARect: TRect; AState: TGridDrawState);
var
  v: string;
  f: Cardinal;
begin
  v := fLink.ValueString(aCol, aRow);

  if True then
    f := DT_WORDBREAK
  else
    f := DT_SINGLELINE or DT_VCENTER;

  // case ColsDefaultAlignment[aCol] of
  // taLeftJustify:
  // f := f or DT_LEFT;
  // taRightJustify:
  // f := f or DT_RIGHT;
  // taCenter:
  // f := f or DT_CENTER;
  // end;
  DrawText(Canvas.Handle, PChar(v), Length(v), ARect, f);

end;

procedure TxnGrid.ForceInvalidateCell(aCol, aRow: integer);
begin
  InvalidateCell(aCol, aRow);
end;

procedure TxnGrid.InvalidateColsFrom(aIndex: integer);
var
  c: integer;
begin
  if aIndex >= LeftCol then
    if aIndex <= LeftCol + VisibleColCount - 1 then
      for c := aIndex to LeftCol + VisibleColCount - 1 do
        InvalidateCol(c);
end;

procedure TxnGrid.InvalidateRowsFrom(aIndex: integer);
var
  r: integer;
begin
  if aIndex >= TopRow then
    if aIndex <= TopRow + VisibleRowCount - 1 then
      for r := aIndex to TopRow + VisibleRowCount - 1 do
        InvalidateRow(r);
end;

procedure TxnGrid.OnColAdd(aIndex: integer);
begin
  // ShowMessage('OnColAdd ' + IntToStr(aIndex));
  ColCount := fColumns.Count;
end;

procedure TxnGrid.OnColChange(aIndex: integer);
begin
  // ShowMessage('OnColChange ' + IntToStr(aIndex));
end;

procedure TxnGrid.OnColDelete(aIndex: integer);
begin
  // ShowMessage('OnColDelete ' + IntToStr(aIndex));
  ColCount := fColumns.Count - 1;
end;

procedure TxnGrid.OnRowAdd(aIndex: integer);
begin
  RowCount := fLink.RowCount;
  InvalidateRowsFrom(aIndex);
end;

procedure TxnGrid.OnRowChange(aIndex: integer);
begin
  InvalidateRow(aIndex);
end;

procedure TxnGrid.OnRowDelete(aIndex: integer);
begin
  RowCount := fLink.RowCount;
  InvalidateRowsFrom(aIndex);
end;

function TxnGrid.OptionsEditingGet: Boolean;
begin
  Result := goEditing in Options;
end;

procedure TxnGrid.OptionsEditingSet(aValue: Boolean);
begin
  if aValue then
    Options := Options + [goEditing]
  else
    Options := Options - [goEditing];
end;

function TxnGrid.RowCountGet: integer;
begin
  Result := RowCount
end;

{ TxnGridColumns }

constructor TxnGridColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  fOnColAdd := nil;
  fOnColDelete := nil;
  fOnColChange := nil;
end;

// function TxnGridColumns.IndexOf(aCol: TxnGridColumn): integer;
// var
// i: integer;
// begin
// for i := 0 to Count - 1 do
// if Items[i] = aCol then
// Exit(i);
// Exit(-1);
// end;

function TxnGridColumns.ItemGet(aIndex: integer): TxnGridColumn;
begin
  Result := TxnGridColumn(inherited GetItem(aIndex));
end;

procedure TxnGridColumns.ItemSet(aIndex: integer; aValue: TxnGridColumn);
begin
  inherited SetItem(aIndex, aValue);
end;

{ TxnGridLink }

procedure TxnGridLink.Append(aString: string);
begin
  fItems.Add(aString);
  fGrid.OnRowAdd(fItems.Count);
end;

procedure TxnGridLink.Insert(aIndex: integer; aString: string);
begin
  fItems.Insert(aIndex, aString);
  fGrid.OnRowAdd(aIndex);
end;

procedure TxnGridLink.Delete(aIndex: integer);
begin
  fItems.Delete(aIndex);
  fGrid.OnRowDelete(aIndex);
end;

function TxnGridLink.AsDebug: string;
begin
  raise Exception.Create('TxnGridLink.AsDebug');
end;

procedure TxnGridLink.Change(aIndex: integer; aString: string);
begin
  fItems[aIndex] := aString;
  fGrid.OnRowChange(aIndex);
end;

constructor TxnGridLink.Create(aGrid: TxnGrid);
begin
  Assert(aGrid <> nil);
  fGrid := aGrid;

  fItems := TList<string>.Create;
end;

destructor TxnGridLink.Destroy;
begin
  fItems.Clear;
  fItems.Free;
  inherited;
end;

function TxnGridLink.RowCount: LongInt;
begin
  Result := fItems.Count;
end;

function TxnGridLink.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TxnGridLink.ValueString(aCol, aRow: LongInt): String;
begin
  if aRow < 0 then
    Exit('');
  if aRow >= fItems.Count then
    Exit('');

  Result := fItems[aRow] + '(' + aCol.ToString() + '.' + aRow.ToString() + ')'
end;

{ TxnGridColumn }

constructor TxnGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  if Collection <> nil then
    if Collection is TxnGridColumns then
      if Assigned(TxnGridColumns(Collection).fOnColAdd) then
        TxnGridColumns(Collection).fOnColAdd(Self.Index);
end;

destructor TxnGridColumn.Destroy;
begin
  if Collection <> nil then
    if Collection is TxnGridColumns then
      if Assigned(TxnGridColumns(Collection).fOnColDelete) then
        TxnGridColumns(Collection).fOnColDelete(Self.Index);

  inherited Destroy;
end;

procedure TxnGridColumn.SetIndex(aIndex: integer);
var
  IndexOld: integer;
  IndexNew: integer;
begin
  IndexOld := Index;
  IndexNew := aIndex;
  if (IndexOld >= 0) and (IndexOld <> IndexNew) then
  begin
    inherited SetIndex(IndexNew);

    if Collection <> nil then
      if Collection is TxnGridColumns then
        if Assigned(TxnGridColumns(Collection).fOnColChange) then
        begin
          TxnGridColumns(Collection).fOnColChange(IndexOld);
          TxnGridColumns(Collection).fOnColChange(IndexNew);
        end;
  end;
end;

end.
