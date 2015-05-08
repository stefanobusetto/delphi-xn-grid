unit xn.grid;

interface

uses Generics.Collections,
  Vcl.Controls, Vcl.Grids, Vcl.Themes, Vcl.Graphics, Vcl.GraphUtil,
  System.SysUtils, System.Classes, System.Math, System.UITypes,
  Windows,
  xn.grid.data, xn.grid.common;

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
    fLog: TStrings;
    fColumns: TxnGridColumns;
    procedure OptionsEditingSet(aValue: Boolean);
    function OptionsEditingGet: Boolean;
    procedure ColumnsSet(aValue: TxnGridColumns);
    function ColumnsGet: TxnGridColumns;
    procedure LogSet(aValue: TStrings);
    function LogGet: TStrings;
    procedure LogString(aString: String);

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
    procedure InvalidateCell(aCol, aRow: integer);
    procedure InvalidateRow(aRow: integer);
    procedure InvalidateCol(aCol: integer);
    procedure InvalidateGrid;

    procedure RowCountSet(aValue: LongInt);
    function RowCountGet: LongInt;
    procedure ColCountSet(aValue: LongInt);
    function ColCountGet: LongInt;

    procedure LogLink;
  published
    property Log: TStrings read LogGet write LogSet;
    property Columns: TxnGridColumns read ColumnsGet write ColumnsSet;
    property OptionsEditing: Boolean read OptionsEditingGet write OptionsEditingSet;

    property Link: IxnGridLink read fLink;

    property ColCount: LongInt read ColCountGet write ColCountSet;
    property RowCount: LongInt read RowCountGet write RowCountSet;
  end;

implementation

{ TxnGrid }

function TxnGrid.ColumnsGet: TxnGridColumns;
begin
  Result := fColumns;
end;

procedure TxnGrid.ColumnsSet(aValue: TxnGridColumns);
begin
  fColumns.Assign(aValue);
end;

function TxnGrid.ColCountGet: LongInt;
begin
  Result := inherited ColCount
end;

procedure TxnGrid.ColCountSet(aValue: integer);
begin
  if aValue <> RowCount then
  begin
    inherited ColCount := aValue;
    FixedCols := IfThen(aValue > 1, 1, 0);
  end;
end;

constructor TxnGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultDrawing := True;

  fLog := nil;
  fLink := TxnGridLink.Create(Self);
  fColumns := TxnGridColumns.Create(Self, TxnGridColumn);
  ColCountSet(1);
  RowCountSet(1);

  DefaultColWidth := DefaultColWidth * 3 div 2 - 4;
  DefaultRowHeight := DefaultRowHeight * 3 div 2;

  RowCountSet(fLink.RowCount());

  fColumns.fOnColAdd := OnColAdd;
  fColumns.fOnColDelete := OnColDelete;
  fColumns.fOnColChange := OnColChange;
end;

destructor TxnGrid.Destroy;
begin
  fColumns.Free;
  inherited Destroy;
end;

function StyleServices: TCustomStyleServices;
begin
  Result := TStyleManager.ActiveStyle;
end;

procedure TxnGrid.DrawCell(aCol, aRow: integer; ARect: TRect; AState: TGridDrawState);
var
  r: TRect;
  v: string;
  f: Cardinal;

  LStyle: TCustomStyleServices;
  LColor: TColor;
  LineColor: TColor;
  LFixedColor: TColor;
  LFixedBorderColor: TColor;

begin
//  if aCol = 0 then
//    exit;
//  if aRow = 0 then
//    exit;

//  v := fLink.ValueString(aCol - 1, aRow - 1) + ' ' +
//    fLink.ValueString(aCol - 1, aRow - 1) + ' ' +
//    fLink.ValueString(aCol - 1, aRow - 1) + ' ' +
//    fLink.ValueString(aCol - 1, aRow - 1) + ' ';

    v:='x';


  {
    FInternalColor := Color;
    LStyle := StyleServices;

    if (FInternalDrawingStyle = gdsThemed) then
    begin
    LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecBorderColor, LineColor);
    if seClient in StyleElements then
    LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecFillColor, FInternalColor);
    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecBorderColor, LFixedBorderColor);
    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecFillColor, LFixedColor);
    end
    else
    begin
    if FInternalDrawingStyle = gdsGradient then
    begin
    LineColor := $F0F0F0;
    LFixedColor := Color;
    LFixedBorderColor := GetShadowColor($F0F0F0, -45);

    if LStyle.Enabled then
    begin
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientCellNormal), ecBorderColor, LColor) and (LColor <> clNone) then
    LineColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientCellNormal), ecFillColor, LColor) and (LColor <> clNone) then
    FInternalColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientFixedCellNormal), ecBorderColor, LColor) and (LColor <> clNone) then
    LFixedBorderColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientFixedCellNormal), ecFillColor, LColor) and (LColor <> clNone) then
    LFixedColor := LColor;
    end;
    end
    else
    begin
    LineColor := clSilver;
    LFixedColor := FixedColor;
    LFixedBorderColor := clBlack;

    if LStyle.Enabled then
    begin
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicCellNormal), ecBorderColor, LColor) and (LColor <> clNone) then
    LineColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicCellNormal), ecFillColor, LColor) and (LColor <> clNone) then
    FInternalColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicFixedCellNormal), ecBorderColor, LColor) and (LColor <> clNone) then
    LFixedBorderColor := LColor;
    if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicFixedCellNormal), ecFillColor, LColor) and (LColor <> clNone) then
    LFixedColor := LColor;
    end;
    end;
    end;
  }

  if False then
    f := DT_WORDBREAK
  else
    f := DT_SINGLELINE; // or DT_VCENTER;

  r := ARect;
  r.Top := r.Top + 0;
  r.Left := r.Left + 3;
  r.Right := r.Right - 2;
  r.Bottom := r.Bottom - 3;

  // case ColsDefaultAlignment[aCol] of
  // taLeftJustify:
  // f := f or DT_LEFT;
  // taRightJustify:
  // f := f or DT_RIGHT;
  // taCenter:
  // f := f or DT_CENTER;
  // end;

//  if aRow = 0 then
//  begin
DrawCellBackground(ARect, 0, [gdFixed], aCol, aRow);
//  end;

    Canvas.Brush.Color := clLtGray;
     Canvas.FillRect(r) ;

  DrawText(Canvas.Handle, PChar(v), Length(v), r, f);

end;

procedure TxnGrid.InvalidateCell(aCol, aRow: integer);
begin
  LogString(Format('InvalidateCell(%d,%d);', [aCol, aRow]));
  inherited InvalidateCell(aCol, aRow);
end;

procedure TxnGrid.InvalidateCol(aCol: integer);
begin
  LogString(Format('InvalidateCol(%d);', [aCol]));
  inherited InvalidateCol(aCol);
end;

procedure TxnGrid.InvalidateGrid;
begin
  LogString(Format('InvalidateGrid();', []));
  inherited InvalidateGrid;
end;

procedure TxnGrid.InvalidateRow(aRow: integer);
begin
  LogString(Format('InvalidateRow(%d);', [aRow - 1]));
  inherited InvalidateRow(aRow);
end;

procedure TxnGrid.InvalidateColsFrom(aIndex: integer);
var
  c: integer;
begin
  LogString(Format('InvalidateColsFrom(%d);', [aIndex - 1]));
  if aIndex >= LeftCol then
    if aIndex <= LeftCol + VisibleColCount - 1 then
      for c := aIndex to LeftCol + VisibleColCount - 1 do
        InvalidateCol(c);
end;

procedure TxnGrid.InvalidateRowsFrom(aIndex: integer);
var
  r: integer;
begin
  LogString(Format('InvalidateRowsFrom(%d);', [aIndex - 1]));
  if aIndex >= TopRow then
    if aIndex <= TopRow + VisibleRowCount - 1 then
      for r := aIndex to TopRow + VisibleRowCount - 1 do
        InvalidateRow(r);
end;

procedure TxnGrid.LogString(aString: String);
begin
  if fLog <> nil then
    fLog.add(aString);
end;

function TxnGrid.LogGet: TStrings;
begin
  Result := fLog
end;

procedure TxnGrid.LogLink;
begin
  LogString(fLink.AsDebug);
end;

procedure TxnGrid.LogSet(aValue: TStrings);
begin
  fLog := aValue
end;

procedure TxnGrid.OnColAdd(aIndex: integer);
begin
  ColCountSet(fColumns.Count + 1);
end;

procedure TxnGrid.OnColChange(aIndex: integer);
begin
end;

procedure TxnGrid.OnColDelete(aIndex: integer);
begin
  FixedCols := IfThen(fColumns.Count > 0, 1, 0);
end;

procedure TxnGrid.OnRowAdd(aIndex: integer);
begin
  LogString(Format('OnRowAdd(%d);', [aIndex]));
  RowCountSet(fLink.RowCount + 1);
  InvalidateRowsFrom(aIndex + 1);
end;

procedure TxnGrid.OnRowChange(aIndex: integer);
begin
  LogString(Format('OnRowChange(%d);', [aIndex]));
  InvalidateRow(aIndex + 1);
end;

procedure TxnGrid.OnRowDelete(aIndex: integer);
begin
  LogString(Format('OnRowDelete(%d);', [aIndex]));
  RowCountSet(fLink.RowCount + 1);
  InvalidateRowsFrom(aIndex + 1);
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

function TxnGrid.RowCountGet: LongInt;
begin
  Result := inherited RowCount
end;

procedure TxnGrid.RowCountSet(aValue: integer);
begin
  if aValue <> RowCount then
  begin
    inherited RowCount := aValue;
    FixedRows := IfThen(aValue > 1, 1, 0);

    // InvalidateRowsFrom(aValue);
  end;
end;

{ TxnGridColumns }

constructor TxnGridColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  fOnColAdd := nil;
  fOnColDelete := nil;
  fOnColChange := nil;
end;

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
  fItems.add(aString);
  fGrid.OnRowAdd(RowCount - 1);
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
var
  s: string;
begin
  Result := '';
  for s in fItems do
    Result := Result + s + '#'
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
    exit('');
  if aRow >= fItems.Count then
    exit('');

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
