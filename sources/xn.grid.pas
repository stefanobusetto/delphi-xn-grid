unit xn.grid;

interface

uses Generics.Collections,
  Vcl.Styles, Vcl.Controls, Vcl.Grids, Vcl.Themes, Vcl.Graphics, Vcl.GraphUtil, Vcl.ExtCtrls,
  System.SysUtils, System.Classes, System.Math, System.UITypes, Windows,
  xn.grid.data, xn.grid.common, xn.grid.link.sample;

type
  TxnGridColumn = class(TCollectionItem)
  strict private
    fAlignment: TAlignment;
    fCaption: string;
    fWidth: Integer;
  protected
    procedure SetIndex(aIndex: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read fAlignment write fAlignment;
    property Width: Integer read fWidth write fWidth;
    property Caption: String read fCaption write fCaption;
  end;

  TxnGridColumns = class(TOwnedCollection)
  strict private
    function ItemGet(aIndex: Integer): TxnGridColumn;
    procedure ItemSet(aIndex: Integer; aValue: TxnGridColumn);
  private
    fNotify: TxnGridColNotify;
    function CaptionGet(aIndex: Integer): string;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Items[aIndex: Integer]: TxnGridColumn read ItemGet write ItemSet; default;
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

    procedure NotifyCol(fData: TxnGridNotifyData);
    procedure NotifyRow(fData: TxnGridNotifyData);

    procedure OnSelectCell_(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);

    procedure RowCountSet(aValue: Integer);
    procedure ColCountSet(aValue: Integer);
    procedure LinkSet(aValue: IxnGridLink);
    function LinkGet: IxnGridLink;

  protected
    procedure InvalidateRowsFrom(aIndex: Integer);
    procedure InvalidateColsFrom(aIndex: Integer);

    procedure DrawCell(aCol, aRow: Integer; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InvalidateGrid;
    procedure InvalidateRow(aRow: Integer);
    procedure InvalidateCol(aCol: Integer);
    procedure InvalidateCell(aCol, aRow: Integer);

    procedure OnRecNo(aIndex: Integer);
  published
    property Log: TStrings read LogGet write LogSet;
    property Columns: TxnGridColumns read ColumnsGet write ColumnsSet;
    property OptionsEditing: Boolean read OptionsEditingGet write OptionsEditingSet;

    property link: IxnGridLink read LinkGet write LinkSet;
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

// function TxnGrid.ColCountGet: Integer;
// begin
// Result := inherited ColCount
// end;

procedure TxnGrid.ColCountSet(aValue: Integer);
begin
  if aValue <> RowCount then
  begin
    if aValue < 1 then
      aValue := 1;

    inherited ColCount := aValue;
    FixedCols := IfThen(aValue > 1, 1, 0);

    ColWidths[0] := 11;
  end;
end;

constructor TxnGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnSelectCell := OnSelectCell_;

  DefaultDrawing := True;

  fLog := nil;
  fLink := nil;

  fColumns := TxnGridColumns.Create(Self, TxnGridColumn);

  Font.Size := 10;

  DefaultColWidth := 40;
  DefaultRowHeight := 20; // Canvas.TextHeight('Wg') * 2 ;

  ColCountSet(1);
  if fLink = nil then
    RowCountSet(1)
  else
    RowCountSet(fLink.RowCount());

  fColumns.fNotify := NotifyCol;
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

procedure TxnGrid.DrawCell(aCol, aRow: Integer; ARect: TRect; AState: TGridDrawState);
var
  r: TRect;
  v: string;
  s: Integer;
  f: Cardinal;

  pc: TColor;
  bc: TColor;

  triangle: array [0 .. 2] of TPoint;
const
  spacing = 4;

  // LStyle: TCustomStyleServices;
  // LColor: TColor;
  // LineColor: TColor;
  // LFixedColor: TColor;
  // LFixedBorderColor: TColor;

begin
  if aCol < 0 then
    exit;
  if aRow < 0 then
    exit;

  if fLink = nil then
    v := ''
  else
  begin
    if aRow = 0 then
      v := fColumns.CaptionGet(aCol - 1)
    else
      v := fLink.ValueString(aCol - 1, aRow - 1);
  end;

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

  if aRow = 0 then
    Canvas.Font.Size := Font.Size - 2;

  r := ARect;
  r.Top := r.Top + 2;
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

  // if aRow = 0 then
  // begin
  //
  // end;

  // Canvas.Brush.Color := clRed;
  // Canvas.FillRect(r) ;

  // DrawCellBackground(ARect, clGray, [gdFixed], aCol, aRow);

  if fLink = nil then
    s := -1
  else
    s := fLink.RecNo + 1;

  if aCol = 0 then
    if aRow = 0 then
      v := ''
    else
      if aRow = s then
      v := '>'
    else
      v := '';

  // begin
  // bc := Canvas.Brush.Color;
  // pc := Canvas.Pen.Color;
  // triangle[0] := TPoint.Create(ARect.Left + spacing, ARect.Top + spacing);
  // triangle[1] := TPoint.Create(ARect.Left + spacing, ARect.Top + ARect.Height - spacing);
  // triangle[2] := TPoint.Create(ARect.Left + ARect.Width - spacing, ARect.Top + ARect.Height div 2);
  //
  // Canvas.Pen.Color := clBlack;
  // Canvas.Brush.Color := clBlack;
  // Canvas.Polygon(triangle);
  // Canvas.FloodFill(ARect.Left + ARect.Width div 2, ARect.Top + ARect.Height div 2, clBlack, fsSurface);
  //
  // Canvas.Pen.Color := pc;
  // Canvas.Brush.Color := bc;
  // end
  // end
  // else

  // if aCol = 0 then

  DrawText(Canvas.Handle, PChar(v), Length(v), r, f);

end;

procedure TxnGrid.InvalidateCell(aCol, aRow: Integer);
begin
  LogString(Format('TxnGrid.InvalidateCell(%d,%d);', [aCol, aRow]));

  inherited InvalidateCell(aCol, aRow);
end;

procedure TxnGrid.InvalidateCol(aCol: Integer);
begin
  LogString(Format('TxnGrid.InvalidateCol(%d);', [aCol]));

  inherited InvalidateCol(aCol);
end;

procedure TxnGrid.InvalidateGrid;
begin
  LogString(Format('TxnGrid.InvalidateGrid();', []));

  inherited InvalidateGrid;
end;

procedure TxnGrid.InvalidateRow(aRow: Integer);
begin
  LogString(Format('TxnGrid.InvalidateRow(%d);', [aRow]));

  inherited InvalidateRow(aRow);
end;

procedure TxnGrid.InvalidateColsFrom(aIndex: Integer);
var
  c: Integer;
begin
  LogString(Format('TxnGrid.InvalidateColsFrom(%d);', [aIndex]));

  if aIndex >= LeftCol then
    if aIndex <= LeftCol + VisibleColCount - 1 then
      for c := aIndex to LeftCol + VisibleColCount - 1 do
        InvalidateCol(c);
end;

procedure TxnGrid.InvalidateRowsFrom(aIndex: Integer);
var
  r: Integer;
begin
  LogString(Format('TxnGrid.InvalidateRowsFrom(%d);', [aIndex]));

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

function TxnGrid.LinkGet: IxnGridLink;
begin
  Result := fLink;
end;

procedure TxnGrid.LinkSet(aValue: IxnGridLink);
begin
  LogString(Format('TxnGrid.LinkSet();', []));

  fLink := aValue;
  fLink.Notify := NotifyRow;
end;

function TxnGrid.LogGet: TStrings;
begin
  Result := fLog
end;

procedure TxnGrid.LogSet(aValue: TStrings);
begin
  fLog := aValue
end;

procedure TxnGrid.NotifyCol(fData: TxnGridNotifyData);
begin
  LogString(Format('TxnGrid.Col.%s(%d);', [xnGridEventKindDes(fData.Kind), fData.Col]));

  case fData.Kind of
    gekAdd:
      ColCountSet(fColumns.Count + 1);
    gekDel:
      FixedCols := 0; // IfThen(fColumns.Count > 0, 1, 0);
    gekEdit:
      ;
    gekMove:
      ;
  end;
end;

procedure TxnGrid.OnRecNo(aIndex: Integer);
begin
  LogString(Format('zzz TxnGrid.OnRecNo(%d);', [aIndex]));

  if fLink = nil then
    exit;

  if aIndex + 1 <> Row then
    Row := aIndex + 1;
end;

procedure TxnGrid.NotifyRow(fData: TxnGridNotifyData);
begin
  LogString(Format('TxnGrid.NotifyRow.%s(%d,%d);', [xnGridEventKindDes(fData.Kind), fData.Row, fData.Col]));

  if fLink = nil then
    exit;

  case fData.Kind of
    gekAdd:
      begin
        RowCountSet(fLink.RowCount + 1);
        InvalidateRowsFrom(fData.Row + 1);
        OnRecNo(fData.Row);
      end;
    gekDel:
      begin
        RowCountSet(fLink.RowCount + 1);
        InvalidateRowsFrom(fData.Row + 1);
      end;
    gekEdit:
      begin
        InvalidateRow(fData.Row + 1);
      end;
    gekMove:
      begin
        InvalidateRow(fData.Row);
        OnRecNo(fData.Row);
      end;
  end;
end;

function TxnGrid.OptionsEditingGet:
  Boolean;
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

// function TxnGrid.RowCountGet: Integer;
// begin
// Result := inherited RowCount
// end;

procedure TxnGrid.RowCountSet(aValue: Integer);
begin
  LogString(Format('TxnGrid.RowCountSet(%d);', [aValue]));

  if aValue <> RowCount then
  begin
    if aValue < 2 then
      aValue := 2;

    inherited RowCount := aValue;
    FixedRows := IfThen(aValue > 1, 1, 0);

    RowHeights[0] := 17;
  end;
end;

procedure TxnGrid.OnSelectCell_(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  LogString(Format('TxnGrid.OnSelectCell(%d.%d).Outer', [aCol, aRow]));

  if fLink = nil then
    exit;

  if aRow - 1 <> fLink.RecNoGet() then
  begin
    LogString(Format('TxnGrid.OnSelectCell(%d,%d,%d).Inner', [aCol, aRow, fLink.RecNoGet()]));

    InvalidateCell(0, Row);
    InvalidateCell(0, aRow);
    fLink.RecNoSet(aRow - 1);
  end;
end;

{ TxnGridColumns }

function TxnGridColumns.CaptionGet(aIndex: Integer): string;
begin
  if aIndex < 0 then
    exit('');
  if aIndex >= Count then
    exit('');

  Result := TxnGridColumn(inherited GetItem(aIndex)).Caption;
end;

constructor TxnGridColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  fNotify := nil;
end;

function TxnGridColumns.ItemGet(aIndex: Integer): TxnGridColumn;
begin
  Result := TxnGridColumn(inherited GetItem(aIndex));
end;

procedure TxnGridColumns.ItemSet(aIndex: Integer; aValue: TxnGridColumn);
begin
  inherited SetItem(aIndex, aValue);
end;

{ TxnGridColumn }

constructor TxnGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  if Collection <> nil then
    if Collection is TxnGridColumns then
      if Assigned(TxnGridColumns(Collection).fNotify) then
        TxnGridColumns(Collection).fNotify(xnGridNotifyDataCreateColEvent(Self.Index, gekAdd));
end;

destructor TxnGridColumn.Destroy;
begin
  if Collection <> nil then
    if Collection is TxnGridColumns then
      if Assigned(TxnGridColumns(Collection).fNotify) then
        TxnGridColumns(Collection).fNotify(xnGridNotifyDataCreateColEvent(Self.Index, gekDel));

  inherited Destroy;
end;

procedure TxnGridColumn.SetIndex(aIndex: Integer);
var
  IndexOld: Integer;
  IndexNew: Integer;
begin
  IndexOld := Index;
  IndexNew := aIndex;
  if (IndexOld >= 0) and (IndexOld <> IndexNew) then
  begin
    inherited SetIndex(IndexNew);

    if Collection <> nil then
      if Collection is TxnGridColumns then
        if Assigned(TxnGridColumns(Collection).fNotify) then
        begin
          TxnGridColumns(Collection).fNotify(xnGridNotifyDataCreateColEvent(IndexOld, gekEdit));
          TxnGridColumns(Collection).fNotify(xnGridNotifyDataCreateColEvent(IndexNew, gekEdit));
        end;
  end;
end;

end.
