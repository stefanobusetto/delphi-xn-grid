unit cSampleData;

interface

uses Generics.Collections, xn.grid.common;

type
  TSampleData = class
  private
    const
    TABLE_20: array [0 .. 19, 0 .. 5] of string = (
      ('0', 'Aqua', 'MD', '2,01', '71', 'T'),
      ('2', 'Yellow', 'LA', '3,76', '29', 'T'),
      ('4', 'DkGray', 'VT', '4,74', '47', 'T'),
      ('6', 'Teal', 'NE', '2,06', '6', 'F'),
      ('8', 'Red', 'LA', '2,82', '80', 'F'),
      ('10', 'Black', 'OH', '2,06', '6', 'T'),
      ('12', 'Lime', 'HI', '2,82', '32', 'F'),
      ('14', 'White', 'UT', '2,9', '75', 'T'),
      ('16', 'Lime', 'VT', '3,17', '75', 'F'),
      ('18', 'Olive', 'VA', '3,17', '88', 'F'),
      ('20', 'Gray', 'PA', '1,53', '25', 'F'),
      ('22', 'Red', 'WA', '2,82', '34', 'F'),
      ('24', 'White', 'RI', '4,04', '29', 'T'),
      ('26', 'Purple', 'MS', '3,17', '70', 'T'),
      ('28', 'Fuchsia', 'ND', '3,6', '77', 'F'),
      ('30', 'Silver', 'AR', '3,76', '25', 'F'),
      ('32', 'Maroon', 'MT', '3,94', '88', 'T'),
      ('34', 'LtGray', 'AZ', '3,94', '10', 'F'),
      ('36', 'Silver', 'NY', '3,76', '8', 'F'),
      ('38', 'Purple', 'HI', '2,56', '68', 'F'));
  public
    class function RowCount: Integer;
    class function Value(aCol, aRow: Integer): string;
  end;

  TSampleGridData = class(TInterfacedObject, IxnGridData)
  public
    function RowCount: Integer;
    function AsDebug: string;
    function ValueString(aCol, aRow: Integer): String;
    function ValueFloat(aCol, aRow: Integer): Double;
  end;

  TSampleGridData2By2 = class(TInterfacedObject, IxnGridData)
  private
    fItems: TList<Integer>;
    procedure Fill(aCount: Integer); virtual;
    procedure Rand; virtual;
  public
    constructor Create(aCount: Integer); virtual;
    destructor Destroy; override;
    function RowCount: Integer; virtual;
    function AsDebug: string; virtual;
    function ValueString(aCol, aRow: Integer): String; virtual;
    function ValueFloat(aCol, aRow: Integer): Double; virtual;
  end;

  TSampleGridDataRandom = class(TSampleGridData2By2)
  private
    procedure Fill(aCount: Integer); override;
  end;

  TSampleGridDataList = class(TInterfacedObject, IxnGridData)
  type
    TItem = record
      NN: String;
      CC: String;
      SS: String;
      FF: String;
      II: string;
      BB: string;
      constructor Create(aNN, aCC, aSS, aFF, aII, aBB: string);
    end;
  private
    fItems: TList<TItem>;
  public
    constructor Create;
    destructor Destroy; override;
    function RowCount: Integer;
    function AsDebug: string;
    function ValueString(aCol, aRow: Integer): String;
    function ValueFloat(aCol, aRow: Integer): Double;
  end;

implementation

uses System.Math, System.SysUtils;

{ TABLE_20
  0;Aqua;MD;2,01;71;T
  2;Yellow;LA;3,76;29;T
  4;DkGray;VT;4,74;47;T
  6;Teal;NE;2,06;6;F
  8;Red;LA;2,82;80;F
  10;Black;OH;2,06;6;T
  12;Lime;HI;2,82;32;F
  14;White;UT;2,9;75;T
  16;Lime;VT;3,17;75;F
  18;Olive;VA;3,17;88;F
  20;Gray;PA;1,53;25;F
  22;Red;WA;2,82;34;F
  24;White;RI;4,04;29;T
  26;Purple;MS;3,17;70;T
  28;Fuchsia;ND;3,6;77;F
  30;Silver;AR;3,76;25;F
  32;Maroon;MT;3,94;88;T
  34;LtGray;AZ;3,94;10;F
  36;Silver;NY;3,76;8;F
  38;Purple;HI;2,56;68;F
}

class function TSampleData.RowCount: Integer;
begin
  Result := Length(TABLE_20);
end;

class function TSampleData.Value(aCol, aRow: Integer): string;
begin
  Result := TABLE_20[aRow, aCol];
end;

{ TSampleGridData }

function TSampleGridData.AsDebug: string;
var
  r: Integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + ValueString(0, r) + ','
end;

function TSampleGridData.RowCount: Integer;
begin
  Result := TSampleData.RowCount
end;

function TSampleGridData.ValueFloat(aCol, aRow: Integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TSampleGridData.ValueString(aCol, aRow: Integer): String;
begin
  Result := TSampleData.Value(aCol, aRow);
end;

{ TSampleGridData10000 }

function TSampleGridData2By2.AsDebug: string;
var
  r: Integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + ValueString(0, r) + ','
end;

constructor TSampleGridData2By2.Create(aCount: Integer);
begin
  inherited Create;
  fItems := TList<Integer>.Create;
  Fill(aCount);
  Rand;
end;

destructor TSampleGridData2By2.Destroy;
begin
  fItems.Free;
end;

procedure TSampleGridData2By2.Fill(aCount: Integer);
var
  i: Integer;
begin
  for i := 0 to aCount - 1 do
    fItems.Add(2 * i);
end;

procedure TSampleGridData2By2.Rand;
var
  i: Integer;
  a: Integer;
  b: Integer;
begin
  for i := 0 to RowCount - 1 do
  begin
    a := RandomRange(0, RowCount);
    b := RandomRange(0, RowCount);
    fItems.Exchange(a, b);
  end;
end;

function TSampleGridData2By2.RowCount: Integer;
begin
  Result := fItems.Count;
end;

function TSampleGridData2By2.ValueFloat(aCol, aRow: Integer): Double;
begin
  Result := fItems[aRow];
end;

function TSampleGridData2By2.ValueString(aCol, aRow: Integer): String;
begin
  Result := IntToStr(fItems[aRow]);
end;

{ TSampleGridDataRandom }

procedure TSampleGridDataRandom.Fill(aCount: Integer);
var
  i: Integer;
begin
  for i := 0 to aCount - 1 do
    fItems.Add(RandomRange(0, 100));
end;

{ TSampleList.TItem }

constructor TSampleGridDataList.TItem.Create(aNN, aCC, aSS, aFF, aII, aBB: string);
begin
  NN := aNN;
  CC := aCC;
  SS := aSS;
  FF := aFF;
  II := aII;
  BB := aBB;
end;

{ TSampleList }

function TSampleGridDataList.AsDebug: string;
var
  r: Integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + ValueString(0, r) + ','
end;

constructor TSampleGridDataList.Create;
var
  i: Integer;
begin
  fItems := TList<TItem>.Create;
  for i := 0 to Length(TSampleData.TABLE_20) - 1 do
    fItems.Add(TItem.Create(
      TSampleData.TABLE_20[i][0],
      TSampleData.TABLE_20[i][1],
      TSampleData.TABLE_20[i][2],
      TSampleData.TABLE_20[i][3],
      TSampleData.TABLE_20[i][4],
      TSampleData.TABLE_20[i][5]));
end;

destructor TSampleGridDataList.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TSampleGridDataList.RowCount: Integer;
begin
  Result := fItems.Count;
end;

function TSampleGridDataList.ValueFloat(aCol, aRow: Integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TSampleGridDataList.ValueString(aCol, aRow: Integer): String;
begin
  case aCol of
    0:
      Result := fItems[aRow].NN;
    1:
      Result := fItems[aRow].CC;
    2:
      Result := fItems[aRow].SS;
    3:
      Result := fItems[aRow].FF;
    4:
      Result := fItems[aRow].II;
    5:
      Result := fItems[aRow].BB;
  else
    raise Exception.Create('Invalid column index.');
  end;
end;

end.
