unit cSampleData;

interface

uses xn.grid.link;

type
  TSampleData = class
  private
    const
    TABLE_20: array [0 .. 19, 0 .. 5] of string = (
      ('0', 'Aqua', 'MD', '2,01', '71', 'T'),
      ('1', 'Yellow', 'LA', '3,76', '29', 'T'),
      ('2', 'DkGray', 'VT', '4,74', '47', 'T'),
      ('3', 'Teal', 'NE', '2,06', '6', 'F'),
      ('4', 'Red', 'LA', '2,82', '80', 'F'),
      ('5', 'Black', 'OH', '2,06', '6', 'T'),
      ('6', 'Lime', 'HI', '2,82', '32', 'F'),
      ('7', 'White', 'UT', '2,9', '75', 'T'),
      ('8', 'Lime', 'VT', '3,17', '75', 'F'),
      ('9', 'Olive', 'VA', '3,17', '88', 'F'),
      ('10', 'Gray', 'PA', '1,53', '25', 'F'),
      ('11', 'Red', 'WA', '2,82', '34', 'F'),
      ('12', 'White', 'RI', '4,04', '29', 'T'),
      ('13', 'Purple', 'MS', '3,17', '70', 'T'),
      ('14', 'Fuchsia', 'ND', '3,6', '77', 'F'),
      ('15', 'Silver', 'AR', '3,76', '25', 'F'),
      ('16', 'Maroon', 'MT', '3,94', '88', 'T'),
      ('17', 'LtGray', 'AZ', '3,94', '10', 'F'),
      ('18', 'Silver', 'NY', '3,76', '8', 'F'),
      ('19', 'Purple', 'HI', '2,56', '68', 'F'));

  public
    class function RowCount: LongInt;
    class function Value(aCol, aRow: integer): string;
  end;

  TSampleGridData = class(TInterfacedObject, IxnGridData)
    function RowCount: LongInt;
    function AsDebug: string;
    function ValueString(aCol, aRow: LongInt): String;
    function ValueFloat(aCol, aRow: LongInt): Double;
  end;

implementation

uses System.SysUtils;

{ TABLE_20
  0;Aqua;MD;2,01;71;T
  1;Yellow;LA;3,76;29;T
  2;DkGray;VT;4,74;47;T
  3;Teal;NE;2,06;6;F
  4;Red;LA;2,82;80;F
  5;Black;OH;2,06;6;T
  6;Lime;HI;2,82;32;F
  7;White;UT;2,9;75;T
  8;Lime;VT;3,17;75;F
  9;Olive;VA;3,17;88;F
  10;Gray;PA;1,53;25;F
  11;Red;WA;2,82;34;F
  12;White;RI;4,04;29;T
  13;Purple;MS;3,17;70;T
  14;Fuchsia;ND;3,6;77;F
  15;Silver;AR;3,76;25;F
  16;Maroon;MT;3,94;88;T
  17;LtGray;AZ;3,94;10;F
  18;Silver;NY;3,76;8;F
  19;Purple;HI;2,56;68;F
}

class function TSampleData.RowCount: LongInt;
begin
  Result := Length(TABLE_20);
end;

class function TSampleData.Value(aCol, aRow: integer): string;
begin
  Result := TABLE_20[aRow, aCol];
end;

{ TSampleGridData }

function TSampleGridData.AsDebug: string;
var
  r: integer;
begin
  Result := '';
  for r := 0 to RowCount - 1 do
    Result := Result + ValueString(0, r) + ','
end;

function TSampleGridData.RowCount: LongInt;
begin
  Result := TSampleData.RowCount
end;

function TSampleGridData.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TSampleGridData.ValueString(aCol, aRow: integer): String;
begin
  Result := TSampleData.Value(aCol, aRow);
end;

end.
