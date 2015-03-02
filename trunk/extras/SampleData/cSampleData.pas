unit cSampleData;

interface

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
      ('19', 'Purple', 'HI', '2,56', '68', 'F')
      );

    NN:
      array [0 .. 19] of string =
      ('0', '1', '2', '3',
      '4', '5', '6', '7',
      '8', '9', '10', '11',
      '12', '13', '14', '15',
      '16', '17', '18', '19');

    CC: array [0 .. 19] of string =
      ('Aqua', 'Yellow', 'DkGray', 'Teal',
      'Red', 'Black', 'Lime', 'White',
      'Lime', 'Olive', 'Gray', 'Red',
      'White', 'Purple', 'Fuchsia', 'Silver',
      'Maroon', 'LtGray', 'Silver', 'Purple');

    SS: array [0 .. 19] of string =
      ('MD', 'LA', 'VT', 'NE',
      'LA', 'OH', 'HI', 'UT',
      'VT', 'VA', 'PA', 'WA',
      'RI', 'MS', 'ND', 'AR',
      'MT', 'AZ', 'NY', 'HI');

    FF: array [0 .. 19] of string =
      ('2,01', '3,76', '4,74', '2,06',
      '2,82', '2,06', '2,82', '2,9',
      '3,17', '3,17', '1,53', '2,82',
      '4,04', '3,17', '3,6', '3,76',
      '3,94', '3,94', '3,76', '2,56');

    II: array [0 .. 19] of string =
      ('71', '29', '47', '6',
      '80', '6', '32', '75',
      '75', '88', '25', '34',
      '29', '70', '77', '25',
      '88', '10', '8', '68');

    BB:
      array [0 .. 19] of string =
      ('T', 'T', 'T', 'F',
      'F', 'T', 'F', 'T',
      'F', 'F', 'F', 'F',
      'T', 'T', 'F', 'F',
      'T', 'F', 'F', 'F');

  public
    class function Length: integer;
    class function Value(aCol, aRow: integer): string;
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

class function TSampleData.Length: integer;
begin
  Result := 20;
end;

class function TSampleData.Value(aCol, aRow: integer): string;
begin
  if aCol = 0 then
    Exit(NN[aRow])
  else if aCol = 1 then
    Exit(CC[aRow])
  else if aCol = 2 then
    Exit(SS[aRow])
  else if aCol = 3 then
    Exit(FF[aRow])
  else if aCol = 4 then
    Exit(II[aRow])
  else if aCol = 5 then
    Exit(BB[aRow])
  else
    raise Exception.Create('Invalid column index');
end;

end.
