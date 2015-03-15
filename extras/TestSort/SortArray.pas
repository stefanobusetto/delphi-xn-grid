unit SortArray;

interface

procedure QuickSortArray(var aData: array of string; aLo, aHi: integer);

implementation

procedure QuickSortArray(var aData: array of string; aLo, aHi: integer);
var
  iLo: integer;
  iHi: integer;
  iPivot: string;

  function Value(aIndex: integer): string;
  begin
    Result := aData[aIndex];
  end;

  function Compare(aLeft, aRight: string): integer;
  begin
    if aLeft < aRight then
      exit(-1)
    else if aLeft > aRight then
      exit(+1)
    else
      exit(0)
  end;

  procedure Swap(var a, b: string);
  var
    t: string;
  begin
    t := a;
    a := b;
    b := t;
  end;

begin
  iLo := aLo;
  iHi := aHi;
  iPivot := Value((iLo + iHi) div 2);
  repeat
    while (Compare(Value(iLo), iPivot) < 0) do
      Inc(iLo);
    while (Compare(Value(iHi), iPivot) > 0) do
      Dec(iHi);

    if iLo <= iHi then
    begin
      if iLo <> iHi then
        Swap(aData[iLo], aData[iHi]);
      Inc(iLo);
      Dec(iHi);
    end;
  until iLo > iHi;

  if iHi > aLo then
    QuickSortArray(aData, aLo, iHi);
  if iLo < aHi then
    QuickSortArray(aData, iLo, aHi);
end;

end.
