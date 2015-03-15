unit SortIndex;

interface

uses System.Generics.Collections, System.SysUtils;

type
  TxnQuickSort = class
  type
    TGetter = reference to function(const aIndex: integer): string;
    TComparer = reference to function(const aLeft, aRight: string): integer;

  strict private
    fGetter: TGetter;
    fComparer: TComparer;
  public
    constructor Create(aGetter: TGetter; aComparer: TComparer);

    procedure Sort(var aData: TList<integer>; aLo, aHi: integer); overload;
    procedure Sort(var aData: TList<integer>); overload;
  end;

implementation

procedure TxnQuickSort.Sort(var aData: TList<integer>; aLo, aHi: integer);
var
  iLo: integer;
  iHi: integer;
  iPivot: string;

  function Get(aIndex: integer): string;
  begin
    Result := fGetter(aData[aIndex])
  end;

begin
  iLo := aLo;
  iHi := aHi;
  iPivot := Get((iLo + iHi) div 2);
  repeat
    while (fComparer(Get(iLo), iPivot) < 0) do
      Inc(iLo);
    while (fComparer(Get(iHi), iPivot) > 0) do
      Dec(iHi);

    if iLo <= iHi then
    begin
      if iLo <> iHi then
        aData.Exchange(iLo, iHi);
      Inc(iLo);
      Dec(iHi);
    end;
  until iLo > iHi;

  if iHi > aLo then
    Sort(aData, aLo, iHi);
  if iLo < aHi then
    Sort(aData, iLo, aHi);
end;

constructor TxnQuickSort.Create(aGetter: TGetter; aComparer: TComparer);
begin
  inherited Create;
  fGetter := aGetter;
  fComparer := aComparer;
end;

procedure TxnQuickSort.Sort(var aData: TList<integer>);
begin
  Sort(aData, 0, aData.Count - 1);
end;

end.
