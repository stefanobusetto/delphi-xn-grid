unit xn.grid.link;

interface

type
  IxnGridData = interface
    ['{D88DE50E-5A96-4955-B8C3-DD321FB97458}']
    function RowCount: LongInt;
    function AsDebug: string;
    function Value(aCol, aRow: LongInt): String;
  end;

  IxnGridLink = interface(IxnGridData)
    ['{6CDF8790-F13F-4507-9DB3-E173799CEAD4}']
    procedure Append(aString: string);
    procedure Insert(aIndex: integer; aString: string);
    procedure Change(aIndex: integer; aString: string);
    procedure Delete(aIndex: integer);
  end;

implementation

end.
