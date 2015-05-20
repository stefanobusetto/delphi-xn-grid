unit xn.grid.common;

interface

type
  TxnGridOnRowAdd = procedure(aIndex: integer) of object;
  TxnGridOnRowDel = procedure(aIndex: integer) of object;
  TxnGridOnRowEdit = procedure(aIndex: integer) of object;
  TxnGridOnRecNo = procedure(aIndex: integer) of object;

  IxnGridData = interface
    ['{D88DE50E-5A96-4955-B8C3-DD321FB97458}']
    function RowCount: LongInt;
    function AsDebug: string;
    function ValueString(aCol, aRow: LongInt): String;
    function ValueFloat(aCol, aRow: LongInt): Double;
  end;

  IxnGridLink = interface(IxnGridData)
    ['{6CDF8790-F13F-4507-9DB3-E173799CEAD4}']
    procedure Append(aString: string);
    procedure Insert(aIndex: integer; aString: string);
    procedure Edit(aIndex: integer; aString: string);
    procedure Delete(aIndex: integer);

    procedure RecNoSet(aIndex: integer);
    function RecNoGet: integer;

    procedure OnRowAddSet(aNotifyRowAdd: TxnGridOnRowAdd);
    procedure OnRowDelSet(aNotifyRowAdd: TxnGridOnRowDel);
    procedure OnRowEditSet(aNotifyRowEdit: TxnGridOnRowEdit);
    procedure OnRecNoSet(aOnRecNo: TxnGridOnRecNo);

    property OnRowAdd: TxnGridOnRowAdd write OnRowAddSet;
    property OnRowDel: TxnGridOnRowDel write OnRowDelSet;
    property OnRowEdit: TxnGridOnRowEdit write OnRowEditSet;
    property OnRecNo: TxnGridOnRecNo write OnRecNoSet;

    property RecNo: integer read RecNoGet write RecNoSet;
  end;

implementation

end.
