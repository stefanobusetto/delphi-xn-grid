unit xn.grid.common;

interface

type
  TxnGridNotifyData = record
  type
    TKind = (gekAdd, gekDel, gekEdit, gekMove);
  private
    fRow: Integer;
    fCol: Integer;
    fKind: TKind;
  public
    constructor Create(aCol, aRow: Integer; aKind: TKind);

    property Row: Integer read fRow;
    property Col: Integer read fCol;
    property Kind: TKind read fKind;
  end;

  TxnGridColNotify = procedure(fData: TxnGridNotifyData) of object;
  TxnGridLinkNotify = procedure(fData: TxnGridNotifyData) of object;

  IxnGridData = interface
    ['{D88DE50E-5A96-4955-B8C3-DD321FB97458}']
    function RowCount: Integer;
    function AsDebug: String;
    function ValueString(aCol, aRow: Integer): String;
    function ValueFloat(aCol, aRow: Integer): Double;
  end;

  IxnGridLink = interface(IxnGridData)
    ['{6CDF8790-F13F-4507-9DB3-E173799CEAD4}']
    procedure RecNoSet(aIndex: Integer);
    function RecNoGet: Integer;
    property RecNo: Integer read RecNoGet write RecNoSet;

    procedure NotifySet(aRowEvent: TxnGridLinkNotify);
    property Notify: TxnGridLinkNotify write NotifySet;
  end;

function xnGridEventKindDes(aGridEventKind: TxnGridNotifyData.TKind): string;

function xnGridNotifyDataCreateColEvent(aCol: Integer; aKind: TxnGridNotifyData.TKind): TxnGridNotifyData;
function xnGridNotifyDataCreateLinkEvent(aRow: Integer; aKind: TxnGridNotifyData.TKind): TxnGridNotifyData;

implementation

uses System.Math, System.TypInfo;

function xnGridEventKindDes(aGridEventKind: TxnGridNotifyData.TKind): string;
begin
  Result := GetEnumName(TypeInfo(TxnGridNotifyData.TKind), Ord(aGridEventKind));
end;

{ TxnGridNotifyData }

constructor TxnGridNotifyData.Create(aCol, aRow: Integer; aKind: TKind);
begin
  fCol := aCol;
  fRow := aRow;
  fKind := aKind;
end;

function xnGridNotifyDataCreateColEvent(aCol: Integer; aKind: TxnGridNotifyData.TKind): TxnGridNotifyData;
begin
  Result.Create(aCol, -1, aKind);
end;

function xnGridNotifyDataCreateLinkEvent(aRow: Integer; aKind: TxnGridNotifyData.TKind): TxnGridNotifyData;
begin
  Result.Create(-1, aRow, aKind);
end;

end.
