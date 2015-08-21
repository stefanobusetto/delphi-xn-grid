unit xn.grid.common;

interface

type
  TxnGridLinkNotifyData = record
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

  TxnGridColNotify = procedure(fData: TxnGridLinkNotifyData) of object;
  TxnGridLinkNotify = procedure(fData: TxnGridLinkNotifyData) of object;

  IxnGridData = interface
    ['{D88DE50E-5A96-4955-B8C3-DD321FB97458}']
    function RowCountGet: Integer;
    function ValueString(aCol, aRow: Integer): String;
    function ValueFloat(aCol, aRow: Integer): Double;
  end;

  IxnGridLink = interface(IxnGridData)
    ['{6CDF8790-F13F-4507-9DB3-E173799CEAD4}']
    procedure RecNoSet(aIndex: Integer);
    function RecNoGet: Integer;
    procedure NotifySet(aRowEvent: TxnGridLinkNotify);
  end;

function xnGridEventKindDes(aGridEventKind: TxnGridLinkNotifyData.TKind): string;

function xnGridNotifyDataCreateColEvent(aCol: Integer; aKind: TxnGridLinkNotifyData.TKind): TxnGridLinkNotifyData;
function xnGridNotifyDataCreateLinkEvent(aRow: Integer; aKind: TxnGridLinkNotifyData.TKind): TxnGridLinkNotifyData;

implementation

uses System.Math, System.TypInfo;

function xnGridEventKindDes(aGridEventKind: TxnGridLinkNotifyData.TKind): string;
begin
  Result := GetEnumName(TypeInfo(TxnGridLinkNotifyData.TKind), Ord(aGridEventKind));
end;

{ TxnGridNotifyData }

constructor TxnGridLinkNotifyData.Create(aCol, aRow: Integer; aKind: TKind);
begin
  fCol := aCol;
  fRow := aRow;
  fKind := aKind;
end;

function xnGridNotifyDataCreateColEvent(aCol: Integer; aKind: TxnGridLinkNotifyData.TKind): TxnGridLinkNotifyData;
begin
  Result.Create(aCol, -1, aKind);
end;

function xnGridNotifyDataCreateLinkEvent(aRow: Integer; aKind: TxnGridLinkNotifyData.TKind): TxnGridLinkNotifyData;
begin
  Result.Create(-1, aRow, aKind);
end;

end.
