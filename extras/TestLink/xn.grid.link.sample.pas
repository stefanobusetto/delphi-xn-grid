unit xn.grid.link.sample;

interface

uses Generics.Collections,
  xn.grid.common;

type
  TxnGridLinkSample = class(TInterfacedObject, IxnGridLink)
  strict private
    fItems: TList<string>;
    fRecNo: integer;
  public
    fOnRowAdd: TxnGridOnRowAdd;
    fOnRowDel: TxnGridOnRowDel;
    fOnRowEdit: TxnGridOnRowEdit;
    fOnRecNo: TxnGridOnRecNo;

    constructor Create;
    destructor Destroy; override;
    procedure Append(aString: string);
    procedure Insert(aIndex: integer; aString: string);
    procedure Edit(aIndex: integer; aString: string);
    procedure Delete(aIndex: integer);

    procedure OnRowAddSet(aOnRowAdd: TxnGridOnRowAdd);
    procedure OnRowDelSet(aOnRowDel: TxnGridOnRowDel);
    procedure OnRowEditSet(aOnRowEdit: TxnGridOnRowEdit);
    procedure OnRecNoSet(aOnRecNo: TxnGridOnRecNo);

    procedure RecNoSet(aIndex: integer);
    function RecNoGet: integer;

    function RowCount: LongInt;
    function AsDebug: string;
    function ValueString(aCol, aRow: LongInt): String;
    function ValueFloat(aCol, aRow: LongInt): Double;
  end;

implementation

uses System.SysUtils;

{ TxnGridLinkSample }

procedure TxnGridLinkSample.OnRecNoSet(aOnRecNo: TxnGridOnRecNo);
begin
  fOnRecNo := aOnRecNo;
end;

procedure TxnGridLinkSample.OnRowAddSet(aOnRowAdd: TxnGridOnRowAdd);
begin
  fOnRowAdd := aOnRowAdd;
end;

procedure TxnGridLinkSample.OnRowEditSet(aOnRowEdit: TxnGridOnRowEdit);
begin
  fOnRowEdit := aOnRowEdit
end;

procedure TxnGridLinkSample.OnRowDelSet(aOnRowDel: TxnGridOnRowDel);
begin
  fOnRowDel := aOnRowDel;
end;

procedure TxnGridLinkSample.Append(aString: string);
begin
  fItems.Add(aString);
  fRecNo := RowCount;
  fOnRowAdd(RowCount - 1);
end;

procedure TxnGridLinkSample.Insert(aIndex: integer; aString: string);
begin
  if aIndex < 0 then
    aIndex := 0;

  fItems.Insert(aIndex, aString);
  fRecNo := aIndex + 1;
  fOnRowAdd(aIndex);
end;

procedure TxnGridLinkSample.Delete(aIndex: integer);
begin
  fItems.Delete(aIndex);
  fOnRowDel(aIndex);
end;

function TxnGridLinkSample.AsDebug: string;
var
  s: string;
begin
  Result := fRecNo.ToString + ':';
  for s in fItems do
    Result := Result + s + '#'
end;

procedure TxnGridLinkSample.Edit(aIndex: integer; aString: string);
begin
  fItems[aIndex] := aString;
  fOnRowEdit(aIndex);
end;

constructor TxnGridLinkSample.Create;
begin
  fRecNo := 0;
  fItems := TList<string>.Create;
end;

destructor TxnGridLinkSample.Destroy;
begin
  fItems.Clear;
  fItems.Free;
  inherited;
end;

function TxnGridLinkSample.RecNoGet: integer;
begin
  Result := fRecNo;
end;

procedure TxnGridLinkSample.RecNoSet(aIndex: integer);
begin
  if fRecNo <> aIndex then
    fRecNo := aIndex;
end;

function TxnGridLinkSample.RowCount: LongInt;
begin
  Result := fItems.Count;
end;

function TxnGridLinkSample.ValueFloat(aCol, aRow: integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TxnGridLinkSample.ValueString(aCol, aRow: LongInt): String;
begin
  if aRow < 0 then
    exit('');
  if aRow >= fItems.Count then
    exit('');

  Result := fItems[aRow] // + '(' + aCol.ToString() + '.' + aRow.ToString() + ')'
end;

end.
