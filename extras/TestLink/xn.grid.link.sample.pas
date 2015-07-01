unit xn.grid.link.sample;

interface


uses System.SysUtils, System.Classes, Generics.Collections,
  xn.grid.common;

type
  IxnGridLinkCustom<T> = interface(IxnGridLink)
    ['{29DA3AC6-EA0C-4CE0-934D-D6D0A48CEF4C}']
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
    procedure Clear;
    procedure Append(aString: string);
    procedure Insert(aIndex: Integer; aString: string);
    procedure Edit(aIndex: Integer; aString: string);
    procedure Delete(aIndex: Integer);

  end;

  TxnGridLinkCustom<T> = class(TInterfacedObject, IxnGridLinkCustom<T>)
  strict private
    fLog: TStrings;
    fItems: TList<string>;
    fRecNo: Integer;
    fNotify: TxnGridLinkNotify;

    function RowCount: Integer;
    function AsDebug: String;

    procedure RecNoSet(aIndex: Integer);
    function RecNoGet: Integer;

    procedure LogSet(aValue: TStrings);
    function LogGet: TStrings;
    procedure LogString(aString: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure First;
    procedure Last;
    procedure Prior;
    procedure Next;
    procedure Clear;
    procedure Move(aIndex: Integer);
    procedure Append(aString: string);
    procedure Insert(aIndex: Integer; aString: string);
    procedure Edit(aIndex: Integer; aString: string);
    procedure Delete(aIndex: Integer);

    procedure NotifySet(aRowEvent: TxnGridLinkNotify);

    function ValueString(aCol, aRow: Integer): String;
    function ValueFloat(aCol, aRow: Integer): Double;

    property Log: TStrings read LogGet write LogSet;
  end;

  TxnGridLinkSample = class(TxnGridLinkCustom<string>)
  end;

implementation

{ TxnGridLinkSample }

procedure TxnGridLinkCustom<T>.Append(aString: string);
begin
  LogString(Format('TxnGridLinkCustom<T>.Append()', []));

  Insert(RowCount, aString);
end;

procedure TxnGridLinkCustom<T>.Insert(aIndex: Integer; aString: string);
begin
  LogString(Format('TxnGridLinkCustom<T>.Insert(%d)', [aIndex]));

  fItems.Insert(aIndex, aString);
  if Assigned(fNotify) then
    fNotify(xnGridNotifyDataCreateLinkEvent(aIndex, gekAdd));
  RecNoSet(aIndex);
end;

procedure TxnGridLinkCustom<T>.Delete(aIndex: Integer);
begin
  LogString(Format('TxnGridLinkCustom<T>.Delete(%d)', [aIndex]));

  fItems.Delete(aIndex);
  if Assigned(fNotify) then
    fNotify(xnGridNotifyDataCreateLinkEvent(aIndex, gekDel));
  RecNoSet(aIndex);
end;

procedure TxnGridLinkCustom<T>.Edit(aIndex: Integer; aString: string);
begin
  LogString(Format('TxnGridLinkCustom<T>.Edit(%d)', [aIndex]));

  fItems[aIndex] := aString;
  if Assigned(fNotify) then
    fNotify(xnGridNotifyDataCreateLinkEvent(aIndex, gekEdit));
  RecNoSet(aIndex);
end;

procedure TxnGridLinkCustom<T>.Move(aIndex: Integer);
begin
  LogString(Format('TxnGridLinkCustom<T>.Move.Outer(%d)', [aIndex]));

  if RecNoGet() = aIndex then
    Exit;

  LogString(Format('TxnGridLinkCustom<T>.Move.Inner(%d)', [aIndex]));

  if Assigned(fNotify) then
    fNotify(xnGridNotifyDataCreateLinkEvent(aIndex, gekMove));
  RecNoSet(aIndex);
end;

procedure TxnGridLinkCustom<T>.First;
begin
  LogString(Format('TxnGridLinkCustom<T>.First(%d)', [0]));

  if RecNoGet() > 0 then
    Move(0);
end;

procedure TxnGridLinkCustom<T>.Last;
var
  r: Integer;
begin
  r := RowCount() - 1;
  LogString(Format('TxnGridLinkCustom<T>.Last(%d)', [r]));

  if RecNoGet() < r then
    Move(r);
end;

procedure TxnGridLinkCustom<T>.Prior;
var
  r: Integer;
begin
  r := RecNoGet();
  LogString(Format('TxnGridLinkCustom<T>.Prior(%d)', [r - 1]));

  if r > 0 then
    Move(r - 1);
end;

procedure TxnGridLinkCustom<T>.Next;
var
  r: Integer;
begin
  r := RecNoGet();
  LogString(Format('TxnGridLinkCustom<T>.Next(%d)', [r + 1]));

  if r < RowCount() - 1 then
    Move(r + 1);
end;

function TxnGridLinkCustom<T>.LogGet: TStrings;
begin
  Result := fLog
end;

procedure TxnGridLinkCustom<T>.LogSet(aValue: TStrings);
begin
  fLog := aValue
end;

procedure TxnGridLinkCustom<T>.LogString(aString: String);
begin
  if fLog <> nil then
    fLog.add(aString);
end;

function TxnGridLinkCustom<T>.AsDebug: String;
begin
  raise Exception.Create('TxnGridLinkCustom<T>.AsDebug not implemented!');
end;

procedure TxnGridLinkCustom<T>.Clear;
begin
  LogString(Format('TxnGridLinkCustom<T>.Clear()', []));

  fItems.Clear;
  if Assigned(fNotify) then
    fNotify(xnGridNotifyDataCreateLinkEvent(0, gekAdd));
  RecNoSet(-1);
end;

constructor TxnGridLinkCustom<T>.Create;
begin
  fLog := nil;
  fNotify := nil;
  fRecNo := -1;

  fItems := TList<string>.Create;
end;

destructor TxnGridLinkCustom<T>.Destroy;
begin
  fLog := nil;
  fItems.Clear;
  fItems.Free;
  inherited;
end;

function TxnGridLinkCustom<T>.RecNoGet(): Integer;
begin
  Result := fRecNo
end;

procedure TxnGridLinkCustom<T>.RecNoSet(aIndex: Integer);
begin
  LogString(Format('TxnGridLinkCustom<T>.RecNoSet(%d)', [aIndex]));

  if aIndex < 0 then
    aIndex := 0;

  if aIndex > RowCount - 1 then
    aIndex := RowCount - 1;

  if fRecNo <> aIndex then
    fRecNo := aIndex;
end;

function TxnGridLinkCustom<T>.RowCount: Integer;
begin
  Result := fItems.Count;
end;

procedure TxnGridLinkCustom<T>.NotifySet(aRowEvent: TxnGridLinkNotify);
begin
  fNotify := aRowEvent;
end;

function TxnGridLinkCustom<T>.ValueFloat(aCol, aRow: Integer): Double;
begin
  Result := StrToFloat(ValueString(aCol, aRow));
end;

function TxnGridLinkCustom<T>.ValueString(aCol, aRow: Integer): String;
begin
  if aRow < 0 then
    Exit('');
  if aRow >= fItems.Count then
    Exit('');

  Result := fItems[aRow]
end;

end.
