unit uCommands;

interface

uses Data.DB,
  xn.grid.link.sample,
  xn.grid.common;

type
  TxnCommands = class
  const
    LINK_NIL = 'Link not assigned!';
    DATASET_NIL = 'Dataset not assigned!';

  private
    fLink: IxnGridLinkCustom<string>;
    fDataSet: TDataSet;

    fId: Integer;

    procedure Clear;
    procedure Asserts;

    procedure DatasetValues(aValue: string);
  public
    constructor Create;
    function NewId(): string;

    procedure Init(aLink: IxnGridLinkCustom<string>; aDataSet: TDataSet);

    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;

    procedure Append(aValue: string);
    procedure Insert(aValue: string);
    procedure Edit(aValue: string);
    procedure Delete;

    function RandNumber: Integer;
    function RandCommand: String;

    procedure Execute(aCommand: String);
  end;

var
  xnCommands: TxnCommands;

implementation

uses System.SysUtils, System.Math;

function TxnCommands.NewId(): string;
begin
  Inc(fId);
  Result := IntToStr(fId);
end;

procedure TxnCommands.Asserts;
begin
  Assert(fDataSet <> nil, DATASET_NIL);
  Assert(fLink <> nil, LINK_NIL);
end;

procedure TxnCommands.Init(aLink: IxnGridLinkCustom<string>; aDataSet: TDataSet);
begin
  fLink := aLink;
  fDataSet := aDataSet;
end;

procedure TxnCommands.DatasetValues(aValue: string);
begin
  Asserts;
  fDataSet.Fields[0].AsString := aValue;
  fDataSet.Fields[1].AsString := aValue;
  fDataSet.Fields[2].AsString := aValue;
end;

procedure TxnCommands.Append(aValue: string);
begin
  if fLink.RecNoGet > 10 then
    Exit;

  Asserts;
  fLink.Append(aValue);

  fDataSet.Append;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure TxnCommands.Insert(aValue: string);
begin
  if fLink.RecNoGet > 10 then
    Exit;

  Asserts;
  if fLink.RecNoGet < 0 then
    fLink.Insert(0, aValue)
  else
    fLink.Insert(fLink.RecNoGet, aValue);

  fDataSet.Insert;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure TxnCommands.Edit(aValue: string);
begin
  if fLink.RowCountGet = 0 then
    Exit;

  Asserts;
  fLink.Edit(fLink.RecNoGet, aValue);

  fDataSet.Edit;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure TxnCommands.Delete;
begin
  if fLink.RowCountGet = 0 then
    Exit;

  Asserts;
  fLink.Delete(fLink.RecNoGet);

  fDataSet.Delete;
end;

procedure TxnCommands.Clear;
begin
  Asserts;

  fLink.Clear;

  while not fDataSet.IsEmpty do
    fDataSet.Delete;
end;

procedure TxnCommands.First;
begin
  Asserts;

  fLink.First;
  fDataSet.First;
end;

procedure TxnCommands.Last;
begin
  Asserts;

  fLink.Last;
  fDataSet.Last;
end;

procedure TxnCommands.Prior;
begin
  Asserts;

  fLink.Prior;
  fDataSet.Prior;
end;

procedure TxnCommands.Next;
begin
  Asserts;

  fLink.Next;
  fDataSet.Next;
end;

function TxnCommands.RandNumber: Integer;
begin
  Result := RandomRange(1, 10);
end;

function TxnCommands.RandCommand: String;
begin
  case RandNumber() of
    1:
      Exit('append');
    2:
      Exit('insert');
    3:
      Exit('delete');
    4:
      Exit('edit');
    5:
      Exit('first');
    6:
      Exit('last');
    7:
      Exit('prior');
    8:
      Exit('next');
    9:
      Exit('clear');
  else
    raise Exception.Create('Invalid number');
  end;
end;

procedure TxnCommands.Execute(aCommand: String);
begin
  if SameText(aCommand, 'clear') then
    Clear
  else if SameText(aCommand, 'delete') then
    Delete
  else if SameText(aCommand, 'edit') then
    Edit(NewId())
  else if SameText(aCommand, 'append') then
    Append(NewId())
  else if SameText(aCommand, 'insert') then
    Insert(NewId())
  else if SameText(aCommand, 'first') then
    First
  else if SameText(aCommand, 'last') then
    Last
  else if SameText(aCommand, 'prior') then
    Prior
  else if SameText(aCommand, 'next') then
    Next
  else
    raise Exception.CreateFmt('Invalid command "%s"', [aCommand]);
end;

constructor TxnCommands.Create;
begin
  fLink := nil;
  fDataSet := nil;
  fId := 0;
end;

initialization

xnCommands := TxnCommands.Create;

finalization

xnCommands.Free;

end.
