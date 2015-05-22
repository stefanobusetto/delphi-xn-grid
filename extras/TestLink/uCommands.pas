unit uCommands;

interface

uses Data.DB,
  xn.grid.common;

function NewId(): string;

procedure Init(aLink: IxnGridLink; aDataSet: TDataSet);

procedure Append(aValue: string);
procedure Insert(aValue: string);
procedure Edit(aValue: string);
procedure Delete;

function RandCommand: String;

procedure Execute(aCommand: String);

implementation

uses System.SysUtils, System.Math;

const
  LINK_NIL = 'Link not assigned!';
  DATASET_NIL = 'Dataset not assigned!';

var
  fLink: IxnGridLink = nil;
  fDataSet: TDataSet = nil;

  fId: Integer = 0;

function NewId(): string;
begin
  Inc(fId);
  Result := IntToStr(fId);
end;

procedure Asserts;
begin
  Assert(fDataSet <> nil, DATASET_NIL);
  Assert(fLink <> nil, LINK_NIL);
end;

procedure Init(aLink: IxnGridLink; aDataSet: TDataSet);
begin
  fLink := aLink;
  fDataSet := aDataSet;
end;

procedure DatasetValues(aValue: string);
begin
  Asserts;
  fDataSet.Fields[0].AsString := aValue;
  fDataSet.Fields[1].AsString := aValue;
  fDataSet.Fields[2].AsString := aValue;
end;

procedure Append(aValue: string);
begin
  if fLink.RecNo > 10 then
    Exit;

  Asserts;
  fLink.Append(aValue);

  fDataSet.Append;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Insert(aValue: string);
begin
  if fLink.RecNo > 10 then
    Exit;

  Asserts;
  if fLink.RecNo < 0 then
    fLink.Insert(0, aValue)
  else
    fLink.Insert(fLink.RecNo, aValue);

  fDataSet.Insert;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Edit(aValue: string);
begin
  if fLink.RowCount = 0 then
    Exit;

  Asserts;
  fLink.Edit(fLink.RecNo, aValue);

  fDataSet.Edit;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Delete;
begin
  if fLink.RowCount = 0 then
    Exit;

  Asserts;
  fLink.Delete(fLink.RecNo);

  fDataSet.Delete;
end;

function RandNumber: Integer;
begin
  Result := RandomRange(1, 4);
end;

function RandCommand: String;
begin
  case RandNumber() of
    1:
      Exit('a');
    2:
      Exit('i');
    3:
      Exit('d');
    4:
      Exit('e');
  else
    raise Exception.Create('Invalid Number');
  end;
end;

procedure Execute(aCommand: String);
begin
  if SameText(aCommand, 'a') then
    Append(NewId())
  else if SameText(aCommand, 'i') then
    Insert(NewId())
  else if SameText(aCommand, 'd') then
    Delete
  else if SameText(aCommand, 'e') then
    Edit(NewId())
  else
    raise Exception.Create('Invalid Execute');
end;

end.
