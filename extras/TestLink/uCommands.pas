unit uCommands;

interface

uses Data.DB,
  xn.grid.common;

function NewId(): string;

procedure Init(aLink: IxnGridLink; aDataSet: TDataSet);

procedure Append(aValue: string);
procedure Insert(aValue: string);
procedure Edit(aValue: string);
procedure Edit1(aValue: string);
procedure Delete;

implementation

uses System.SysUtils;

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
  Asserts;
  fLink.Append(aValue);

  fDataSet.Append;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Insert(aValue: string);
begin
  Asserts;
  fLink.Insert(fLink.RecNo - 1, aValue);

  fDataSet.Insert;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Edit(aValue: string);
begin
  Asserts;
  fLink.Edit(fLink.RecNo - 1, aValue);

  fDataSet.Edit;
  DatasetValues(aValue);
  fDataSet.Post;
end;

procedure Edit1(aValue: string);
begin
  Asserts;
  fLink.Edit(1, aValue);
end;

procedure Delete;
begin
  Assert(fLink <> nil, LINK_NIL);
  fLink.Delete(fLink.RecNo - 1);

  Assert(fDataSet <> nil, DATASET_NIL);
  fDataSet.Delete;
end;

end.
