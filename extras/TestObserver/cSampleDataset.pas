unit cSampleDataset;

interface

uses Generics.Collections, Data.Db;

type
  TSampleDataset = class
  const
    NUM_REC = 20;

  private
    fDS: TDataset;
    fKeys: TList<integer>;

    function RandomNum: integer;
    function RandomKey: integer;
    function RandomHex: string;

    procedure RandomMove;
  public
    constructor Create(aDs: TDataset);
    destructor Destroy; override;

    procedure ActionClear;
    procedure ActionAppend;
    procedure ActionInsert;
    procedure ActionDelete;
    procedure ActionUpdate;

    procedure ActionRandom;

    function AsDebug: string;
  end;

implementation

uses System.Classes, System.Math, System.SysUtils;

function TSampleDataset.RandomNum: integer;
begin
  Result := RandomRange(0, 99999999);
end;

function TSampleDataset.RandomKey: integer;
var
  i: integer;
begin
  for i := 0 to 999 do
  begin
    Result := RandomRange(1, NUM_REC + 1);
    if not fKeys.Contains(Result) then
    begin
      fKeys.Add(Result);
      Exit;
    end;
  end;

  raise Exception.Create('RandomKey : Unable to generate unique key');
end;

function TSampleDataset.RandomHex: string;
begin
  Result := IntToHex(RandomNum(), 8);
end;

procedure TSampleDataset.ActionRandom;
begin
  case RandomRange(0, 4) of
    0:
      ActionAppend;
    1:
      ActionInsert;
    2:
      ActionDelete;
    3:
      ActionUpdate;
  else
    raise Exception.Create('Invalid action');
  end;
end;

procedure TSampleDataset.RandomMove;
begin
  Exit;
  fDS.First;
  fDS.MoveBy(RandomRange(0, fKeys.Count) div 3 * 2);
end;

procedure TSampleDataset.ActionAppend;
begin
  fDS.Append;
  fDS.FieldByName('nn').AsInteger := RandomKey;
  fDS.FieldByName('ss').AsString := RandomHex;
  fDS.Post;
end;

procedure TSampleDataset.ActionInsert;
begin
  RandomMove;
  fDS.Insert;
  fDS.FieldByName('nn').AsInteger := RandomKey;
  fDS.FieldByName('ss').AsString := RandomHex;
  fDS.Post;
end;

procedure TSampleDataset.ActionDelete;
begin
  if fDS.IsEmpty then
    Exit;

  RandomMove;
  fKeys.Remove(fDS.FieldByName('nn').AsInteger);
  fDS.Delete;
end;

procedure TSampleDataset.ActionUpdate;
begin
  if fDS.IsEmpty then
    Exit;

  RandomMove;
  fDS.Edit;
  fDS.FieldByName('ss').AsString := RandomHex;
  fDS.Post;
end;

function TSampleDataset.AsDebug: string;
var
  c: string;
  r: string;
begin
  // if fDS.IsEmpty then
  // Exit('0');
  //
  Exit(IntToStr(fDS.RecNo));

  // c := fDS.FieldByName('nn').AsString;

  fDS.First;
  while not fDS.Eof do
  begin
    r := r + fDS.FieldByName('nn').AsString + ',';
    fDS.Next;
  end;

  r := '[' + c + ']' + r;
  Result := r;
end;

procedure TSampleDataset.ActionClear;
begin
  while not fDS.IsEmpty do
    fDS.Delete;
  fKeys.Clear;
end;

constructor TSampleDataset.Create(aDs: TDataset);
begin
  fDS := aDs;
  fKeys := TList<integer>.Create;
end;

destructor TSampleDataset.Destroy;
begin
  fKeys.Free;
  inherited;
end;

end.
