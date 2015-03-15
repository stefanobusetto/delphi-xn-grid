unit xn.timers;

interface

uses System.Classes;

type
  TxnTimer = class
  strict private
    fStart: TDateTime;
    fStop: TDateTime;
    fElapsed: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Reset;
    function Elapsed: Cardinal;
    procedure Print(aStrings: TStrings; aCaption: string); overload;
    procedure Print(aStrings: TStrings); overload;
  end;

  TxnTimers = class
  const
    MAX_TIMERS = 100;
  strict private
    fTimers: array [0 .. MAX_TIMERS] of TxnTimer;
    procedure IndexCheck(aIndex: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(aIndex: integer);
    procedure Stop(aIndex: integer);
    procedure Reset(aIndex: integer);
    function Elapsed(aIndex: integer): Cardinal;
    procedure Print(aIndex: integer; aStrings: TStrings; aCaption: string); overload;
    procedure Print(aIndex: integer; aStrings: TStrings); overload;
  end;

var
  xnTimers: TxnTimers;

implementation

uses Vcl.Forms, System.SysUtils;

{ TTimer }

constructor TxnTimer.Create;
begin
  Reset;
end;

destructor TxnTimer.Destroy;
begin
  inherited;
end;

function TxnTimer.Elapsed: Cardinal;
begin
  Result := fElapsed;
end;

procedure TxnTimer.Print(aStrings: TStrings);
begin
  Print(aStrings, '');
end;

procedure TxnTimer.Print(aStrings: TStrings; aCaption: string);
begin
  if aCaption <> '' then
    aCaption := aCaption + ' ';

  aStrings.Add(aCaption + IntToStr(Elapsed()));
end;

procedure TxnTimer.Reset;
begin
  fElapsed := 0;
end;

procedure TxnTimer.Start;
begin
  Application.ProcessMessages;
  fStart := Now;
end;

procedure TxnTimer.Stop;
begin
  Application.ProcessMessages;
  fStop := Now;
  fElapsed := fElapsed + Trunc((fStop - fStart) * 24 * 60 * 60 * 1000);
end;

{ TTimers }

constructor TxnTimers.Create;
var
  i: integer;
begin
  for i := 0 to MAX_TIMERS do
    fTimers[i] := TxnTimer.Create;
end;

destructor TxnTimers.Destroy;
var
  i: integer;
begin
  for i := 0 to MAX_TIMERS do
    fTimers[i].Free;
  inherited;
end;

function TxnTimers.Elapsed(aIndex: integer): Cardinal;
begin
  IndexCheck(aIndex);
  Result := fTimers[aIndex].Elapsed();
end;

procedure TxnTimers.IndexCheck(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex > MAX_TIMERS) then
    raise Exception.Create('Invalid timer index.');
end;

procedure TxnTimers.Print(aIndex: integer; aStrings: TStrings);
begin
  Print(aIndex, aStrings, '');
end;

procedure TxnTimers.Print(aIndex: integer; aStrings: TStrings; aCaption: string);
begin
  IndexCheck(aIndex);
  fTimers[aIndex].Print(aStrings, aCaption);
end;

procedure TxnTimers.Reset(aIndex: integer);
begin
  IndexCheck(aIndex);
  fTimers[aIndex].Reset;
end;

procedure TxnTimers.Start(aIndex: integer);
begin
  IndexCheck(aIndex);
  fTimers[aIndex].Start;
end;

procedure TxnTimers.Stop(aIndex: integer);
begin
  IndexCheck(aIndex);
  fTimers[aIndex].Stop;
end;

initialization

xnTimers := TxnTimers.Create;

finalization

xnTimers.Free;

end.
