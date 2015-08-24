unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections,
  System.Generics.Defaults,
  xn.list,
  xn.list.indexed;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses math;

{$R *.dfm}


function IntegerComparison(const aLeft, aRight: integer): integer;
begin
  Result := aLeft - aRight;
end;

function AsDebug(aList: TList<integer>): string; overload;
var
  i: integer;
begin
  Result := '';
  for i in aList do
    Result := Result + i.ToString() + ' ';
end;

function AsDebug(aList: IxnListIndexed<integer>): string; overload;
var
  i: integer;
begin
  Result := '';
  for i in aList do
    Result := Result + i.ToString() + ' ';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  z: TList<integer>;
  zs: string;

  l: IxnList<integer>;
  li: IxnListIndexed<integer>;

  lis: string;

  c: integer;
  i: integer;
  n: integer;
  r: integer;

  t0: TDateTime;
  t1: TDateTime;
begin
  for c := 1 to 1 do
  begin
    t0 := Now;
    Application.ProcessMessages;

    l := TxnList<integer>.Create;

    z := TList<integer>.Create;
    try
      // execution time
      // 100 items = 1ms
      // 1000 items = 2ms
      // 10000 items = 25ms
      // 100000 items = 1031ms

      // add random items
      for i := 0 to 500 do
      begin
        n := RandomRange(1, 100);
        z.Add(n);
        l.Add(n);
      end;

      // // remove random items
      for i := 1 to 100 do
      begin
        n := RandomRange(1, z.Count);
        l.Delete(n);
        z.Delete(n);
      end;

      z.Sort(TComparer<integer>.Construct(IntegerComparison));
      zs := AsDebug(z);
      Memo1.Text := zs;
    finally
      z.Free;
    end;

    li := TxnListIndexed<integer>.Create(l, TComparer<integer>.Construct(IntegerComparison));
    l.ObserverRegister(li);

    lis := AsDebug(li);
    Memo2.Text := lis;

    if not SameStr(zs, lis) then
      ShowMessage('errore');

    Application.ProcessMessages;
    t1 := Now;
    Application.ProcessMessages;
    Form1.Caption := inttostr(c) + ' elapsed time ' + FormatDateTime('ss:zzz', t1 - t0);

    l.ObserverRegister(nil);
  end;

end;

end.
