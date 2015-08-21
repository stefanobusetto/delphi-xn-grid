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
begin
  for c := 1 to 1 do
  begin
    Application.ProcessMessages;
    Form1.Caption := c.ToString();
    Application.ProcessMessages;

    l := TxnList<integer>.Create;
    li := TxnListIndexed<integer>.Create(l, TComparer<integer>.Construct(IntegerComparison));

    z := TList<integer>.Create;
    try
      for i := 0 to 250 do
      begin
        n := RandomRange(1, 100);
        z.Add(n);
        li.Add(n);
      end;

      z.Sort(TComparer<integer>.Construct(IntegerComparison));

      r := z.Remove(z.Items[45]);

      zs := '';
      for i in z do
        zs := zs + i.ToString() + ' ';
      Memo1.Text := zs;
    finally
      z.Free;
    end;

    // li.Fill;
    // li.Sort;
    lis := '';
    for i in li do
      lis := lis + i.ToString() + ' ';
    Memo2.Text := lis;

    if not SameStr(zs, lis) then
      ShowMessage('errore');
  end;
end;

end.
