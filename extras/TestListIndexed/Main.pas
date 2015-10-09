unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections,
  System.Generics.Defaults,
  xn.list,
  xn.list.index, helpers, helpers.integer, helpers.myrecord, helpers.myobject;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    Memo3: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  NUM_TESTS = 1;

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
  IntegerTest(Form1, Memo1.Lines, Memo2.Lines, NUM_TESTS);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MyRecordTest(Form1, Memo1.Lines, Memo2.Lines, NUM_TESTS);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RandSeed := 0;

  MyObjectTest(Form1, Memo1.Lines, Memo2.Lines, NUM_TESTS);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Button1.Click;
  Button2.Click;
  Button3.Click;
end;

initialization

end.
