unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, xn.grid;

type
  TForm1 = class(TForm)
    xnGrid1: TxnGrid;
    bt_fill: TButton;
    bt_insert: TButton;
    bt_delete: TButton;
    bt_append: TButton;
    bt_edit: TButton;
    procedure bt_fillClick(Sender: TObject);
    procedure bt_insertClick(Sender: TObject);
    procedure bt_deleteClick(Sender: TObject);
    procedure bt_appendClick(Sender: TObject);
    procedure bt_editClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.bt_appendClick(Sender: TObject);
begin
  xnGrid1.Link.Append('2');
end;

procedure TForm1.bt_deleteClick(Sender: TObject);
begin
  xnGrid1.Link.delete(0);
end;

procedure TForm1.bt_editClick(Sender: TObject);
begin
  xnGrid1.Link.Change(1, 'aaa');
end;

procedure TForm1.bt_fillClick(Sender: TObject);
var
  c: Char;
begin
  for c := 'a' to 'd' do
    xnGrid1.Link.Append(c);
end;

procedure TForm1.bt_insertClick(Sender: TObject);
begin
  xnGrid1.Link.insert(2, '2');
end;

end.
