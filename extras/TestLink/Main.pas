unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, xn.grid,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    xnGrid1: TxnGrid;
    bt_fill: TButton;
    bt_append: TButton;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    bt_insert: TButton;
    bt_delete: TButton;
    bt_edit: TButton;
    edIndex: TEdit;
    Button1: TButton;
    Panel1: TPanel;
    Image2: TImage;
    procedure bt_fillClick(Sender: TObject);
    procedure bt_insertClick(Sender: TObject);
    procedure bt_deleteClick(Sender: TObject);
    procedure bt_appendClick(Sender: TObject);
    procedure bt_editClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  Id: integer = 0;

implementation

uses xn.timers;

{$R *.dfm}


function Index(): integer;
begin
  Result := StrToInt(Form1.edIndex.Text)
end;

function NewId(): string;
begin
  Inc(Id);
  Result := IntToStr(Id);
end;

procedure TForm1.bt_appendClick(Sender: TObject);
begin
  xnGrid1.Link.Append(NewId());
  xnGrid1.LogLink;
end;

procedure TForm1.bt_deleteClick(Sender: TObject);
begin
  if xnGrid1.Link.RowCount > Index() then
  begin
    xnGrid1.Link.delete(Index());
    xnGrid1.LogLink;
  end;
end;

procedure TForm1.bt_editClick(Sender: TObject);
begin
  if xnGrid1.Link.RowCount > Index() then
  begin
    xnGrid1.Link.Change(Index(), 'aaa');
    xnGrid1.LogLink;
  end;
end;

procedure TForm1.bt_fillClick(Sender: TObject);
var
  c: Char;
begin
  for c := 'a' to 'd' do
    xnGrid1.Link.Append(c);
  xnGrid1.LogLink;
end;

procedure TForm1.bt_insertClick(Sender: TObject);
begin
  xnGrid1.Link.insert(Index(), NewId());
  xnGrid1.LogLink;
end;

function BitmapCrop(aBitmap: TBitmap; const aRect: TRect): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := aRect.Right - aRect.Left;
  Result.Height := aRect.Bottom - aRect.Top;

  Result.Canvas.CopyRect(Rect(0, 0, Result.Width, Result.Height), aBitmap.Canvas, aRect);
end;

function BitmapControl(aForm: TForm; const aControl: TControl): TBitmap;
var
  b: TBitmap;
  r: TRect;
begin
  b := Form1.GetFormImage;
  try
    r.Top := aControl.Top;
    r.Left := aControl.Left;
    r.Width := aControl.Width;
    r.Height := aControl.Height;

    Result := BitmapCrop(b, r);
  finally
    b.free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  c: TBitmap;
begin
  c := BitmapControl(Form1, xnGrid1);

  try
    Image2.Picture.Bitmap.Assign(c);
  finally
    c.free;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  xnGrid1.Log := Memo1.Lines;
end;

end.
