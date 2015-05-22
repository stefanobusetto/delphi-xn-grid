unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, xn.grid,
  Vcl.ExtCtrls, uBitmaps, Vcl.DBGrids, Data.DB, Datasnap.DBClient, uCommands;

type
  TForm1 = class(TForm)
    bt_fill: TButton;
    bt_append: TButton;
    Memo1: TMemo;
    btBitmapCompare: TButton;
    d0: TDataSource;
    DBGrid1: TDBGrid;
    xnGrid1: TxnGrid;
    xnGridImage: TImage;
    DbGridImage: TImage;
    DiffImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DiffCount: TLabel;
    cds0: TClientDataSet;
    cds0cod: TStringField;
    cds0des: TStringField;
    cds0grp: TStringField;
    bt_insert: TButton;
    bt_delete: TButton;
    bt_edit: TButton;
    xnGrid1_RecNo: TLabel;
    DbGrid1_RecNo: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    xnGrid1_RecCount: TLabel;
    DbGrid1_RecCount: TLabel;
    procedure bt_fillClick(Sender: TObject);
    procedure bt_insertClick(Sender: TObject);
    procedure bt_deleteClick(Sender: TObject);
    procedure bt_appendClick(Sender: TObject);
    procedure bt_editClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btBitmapCompareClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses xn.timers, System.Math;

{$R *.dfm}


function BitmapCompare: integer;
var
  bb: TBitmap;
  b1: TBitmap;
  b2: TBitmap;
  c: TBitmapCompareResult;
begin
  result := 9999;
  bb := TBitmap.Create;
  try
    bb.Width := 1;
    bb.Height := 1;
    Form1.xnGridImage.Picture.Bitmap.Assign(bb);
    Form1.DbGridImage.Picture.Bitmap.Assign(bb);
    Form1.DiffImage.Picture.Bitmap.Assign(bb);
    Form1.DiffCount.Caption := '';
  finally
    bb.Free;
  end;

  b1 := BitmapControl(Form1, Form1.xnGrid1, TRect.Create(12, 18, 20, 0));
  BitmapGrayscale(b1);

  try
    b2 := BitmapControl(Form1, Form1.DBGrid1, TRect.Create(12, 18, 20, 0));
    BitmapGrayscale(b2);
    try
      c := uBitmaps.BitmapCompare(b1, b2, 0);

      try
        if c.Count > 0 then
        begin
          Form1.xnGridImage.Picture.Bitmap.Assign(b1);
          Form1.DbGridImage.Picture.Bitmap.Assign(b2);
          Form1.DiffImage.Picture.Bitmap.Assign(c.Bitmap);
          Form1.DiffCount.Caption := c.Count.ToString();
          result := c.Count;
        end;
      finally
        c.Bitmap.Free
      end;
    finally
      b2.Free;
    end;
  finally
    b1.Free;
  end;
end;

procedure AfterCommand;
begin
  if BitmapCompare() > 50 then
    raise Exception.Create('Bitmap compare error.');

  if Form1.cds0.RecordCount <> Form1.xnGrid1.link.RowCount then
    raise Exception.Create('RecordCount() error.');

  // if Form1.cds0.RecNo <> Form1.xnGrid1.link.RecNo + 1 then
  // raise Exception.Create('RecNo() error.');

  Form1.xnGrid1.LogLink;

  Form1.xnGrid1_RecNo.Caption := Form1.xnGrid1.link.RecNo.ToString();
  Form1.xnGrid1_RecCount.Caption := Form1.xnGrid1.link.RowCount.ToString();
  Form1.DbGrid1_RecNo.Caption := Form1.cds0.RecNo.ToString();
  Form1.DbGrid1_RecCount.Caption := Form1.cds0.RecordCount.ToString();
end;

procedure TForm1.bt_appendClick(Sender: TObject);
begin
  uCommands.Append(NewId());
  AfterCommand;
end;

procedure TForm1.bt_deleteClick(Sender: TObject);
begin
  uCommands.delete;
  AfterCommand;
end;

procedure TForm1.bt_editClick(Sender: TObject);
begin
  uCommands.Edit(NewId());
  AfterCommand;
end;

procedure TForm1.bt_fillClick(Sender: TObject);
begin
  uCommands.Append(NewId());
  uCommands.Append(NewId());
  uCommands.Append(NewId());
  uCommands.Append(NewId());
  AfterCommand;
end;

procedure TForm1.bt_insertClick(Sender: TObject);
begin
  uCommands.insert(NewId());
  AfterCommand;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 50 do
  begin
    Execute(RandCommand());
    Application.ProcessMessages;
    AfterCommand;
  end;
end;

procedure TForm1.btBitmapCompareClick(Sender: TObject);
begin
  BitmapCompare()
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  cds0.CreateDataSet;
  cds0.LogChanges := False;
  cds0.Open;

  uCommands.Init(xnGrid1.link, cds0);

  xnGrid1.Log := Memo1.Lines;

  AfterCommand;
end;

end.
