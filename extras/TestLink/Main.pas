unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, xn.grid,
  Vcl.ExtCtrls, uBitmaps, Vcl.DBGrids, Data.DB, Datasnap.DBClient, uCommands,
  xn.grid.common, xn.grid.Link.sample;

type
  TForm1 = class(TForm)
    bt_fill: TButton;
    bt_append: TButton;
    Memo1: TMemo;
    btBitmapCompare: TButton;
    d0: TDataSource;
    DBGrid1: TDBGrid;
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
    bt_loop: TButton;
    xnGrid1_RecCount: TLabel;
    DbGrid1_RecCount: TLabel;
    bt_clear: TButton;
    bt_first: TButton;
    bt_last: TButton;
    bt_prior: TButton;
    bt_next: TButton;
    Memo2: TMemo;
    Button2: TButton;
    log_clear: TButton;
    Label4: TLabel;
    Label5: TLabel;
    DiffMin: TLabel;
    DiffMax: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Differenze: TLabel;
    Label8: TLabel;
    xnGrid1: TxnGrid;
    StringGrid1: TStringGrid;
    procedure bt_fillClick(Sender: TObject);
    procedure bt_insertClick(Sender: TObject);
    procedure bt_deleteClick(Sender: TObject);
    procedure bt_appendClick(Sender: TObject);
    procedure bt_editClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btBitmapCompareClick(Sender: TObject);
    procedure bt_loopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bt_clearClick(Sender: TObject);
    procedure bt_firstClick(Sender: TObject);
    procedure bt_lastClick(Sender: TObject);
    procedure bt_priorClick(Sender: TObject);
    procedure bt_nextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure log_clearClick(Sender: TObject);
  private
    { Private declarations }
    link0: IxnGridLinkCustom<string>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses xn.timers, System.Math;

{$R *.dfm}


function BitmapCompare: Integer;
var
  bb: TBitmap;
  b1: TBitmap;
  b2: TBitmap;
  c: TBitmapCompareResult;
begin
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
        Form1.xnGridImage.Picture.Bitmap.Assign(b1);
        Form1.DbGridImage.Picture.Bitmap.Assign(b2);
        Form1.DiffImage.Picture.Bitmap.Assign(c.Bitmap);
        Form1.DiffCount.Caption := c.Count.ToString();

        result := c.Count;
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
var
  r: Integer;
  c: Integer;
begin
  Form1.xnGrid1_RecNo.Caption := Form1.link0.RecNoGet.ToString();
  Form1.xnGrid1_RecCount.Caption := Form1.link0.RowCountGet.ToString();
  Form1.DbGrid1_RecNo.Caption := Form1.cds0.RecNo.ToString();
  Form1.DbGrid1_RecCount.Caption := Form1.cds0.RecordCount.ToString();

  if Form1.xnGrid1.Link <> nil then
  begin
    c := BitmapCompare();
    if c < StrToInt(Form1.DiffMin.Caption) then
      Form1.DiffMin.Caption := c.ToString();
    if c > StrToInt(Form1.DiffMax.Caption) then
      Form1.DiffMax.Caption := c.ToString();

    if c > 3 * Form1.link0.RowCountGet + 10 then
      raise Exception.Create('Bitmap compare error.');
  end;

  r := Form1.cds0.RecordCount;
  if r <> Form1.link0.RowCountGet then
    raise Exception.Create('RecordCount() error.');

  if Form1.cds0.RecordCount = 0 then
    r := 0
  else
    r := Form1.cds0.RecNo;
  if r <> Form1.link0.RecNoGet + 1 then
    raise Exception.Create('RecNo() error.');
end;

procedure ExecuteCommand(aCommand: String);
begin
  xnCommands.Execute(aCommand);
  Form1.Memo1.Lines.Add('------------------');
  Application.ProcessMessages;

  AfterCommand;
end;

procedure TForm1.bt_appendClick(Sender: TObject);
begin
  ExecuteCommand('append');
end;

procedure TForm1.bt_clearClick(Sender: TObject);
begin
  ExecuteCommand('clear');
end;

procedure TForm1.bt_insertClick(Sender: TObject);
begin
  ExecuteCommand('insert');
end;

procedure TForm1.bt_lastClick(Sender: TObject);
begin
  ExecuteCommand('last');
end;

procedure TForm1.bt_nextClick(Sender: TObject);
begin
  ExecuteCommand('next');
end;

procedure TForm1.bt_priorClick(Sender: TObject);
begin
  ExecuteCommand('prior');
end;

procedure TForm1.bt_deleteClick(Sender: TObject);
begin
  ExecuteCommand('delete');
end;

procedure TForm1.bt_editClick(Sender: TObject);
begin
  ExecuteCommand('edit');
end;

procedure TForm1.bt_fillClick(Sender: TObject);
begin
  ExecuteCommand('append');
  ExecuteCommand('append');
  ExecuteCommand('append');
  ExecuteCommand('append');
  ExecuteCommand('append');
  ExecuteCommand('append');
end;

procedure TForm1.bt_firstClick(Sender: TObject);
begin
  ExecuteCommand('first');
end;

procedure TForm1.bt_loopClick(Sender: TObject);
var
  i: Integer;
  c: string;
begin
  for i := 0 to 250 do
  begin
    if i mod 50 = 0 then
      Memo1.Lines.Clear;

    c := xnCommands.RandCommand();
    Memo2.Lines.Add(c);

    if SameText(c, 'clear') then
      Memo2.Lines.Clear;

    ExecuteCommand(c);

    Application.ProcessMessages;
    Form1.Caption := IntToStr(i);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ExecuteCommand('append');
  ExecuteCommand('append');
  ExecuteCommand('append');
  // ExecuteCommand('append');
  ExecuteCommand('first');
  log_clear.Click;
  ExecuteCommand('delete');
  // ExecuteCommand('prior');
end;

procedure TForm1.btBitmapCompareClick(Sender: TObject);
begin
  BitmapCompare()
end;

procedure TForm1.FormActivate(Sender: TObject);
begin

  xnCommands.Init(link0, cds0);

  xnGrid1.Link := link0;
  xnGrid1.Log := Memo1.Lines;

  AfterCommand;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cds0.CreateDataSet;
  cds0.LogChanges := False;
  cds0.Open;

  link0 := TxnGridLinkSample.Create;

  link0.LogSet(Memo1.Lines);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  xnGrid1.Log := nil;
  link0.LogSet(nil);
end;

procedure TForm1.log_clearClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

end.
