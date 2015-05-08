unit Main;

interface

uses Generics.collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UniProvider, SQLServerUniProvider,
  Data.DB, MemDS, DBAccess, Uni, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, dxmdaset,
  cSampleDataset, cDatasource, Vcl.Mask, Vcl.DBCtrls, xnUniQuery, Vcl.ExtCtrls,
  Vcl.ComCtrls, xn.dataset.cache;

type
  TForm1 = class(TForm)
    DB: TUniConnection;
    q0: TUniQuery;
    mssql: TSQLServerUniProvider;
    qc0: TUniQuery;
    d0: TDataSource;
    m1: TdxMemData;
    d1: TDataSource;
    m1nn: TIntegerField;
    m1ss: TStringField;
    qc11: TUniSQL;
    qc13: TUniSQL;
    q2: TxnUniQuery;
    d2: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    g1: TDBGrid;
    clear_memo: TButton;
    Button1: TButton;
    g0: TDBGrid;
    g2: TDBGrid;
    q2_open: TButton;
    q2_close: TButton;
    q2_apply_updates: TButton;
    q2_cancel_updates: TButton;
    DBNavigator1: TDBNavigator;
    q2_create_filled: TButton;
    DBNavigator2: TDBNavigator;
    sg2: TStringGrid;
    q2_create_empty: TButton;
    qc12a: TUniSQL;
    qc12b: TUniSQL;
    q2_create_opt: TRadioGroup;
    l2: TMemo;
    Button3: TButton;
    l2b: TMemo;
    procedure dbAfterConnect(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure clear_memoClick(Sender: TObject);
    procedure q2_openClick(Sender: TObject);
    procedure q2_closeClick(Sender: TObject);
    procedure q2_apply_updatesClick(Sender: TObject);
    procedure q2_create_filledClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure q2AfterScroll(dataset: TDataSet);
    procedure Button3Click(Sender: TObject);
    procedure q2AfterPost(dataset: TDataSet);
    procedure q2NewRecord(dataset: TDataSet);
    procedure q2AfterInsert(dataset: TDataSet);
    procedure q2BeforePost(DataSet: TDataSet);
    procedure q2BeforeInsert(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  me: TmyEdit;

  sds: TSampleDataset;

  q2_cache: TxnDatasetCache;

  // xnDataSource: TxnDataSource;

implementation

uses Math;

{$R *.dfm}

function BookmarkToStr(aBookmark:Tbookmark): string;
var
  b: byte;
begin
  for b in aBookmark do
  begin
    if Result <> '' then
      Result := Result + ':';
    Result := Result + IntToHex(b, 2);
  end;
end;


procedure info;
var
  a: string;
  b: string;
begin
  a := sds.AsDebug;
  b := me.Link.AsDebug;

  if a <> b then
    raise Exception.Create('Error Message');
  exit;

  with Form1.Memo1.Lines do
    if a = b then
      Add(a + ' = ' + b)
    else
      Add(a + ' = ' + b + ' error ');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  q0.Open
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: integer;
  j: integer;
begin
  Form1.Memo1.Lines.Add('=========== append ');
  sds.ActionAppend;
  Form1.Memo1.Lines.Add('=========== update ');
  sds.ActionUpdate;
  Form1.Memo1.Lines.Add('=========== append ');
  sds.ActionAppend;
  exit;
  sds.ActionAppend;
  exit;

  m1.Prior;
  sds.ActionUpdate;
  sds.ActionInsert;
  m1.Prior;
  m1.Prior;

  info;
  sds.ActionDelete;
  info;
  exit;

  sds.ActionAppend;
  info;
  sds.ActionUpdate;
  info;
  exit;

  for j := 0 to 0 do
  begin

    // d.ActionAppend;
    // d.ActionAppend;

    for i := 1 to sds.NUM_REC * 2 do
    begin
      if m1.RecordCount >= sds.NUM_REC then
        sds.ActionClear;

      sds.ActionRandom;
      info;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  q2_cache.PaintGrid;
end;

procedure TForm1.q2AfterInsert(dataset: TDataSet);
begin
  l2b.Lines.Add('q2AfterInsert()');
  l2b.Lines.Add( IntToStr( DataSet.RecNo));
  l2b.Lines.Add( BookmarkToStr( DataSet.Bookmark));
  q2_cache.OnAfterInsert(dataset);
end;

procedure TForm1.q2AfterPost(dataset: TDataSet);
begin
  l2b.Lines.Add('q2AfterPost()');
  l2b.Lines.Add( IntToStr( DataSet.RecNo));
  l2b.Lines.Add( BookmarkToStr( DataSet.Bookmark));
  q2_cache.OnAfterPost(dataset);
end;

procedure TForm1.q2AfterScroll(dataset: TDataSet);
begin
  // l2.Lines.Add(IntToStr(dataset.RecNo));
end;

procedure TForm1.q2BeforeInsert(DataSet: TDataSet);
begin
    l2b.Lines.Add('q2BeforeInsert()');
  l2b.Lines.Add( IntToStr( DataSet.RecNo));
    l2b.Lines.Add( BookmarkToStr( DataSet.Bookmark));
end;

procedure TForm1.q2BeforePost(DataSet: TDataSet);
begin
    l2b.Lines.Add('q2BeforePost()');
  l2b.Lines.Add( IntToStr( DataSet.RecNo));
    l2b.Lines.Add( BookmarkToStr( DataSet.Bookmark));
end;

procedure TForm1.q2NewRecord(dataset: TDataSet);
begin
  l2b.Lines.Add('q2NewRecord()');
  l2b.Lines.Add( IntToStr( DataSet.RecNo));
  l2b.Lines.Add( BookmarkToStr( DataSet.Bookmark));
  q2_cache.OnNewRecord(dataset);
end;

procedure TForm1.q2_apply_updatesClick(Sender: TObject);
begin
  if q2.UpdatesPending then
    q2.ApplyUpdates;
end;

procedure TForm1.q2_closeClick(Sender: TObject);
begin
  q2.Close;
end;

procedure TForm1.q2_create_filledClick(Sender: TObject);
begin
  qc11.Execute;
  if TButton(Sender).Tag = 0 then
    if q2_create_opt.ItemIndex = 0 then
      qc12a.Execute
    else
      qc12b.Execute;
  qc13.Execute;
end;

procedure TForm1.q2_openClick(Sender: TObject);
begin
  q2.Open;

  q2_cache.Clear;
  q2_cache.Load(q2);
  q2_cache.PaintGrid();
end;

procedure TForm1.clear_memoClick(Sender: TObject);
begin
  Memo1.Clear;
  Memo2.Clear;
end;

procedure TForm1.dbAfterConnect(Sender: TObject);
begin
  qc0.ExecSQL;
  q2_create_filled.Click
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // xnDataSource := TxnDataSource.Create(Form1);
  // xnDataSource.DataSet := m1;
  // g2.DataSource := xnDataSource;

  sg2.ColWidths[0] := 11;

  sds := TSampleDataset.Create(m1);

  me := TmyEdit.Create(Form1);

  me.Strings := Memo1.Lines;
  me.Strings2 := Memo2.Lines;
  me.Link.Strings := me.Strings;
  me.Link.Strings2 := me.Strings2;
  me.Link.DataSource := d1;
  me.Link.DataLoad;

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  q2_cache.Free;
  sds.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  q2_cache := TxnDatasetCache.Create(d2, 2);
  q2_cache.Strings := l2.Lines;
  q2_cache.Grid := sg2
end;

end.
