unit Main;

interface

uses Generics.Collections, Generics.Defaults, cSampleData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SortArray, SortIndex,
  xn.timers, xn.grid.common, xn.grid.data, xn.grid.link;

type
  TFormMain = class(TForm)
    bt_QuickSort: TButton;
    memo_2: TMemo;
    bt_seek1: TButton;
    bt_seek2: TButton;
    procedure bt_QuickSortClick(Sender: TObject);
    procedure bt_seek1Click(Sender: TObject);
    procedure bt_seek2Click(Sender: TObject);
  protected
    procedure InitArray(aCount: Integer);

    { Private declarations }
  public
    { Public declarations }
    fIndex: TList<Integer>;
  end;

var
  FormMain: TFormMain;

implementation

const
  COL = 2;

{$R *.dfm}


function ArrayDebug(aCount, aColumn: Integer): string;
var
  r: Integer;
  // c: Integer;
begin
  Result := '';
  for r := 0 to aCount - 1 do
  begin
    // for c := 0 to 5 do
    // begin
    Result := Result + TSampleData.Value(aColumn, FormMain.fIndex[r]) + ' ';
    // end;
    // Result := Result + #13 + #10;
  end;
end;

procedure TFormMain.bt_QuickSortClick(Sender: TObject);
const
  NN = 2000;
  CC = 0;
var
  c: TxnQuickSort.TComparer;
  v: TxnQuickSort.TGetter;
begin
  c := function(const aLeft, aRight: string): Integer
    begin
      if aLeft < aRight then
        exit(-1)
      else if aLeft > aRight then
        exit(+1)
      else
        exit(0);
    end;

  v := function(const aIndex: Integer): string
    begin
      Result := TSampleData.Value(CC, aIndex)
    end;

  InitArray(NN);
  FormMain.memo_2.Lines.Clear;
  with TxnQuickSort.Create(v, c) do
    try
      xnTimers.Start(0);
      sort(fIndex);
      xnTimers.Stop(0);
    finally
      Free;
    end;
  FormMain.memo_2.Lines.add(ArrayDebug(NN, CC));
  xnTimers.Print(0, FormMain.memo_2.Lines);
end;

function BuildArray(aNumber: double): TArray<variant>;
begin
  SetLength(Result, 1);
  Result[0] := aNumber;
end;

procedure TFormMain.bt_seek1Click(Sender: TObject);
var
  gd: IxnGridData;
  sgd: TxnGridDataSort;
  fi: IxnGridFilterItems;
  si: IxnGridSortItems;
  n: Integer;
  s: Integer;
  a: TArray<variant>;
begin
  Application.ProcessMessages;

  SetLength(a, 1);

  memo_2.Lines.Clear;
  gd := TSampleGridData2By2.Create(1000);
  fi := TxnGridFilterItems.Create;
  si := TxnGridSortItems.Create;
  si.add(TxnGridSortItem.Create(0, gskNum, gsoAsc));

  sgd := TxnGridDataSort.Create(gd, si);
  try
    xnTimers.Reset(0);
    xnTimers.Start(0);
    sgd.sort;
    memo_2.Lines.add('ok count = ' + IntToStr(gd.RowCountGet));
    // memo_2.Lines.add('array = ' + gd.AsDebug);

    for n := 0 to sgd.RowCountGet - 1 do
    begin
      a := BuildArray(2 * n);
      s := sgd.Seek1(a);

      if s <> n then
        memo_2.Lines.add('error not found ' + IntToStr(a[0]));
    end;

    memo_2.Lines.add('ok count = ' + IntToStr(sgd.RowCountGet));
    // memo_2.Lines.add('array = ' + sgd.AsDebug);
    xnTimers.Stop(0);
    xnTimers.Print(0, FormMain.memo_2.Lines, 'ok time =');
  finally
    sgd.Free;
  end;
end;

procedure TFormMain.bt_seek2Click(Sender: TObject);
var
  gd: IxnGridData;
  sgd: TxnGridDataSort;
  fi: IxnGridFilterItems;
  si: IxnGridSortItems;
  n: Integer;
  s: Integer;
  a: TArray<variant>;
begin
  memo_2.Lines.Clear;
  gd := TSampleGridDataRandom.Create(1000);
  fi := TxnGridFilterItems.Create;
  si := TxnGridSortItems.Create;
  si.add(TxnGridSortItem.Create(0, gskNum, gsoAsc));

  sgd := TxnGridDataSort.Create(gd, si);
  try
    xnTimers.Reset(0);
    xnTimers.Start(0);
    sgd.sort;
    memo_2.Lines.add('ok count = ' + IntToStr(gd.RowCountGet));
    // memo_2.Lines.add('array = ' + gd.AsDebug);

    for n := -2 to sgd.RowCountGet + 2 do
    begin
      Application.ProcessMessages;
      FormMain.Caption := IntToStr(n);
      a := BuildArray(n);
      s := sgd.Seek2(a);

      if s = 0 then
      begin
        if a[0] > sgd.ValueFloat(0, s) then
          memo_2.Lines.add('error 0 seek(' + IntToStr(a[0]) + ') = ' + IntToStr(s));
      end
      else if s = -1 then
      begin
        if a[0] < sgd.ValueFloat(0, sgd.RowCountGet - 1) then
          memo_2.Lines.add('error 1 seek(' + IntToStr(a[0]) + ') = ' + IntToStr(s));
      end
      else
      begin
        if (a[0] < sgd.ValueFloat(0, s - 1))
          or (a[0] > sgd.ValueFloat(0, s)) then
          memo_2.Lines.add('error 2 seek(' + IntToStr(a[0]) + ') = ' + IntToStr(s));
      end;
    end;

    memo_2.Lines.add('ok count = ' + IntToStr(sgd.RowCountGet));
    // memo_2.Lines.add('array = ' + sgd.AsDebug);
    xnTimers.Stop(0);
    xnTimers.Print(0, FormMain.memo_2.Lines, 'ok time =');
  finally
    sgd.Free;
  end;
end;

procedure TFormMain.InitArray(aCount: Integer);
var
  i: Integer;
  n: Integer;
begin
  fIndex := TList<Integer>.Create;
  n := 0;
  for i := 0 to aCount - 1 do
  begin
    fIndex.add(n);
    inc(n);
    if n > 19 then
      n := 0;
  end;
end;

end.
