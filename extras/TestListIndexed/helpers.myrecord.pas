unit helpers.myrecord;

interface

uses System.Classes, Vcl.Dialogs, Vcl.Forms, xn.list.observer;

procedure MyRecordTest(aForm: TForm; aMemo1, aMemo2: TStrings; aCount: integer);

implementation

uses System.Math, System.SysUtils, System.StrUtils,
  System.Generics.Collections, System.Generics.Defaults,
  xn.list, xn.list.index, helpers;

procedure MyRecordTest(aForm: TForm; aMemo1, aMemo2: TStrings; aCount: integer);
var
  z: TMyRecord;
  l2: IxnList<TMyRecord>;
  l2s: string;
  l1: IxnListObserver<TMyRecord>;
  i1: IxnListIndex<TMyRecord>;
  i1s: string;
  c: integer;
  i: integer;
  j: integer;
  n: integer;
begin
  for c := 1 to aCount do
  begin
    TimerStart;
    l1 := TxnListObserver<TMyRecord>.Create;
    l2 := TxnList<TMyRecord>.Create;

    for i := 0 to 500 do // add
    begin
      n := RandomRange(1, 100);
      z.Create(n);
      l1.Add(z);
      l2.Add(z);
    end;

    for i := 1 to 100 do // delete
    begin
      j := RandomRange(1, l2.Count);
      l1.Delete(j);
      l2.Delete(j);
    end;

    i1 := TxnListIndex<TMyRecord>.ConstructIndex(l1, TComparer<TMyRecord>.Construct(MyRecordComparison));

    for i := 1 to 100 do // modify
    begin
      j := RandomRange(1, l2.Count);
      n := RandomRange(-50, 150);
      l1.Items[j].Cod := n; // do nothing
      l2.Items[j].Cod := n; // do nothing
      l1.NotifyModify(j);
    end;

    l2.Sort(TComparer<TMyRecord>.Construct(MyRecordComparison));

    i1s := AsDebug(i1);
    aMemo2.Text := i1s;
    l2s := AsDebug(l2);
    aMemo1.Text := l2s;

    if not SameStr(l2s, i1s) then
      ShowMessage('error !');

    l1.ObserversUnregister;
    TimerStop(aForm, IntToStr(c));
  end;
end;

end.
