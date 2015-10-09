unit helpers.myobject;

interface

uses System.Classes, Vcl.Dialogs, Vcl.Forms, xn.list.observer;

procedure MyObjectTest(aForm: TForm; aMemo1, aMemo2: TStrings; aCount: integer);

implementation

uses System.Math, System.SysUtils, System.StrUtils,
  System.Generics.Collections, System.Generics.Defaults,
  xn.list, xn.list.index, helpers;

procedure MyObjectTest(aForm: TForm; aMemo1, aMemo2: TStrings; aCount: integer);
var
  z: IMyObject;
  l2: IxnList<IMyObject>;
  l2s: string;
  l1: IxnListObserver<IMyObject>;
  i1: IxnListIndex<IMyObject>;
  i1s: string;
  c: integer;
  i: integer;
  j: integer;
  n: integer;
begin
  for c := 1 to aCount do
  begin
    TimerStart;
    l1 := TxnListObserver<IMyObject>.Create;
    l2 := TxnList<IMyObject>.Create;

    for i := 0 to 500 do // add
    begin
      n := RandomRange(1, 100);
      z := TMyObject.Create(n);
      l1.Add(z);
      l2.Add(z);
    end;

    for i := 1 to 100 do // delete
    begin
      j := RandomRange(1, l2.Count);
      l1.Delete(j);
      l2.Delete(j);
    end;

    i1 := TxnListIndex<IMyObject>.ConstructIndex(l1, TComparer<IMyObject>.Construct(MyObjectComparison));

    for i := 1 to 100 do // modify
    begin
      j := RandomRange(1, l2.Count);
      n := RandomRange(-50, 150);
      l1.Items[j].Cod := n;
      l2.Items[j].Cod := n;
      l1.NotifyModify(j);
    end;

    l2.Sort(TComparer<IMyObject>.Construct(MyObjectComparison));

    i1s := AsDebug(i1);
    aMemo2.Text := i1s;
    l2s := AsDebug(l2);
    aMemo1.Text := l2s;

    { n := i1.Seek1b(TxnComparer<IMyObject>.Construct(
      function(const aValue: IMyObject): integer
      begin
      Result := aValue.Cod - -2
      end));

      ShowMessage(inttostr(n));

      ShowMessage(i1.Items[n].Cod.ToString());
      ShowMessage(i1.Items[n + 1].Cod.ToString()); }
    // ShowMessage(i1.Items[n+2].Cod.ToString());
    // ShowMessage(i1.Items[n+3].Cod.ToString());

    if not SameStr(l2s, i1s) then
      ShowMessage('error !');

    l1.ObserversUnregister;
    TimerStop(aForm, inttostr(c));
  end;
end;

end.
