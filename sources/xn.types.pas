unit xn.types;

interface

type
  TxnNotifyAction = (naAdd, naModify, naDelete, naClear, naSort);

  IxnItemsNotify<T> = interface
    ['{A1638A29-DC7B-4832-931D-1F73BA208708}']
    procedure Notify(aAction: TxnNotifyAction; aIndex: integer);
  end;

implementation

end.
