unit xn.types;

interface

type
  IxnAsDebug = interface
    ['{5B72A0DE-137E-4EA5-85EB-E1AA48040612}']
    function AsDebug: String;
  end;

  TxnAction = (naAdd, naChange, naDelete, naClear, naSort);

  IxnActionObserver = interface
    ['{A1638A29-DC7B-4832-931D-1F73BA208708}']
    procedure Notify(aAction: TxnAction; aIndex: integer);
  end;

//  IxnActionObservable = interface
//    ['{2C534D54-0AFC-4213-9847-B39F08D15274}']
//    procedure Notify(aAction: TxnAction; aIndex: integer);
//
//    procedure ObserverRegister(aObserver: IxnActionObserver);
//    procedure ObserverUnregister(aObserver: IxnActionObserver);
//    procedure ObserverUnregisterAll;
//  end;

  // IFoo = interface
  // end;
  //
  // IBar = interface
  // end;
  //
  // TFooBar = class(TInterfacedObject, IFoo, IBar)
  // end;

implementation

end.
