unit xn.items;

interface

uses System.Generics.Collections, System.Generics.Defaults, xn.Types;

type
  IxnAsDebug = interface
    ['{5B72A0DE-137E-4EA5-85EB-E1AA48040612}']
    function AsDebug: String;
  end;

  IxnItems<T> = interface
    ['{E9BF116D-E07E-49DF-B065-AF9F3EB87D72}']
    function Count: integer;

    function ItemGet(aIndex: integer): T;
    property items[aIndex: integer]: T read ItemGet; default;
  end;

implementation

end.
