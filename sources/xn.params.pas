unit xn.params;

interface

uses xn.dictionary;

type
  IxnParams = interface(IxnDictionary<string, string>)
    ['{580EDDAE-0042-485D-AD49-9C13DAE410C5}']
    function ReplaceParams(aString: string): String;
    function ReplaceParamsQuoted(aString: string): String;
  end;

  TxnParams = class(TxnDictionary<string, string>, IxnParams)
    function ReplaceParams(aString: string): String;
    function ReplaceParamsQuoted(aString: string): String;
  end;

implementation

uses System.SysUtils, System.StrUtils;

{ TxnParams }

function TxnParams.ReplaceParams(aString: string): String;
var
  k: string;
begin
  Result := aString;
  for k in Keys do
    Result := AnsiReplaceText(Result, k, Items[k]);
end;

function TxnParams.ReplaceParamsQuoted(aString: string): String;
var
  k: string;
begin
  Result := aString;
  for k in Keys do
    Result := AnsiReplaceText(Result, k, QuotedStr(Items[k]));
end;

end.
