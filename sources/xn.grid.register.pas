unit xn.grid.register;

interface

uses System.Classes;

procedure Register;

implementation

uses xn.grid;

procedure Register;
begin
  RegisterComponents('Data Controls', [TxnGrid]);
end;

end.
