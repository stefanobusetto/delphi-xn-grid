unit xn.grid.Register;

interface

uses ToolsAPI, System.Classes;

type
  TxnGridAbout = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
  end;

procedure Register;

implementation

uses Vcl.Menus, Vcl.Dialogs, Vcl.Graphics,
  xn.grid;

// *****************************************************************************
// *****************************************************************************
procedure SplashTextAdd;
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    b.LoadFromResourceName(HInstance, 'LOGO');
    SplashScreenServices.AddPluginBitmap('xnGrid', b.Handle, false, '', '');
  finally
    b.Free;
  end;
end;

// *****************************************************************************
// *****************************************************************************

procedure Register;
begin
  RegisterComponents('Data Controls', [TxnGrid]);

  RegisterPackageWizard(TxnGridAbout.Create);
end;

{ TxnGridAbout }

procedure TxnGridAbout.Execute;
begin
  ShowMessage('stefano.busetto@gmail.com')
end;

function TxnGridAbout.GetIDString: string;
begin
  Result := 'xn.grid.about';
end;

function TxnGridAbout.GetMenuText: string;
begin
  Result := 'About xnGrid'
end;

function TxnGridAbout.GetName: string;
begin
  Result := 'About xnGrid'
end;

function TxnGridAbout.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

initialization

SplashTextAdd;

finalization

end.
