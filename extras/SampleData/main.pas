unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, cSampleData;

type
  TFormMain = class(TForm)
    bt_create: TButton;
    txt_pas: TMemo;
    txt_csv: TMemo;
    procedure bt_createClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses System.Math;

{$R *.dfm}


const
  CC: array [1 .. 18] of string = ('Black', 'Maroon', 'Green', 'Olive', 'Navy', 'Purple', 'Teal',
    'Gray', 'Silver', 'Red', 'Lime', 'Yellow', 'Blue', 'Fuchsia', 'Aqua', 'LtGray', 'DkGray', 'White');

  SS: array [1 .. 50] of string = ('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI',
    'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE',
    'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX',
    'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY');

  FF: array [1 .. 18] of string = ('2,56', '4,04', '3,94', '1,53', '1,48', '3,17', '3,76',
    '1,46', '1,51', '2,01', '2,9', '2,82', '2,06', '4,74', '4,88', '3,6', '2,06', '1,97');

  II: array [1 .. 18] of string = ('80', '71', '32', '68', '47', '34',
    '29', '10', '8', '25', '65', '77', '70', '75', '10', '71', '6', '88');

  BB: array [1 .. 2] of string = ('T', 'F');

function RowCreate(aNN, aCC, aSS, aFF, aII, aBB: string; aQuoted: Boolean; aSeparator: string): string;
begin
  if aQuoted then
  begin
    aNN := QuotedStr(aNN);
    aCC := QuotedStr(aCC);
    aSS := QuotedStr(aSS);
    aFF := QuotedStr(aFF);
    aII := QuotedStr(aII);
    aBB := QuotedStr(aBB);
  end;

  Result := aNN + aSeparator + aCC + aSeparator +
    aSS + aSeparator + aFF + aSeparator +
    aII + aSeparator + aBB;
end;

procedure TableCreate(aName: string; aCount: Integer);
var
  n, c: Integer;
  s, f: Integer;
  i, b: Integer;

  xls: string;
  pas: string;
begin
  xls := '{';
  pas := aName + ' : array [ 0 .. ' + IntToStr(aCount - 1) + ' , 0 .. 5 ] of string = ( ';
  for n := 1 to aCount do
  begin
    c := RandomRange(Low(CC), High(CC) + 1);
    s := RandomRange(Low(SS), High(SS) + 1);
    f := RandomRange(Low(FF), High(FF) + 1);
    i := RandomRange(Low(II), High(II) + 1);
    b := RandomRange(Low(BB), High(BB) + 1);

    xls := xls + #13 + #10 + '' + RowCreate(IntToStr(n - 1), CC[c], SS[s], FF[f], II[i], BB[b], False, ';') + '';
    pas := pas + #13 + #10 + '(' + RowCreate(IntToStr(n - 1), CC[c], SS[s], FF[f], II[i], BB[b], True, ',') + ')';

    if n <> aCount then
      pas := pas + ' , ';
  end;
  pas := pas + #13 + #10 + ') ;';
  xls := xls + #13 + #10 + '} ';

  FormMain.txt_pas.Lines.Text := pas;
  FormMain.txt_csv.Lines.Text := xls;
end;

procedure TFormMain.bt_createClick(Sender: TObject);
begin
  TableCreate('TABLE_20', 20);

  // TableCreate('TABLE_200', 200);
  // TableCreate('TABLE_2000', 2000);
end;

end.
