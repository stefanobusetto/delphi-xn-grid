unit uBitmaps;

interface

uses Winapi.Windows, Vcl.Graphics, Vcl.Forms, Vcl.Controls, System.Classes, System.Math;

type
  IBitmapCompareResult = interface
    ['{BC211798-7B90-4DF7-A0CB-E5344CD23EF7}']
    function BitmapGet: TBitmap;
    property Bitmap: TBitmap read BitmapGet;
    function CountGet: Integer;
    property Count: Integer read CountGet;
  end;

  TBitmapCompareResult = class(TInterfacedObject, IBitmapCompareResult)
  protected
    fBitmap: TBitmap;
    fCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function BitmapGet: TBitmap;
    property Bitmap: TBitmap read BitmapGet;
    function CountGet: Integer;
    property Count: Integer read CountGet;
  end;

function BitmapCrop(aBitmap: TBitmap; const aRect: TRect): TBitmap;

function BitmapControl(aForm: TForm; const aControl: TControl): TBitmap; overload;
function BitmapControl(aForm: TForm; const aControl: TControl; aBorder: TRect): TBitmap; overload;

function BitmapCompare(aBitmap1, aBitmap2: TBitmap; aThreshold: Integer): IBitmapCompareResult;

procedure BitmapGrayscale(aBitmap: TBitmap);

implementation

type
  PRGB32Record = ^TRGB32Record;

  TRGB32Record = packed record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;

  PRGB32Array = ^TRGB32Array;
  TRGB32Array = packed array [0 .. MaxInt div SizeOf(TRGB32Record) - 1] of TRGB32Record;

function BitmapCrop(aBitmap: TBitmap; const aRect: TRect): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := aRect.Right - aRect.Left;
  Result.Height := aRect.Bottom - aRect.Top;

  Result.PixelFormat := pf32bit;

  Result.Canvas.CopyRect(Rect(0, 0, Result.Width, Result.Height), aBitmap.Canvas, aRect);
end;

function BitmapControl(aForm: TForm; const aControl: TControl): TBitmap;
var
  b: TBitmap;
  r: TRect;
begin
  b := aForm.GetFormImage;
  b.PixelFormat := pf32bit;
  try
    r.Top := aControl.Top;
    r.Left := aControl.Left;
    r.Width := aControl.Width;
    r.Height := aControl.Height;

    Result := BitmapCrop(b, r);
  finally
    b.free;
  end;
end;

function BitmapControl(aForm: TForm; const aControl: TControl; aBorder: TRect): TBitmap; overload;
var
  zz: TBitmap;
  rr: TRect;
begin
  zz := BitmapControl(aForm, aControl);
  try
    rr.Top := aBorder.Top;
    rr.Left := aBorder.Left;
    rr.Right := zz.Width - aBorder.Right;
    rr.Bottom := zz.Height - aBorder.Bottom;

    Result := BitmapCrop(zz, rr);
    Result.PixelFormat := pf32bit;
  finally
    zz.free;
  end;
end;

procedure BitmapGrayscale(aBitmap: TBitmap);
var
  X: Integer;
  Y: Integer;
  P: PRGB32Record;
  G: Byte;
begin
  Assert(aBitmap.PixelFormat = pf32bit);
  for Y := 0 to (aBitmap.Height - 1) do
  begin
    P := aBitmap.ScanLine[Y];
    for X := 0 to (aBitmap.Width - 1) do
    begin
      G := Round(0.30 * P.Red + 0.59 * P.Green + 0.11 * P.Blue);
      P.Red := G;
      P.Green := G;
      P.Blue := G;
      Inc(P);
    end;
  end;
end;

function BitmapCompare(aBitmap1, aBitmap2: TBitmap; aThreshold: Integer): IBitmapCompareResult;
var
  l1: PRGB32Array;
  l2: PRGB32Array;
  X: Integer;
  Y: Integer;
  r: TBitmapCompareResult;
begin
  r := TBitmapCompareResult.Create;
  Result := r;

  r.fCount := 0;

  r.fBitmap := TBitmap.Create;
  r.fBitmap.Width := Max(aBitmap1.Width, aBitmap2.Width);
  r.fBitmap.Height := Max(aBitmap1.Height, aBitmap2.Height);

  r.fBitmap.PixelFormat := pf32bit;

  // Unione delle bitmap clLime
  r.fBitmap.Canvas.Pen.Color := clLime;
  r.fBitmap.Canvas.Brush.Color := clLime;
  r.fBitmap.Canvas.Rectangle(0, 0, Max(aBitmap1.Width, aBitmap2.Width), Max(aBitmap1.Height, aBitmap2.Height));

  // Intersezione delle bitmap clWhite
  r.fBitmap.Canvas.Pen.Color := clWhite;
  r.fBitmap.Canvas.Brush.Color := clWhite;
  r.fBitmap.Canvas.Rectangle(0, 0, Min(aBitmap1.Width, aBitmap2.Width), Min(aBitmap1.Height, aBitmap2.Height));

  // differenze clRed
  for Y := 0 to Min(aBitmap1.Height, aBitmap2.Height) - 1 do
  begin
    l1 := aBitmap1.ScanLine[Y];
    l2 := aBitmap2.ScanLine[Y];

    for X := 0 to Min(aBitmap1.Width, aBitmap2.Width) - 1 do
      if (abs(l1[X].Red - l2[X].Red) > aThreshold)
        or (abs(l1[X].Blue - l2[X].Blue) > aThreshold)
        or (abs(l1[X].Green - l2[X].Green) > aThreshold) then
      begin
        r.fCount := r.fCount + 1;
        r.fBitmap.Canvas.Pixels[X, Y] := clRed
      end;
  end;
end;

{ TBitmapCompareResult }

function TBitmapCompareResult.BitmapGet: TBitmap;
begin
  Result := fBitmap
end;

function TBitmapCompareResult.CountGet: Integer;
begin
  Result := fCount
end;

constructor TBitmapCompareResult.Create;
begin
  fBitmap := nil;
end;

destructor TBitmapCompareResult.Destroy;
begin
  fBitmap.free;
  inherited;
end;

end.
