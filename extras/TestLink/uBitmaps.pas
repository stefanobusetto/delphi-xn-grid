unit uBitmaps;

interface

uses Winapi.Windows, Vcl.Graphics, Vcl.Forms, Vcl.Controls, System.Classes, System.Math;

type
  TBitmapCompareResult = record
    Bitmap: TBitmap;
    Count: Integer;
  end;

function BitmapCrop(aBitmap: TBitmap; const aRect: TRect): TBitmap;

function BitmapControl(aForm: TForm; const aControl: TControl): TBitmap; overload;
function BitmapControl(aForm: TForm; const aControl: TControl; aBorder: TRect): TBitmap; overload;

function BitmapCompare(aBitmap1, aBitmap2: TBitmap; aThreshold: Integer): TBitmapCompareResult;

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

function BitmapCompare(aBitmap1, aBitmap2: TBitmap; aThreshold: Integer): TBitmapCompareResult;
var
  l1: PRGB32Array;
  l2: PRGB32Array;
  X: Integer;
  Y: Integer;
begin
  Result.Count := 0;

  Result.Bitmap := TBitmap.Create;
  Result.Bitmap.Width := Max(aBitmap1.Width, aBitmap2.Width);
  Result.Bitmap.Height := Max(aBitmap1.Height, aBitmap2.Height);

  Result.Bitmap.PixelFormat := pf32bit;

  // Unione delle bitmap clLime
  Result.Bitmap.Canvas.Pen.Color := clLime;
  Result.Bitmap.Canvas.Brush.Color := clLime;
  Result.Bitmap.Canvas.Rectangle(0, 0, Max(aBitmap1.Width, aBitmap2.Width), Max(aBitmap1.Height, aBitmap2.Height));

  // Intersezione delle bitmap clWhite
  Result.Bitmap.Canvas.Pen.Color := clWhite;
  Result.Bitmap.Canvas.Brush.Color := clWhite;
  Result.Bitmap.Canvas.Rectangle(0, 0, Min(aBitmap1.Width, aBitmap2.Width), Min(aBitmap1.Height, aBitmap2.Height));

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
        Result.Count := Result.Count + 1;
        Result.Bitmap.Canvas.Pixels[X, Y] := clRed
      end;
  end;
end;

end.
