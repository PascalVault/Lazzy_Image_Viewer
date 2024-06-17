unit PV_Filters;

//Lazzy Image Viewer
//github.com/PascalVault
//License: MIT

interface

uses Math, PV_Bitmap;

// Resampling ------------------------------------------------------------------
// Based on Bitmap Resampler by Anders Melander (15-03-1998), which was based on
// filter.c by Dale Schumacher (published in Graphics Gems III, p. 8-16), with
// improvements by David Ullrich.

type
  TResampleFilter = (rfBox, rfBilinear, rfHermite, rfBell, rfSpline, rfLanczos3, rfMitchell);

  { TPV_Bitmap }

  TPV_Bitmap = class(PV_Bitmap.TPV_Bitmap)
  public
    procedure Convolution(Kernel: array of Double; Size: Byte; Divider: Integer=1);

    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;

    procedure AddNoise(Amount: Byte);
    procedure DeNoise;
    procedure SmarterBlur;

    procedure Brightness(Amount: Byte);
    procedure Contrast(Amount: Extended);
    procedure FindEdges;
    procedure FindEdges2;
    procedure FindEdges3;

    procedure Negate;
    procedure Gamma(Amount: Extended);

    procedure Sharpen;
    procedure Emboss;
    procedure BoxBlur;
    procedure GaussBlur;
    procedure GaussBlur5;
    procedure Unsharp;
    procedure Sepia(Amount: Byte = 30);

    function ResampleTo(DstWidth, DstHeight: Integer; Filter: TResampleFilter): TPV_Bitmap;
    procedure Resample(AWidth, AHeight: Integer; Filter: TResampleFilter);
    procedure ResamplePercent(AWidth, AHeight: Integer; Filter: TResampleFilter);

    procedure BGR;
    procedure BRG;
    procedure GBR;
    procedure GRB;
    procedure RBG;

    procedure ExtractRed;
    procedure ExtractGreen;
    procedure ExtractBlue;
    procedure ExtractAlpha;

    procedure ExtractCyan;
    procedure ExtractMagenta;
    procedure ExtractYellow;
    procedure ExtractBlack;
  end;

implementation

// -----------------------------------------------------------------------------
//
//			Filter functions
//
// -----------------------------------------------------------------------------

// Hermite filter
function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Box filter
// a.k.a. "Nearest Neighbour" filter
function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter
function TriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter
function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end else
    Result := 0.0;
end;

// B-spline filter
function SplineFilter(Value: Single): Single;
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5*tt*Value - tt + 2.0 / 3.0;
  end else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0/6.0 * Sqr(Value) * Value;
  end else
    Result := 0.0;
end;

// Lanczos3 filter
function Lanczos3Filter(Value: Single): Single;
  function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B		= (1.0 / 3.0);
  C		= (1.0 / 3.0);
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end else
  if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end else
    Result := 0.0;
end;

// -----------------------------------------------------------------------------
//			Interpolator
// -----------------------------------------------------------------------------
type
  TFilterProc = function(Value: Single): Single;
  TResamplers = record
    Filter: TFilterProc;
    Width: Single;
  end;

  TRGB = record
    R,G,B: Single;
  end;

  // Contributor for a pixel
  TContributor = record
    pixel: integer;		// Source pixel
    weight: single;		// Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: integer;
    p: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  // Physical bitmap scanline (row)
  TRGBList = packed array[0..0] of TPix;
  PRGBList = ^TRGBList;

const
  ResampleFilters: array[0..6] of TResamplers = (
    (Filter: @BoxFilter;	Width: 0.5),
    (Filter: @TriangleFilter;	Width: 1.0),
    (Filter: @HermiteFilter;	Width: 1.0),
    (Filter: @BellFilter;	Width: 1.5),
    (Filter: @SplineFilter;	Width: 2.0),
    (Filter: @Lanczos3Filter;	Width: 3.0),
    (Filter: @MitchellFilter;	Width: 2.0)
    );

function TPV_Bitmap.ResampleTo(DstWidth, DstHeight: Integer; Filter: TResampleFilter): TPV_Bitmap;
//Nearest Neighbor and Bilinear are broken
var xscale, yscale	: single;		// Zoom scale factors
  i, j, k		: integer;		// Loop variables
  center		: single;		// Filter calculation variables
  wwidth, fscale, weight: single;		// Filter calculation variables
  left, right		: integer;		// Filter calculation variables
  n			: integer;		// Pixel number
  Work			: TPV_Bitmap;
  contrib		: PCListList;
  rgb			: TRGB;
  color			: TPix;
  SrcWidth		: Integer;
  SrcHeight		: integer;
  FilterProc            : TFilterProc;
  fwidth                : single;
begin
  FilterProc := ResampleFilters[ord(filter)].Filter;
  fwidth := ResampleFilters[ord(filter)].Width;

  Result := TPV_Bitmap.Create;
  Result.SetSize(DstWidth, DstHeight);

  SrcWidth := Self.Width;
  SrcHeight := Self.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then Exit;

  if (SrcWidth = DstWidth) and (SrcHeight = DstHeight) then begin
    Result.CopyFrom(Self);
    Exit;
  end;


  // Create intermediate image to hold horizontal zoom
  Work := TPV_Bitmap.Create;
  try
    Work.SetSize(DstWidth, SrcHeight);

    // Improvement suggested by David Ullrich:
    if (SrcWidth = 1) then
      xscale:= DstWidth / SrcWidth
    else
      xscale:= (DstWidth - 1) / (SrcWidth - 1);
    if (SrcHeight = 1) then
      yscale:= DstHeight / SrcHeight
    else
      yscale:= (DstHeight - 1) / (SrcHeight - 1);

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth* sizeof(TCList));
    // Horizontal sub-sampling
    if (xscale >= 1.0) then
    begin
      wwidth := fwidth;
      fscale := 1.0;
    end
    else begin
      wwidth := fwidth / xscale;
      fscale := 1.0 / xscale;
    end;

    for i := 0 to DstWidth-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(wwidth * 2.0 + 1) * sizeof(TContributor));
      center := i / xscale;
      left := floor(center - wwidth);
      right := ceil(center + wwidth);
      for j := left to right do
      begin
        weight := FilterProc((center - j) / fscale) / fscale;
        if (weight = 0.0) then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= SrcWidth) then
          n := SrcWidth - j + SrcWidth - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;

    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for k := 0 to SrcHeight-1 do
    begin
      for i := 0 to DstWidth-1 do
      begin
        rgb.r := 0.0;
        rgb.g := 0.0;
        rgb.b := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := Self.Pixel[contrib^[i].p^[j].pixel, k];
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;

        color.r := Clip(rgb.r);
        color.g := Clip(rgb.g);
        color.b := Clip(rgb.b);

        Work.Pixel[i, k] := color;
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for i := 0 to DstWidth-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight* sizeof(TCList));

    // Vertical sub-sampling
    if (yscale >= 1.0) then
    begin
      wwidth := fwidth;
      fscale := 1.0;
    end
    else begin
      wwidth := fwidth / yscale;
      fscale := 1.0 / yscale;
    end;

    for i := 0 to DstHeight-1 do
    begin
      contrib^[i].n := 0;
      GetMem(contrib^[i].p, trunc(wwidth * 2.0 + 1) * sizeof(TContributor));
      center := i / yscale;
      left := floor(center - wwidth);
      right := ceil(center + wwidth);
      for j := left to right do
      begin
        weight := FilterProc((center - j) / fscale) / fscale;
        if (weight = 0.0) then
          continue;
        if (j < 0) then
          n := -j
        else if (j >= SrcHeight) then
          n := SrcHeight - j + SrcHeight - 1
        else
          n := j;
        k := contrib^[i].n;
        contrib^[i].n := contrib^[i].n + 1;
        contrib^[i].p^[k].pixel := n;
        contrib^[i].p^[k].weight := weight;
      end;
    end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
    for k := 0 to DstWidth-1 do
    begin
      for i := 0 to DstHeight-1 do
      begin
        rgb.r := 0;
        rgb.g := 0;
        rgb.b := 0;
        // weight := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
          color := Work.Pixel[k, contrib^[i].p^[j].pixel];
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;

        color.r := Clip(rgb.r);
        color.g := Clip(rgb.g);
        color.b := Clip(rgb.b);

        Result.Pixel[k, i] := color;
      end;
    end;

    // Free the memory allocated for vertical filter weights
    for i := 0 to DstHeight-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

  finally
    Work.Free;
  end;
end;


procedure TPV_Bitmap.Rotate90;
var x,y: Integer;
    P: TPix;
    Tmp: TPV_Bitmap;
begin
  Tmp := TPV_Bitmap.Create;
  Tmp.SetSize(Height, Width);

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      Tmp[y,x] := Self[Width-x,y];
    end;

  Self.SetSize(Tmp.Width, Tmp.Height);
  Self.CopyFrom(Tmp);

  Tmp.Free;
end;

procedure TPV_Bitmap.Rotate180;
var x,y: Integer;
    P: TPix;
    Tmp: TPV_Bitmap;
begin
  Tmp := TPV_Bitmap.Create;
  Tmp.SetSize(Width, Height);

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      Tmp[x,y] := Self[Width-x,Height-y];
    end;

  Self.SetSize(Tmp.Width, Tmp.Height);
  Self.CopyFrom(Tmp);

  Tmp.Free;
end;

procedure TPV_Bitmap.Rotate270;
var x,y: Integer;
    P: TPix;
    Tmp: TPV_Bitmap;
begin
  Tmp := TPV_Bitmap.Create;
  Tmp.SetSize(Height, Width);

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      Tmp[y,x] := Self[x,Height-y];
    end;

  Self.SetSize(Tmp.Width, Tmp.Height);
  Self.CopyFrom(Tmp);

  Tmp.Free;
end;


procedure TPV_Bitmap.AddNoise(Amount: Byte);
var x,y: Integer;
    P: PPix;
    R: Byte;
    White,Black: TPix;
begin
  P := Self.Scanline[0];

  if Amount > 100 then Amount := 100;

  White := MakePix(255,255,255,255);
  Black := MakePix(0,0,0,255);

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      R := Random(100);

      if R < Amount then begin
        if R mod 2 = 0 then P^.RGBA := White.RGBA
        else                P^.RGBA := Black.RGBA;
      end;

      Inc(P);
    end;
end;

procedure TPV_Bitmap.Brightness(Amount: Byte);
var x,y: Integer;
    P: PPix;
    R: Byte;
    Amount2: Single;
begin
  P := Self.Scanline[0];

  if Amount > 100 then Amount := 100;

  Amount2 := Amount * 2.55;

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin

      P^.R := Clip(P^.R + Amount);
      P^.G := Clip(P^.G + Amount);
      P^.B := Clip(P^.B + Amount);

      Inc(P);
    end;
end;

procedure TPV_Bitmap.Contrast(Amount: Extended);
var x,y: Integer;
    P: PPix;
    R: Byte;
    Amount2: Single;
    LUT: array[0..255] of Byte;
    Val: Extended;
    i: Integer;
begin
  P := Self.Scanline[0];

  for i:=0 to 255 do begin
    Val := Amount * (i- 127) + 127;
    LUT[i] := Clip(Val);
  end;

  Amount2 := Amount / 100;

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin

      P^.R := LUT[P^.R];
      P^.G := LUT[P^.G];
      P^.B := LUT[P^.B];

      Inc(P);
    end;
end;

procedure TPV_Bitmap.FindEdges;
begin
  Convolution([0,-1,0,
              -1,4,-1,
              0,-1,0], 3);
end;

procedure TPV_Bitmap.FindEdges2;
begin
  Convolution([-1,-1,-1,
               -1,8,-1,
               -1,-1,-1], 3);
end;

procedure TPV_Bitmap.FindEdges3;
begin
  Convolution([1,0,-1,
               0,0,0,
               -1,0,1], 3);
end;

procedure TPV_Bitmap.Negate;
var x,y: Integer;
    P: TPix;
begin
  Self.SetSize(Width, Height);

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
       P := Self[x,y];

       Self.SetRGB(x,y, 255-P.R, 255-P.G, 255-P.B);
    end;
end;

procedure TPV_Bitmap.Gamma(Amount: Extended);
var x,y: Integer;
    P: PPix;
    R: Byte;
    Amount2: Single;
    LUT: array[0..255] of Byte;
    Val: Extended;
    i: Integer;
begin
  P := Self.Scanline[0];

  for i:=0 to 255 do begin
    Val := 255 * Power(i/255, 1/Amount);
    LUT[i] := Clip(Val);
  end;

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin

      P^.R := LUT[P^.R];
      P^.G := LUT[P^.G];
      P^.B := LUT[P^.B];

      Inc(P);
    end;
end;


procedure TPV_Bitmap.Sepia(Amount: Byte); //20-40
var x,y: Integer;
    P: PPix;
    G: Byte;
    i: Integer;
begin
  P := Self.Scanline[0];

  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin

      G := (P^.R + P^.G + P^.B) div 3;

      P^.R := Clip(G + 2*Amount);
      P^.G := Clip(G + Amount);
      P^.B := G;

      Inc(P);
    end;
end;

procedure TPV_Bitmap.Resample(AWidth, AHeight: Integer; Filter: TResampleFilter);
var New: TPV_Bitmap;
begin
  New := ResampleTo(AWidth, AHeight, Filter);

  CopyFrom(New);
  New.Free;
end;

procedure TPV_Bitmap.ResamplePercent(AWidth, AHeight: Integer;
  Filter: TResampleFilter);
begin
  Resample(Round(AWidth * Self.Width/100), Round(AHeight * Self.Height/100), Filter);
end;

procedure TPV_Bitmap.BGR;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.B, P.G, P.R, P.A);
    end;
end;

procedure TPV_Bitmap.BRG;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.B, P.R, P.G, P.A);
    end;
end;

procedure TPV_Bitmap.GBR;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.G, P.B, P.R, P.A);
    end;
end;

procedure TPV_Bitmap.GRB;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.G, P.R, P.B, P.A);
    end;
end;

procedure TPV_Bitmap.RBG;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.R, P.B, P.G, P.A);
    end;
end;

procedure TPV_Bitmap.ExtractRed;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.R, P.R, P.R, P.A);
    end;
end;

procedure TPV_Bitmap.ExtractGreen;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.G, P.G, P.G, P.A);
    end;
end;

procedure TPV_Bitmap.ExtractBlue;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGBA(x,y, P.B, P.B, P.B, P.A);
    end;
end;

procedure TPV_Bitmap.ExtractAlpha;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      Self.SetRGB(x,y, P.A, P.A, P.A);
    end;
end;

procedure TPV_Bitmap.ExtractCyan;
var x,y: Integer;
    P: TPix;
    C,M,YY,K: Byte;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      rgb2cmyk(P.R, P.G, P.B, C,M,YY,K);
      P.R := 255-Clip(C*2.55);

      Self.SetRGB(x,y, P.R, P.R, P.R);
    end;
end;

procedure TPV_Bitmap.ExtractMagenta;
var x,y: Integer;
    P: TPix;
    C,M,YY,K: Byte;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      rgb2cmyk(P.R, P.G, P.B, C,M,YY,K);
      P.R := 255-Clip(M*2.55);

      Self.SetRGB(x,y, P.R, P.R, P.R);
    end;
end;

procedure TPV_Bitmap.ExtractYellow;
var x,y: Integer;
    P: TPix;
    C,M,YY,K: Byte;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      rgb2cmyk(P.R, P.G, P.B, C,M,YY,K);
      P.R := 255-Clip(YY*2.55);

      Self.SetRGB(x,y, P.R, P.R, P.R);
    end;
end;

procedure TPV_Bitmap.ExtractBlack;
var x,y: Integer;
    P: TPix;
    C,M,YY,K: Byte;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do begin
      P := Self[x,y];

      rgb2cmyk(P.R, P.G, P.B, C,M,YY,K);
      P.R := 255-Clip(K*2.55);

      Self.SetRGB(x,y, P.R, P.R, P.R);
    end;
end;


procedure TPV_Bitmap.SmarterBlur;
var x,y: Integer;
    Curr, Other: TPix;
    Old: TPV_Bitmap;
    SumR, SumG, SumB: Int64;
    Count: Integer;
    i,j: Integer;
    Avg: TPix;
begin
  Old := TPV_Bitmap.Create;
  Old.SetSize(Width, Height);
  Old.CopyFrom(Self);

  for y:=1 to Height-2 do
    for x:=1 to Width-2 do begin

      SumR := 0;
      SumG := 0;
      SumB := 0;
      Count := 0;
      Curr := Self[x,y];

      for i:=-1 to 1 do
      for j:=-1 to 1 do begin
        Other := Self[x+i, y+j];
        if SamePix(Other, Curr) then begin
          Inc(Count);

          Inc(SumR, Other.R);
          Inc(SumG, Other.G);
          Inc(SumB, Other.B);
        end;
      end;

      Avg.R := Clip(SumR / Count);
      Avg.G := Clip(SumG / Count);
      Avg.B := Clip(SumB / Count);
      Avg.A := 255;

      for i:=-1 to 1 do
      for j:=-1 to 1 do begin
        Other := Self[x+i, y+j];
        if SamePix(Other, Curr) then begin
           Self[x+i, y+j] := Avg;
        end;
      end;
    end;

  Old.Free;
end;

procedure TPV_Bitmap.Sharpen;
begin
  Convolution([ 0,-1, 0,
               -1, 5,-1,
                0,-1, 0], 3);
end;

procedure TPV_Bitmap.DeNoise;
var x,y: Integer;
    Old: TPV_Bitmap;
    P0,P1,P2,P3,P4,P5,P6,P7,P8: TPix;
    Avg: TPix;
begin
  Old := TPV_Bitmap.Create;
  Old.SetSize(Width, Height);
  Old.CopyFrom(Self);

  for y:=1 to Height-2 do
    for x:=1 to Width-2 do begin

      P1 := Old[x-1,y-1];
      P2 := Old[x  ,y-1];
      P3 := Old[x+1,y-1];

      P4 := Old[x-1,y  ];
      P0 := Old[x  ,y  ];
      P5 := Old[x+1,y  ];

      P6 := Old[x-1,y+1];
      P7 := Old[x  ,y+1];
      P8 := Old[x+1,y+1];

      Avg.R := Clip((P1.R + P2.R + P3.R + P4.R + P5.R + P6.R + P7.R + P8.R) / 8);
      Avg.G := Clip((P1.G + P2.G + P3.G + P4.G + P5.G + P6.G + P7.G + P8.G) / 8);
      Avg.B := Clip((P1.B + P2.B + P3.B + P4.B + P5.B + P6.B + P7.B + P8.B) / 8);
      Avg.A := 255;

      if not SamePix(P0,Avg, 130) then begin
        Self[x,y] := Avg;
      end;

    end;

  Old.Free;
end;

procedure TPV_Bitmap.Emboss;
begin
  Convolution([-2,-1, 0,
               -1, 1, 1,
                0, 1, 2], 3);
end;

procedure TPV_Bitmap.BoxBlur;
begin
  Convolution([ 1, 1, 1,
                1, 1, 1,
                1, 1, 1], 3, 9);
end;

procedure TPV_Bitmap.GaussBlur;
begin
  Convolution([1,2,1,
               2,4,2,
               1,2,1], 3, 16);
end;

procedure TPV_Bitmap.GaussBlur5;
begin
  Convolution([ 1, 4, 6, 4, 1,
                4,16,24,16, 4,
                6,24,36,24, 6,
                4,16,24,16, 4,
                1, 4, 6, 4, 1], 5, 256);
end;

procedure TPV_Bitmap.Unsharp;
begin
  Convolution([ 1,  4,   6,  4, 1,
                4, 16,  24, 16, 4,
                6, 24,-476, 24, 6,
                4, 16,  24, 16, 4,
                1,  4,   6,  4, 1], 5, -256);
end;

procedure TPV_Bitmap.Convolution(Kernel: array of Double; Size: Byte; Divider: Integer);
var x,y: Integer;
    i,j: Integer;
    ii: Integer;
    AccR, AccG, AccB: Extended;
    Half: Integer;
    R,G,B: Byte;
    Old: TPV_Bitmap;
begin
  Old := TPV_Bitmap.Create;
  Old.SetSize(Width, Height);
  Old.CopyFrom(Self);

  Half := Floor(Size/2);

  for y:=Half to Height-1-Half do
    for x:=Half to Width-1-Half do begin
      AccR := 0;
      AccG := 0;
      AccB := 0;
      ii := 0;

      for j:=-Half to Half do
        for i:=-Half to Half do begin
          AccR := AccR + Kernel[ii] * Old[x+i, y+j].R;
          AccG := AccG + Kernel[ii] * Old[x+i, y+j].G;
          AccB := AccB + Kernel[ii] * Old[x+i, y+j].B;

          Inc(ii);
        end;

      R := Clip(AccR / Divider);
      G := Clip(AccG / Divider);
      B := Clip(AccB / Divider);

      Self.SetRGB(x,y, R,G,B);
    end;

  Old.Free;
end;

end.
