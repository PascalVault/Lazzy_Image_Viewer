unit PV_Bitmap;

//Lazzy Image Viewer
//github.com/PascalVault
//License: MIT

{$inline on}
interface

uses Classes, Graphics, SysUtils, DateUtils, Math, IntfGraphics, FPImage, Dialogs, PV_Streams;

const PaletteMono: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);

type
   {$IFDEF WINDOWS}
   TPix = packed record
     case Byte of
       1: (B,G,R,A: Byte);
       2: (RGBA: Cardinal);
   end;
   {$ELSE}
   TPix = packed record
     case Byte of
       1: (B,G,R,A: Byte);
       2: (RGBA: Cardinal);
   end;
   {$ENDIF}
   TPal = record
     R,G,B: Byte;
   end;

   TPix3 = Cardinal;

   PPixArray = ^TPixArray;
   TPixArray = array[0..32766] of TPix;

   PPix = ^TPix;
   TPalArray = array of TPix;
   TDither = (ddNone, ddFloyd, ddBurkes, ddStucki, ddJarvis, ddAtkinson, ddSierra2, ddSierra3, ddSierra4);
   TPixInt = record
     R,G,B: Integer;
   end;


   { TPV_Bitmap }

   TPV_Bitmap = class
   public
     function GetPixel(X,Y: Integer): TPix;
     procedure SetPixel(X,Y: Integer; Val: TPix); inline;
   private
     FData: array of TPix;
     FWidth: Integer;
     FHeight: Integer;

     function GetWidth: Integer;
     procedure SetWidth(Val: Integer);
     function GetHeight: Integer;
     procedure SetHeight(Val: Integer);
     function GetScanline(Y: Integer): Pointer;
   public
     FPalette: array of TPix;
     PaletteLen: Integer;
     FormatName: String;

     procedure SetMono(X,Y: Integer; B: Byte); inline;
     procedure SetRGBA(X,Y: Integer; R,G,B,A: Byte); inline;
     procedure SetRGB(X,Y: Integer; R,G,B: Byte); inline;

     procedure SetR(X,Y: Integer; R: Byte); inline;
     procedure SetG(X,Y: Integer; G: Byte); inline;
     procedure SetB(X,Y: Integer; B: Byte); inline;
     procedure SetA(X,Y: Integer; A: Byte); inline;

     procedure AddPal(R,G,B,A: Byte);
     procedure SetPal(X,Y: Integer; Index: Byte); inline;
     function GetPalIndex(X,Y: Integer): Integer;
     procedure AddPalette(Pal: array of TPal; Len: Integer);
     procedure ClearPalette;

     property Scanline[Y: Integer]: Pointer read GetScanline;
     property Pixel[X,Y: Integer]: TPix read GetPixel write SetPixel; default;
     property Width: Integer read GetWidth write SetWidth;
     property Height: Integer read GetHeight write SetHeight;

     constructor Create;
     procedure SetSize(AWidth, AHeight: Integer);
     function LoadFromFile(Filename: String): Boolean;
     procedure SaveToFile(Filename: String); overload;
     procedure SaveToFile(Filename: String; Compression: Byte); overload;
     procedure Draw(Bitmap: TPV_Bitmap; Left,Top, AWidth,AHeight: Integer);
     procedure FlipH;
     procedure FlipV;

     procedure CopyFrom(Bitmap: TPV_Bitmap);
     procedure CopyFrom(Bitmap: TBitmap);
     procedure CopyFrom(Bitmap: TFPMemoryImage);
     function GetPiece(AX,AY, AWidth,AHeight: Integer): TBitmap;

     procedure ReduceColors(MaxColors: Byte; Dither: TDither = ddFloyd);
     procedure Grayscale(MaxColors: Byte; Dither: TDither = ddFloyd);
     procedure BlackWhite(Dither: TDither = ddFloyd);
     procedure Highcolor(Bits: Integer = 16);

     procedure Resize(AWidth, AHeight: Integer);
     procedure ResizePercent(AWidth, AHeight: Integer);
     procedure RemoveAlpha;
     procedure Opaque;

     function ToBitmap: TBitmap;
     procedure DrawTo(X,Y: Integer; Canvas: TCanvas);
     procedure DrawTo(Dest: TRect; Canvas: TCanvas); overload;

     procedure Trim(RR,GG,BB: Byte);
     function CountColors: Integer;
   public

   end;

   TPV_BitmapReader = function(Bmp: TPV_Bitmap; Str: TStream): Boolean;
   TPV_BitmapWriter = procedure(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);

   TXBmpFormat = record
     Ext: String;
     Reader: TPV_BitmapReader;
     Writer: TPV_BitmapWriter;
     Name: String;
   end;

   { TPV_BitmapFormat }

   TPV_BitmapFormat = class
   private
     FList: array of TXBmpFormat;
     FCount: Integer;
   public
     property Count: Integer read FCount;
     procedure Item(Index: Integer; out Ext: String; out Reader: TPV_BitmapReader; out Writer: TPV_BitmapWriter);
     constructor Create;
     function FindReader(Ext: String; out Format: String): TPV_BitmapReader;
     function FindWriter(Ext: String): TPV_BitmapWriter;
     function FindName(Ext: String): String;
     procedure Add(Ext: String; Reader: TPV_BitmapReader; Writer: TPV_BitmapWriter; Name: String);
   end;

   function MakePix(R,G,B,A: Byte): TPix;
   function Clip(V: Extended): Byte;
   function SamePix(P,R: TPix; Threshold: Byte = 5): Boolean;
   function Limit(Min,Max,Val: Integer): Integer;
   procedure rgb2cmyk(R,G,B: Byte; out C,M,Y,K: Byte);
   procedure cmyk2rgb(C,M,Y,K: Byte; out R,G,B: Byte);
   procedure rgb2yuv(R,G,B: Byte; out Y,U,V: Byte);

var BitmapFormats: TPV_BitmapFormat;

implementation

uses PV_Grayscale, PV_Palette;

function MakePix(R, G, B, A: Byte): TPix;
begin
  Result.RGBA := B + (G shl 8) + (R shl 16) + (A shl 24);
end;

function Clip(V: Extended): Byte;
begin
  if V > 255 then Result := 255
  else if V < 0 then Result := 0
  else Result := Round(V);
end;

function SamePix(P, R: TPix; Threshold: Byte): Boolean;
begin
  Result := False;

  if abs(P.R - R.R) > Threshold then Exit;
  if abs(P.G - R.G) > Threshold then Exit;
  if abs(P.B - R.B) > Threshold then Exit;

  Result := True;
end;

function Limit(Min, Max, Val: Integer): Integer;
begin
  if Val < Min then Exit(Min);
  if Val > Max then Exit(Max);
  Exit(Val);
end;

procedure rgb2yuv(R,G,B: Byte; out Y,U,V: Byte);
var YY, UU, VV: Extended;
begin
  YY := +0.2990 * R + 0.5870 * G + 0.1140 * B;
  UU := 128 -0.1687 * R - 0.3313 * G + 0.5000 * B;
  VV := 128 +0.5000 * R - 0.4187 * G - 0.0813 * B;

  Y := floor(YY);
  V := floor(VV);
  U := floor(UU);
end;

procedure lab2rgb(L1,A1,B1: Byte; out R,G,B: Byte);
var LL,AA,BBB: Extended;
    Y,X,Z: Extended;
    Y3,X3,Z3: Extended;
    RR,GG,BB: Extended;
begin
  //http://www.easyrgb.com/
  LL := L1 / 2.55;
  AA := A1 - 128;
  BBB := B1 - 128;

  //CIELAB -> XYZ
  Y := (LL + 16 ) / 116;
  X := AA / 500 + Y;
  Z := Y - BBB / 200;

  Y3 := power(Y,3);
  X3 := power(X,3);
  Z3 := power(Z,3);

  if ( Y3 > 0.008856 ) then Y := Y3
  else			    Y := ( Y - 16 / 116 ) / 7.787;
  if ( X3 > 0.008856 ) then X := X3
  else			    X := ( X - 16 / 116 ) / 7.787;
  if ( Z3 > 0.008856 ) then Z := Z3
  else			    Z := ( Z - 16 / 116 ) / 7.787;

  X := 95.047  * X;
  Y := 100.000 * Y;
  Z := 108.883 * Z;

  //XYZ -> RGB
  X := X / 100;
  Y := Y / 100;
  Z := Z / 100;

  rr := X *  3.2406 + Y * -1.5372 + Z * -0.4986;
  gg := X * -0.9689 + Y *  1.8758 + Z *  0.0415;
  bb := X *  0.0557 + Y * -0.2040 + Z *  1.0570;

  if ( rr > 0.0031308 ) then rr := 1.055 * power(rr, 1 / 2.4 ) - 0.055
  else                       rr := 12.92 * rr;
  if ( gg > 0.0031308 ) then gg := 1.055 * power(gg, 1 / 2.4 ) - 0.055
  else                 	     gg := 12.92 * gg;
  if ( bb > 0.0031308 ) then bb := 1.055 * power(bb, 1 / 2.4 ) - 0.055
  else                 	     bb := 12.92 * bb;

  rr := rr * 255;
  gg := gg * 255;
  bb := bb * 255;

  r := clip(rr);
  g := clip(gg);
  b := clip(bb);
end;
{

function zif_unrle(&source, &dest, packedSize, unitSize) //tga
begin
	i = 0;
	dest = '';
	while (i<packedSize-1)
	begin
		count = ord(source[i]);
		if (count < 128) //uncompressed
		begin
			count = count+1;
			dest .= substr(source, i+1, unitSize*count);
			i += unitSize*count+1;
		end;
		else
		begin
			count = count-128+1;
			for (k=0; k<count; k++)
			begin
				dest .= substr(source, i+1, unitSize);
			end;
			i += unitSize+1;
		end;
	end;
end;

function zif_unrle_rgb(&source, &dest, packedSize) //rgb
begin
	i = 0;
	//dest = '';
	unitSize = 1;
	while (i<packedSize-1)
	begin
		count = ord(source[i]);
		if (count > 128) //uncompressed
		begin
			count = count-128; //+1
			dest .= substr(source, i+1, unitSize*count);
			i += unitSize*count+1;
		end;
		else
		begin
			count = count;//+1
			for (k=0; k<count; k++)
			begin
				dest .= substr(source, i+1, unitSize);
			end;
			i += unitSize+1;
		end;
	end;
end;
function zif_unrle_pcx(&source, &dest, packedSize) //pcx
begin
	i = 0;
	j = 0;
	dest = '';
	while (i<packedSize-1)
	begin
		count = ord(source[i]);
		if ((count && 0xC0) == 0xC0) //compressed
		begin
			count = (count && 0x3F);
			buff = source[i+1];
			i += 2;
		end;
		else
		begin
			buff = chr(count);
			count = 1;
			i++;
		end;
		for (k=0; k<count; k++)
		begin
			dest .= buff;
		end;
	end;
end;

function zif_unrle_cut(&source, &dest, packedSize, unitSize) //cut
begin
	i = 0;
	dest = '';
	while (i<packedSize-1)
	begin
		count = ord(source[i]);
		if (count < 128) //uncompressed
		begin
			count = count+1;
			dest .= substr(source, i+1, unitSize*count);
			i += unitSize*count+1;
		end;
		else
		begin
			count = count-128+1;
			for (k=0; k<count; k++)
			begin
				dest .= substr(source, i+1, unitSize);
			end;
			i += unitSize+1;
		end;
	end;
end;

function zif_unrle_gg(&source, &dest, packedSize) //gg
begin
	dest = '';
	for (i=0; i<packedSize; i++)
	begin
		if (source[i] == chr(254))
		begin
			dest .= str_pad('', ord(source[i+2]), source[i+1]);
			i += 2;
		end;
		else //uncompressed
		begin
			dest .= source[i];
		end;
	end;
end;

function zif_unrle_ami(&source, &dest, packedSize) //ami
begin
	dest = '';
	for (i=0; i<packedSize; i++)
	begin
		if (source[i] == chr(194))
		begin
			if (source[i+1] == "\0") return 1; //EOF

			dest .= str_pad('', ord(source[i+1]), source[i+2]);
			i += 2;
		end;
		else //uncompressed
		begin
			dest .= source[i];
		end;
	end;
end;

function zif_unrle_dlp(&source, &dest, packedSize, escapeByte) //dlp
begin
	dest = '  ';
	for (i=0; i<packedSize; i++)
	begin
		if (source[i] == escapeByte)
		begin
			dest .= str_pad('', ord(source[i+1]), source[i+2]);
			i += 2;
		end;
		else //uncompressed
		begin
			dest .= source[i];
		end;
	end;
end;

function zif_unrle_psd(&source, &dest, packedSize, unitSize = 1) //psd,mac
begin
	i = 0;
	dest = '';
	while (i<packedSize-1)
	begin
		list(,count) = unpack('c', source[i]); //diff from TGA
		if (count == -128)                       //diff from TGA
		begin
			i++;
		end;
		else if (count >=0 ) //uncompressed
		begin
			count = count+1;
			dest .= substr(source, i+1, unitSize*count);
			i += unitSize*count+1;
		end;
		else
		begin
			count = -1*count+1;
			for (k=0; k<count; k++)
			begin
				dest .= substr(source, i+1, unitSize);
			end;
			i += unitSize+1;
		end;
	end;
end;
}

procedure rgb2cmyk(R,G,B: Byte; out C,M,Y,K: Byte);
var RR,GG,BB,KK: Extended;
    Temp: Extended;
begin
  RR := R/255;
  GG := G/255;
  BB := B/255;

  KK := 1-max(RR,max(GG,BB));
  Temp := 1-KK;
  if Temp = 0 then Temp := 0.01;

  C := Round(100*(1-RR-KK) / Temp);
  M := Round(100*(1-GG-KK) / Temp);
  Y := Round(100*(1-BB-KK) / Temp);

  if C>100 then C := 100;
  if M>100 then M := 100;
  if Y>100 then Y := 100;

  K := Round(100*KK);
  if K>100 then K := 100;
end;


procedure cmyk2rgb(C, M, Y, K: Byte; out R, G, B: Byte);
var Temp: Extended;
    CC,MM,YY,KK: Extended;
begin
  CC := C/100;
  MM := M/100;
  YY := Y/100;
  KK := K/100;

  Temp := (1-KK);

  R := Clip(255* (1-CC) * Temp);
  G := Clip(255* (1-MM) * Temp);
  B := Clip(255* (1-YY) * Temp);
end;

procedure TPV_Bitmap.Draw(Bitmap: TPV_Bitmap; Left,Top, AWidth,AHeight: Integer);
begin
  //FBitmap.Canvas.StretchDraw(Rect(Left,Top, Left+AWidth, Top+AHeight),  Bitmap.FBitmap);
  //TODO
end;

procedure TPV_Bitmap.FlipV;
var Old: TPV_Bitmap;
    y: Integer;
begin
  Old := TPV_Bitmap.Create;
  Old.CopyFrom(Self);

  for y:=0 to FHeight-1 do begin
    Move(Old.Scanline[FHeight-y-1]^, Self.Scanline[y]^, FWidth*4);
  end;

  Old.Free;
end;

procedure TPV_Bitmap.FlipH;
var Old: TPV_Bitmap;
    x,y: Integer;
begin
  Old := TPV_Bitmap.Create;
  Old.CopyFrom(Self);

  for y:=0 to FHeight-1 do
    for x:=0 to FWidth-1 do begin
      Self[x,y] := Old[FWidth-x-1,y];
    end;

  Old.Free;
end;

procedure TPV_Bitmap.CopyFrom(Bitmap: TPV_Bitmap);
var x,y: Integer;
begin
  SetSize(Bitmap.Width, Bitmap.Height);

  for y:=0 to Height-1 do
    Move(Bitmap.Scanline[y]^, Self.Scanline[y]^, 4*Bitmap.Width);
end;

{$IFDEF WINDOWS}
procedure TPV_Bitmap.CopyFrom(Bitmap: TBitmap);
var x,y: Integer;
    R: TPix;
    Bpp: TPixelFormat;
    P: PByteArray;
begin
  SetSize(Bitmap.Width, Bitmap.Height);

  Bpp := Bitmap.PixelFormat;

  for y:=0 to Bitmap.Height-1 do begin
    P := Bitmap.Scanline[y];

    case Bpp of
      pf32bit: for x:=0 to Bitmap.Width-1 do
               Self.SetRGBA(x,y, P^[4*x+2], P^[4*x+1], P^[4*x], P^[4*x+3]);

      pf24bit: for x:=0 to Bitmap.Width-1 do
               Self.SetRGB(x,y, P^[3*x+2], P^[3*x+1], P^[3*x]);
    end;
  end;
end;
{$ELSE}
procedure TPV_Bitmap.CopyFrom(Bitmap: TBitmap);
var x,y: Integer;
    R: TPix;
    Bpp: TPixelFormat;
    P: PByteArray;
begin
  SetSize(Bitmap.Width, Bitmap.Height);

  Bpp := Bitmap.PixelFormat;

  for y:=0 to Bitmap.Height-1 do begin
    P := Bitmap.Scanline[y];

    case Bpp of
      pf32bit: for x:=0 to Bitmap.Width-1 do
               Self.SetRGBA(x,y, P^[4*x+2], P^[4*x+1], P^[4*x], P^[4*x+3]);

      pf24bit: for x:=0 to Bitmap.Width-1 do
               Self.SetRGB(x,y, P^[3*x+2], P^[3*x+1], P^[3*x]);
    end;
  end;
end;
{$ENDIF}

procedure TPV_Bitmap.CopyFrom(Bitmap: TFPMemoryImage);
var x,y: Integer;
    Col: TFPColor;
    P: TPix;
begin
  SetSize(Bitmap.Width, Bitmap.Height);

  for y:=0 to Bitmap.Height-1 do
    for x:=0 to Bitmap.Width-1 do begin
      {if Bitmap.UsePalette then
        Col := Bitmap.Palette[Bitmap.Palette[x, y]]
      else  }
        Col := Bitmap.Colors[x, y];


      P.R := Col.Red   shr 8;
      P.G := Col.Green shr 8;
      P.B := Col.Blue  shr 8;
      P.A := Col.Alpha shr 8;

      Self.SetRGBA(x,y, P.R, P.G, P.B, P.A);
    end;
end;

procedure TPV_Bitmap.ReduceColors(MaxColors: Byte; Dither: TDither);
begin
  PV_Palette.ReduceColors(Self, MaxColors, Dither);
end;

procedure TPV_Bitmap.Grayscale(MaxColors: Byte; Dither: TDither);
begin
  PV_Grayscale.Grayscale(Self, MaxColors, Dither);
end;

procedure TPV_Bitmap.BlackWhite(Dither: TDither);
begin
  PV_Grayscale.BlackWhite(Self, Dither);
end;

procedure TPV_Bitmap.Highcolor(Bits: Integer);
var x,y: Integer;
    P: TPix;
begin
  if Bits = 15 then begin
        for y:=0 to FHeight-1 do
          for x:=0 to FWidth-1 do begin
            P := Self[x,y];

            P.R := Byte(P.R shr 3) shl 3;
            P.G := Byte(P.G shr 3) shl 3;
            P.B := Byte(P.B shr 3) shl 3;

            Self.SetRGBA(x,y, P.R, P.G, P.B, P.A);
          end;

  end
  else begin
         for y:=0 to FHeight-1 do
           for x:=0 to FWidth-1 do begin
             P := Self[x,y];

             P.R := Byte(P.R shr 3) shl 3;
             P.G := Byte(P.G shr 2) shl 2;
             P.B := Byte(P.B shr 3) shl 3;

             Self.SetRGBA(x,y, P.R, P.G, P.B, P.A);
           end;

   end;
end;

procedure TPV_Bitmap.Resize(AWidth, AHeight: Integer);
var Tmp,Tmp2: TBitmap;
begin
  Tmp := Self.ToBitmap;

  Tmp2 := TBitmap.Create;
  Tmp2.PixelFormat := pf32bit;
  Tmp2.SetSize(AWidth, AHeight);

  Tmp2.Canvas.StretchDraw(Rect(0,0, AWidth, AHeight), Tmp);
  Tmp.Free;

  CopyFrom(Tmp2);
  Tmp2.Free;
end;

procedure TPV_Bitmap.ResizePercent(AWidth, AHeight: Integer);
begin
  Resize(Round(AWidth * FWidth/100), Round(AHeight * FHeight/100));
end;

procedure TPV_Bitmap.RemoveAlpha;
var x,y: Integer;
    P: TPix;
begin
  for y:=0 to FHeight-1 do
    for x:=0 to FWidth-1 do begin
      P := Self[x,y];
      P.A := 255;

      Self.SetRGB(x,y, P.R,P.G,P.B);
    end;
end;

procedure TPV_Bitmap.Opaque;
var x,y: Integer;
    P: TPix;
    A,A2: Extended;
    BG: TPix;
begin
  Bg := MakePix(255,255,255,255);

  for y:=0 to FHeight-1 do
    for x:=0 to FWidth-1 do begin
      P := Self[x,y];
      A := P.A/255;
      A2 := 1-A;

      P.R := Clip(P.R * A + BG.R * A2);
      P.G := Clip(P.G * A + BG.G * A2);
      P.B := Clip(P.B * A + BG.B * A2);

      Self.SetRGB(x,y, P.R,P.G,P.B);
    end;
end;

{$IFDEF WINDOWS}
function TPV_Bitmap.ToBitmap: TBitmap;
var x,y: Integer;
    P,R: PPixArray;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(FWidth, FHeight);

  for y:=0 to FHeight-1 do begin
    P := Result.Scanline[y];
    R := Scanline[y];

    for x:=0 to FWidth-1 do begin
      P^[x].RGBA := R^[x].RGBA;
    end;
  end;
end;
{$ELSE}
function TPV_Bitmap.ToBitmap: TBitmap;
var x,y: Integer;
    R: PPixArray;
    Color: TColor;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(FWidth, FHeight);

  for y:=0 to FHeight-1 do begin
    R := Scanline[y];

    for x:=0 to FWidth-1 do begin
      Color := R^[x].R + (R^[x].G shl 8) + (R^[x].B shl 16);
      Result.Canvas.Pixels[x,y] := Color;
    end;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function TPV_Bitmap.GetPiece(AX, AY, AWidth, AHeight: Integer): TBitmap;
var x,y: Integer;
    P,R: PPixArray;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AWidth, AHeight);

  AWidth := Min(Width, AWidth);
  AHeight := Min(Height, AHeight);

  for y:=0 to AHeight-1 do begin
    P := Result.Scanline[y];
    R := Scanline[AY+y];

    for x:=0 to AWidth-1 do begin
      P^[x].RGBA := R^[AX+x].RGBA;
    end;
  end;
end;
{$ELSE}
function TPV_Bitmap.GetPiece(AX, AY, AWidth, AHeight: Integer): TBitmap;
var x,y: Integer;
    P: PByteArray;
    R: PPixArray;
    Color: TColor;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.SetSize(AWidth, AHeight);

  AWidth := Min(Width, AWidth);
  AHeight := Min(Height, AHeight);

  for y:=0 to AHeight-1 do begin
    R := Scanline[AY+y];

    for x:=0 to AWidth-1 do begin
      Color := R^[AX+x].R + (R^[AX+x].G shl 8) + (R^[AX+x].B shl 16);
      Result.Canvas.Pixels[x,y] := Color;
    end;
  end;
end;
{$ENDIF}

procedure TPV_Bitmap.DrawTo(X,Y: Integer; Canvas: TCanvas);
var Bmp: TBitmap;
begin
  Bmp := Self.ToBitmap;
  Canvas.Draw(X, Y, Bmp);
  Bmp.Free;
end;

procedure TPV_Bitmap.DrawTo(Dest: TRect; Canvas: TCanvas);
var Bmp: TBitmap;
begin
  Bmp := Self.ToBitmap;
  Canvas.StretchDraw(Dest, Bmp);
  Bmp.Free;
end;

procedure TPV_Bitmap.Trim(RR, GG, BB: Byte);
var L,T,R,B: Integer;
    P: TPix;
    Tmp: TBitmap;

    procedure LeftMargin;
    var x,y: Integer;
    begin
      L := FWidth-1;

      for y:=0 to FHeight-1 do
        for x:=0 to FWidth-1 do begin
          P := Self[x,y];

          if (P.R <> 255) or (P.G <> 255) or (P.B <> 255) then begin
            L := Min(x, L);
            break;
          end;
        end;
    end;
    procedure RightMargin;
    var x,y: Integer;
    begin
      R := 0;

      for y:=0 to FHeight-1 do
        for x:=FWidth-1 downto 0 do begin
          P := Self[x,y];

          if (P.R <> 255) or (P.G <> 255) or (P.B <> 255) then begin
            R := Max(x, R);
          end;
        end;
    end;
    procedure TopMargin;
    var x,y: Integer;
    begin
      T := FHeight-1;

      for x:=0 to FWidth-1 do
        for y:=0 to FHeight-1 do begin
          P := Self[x,y];

          if (P.R <> 255) or (P.G <> 255) or (P.B <> 255) then begin
            T := Min(y, T);
          end;
        end;
    end;
    procedure BottomMargin;
    var x,y: Integer;
    begin
      B := 0;

      for x:=0 to FWidth-1 do
        for y:=FHeight-1 downto 0 do begin
          P := Self[x,y];

          if (P.R <> 255) or (P.G <> 255) or (P.B <> 255) then begin
            B := Max(y, B);
          end;
        end;
    end;
begin
  LeftMargin;
  RightMargin;
  BottomMargin;
  TopMargin;

  Tmp := Self.GetPiece(L, T, R-L, B-T);
  Self.CopyFrom(Tmp);
  Tmp.Free;
end;

function TPV_Bitmap.CountColors: Integer;
var Map: array of array of array of Byte;
    x,y: Integer;
    P: TPix;
    i,j,k: Integer;
begin
  SetLength(Map, 256,256,256); //16 MB

  for i:=0 to 255 do
    for j:=0 to 255 do
      for k:=0 to 255 do Map[i][j][k] := 0;


  for y:=0 to FHeight-1 do
    for x:=0 to FWidth-1 do begin
      P := Self.Pixel[x,y];

      Map[P.R][P.G][P.B] := 1;
    end;

  Result := 0;

  for i:=0 to 255 do
    for j:=0 to 255 do
      for k:=0 to 255 do if Map[i][j][k] <> 0 then Inc(Result);
end;

function TPV_Bitmap.GetPixel(X,Y: Integer): TPix;
var P: PPixArray;
begin
  P := Scanline[Y];
  Result := P^[X];
end;

procedure TPV_Bitmap.SetPixel(X,Y: Integer; Val: TPix);
var P: PPixArray;
begin
  P := Scanline[Y];
  P^[X] := Val;
end;

function TPV_Bitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TPV_Bitmap.SetWidth(Val: Integer);
begin
  FWidth := Val;
  SetLength(FData, FWidth*FHeight);
end;

function TPV_Bitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TPV_Bitmap.SetHeight(Val: Integer);
begin
  FHeight := Val;
  SetLength(FData, FWidth*FHeight);
end;

function TPV_Bitmap.GetScanline(Y: Integer): Pointer;
begin
  Result := @FData[Y* FWidth];
end;

procedure TPV_Bitmap.SetMono(X, Y: Integer; B: Byte);
begin
  FData[Y* FWidth + X].RGBA := PaletteMono[B];
end;

{$IFDEF WINDOWS}
procedure TPV_Bitmap.SetRGBA(X, Y: Integer; R, G, B, A: Byte);
begin
  FData[Y* FWidth + X].RGBA := B + (G shl 8) + (R shl 16) + (A shl 24);
end;

procedure TPV_Bitmap.SetRGB(X, Y: Integer; R, G, B: Byte);
begin
  FData[Y* FWidth + X].RGBA := B + (G shl 8) + (R shl 16) + (255 shl 24);
end;
{$ELSE}
procedure TPV_Bitmap.SetRGBA(X, Y: Integer; R, G, B, A: Byte);
begin
  FData[Y* FWidth + X].R := R;
  FData[Y* FWidth + X].G := G;
  FData[Y* FWidth + X].B := B;
  FData[Y* FWidth + X].A := A;
end;

procedure TPV_Bitmap.SetRGB(X, Y: Integer; R, G, B: Byte);
begin
  FData[Y* FWidth + X].R := R;
  FData[Y* FWidth + X].G := G;
  FData[Y* FWidth + X].B := B;
  FData[Y* FWidth + X].A := 255;
end;
{$ENDIF}

procedure TPV_Bitmap.SetR(X, Y: Integer; R: Byte);
begin
  FData[Y* FWidth + X].R := R;
end;

procedure TPV_Bitmap.SetG(X, Y: Integer; G: Byte);
begin
  FData[Y* FWidth + X].G := G;
end;

procedure TPV_Bitmap.SetB(X, Y: Integer; B: Byte);
begin
  FData[Y* FWidth + X].B := B;
end;

procedure TPV_Bitmap.SetA(X, Y: Integer; A: Byte);
begin
  FData[Y* FWidth + X].A := A;
end;

procedure TPV_Bitmap.AddPal(R, G, B, A: Byte);
begin
  FPalette[PaletteLen].RGBA := B + (G shl 8) + (R shl 16) + (A shl 24);
  Inc(PaletteLen);
end;

procedure TPV_Bitmap.SetPal(X, Y: Integer; Index: Byte);
begin
  FData[Y* FWidth + X].RGBA := FPalette[Index].RGBA;
end;

function TPV_Bitmap.GetPalIndex(X, Y: Integer): Integer;
var P,Pal: TPix;
    i: Integer;
begin
  P := Self.Pixel[x,y];

  for i:=0 to PaletteLen-1 do begin
    Pal := FPalette[i];

    if (Pal.R = P.R) and (Pal.G = P.G) and (Pal.B = P.B) then Exit(i);
  end;

  Result := 0;
end;

procedure TPV_Bitmap.AddPalette(Pal: array of TPal; Len: Integer);
var i: Integer;
begin
  for i:=0 to Len-1 do
    AddPal(Pal[i].R, Pal[i].G, Pal[i].B, 255);
end;

procedure TPV_Bitmap.ClearPalette;
begin
  PaletteLen := 0;
end;

constructor TPV_Bitmap.Create;
begin
  inherited Create;

  FWidth := 1;
  FHeight := 1;
  SetLength(FData, FWidth*FHeight);

  SetLength(FPalette, 256);
  PaletteLen := 0;
  FormatName := '';
end;

procedure TPV_Bitmap.SetSize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FData, FWidth*FHeight);
end;

function TPV_Bitmap.LoadFromFile(Filename: String): Boolean;
var Pic: TPicture;
    Reader: TPV_BitmapReader;
    F: TFileStream;
    Ext: String;
    Res: Boolean;
    AFormat: String;
begin
  Result := False;
  Ext := Copy(ExtractFileExt(Filename), 2);

  F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);

  Reader := BitmapFormats.FindReader(Ext, AFormat);

  if Reader <> nil then begin
    ClearPalette;
    Res := Reader(Self, F);
    if Res then begin
      Result := True;
      Self.FormatName := AFormat;
    end;
  end;

  F.Free;
end;

procedure TPV_Bitmap.SaveToFile(Filename: String);
begin
  SaveToFile(Filename, 0);
end;

procedure TPV_Bitmap.SaveToFile(Filename: String; Compression: Byte);
var Ext: String;
    Writer: TPV_BitmapWriter;
    F: TFileStream;
begin
  Ext := Copy(ExtractFileExt(Filename), 2);

  F := TFileStream.Create(Filename, fmCreate);

  Writer := BitmapFormats.FindWriter(Ext);

  if Writer <> nil then Writer(Self, F, Compression)
  else raise Exception.Create('Unsupported format: ' + Ext);

  F.Free;
end;

procedure TPV_BitmapFormat.Item(Index: Integer; out Ext: String; out
  Reader: TPV_BitmapReader; out Writer: TPV_BitmapWriter);
begin
  Ext := FList[Index].Ext;
  Reader := FList[Index].Reader;
  Writer := FList[Index].Writer;
end;

constructor TPV_BitmapFormat.Create;
begin
  FCount := 0;
  SetLength(FList, 100);
end;

function TPV_BitmapFormat.FindReader(Ext: String; out Format: String): TPV_BitmapReader;
var i: Integer;
begin
  Result := nil;

  Ext := LowerCase(Ext);

  for i:=0 to FCount-1 do
    if FList[i].Ext = Ext then begin
      Format := FList[i].Name;
      Result := FList[i].Reader;
      Exit;
    end;
end;

function TPV_BitmapFormat.FindWriter(Ext: String): TPV_BitmapWriter;
var i: Integer;
begin
  Result := nil;

  Ext := LowerCase(Ext);

  for i:=0 to FCount-1 do
    if FList[i].Ext = Ext then Exit(FList[i].Writer);
end;

function TPV_BitmapFormat.FindName(Ext: String): String;
var i: Integer;
begin
  Result := '';

  Ext := LowerCase(Ext);

  for i:=0 to FCount-1 do
    if FList[i].Ext = Ext then Exit(FList[i].Name);
end;

procedure TPV_BitmapFormat.Add(Ext: String; Reader: TPV_BitmapReader;
  Writer: TPV_BitmapWriter; Name: String);
begin
  FList[FCount].Ext := Ext;
  FList[FCount].Reader := Reader;
  FList[FCount].Writer := Writer;
  FList[FCount].Name := Name;
  Inc(FCount);
end;

initialization
  BitmapFormats := TPV_BitmapFormat.Create;

finalization
  BitmapFormats.Free;

end.
