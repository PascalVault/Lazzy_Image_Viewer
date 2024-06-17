unit PV_Palette;

//Lazzy Image Viewer
//github.com/PascalVault
//License: MIT

interface

uses Graphics, Math, PV_Bitmap;

type
  TOctreeNode = class;	// Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode = Class(TObject)
  public
    IsLeaf		: Boolean;
    PixelCount		: integer;
    SumR, SumG, SumB    : Integer;
    GreenSum		: integer;
    BlueSum		: integer;
    Next		: TOctreeNode;
    Child		: TReducibleNodes;

    constructor Create(Level: integer; ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;
  end;

  TColorQuantizer = class(TObject)
  private
    FTree		: TOctreeNode;
    FLeafCount		: integer;
    FReducibleNodes	: TReducibleNodes;
    FMaxColors		: integer;
    FColorBits		: integer;

  protected
    procedure AddColor(var Node: TOctreeNode; r, g, b: byte; ColorBits: integer;
      Level: integer; var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TPalArray; var Index: integer);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(MaxColors: integer; ColorBits: integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TPalArray);
    function ProcessImage(const Bmp: TPV_Bitmap): boolean;

    property ColorCount: integer read FLeafCount;
  end;

  procedure ReduceColors(Bmp: TPV_Bitmap; MaxColors: Byte; Dither: TDither = ddFloyd);

implementation


function ColorDistance(r,g,b: Byte; rr,gg,bb: Byte): Int64;
begin
  Result := (rr-r)*(rr-r) + (gg-g)*(gg-g) + (bb-b)*(bb-b);
end;

procedure BestColor(r,g,b: Byte; pal: array of TPix; palSize: Integer; out rr,gg,bb: Byte);
var i: Integer;
    BestDist: Int64;
    BestIndex: Byte;
    CurDist: Int64;
begin
  BestDist := 255*255*3;
  BestIndex := 0;

  for i:=0 to PalSize-1 do begin
    CurDist := ColorDistance(r,g,b , pal[i].r, pal[i].g, pal[i].b);
    if CurDist < BestDist then begin
      BestDist := CurDist;
      BestIndex := i;
    end;
  end;

  rr := pal[BestIndex].R;
  gg := pal[BestIndex].G;
  bb := pal[BestIndex].B;
end;

function Clip(Val: Extended): Byte;
begin
  if Val > 255 then Result := 255
  else if Val < 0 then Result := 0
  else Result := Round(Val);
end;

procedure _atkinson(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Bill Atkinson dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-3) then Exit;

  DiffR := (1/8) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR));
  Bmp.SetR(x+2, y  , Clip(Bmp[x+2, y  ].R + DiffR));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR));
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR));
  Bmp.SetR(x  , y+2, Clip(Bmp[x  , y+2].R + DiffR));

  DiffG := (1/8) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG));
  Bmp.SetG(x+2, y  , Clip(Bmp[x+2, y  ].G + DiffG));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG));
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG));
  Bmp.SetG(x  , y+2, Clip(Bmp[x  , y+2].G + DiffG));

  DiffB := (1/8) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB));
  Bmp.SetB(x+2, y  , Clip(Bmp[x+2, y  ].B + DiffB));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB));
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB));
  Bmp.SetB(x  , y+2, Clip(Bmp[x  , y+2].B + DiffB));
end;

procedure _jarvis(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Jarvis-Judice-Ninke dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-3) then Exit;

  DiffR := (1/48) * error.R;
  Bmp.SetR(x + 1, y    , Clip(Bmp[x+1, y  ].R + DiffR * 7));
  Bmp.SetR(x + 2, y    , Clip(Bmp[x+2, y  ].R + DiffR * 5));
  Bmp.SetR(x - 2, y + 1, Clip(Bmp[x-2, y+1].R + DiffR * 3));
  Bmp.SetR(x - 1, y + 1, Clip(Bmp[x-1, y+1].R + DiffR * 5));
  Bmp.SetR(x    , y + 1, Clip(Bmp[x  , y+1].R + DiffR * 7));
  Bmp.SetR(x + 1, y + 1, Clip(Bmp[x+1, y+1].R + DiffR * 5));
  Bmp.SetR(x + 2, y + 1, Clip(Bmp[x+2, y+1].R + DiffR * 3));
  Bmp.SetR(x - 2, y + 2, Clip(Bmp[x-2, y+2].R + DiffR * 1));
  Bmp.SetR(x - 1, y + 2, Clip(Bmp[x-1, y+2].R + DiffR * 3));
  Bmp.SetR(x    , y + 2, Clip(Bmp[x  , y+2].R + DiffR * 5));
  Bmp.SetR(x + 1, y + 2, Clip(Bmp[x+1, y+2].R + DiffR * 3));
  Bmp.SetR(x + 2, y + 2, Clip(Bmp[x+2, y+2].R + DiffR * 1));

  DiffG := (1/48) * error.G;
  Bmp.SetG(x + 1, y    , Clip(Bmp[x+1, y  ].G + DiffG * 7));
  Bmp.SetG(x + 2, y    , Clip(Bmp[x+2, y  ].G + DiffG * 5));
  Bmp.SetG(x - 2, y + 1, Clip(Bmp[x-2, y+1].G + DiffG * 3));
  Bmp.SetG(x - 1, y + 1, Clip(Bmp[x-1, y+1].G + DiffG * 5));
  Bmp.SetG(x    , y + 1, Clip(Bmp[x  , y+1].G + DiffG * 7));
  Bmp.SetG(x + 1, y + 1, Clip(Bmp[x+1, y+1].G + DiffG * 5));
  Bmp.SetG(x + 2, y + 1, Clip(Bmp[x+2, y+1].G + DiffG * 3));
  Bmp.SetG(x - 2, y + 2, Clip(Bmp[x-2, y+2].G + DiffG * 1));
  Bmp.SetG(x - 1, y + 2, Clip(Bmp[x-1, y+2].G + DiffG * 3));
  Bmp.SetG(x    , y + 2, Clip(Bmp[x  , y+2].G + DiffG * 5));
  Bmp.SetG(x + 1, y + 2, Clip(Bmp[x+1, y+2].G + DiffG * 3));
  Bmp.SetG(x + 2, y + 2, Clip(Bmp[x+2, y+2].G + DiffG * 1));

  DiffB := (1/48) * error.B;
  Bmp.SetB(x + 1, y    , Clip(Bmp[x+1, y  ].B + DiffB * 7));
  Bmp.SetB(x + 2, y    , Clip(Bmp[x+2, y  ].B + DiffB * 5));
  Bmp.SetB(x - 2, y + 1, Clip(Bmp[x-2, y+1].B + DiffB * 3));
  Bmp.SetB(x - 1, y + 1, Clip(Bmp[x-1, y+1].B + DiffB * 5));
  Bmp.SetB(x    , y + 1, Clip(Bmp[x  , y+1].B + DiffB * 7));
  Bmp.SetB(x + 1, y + 1, Clip(Bmp[x+1, y+1].B + DiffB * 5));
  Bmp.SetB(x + 2, y + 1, Clip(Bmp[x+2, y+1].B + DiffB * 3));
  Bmp.SetB(x - 2, y + 2, Clip(Bmp[x-2, y+2].B + DiffB * 1));
  Bmp.SetB(x - 1, y + 2, Clip(Bmp[x-1, y+2].B + DiffB * 3));
  Bmp.SetB(x    , y + 2, Clip(Bmp[x  , y+2].B + DiffB * 5));
  Bmp.SetB(x + 1, y + 2, Clip(Bmp[x+1, y+2].B + DiffB * 3));
  Bmp.SetB(x + 2, y + 2, Clip(Bmp[x+2, y+2].B + DiffB * 1));
end;

procedure _sierra2(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Sierra 2 dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-3) then Exit;

  DiffR := (1/16) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 4));
  Bmp.SetR(x+2, y  , Clip(Bmp[x+2, y  ].R + DiffR * 3));
  Bmp.SetR(x-2, y+1, Clip(Bmp[x-2, y+1].R + DiffR * 1));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 2));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 3));
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR * 2));
  Bmp.SetR(x+2, y+1, Clip(Bmp[x+2, y+1].R + DiffR * 1));

  DiffG := (1/16) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 4));
  Bmp.SetG(x+2, y  , Clip(Bmp[x+2, y  ].G + DiffG * 3));
  Bmp.SetG(x-2, y+1, Clip(Bmp[x-2, y+1].G + DiffG * 1));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 2));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 3));
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG * 2));
  Bmp.SetG(x+2, y+1, Clip(Bmp[x+2, y+1].G + DiffG * 1));

  DiffB := (1/16) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 4));
  Bmp.SetB(x+2, y  , Clip(Bmp[x+2, y  ].B + DiffB * 3));
  Bmp.SetB(x-2, y+1, Clip(Bmp[x-2, y+1].B + DiffB * 1));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 2));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 3));
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB * 2));
  Bmp.SetB(x+2, y+1, Clip(Bmp[x+2, y+1].B + DiffB * 1));
end;

procedure _sierra3(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Sierra 3 dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-3) then Exit;

  DiffR := (1/32) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 5));
  Bmp.SetR(x+2, y  , Clip(Bmp[x+2, y  ].R + DiffR * 3));
  Bmp.SetR(x-2, y+1, Clip(Bmp[x-2, y+1].R + DiffR * 2));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 4));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 5));
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR * 4));
  Bmp.SetR(x+2, y+1, Clip(Bmp[x+2, y+1].R + DiffR * 2));
  Bmp.SetR(x-1, y+2, Clip(Bmp[x-1, y+2].R + DiffR * 2));
  Bmp.SetR(x  , y+2, Clip(Bmp[x  , y+2].R + DiffR * 3));
  Bmp.SetR(x+1, y+2, Clip(Bmp[x+1, y+2].R + DiffR * 2));

  DiffG := (1/32) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 5));
  Bmp.SetG(x+2, y  , Clip(Bmp[x+2, y  ].G + DiffG * 3));
  Bmp.SetG(x-2, y+1, Clip(Bmp[x-2, y+1].G + DiffG * 2));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 4));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 5));
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG * 4));
  Bmp.SetG(x+2, y+1, Clip(Bmp[x+2, y+1].G + DiffG * 2));
  Bmp.SetG(x-1, y+2, Clip(Bmp[x-1, y+2].G + DiffG * 2));
  Bmp.SetG(x  , y+2, Clip(Bmp[x  , y+2].G + DiffG * 3));
  Bmp.SetG(x+1, y+2, Clip(Bmp[x+1, y+2].G + DiffG * 2));

  DiffB := (1/32) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 5));
  Bmp.SetB(x+2, y  , Clip(Bmp[x+2, y  ].B + DiffB * 3));
  Bmp.SetB(x-2, y+1, Clip(Bmp[x-2, y+1].B + DiffB * 2));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 4));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 5));
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB * 4));
  Bmp.SetB(x+2, y+1, Clip(Bmp[x+2, y+1].B + DiffB * 2));
  Bmp.SetB(x-1, y+2, Clip(Bmp[x-1, y+2].B + DiffB * 2));
  Bmp.SetB(x  , y+2, Clip(Bmp[x  , y+2].B + DiffB * 3));
  Bmp.SetB(x+1, y+2, Clip(Bmp[x+1, y+2].B + DiffB * 2));
end;

procedure _sierra4(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Sierra 2-4a dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 1) or (x > Bmp.Width-1) or (y > Bmp.Height-2) then Exit;

  DiffR := (1/4) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 2));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 1));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 1));

  DiffG := (1/4) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 2));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 1));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 1));

  DiffB := (1/4) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 2));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 1));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 1));
end;

procedure _stucki(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Stucki dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-3) then Exit;

  DiffR := (1/42) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 8));
  Bmp.SetR(x+2, y  , Clip(Bmp[x+2, y  ].R + DiffR * 4));
  Bmp.SetR(x-2, y+1, Clip(Bmp[x-2, y+1].R + DiffR * 2));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 4));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 8));
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR * 4));
  Bmp.SetR(x+2, y+1, Clip(Bmp[x+2, y+1].R + DiffR * 2));
  Bmp.SetR(x-2, y+2, Clip(Bmp[x-2, y+2].R + DiffR * 1));
  Bmp.SetR(x-1, y+2, Clip(Bmp[x-1, y+2].R + DiffR * 2));
  Bmp.SetR(x  , y+2, Clip(Bmp[x  , y+2].R + DiffR * 4));
  Bmp.SetR(x+1, y+2, Clip(Bmp[x+1, y+2].R + DiffR * 2));
  Bmp.SetR(x+2, y+2, Clip(Bmp[x+2, y+2].R + DiffR * 1));

  DiffG := (1/42) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 8));
  Bmp.SetG(x+2, y  , Clip(Bmp[x+2, y  ].G + DiffG * 4));
  Bmp.SetG(x-2, y+1, Clip(Bmp[x-2, y+1].G + DiffG * 2));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 4));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 8));
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG * 4));
  Bmp.SetG(x+2, y+1, Clip(Bmp[x+2, y+1].G + DiffG * 2));
  Bmp.SetG(x-2, y+2, Clip(Bmp[x-2, y+2].G + DiffG * 1));
  Bmp.SetG(x-1, y+2, Clip(Bmp[x-1, y+2].G + DiffG * 2));
  Bmp.SetG(x  , y+2, Clip(Bmp[x  , y+2].G + DiffG * 4));
  Bmp.SetG(x+1, y+2, Clip(Bmp[x+1, y+2].G + DiffG * 2));
  Bmp.SetG(x+2, y+2, Clip(Bmp[x+2, y+2].G + DiffG * 1));

  DiffB := (1/42) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 8));
  Bmp.SetB(x+2, y  , Clip(Bmp[x+2, y  ].B + DiffB * 4));
  Bmp.SetB(x-2, y+1, Clip(Bmp[x-2, y+1].B + DiffB * 2));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 4));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 8));
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB * 4));
  Bmp.SetB(x+2, y+1, Clip(Bmp[x+2, y+1].B + DiffB * 2));
  Bmp.SetB(x-2, y+2, Clip(Bmp[x-2, y+2].B + DiffB * 1));
  Bmp.SetB(x-1, y+2, Clip(Bmp[x-1, y+2].B + DiffB * 2));
  Bmp.SetB(x  , y+2, Clip(Bmp[x  , y+2].B + DiffB * 4));
  Bmp.SetB(x+1, y+2, Clip(Bmp[x+1, y+2].B + DiffB * 2));
  Bmp.SetB(x+2, y+2, Clip(Bmp[x+2, y+2].B + DiffB * 1));
end;

procedure _burkes(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Burkes dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 2) or (x > Bmp.Width-3) or (y > Bmp.Height-2) then Exit;

  DiffR := (1/32) * error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 8));
  Bmp.SetR(x+2, y  , Clip(Bmp[x+2, y  ].R + DiffR * 4));
  Bmp.SetR(x-2, y+1, Clip(Bmp[x-2, y+1].R + DiffR * 2));
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 4));
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 8));
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR * 4));
  Bmp.SetR(x+2, y+1, Clip(Bmp[x+2, y+1].R + DiffR * 2));

  DiffG := (1/32) * error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 8));
  Bmp.SetG(x+2, y  , Clip(Bmp[x+2, y  ].G + DiffG * 4));
  Bmp.SetG(x-2, y+1, Clip(Bmp[x-2, y+1].G + DiffG * 2));
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 4));
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 8));
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG * 4));
  Bmp.SetG(x+2, y+1, Clip(Bmp[x+2, y+1].G + DiffG * 2));

  DiffB := (1/32) * error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 8));
  Bmp.SetB(x+2, y  , Clip(Bmp[x+2, y  ].B + DiffB * 4));
  Bmp.SetB(x-2, y+1, Clip(Bmp[x-2, y+1].B + DiffB * 2));
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 4));
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 8));
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB * 4));
  Bmp.SetB(x+2, y+1, Clip(Bmp[x+2, y+1].B + DiffB * 2));
end;

procedure _floyd(Error: TPixInt; x, y: Integer; Bmp: TPV_Bitmap);
//Floyd-Steinberg dithering
var DiffR, DiffG, DiffB: Extended;
begin
  if (x < 1) or (x > Bmp.Width-2) or (y > Bmp.Height-2) then Exit;

  DiffR := (1/16) * Error.R;
  Bmp.SetR(x+1, y  , Clip(Bmp[x+1, y  ].R + DiffR * 7) );
  Bmp.SetR(x-1, y+1, Clip(Bmp[x-1, y+1].R + DiffR * 3) );
  Bmp.SetR(x  , y+1, Clip(Bmp[x  , y+1].R + DiffR * 5) );
  Bmp.SetR(x+1, y+1, Clip(Bmp[x+1, y+1].R + DiffR * 1) );

  DiffG := (1/16) * Error.G;
  Bmp.SetG(x+1, y  , Clip(Bmp[x+1, y  ].G + DiffG * 7) );
  Bmp.SetG(x-1, y+1, Clip(Bmp[x-1, y+1].G + DiffG * 3) );
  Bmp.SetG(x  , y+1, Clip(Bmp[x  , y+1].G + DiffG * 5) );
  Bmp.SetG(x+1, y+1, Clip(Bmp[x+1, y+1].G + DiffG * 1) );

  DiffB := (1/16) * Error.B;
  Bmp.SetB(x+1, y  , Clip(Bmp[x+1, y  ].B + DiffB * 7) );
  Bmp.SetB(x-1, y+1, Clip(Bmp[x-1, y+1].B + DiffB * 3) );
  Bmp.SetB(x  , y+1, Clip(Bmp[x  , y+1].B + DiffB * 5) );
  Bmp.SetB(x+1, y+1, Clip(Bmp[x+1, y+1].B + DiffB * 1) );
end;

procedure ReduceColors(Bmp: TPV_Bitmap; MaxColors: Byte; Dither: TDither);
var ColorQuantizer: TColorQuantizer;
    pal: array of TPix;
    i: Integer;
    x,y: Integer;
    P: PPix;
    R,G,B: Byte;
    Error: TPixInt;
    Bits: Byte;
begin
  Bits := Ceil(Log2(MaxColors));

  SetLength(pal, 256);

  ColorQuantizer := TColorQuantizer.Create(MaxColors, Bits);
  try
    ColorQuantizer.ProcessImage(Bmp);
    ColorQuantizer.GetColorTable(pal);
  finally
    ColorQuantizer.Free;
  end;

  //Bmp.FBitmap.BeginUpdate(False);

  for y:=0 to Bmp.Height-1 do begin
     P := Bmp.Scanline[y];

     for x:=0 to Bmp.Width-1 do begin
       BestColor(P^.R, P^.G, P^.B, pal, MaxColors, R,G,B);

       Error.R := P^.R-R;
       Error.G := P^.G-G;
       Error.B := P^.B-B;

       P^.R := R;
       P^.G := G;
       P^.B := B;

       case Dither of
         ddSierra2  : _Sierra2(Error, x,y, Bmp);
         ddSierra3  : _Sierra3(Error, x,y, Bmp);
         ddSierra4  : _Sierra4(Error, x,y, Bmp);
         ddJarvis   : _Jarvis(Error, x,y, Bmp);
         ddAtkinson : _Atkinson(Error, x,y, Bmp);
         ddStucki   : _Stucki(Error, x,y, Bmp);
         ddFloyd    : _Floyd(Error, x,y, Bmp);
         ddBurkes   : _Burkes(Error, x,y, Bmp);
       end;

       Inc(P);
     end;
  end;

  //save palette
  for i:=0 to MaxColors-1 do
    Bmp.AddPal(Pal[i].R, Pal[i].G, Pal[i].B, 255);

  //Bmp.FBitmap.EndUpdate();
end;

////////////////////////////////////////////////////////////////////////////////
//			Octree Color Quantization Engine
//
// Adapted from GifImage 2.2 (by Anders Melander and others), which was adapted
// from Earl F. Glynn's ColorQuantizationLibrary
////////////////////////////////////////////////////////////////////////////////

constructor TOctreeNode.Create(Level: integer; ColorBits: integer;
  var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
var i: Integer;
begin
  PixelCount := 0;
  SumR := 0;
  SumG := 0;
  SumB := 0;

  for i := Low(Child) to High(Child) do Child[i] := nil;

  IsLeaf := (Level = ColorBits);

  if (IsLeaf) then begin
    Next := nil;
    inc(LeafCount);
  end
  else begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := self;
  end;
end;

destructor TOctreeNode.Destroy;
var i: Integer;
begin
  for i := High(Child) downto Low(Child) do
    Child[i].Free;
end;

constructor TColorQuantizer.Create(MaxColors: integer; ColorBits: integer);
var i: Integer;
begin
  ASSERT(ColorBits <= 8, 'ColorBits must be 8 or less');

  FTree := nil;
  FLeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits;
end;

destructor TColorQuantizer.Destroy;
begin
  if (FTree <> nil) then
    DeleteTree(FTree);
end;

procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TPalArray);
var Index: Integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index);
end;

function TColorQuantizer.ProcessImage(const Bmp: TPV_Bitmap): boolean;
var i,j: Integer;
    P: PPix;
begin
  Result := True;

  for j := 0 to Bmp.Height-1 do begin
    P := Bmp.Scanline[j];

    for i:=0 to Bmp.Width-1 do begin
      AddColor(FTree, P^.R, P^.G, P^.B, FColorBits, 0, FLeafCount, FReducibleNodes);

      while FLeafCount > FMaxColors do ReduceTree(FColorbits, FLeafCount, FReducibleNodes);
      inc(P);
    end;
  end;
end;

procedure TColorQuantizer.AddColor(var Node: TOctreeNode; r,g,b: byte;
  ColorBits: integer; Level: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
const Mask:  array[0..7] of BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
var Index, Shift: Integer;
begin
  // If the node doesn't exist, create it.
  if (Node = nil) then
    Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if (Node.IsLeaf) then begin
    inc(Node.PixelCount);
    inc(Node.SumR, r);
    inc(Node.SumG, g);
    inc(Node.SumB, b);
  end
  else begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index := (((r and mask[Level]) SHR Shift) SHL 2)  or
             (((g and mask[Level]) SHR Shift) SHL 1)  or
              ((b and mask[Level]) SHR Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1, LeafCount, ReducibleNodes);
  end;
end;

procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var i: Integer;
begin
  for i := High(TReducibleNodes) downto Low(TReducibleNodes) do
    if (Node.Child[i] <> nil) then
      DeleteTree(Node.Child[i]);

  Node.Free;
  Node := nil;
end;

procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TPalArray; var Index: integer);
var  i: integer;
begin
  if (Node.IsLeaf) then begin
    with RGBQuadArray[Index] do begin
      if (Node.PixelCount <> 0) then begin
        R := BYTE(Node.SumR DIV Node.PixelCount);
        G := BYTE(Node.SumG DIV Node.PixelCount);
        B := BYTE(Node.SumB DIV Node.PixelCount);
      end
      else begin
        R := 0;
        G := 0;
        B := 0;
      end;
      A := 0;
    end;
    inc(Index);
  end
  else begin
    for i := Low(Node.Child) to High(Node.Child) do
      if (Node.Child[i] <> nil) then
        GetPaletteColors(Node.Child[i], RGBQuadArray, Index);
  end;
end;

procedure TColorQuantizer.ReduceTree(ColorBits: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
var SumR, SumG, SumB: Integer;
    Children: Integer;
    i: Integer;
    Node: TOctreeNode;
begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  SumR   := 0;
  SumG := 0;
  SumB  := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
    if (Node.Child[i] <> nil) then begin
      inc(SumR, Node.Child[i].SumR);
      inc(SumG, Node.Child[i].SumG);
      inc(SumB, Node.Child[i].SumB);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      inc(Children);
    end;

  Node.IsLeaf := TRUE;
  Node.SumR := SumR;
  Node.SumG := SumG;
  Node.SumB := SumB;
  dec(LeafCount, Children-1);
end;

end.
