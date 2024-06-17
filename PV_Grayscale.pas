unit PV_Grayscale;

//Lazzy Image Viewer
//github.com/PascalVault
//License: MIT

interface

uses Graphics, Math, PV_Bitmap;


procedure Grayscale(Bmp: TPV_Bitmap; MaxColors: Byte; Dither: TDither = ddFloyd);
procedure BlackWhite(Bmp: TPV_Bitmap; Dither: TDither = ddFloyd);

implementation


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
end;

procedure Grayscale(Bmp: TPV_Bitmap; MaxColors: Byte; Dither: TDither);
var i: Integer;
    x,y: Integer;
    P: PPix;
    R,G,B: Byte;
    Error: TPixInt;
    Ratio: Extended;
begin
  Ratio := 255/MaxColors;

  //Bmp.FBitmap.BeginUpdate(False);

  for y:=0 to Bmp.Height-1 do begin
     P := Bmp.Scanline[y];

     for x:=0 to Bmp.Width-1 do begin
       //R := Floor((P^.R + P^.G + P^.B)/3);
        R := Round(0.2126*P^.R + 0.7152*P^.G + 0.0722*P^.B);
       R := Floor(Ratio * Floor(R/Ratio));

       Error.R := P^.R-R;

       P^.R := R;

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

  //set G,B to R
  for y:=0 to Bmp.Height-1 do
     for x:=0 to Bmp.Width-1 do begin
       R := Bmp[x,y].R;

       Bmp.SetG(x,y, R);
       Bmp.SetB(x,y, R);
     end;

  //Bmp.FBitmap.EndUpdate();
end;

procedure BlackWhite(Bmp: TPV_Bitmap; Dither: TDither);
var i: Integer;
    x,y: Integer;
    P: PPix;
    R,G,B: Byte;
    Error: TPixInt;
begin
  //Bmp.FBitmap.BeginUpdate(False);

  for y:=0 to Bmp.Height-1 do begin
     P := Bmp.Scanline[y];

     for x:=0 to Bmp.Width-1 do begin
       if P^.R > 127 then R := 255
       else R := 0;

       Error.R := P^.R-R;

       P^.R := R;

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

  //set G,B to R
  for y:=0 to Bmp.Height-1 do
     for x:=0 to Bmp.Width-1 do begin
       R := Bmp[x,y].R;

       Bmp.SetG(x,y, R);
       Bmp.SetB(x,y, R);
     end;

  //Bmp.FBitmap.EndUpdate();
end;

end.
