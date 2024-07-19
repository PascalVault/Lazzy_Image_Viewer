unit PV_BitmapFormats;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

interface

uses Classes, Graphics, SysUtils, Dialogs,
  PV_Bitmap, PV_Streams, PV_CRC32Stream,
  ZStream, FPImage, IntfGraphics,
  FPWriteJPEG, FPReadJPEG, FPReadXPM, FPWriteXPM, FPReadPCX, FPReadBMP,
  FPReadPNG, FPReadTGA, FPReadTiff, FPWriteGIF, FPReadGIF, FPReadPSD,
  FPReadXWD, Math;

  type TC64Mem = record
    Bitmap: array[0..7999] of Byte;
    ScreenRAM: array[0..999] of Byte;
    ColorRAM: array[0..999] of Byte;
    bgColor: Byte;
  end;

implementation

const AtariPal: array[0..255] of TPal = (
(r:$2D; g:$2D; b:$2D),(r:$3B; g:$3B; b:$3B),(r:$49; g:$49; b:$49),(r:$57; g:$57; b:$57),
(r:$65; g:$65; b:$65),(r:$73; g:$73; b:$73),(r:$81; g:$81; b:$81),(r:$8F; g:$8F; b:$8F),
(r:$9D; g:$9D; b:$9D),(r:$AB; g:$AB; b:$AB),(r:$B9; g:$B9; b:$B9),(r:$C7; g:$C7; b:$C7),
(r:$D5; g:$D5; b:$D5),(r:$E3; g:$E3; b:$E3),(r:$F1; g:$F1; b:$F1),(r:$FF; g:$FF; b:$FF),
(r:$5C; g:$23; b:$00),(r:$6A; g:$31; b:$00),(r:$78; g:$3F; b:$00),(r:$86; g:$4D; b:$A),
(r:$94; g:$5B; b:$18),(r:$A2; g:$69; b:$26),(r:$B0; g:$77; b:$34),(r:$BE; g:$85; b:$42),
(r:$CC; g:$93; b:$50),(r:$DA; g:$A1; b:$5E),(r:$E8; g:$AF; b:$6C),(r:$F6; g:$BD; b:$7A),
(r:$FF; g:$CB; b:$88),(r:$FF; g:$D9; b:$96),(r:$FF; g:$E7; b:$A4),(r:$FF; g:$F5; b:$B2),
(r:$69; g:$14; b:$09),(r:$77; g:$22; b:$17),(r:$85; g:$30; b:$25),(r:$93; g:$3E; b:$33),
(r:$A1; g:$4C; b:$41),(r:$AF; g:$5A; b:$4F),(r:$BD; g:$68; b:$5D),(r:$CB; g:$76; b:$6B),
(r:$D9; g:$84; b:$79),(r:$E7; g:$92; b:$87),(r:$F5; g:$A0; b:$95),(r:$FF; g:$AE; b:$A3),
(r:$FF; g:$BC; b:$B1),(r:$FF; g:$CA; b:$BF),(r:$FF; g:$D8; b:$CD),(r:$FF; g:$E6; b:$DB),
(r:$6C; g:$0A; b:$38),(r:$7A; g:$18; b:$46),(r:$88; g:$26; b:$54),(r:$96; g:$34; b:$62),
(r:$A4; g:$42; b:$70),(r:$B2; g:$50; b:$7E),(r:$C0; g:$5E; b:$8C),(r:$CE; g:$6C; b:$9A),
(r:$DC; g:$7A; b:$A8),(r:$EA; g:$88; b:$B6),(r:$F8; g:$96; b:$C4),(r:$FF; g:$A4; b:$D2),
(r:$FF; g:$B2; b:$E0),(r:$FF; g:$C0; b:$EE),(r:$FF; g:$CE; b:$FC),(r:$FF; g:$DC; b:$FF),
(r:$64; g:$05; b:$65),(r:$72; g:$13; b:$73),(r:$80; g:$21; b:$81),(r:$8E; g:$2F; b:$8F),
(r:$9C; g:$3D; b:$9D),(r:$AA; g:$4B; b:$AB),(r:$B8; g:$59; b:$B9),(r:$C6; g:$67; b:$C7),
(r:$D4; g:$75; b:$D5),(r:$E2; g:$83; b:$E3),(r:$F0; g:$91; b:$F1),(r:$FE; g:$9F; b:$FF),
(r:$FF; g:$AD; b:$FF),(r:$FF; g:$BB; b:$FF),(r:$FF; g:$C9; b:$FF),(r:$FF; g:$D7; b:$FF),
(r:$52; g:$07; b:$89),(r:$60; g:$15; b:$97),(r:$6E; g:$23; b:$A5),(r:$7C; g:$31; b:$B3),
(r:$8A; g:$3F; b:$C1),(r:$98; g:$4D; b:$CF),(r:$A6; g:$5B; b:$DD),(r:$B4; g:$69; b:$EB),
(r:$C2; g:$77; b:$F9),(r:$D0; g:$85; b:$FF),(r:$DE; g:$93; b:$FF),(r:$EC; g:$A1; b:$FF),
(r:$FA; g:$AF; b:$FF),(r:$FF; g:$BD; b:$FF),(r:$FF; g:$CB; b:$FF),(r:$FF; g:$D9; b:$FF),
(r:$3A; g:$10; b:$9C),(r:$48; g:$1E; b:$AA),(r:$56; g:$2C; b:$B8),(r:$64; g:$3A; b:$C6),
(r:$72; g:$48; b:$D4),(r:$80; g:$56; b:$E2),(r:$8E; g:$64; b:$F0),(r:$9C; g:$72; b:$FE),
(r:$AA; g:$80; b:$FF),(r:$B8; g:$8E; b:$FF),(r:$C6; g:$9C; b:$FF),(r:$D4; g:$AA; b:$FF),
(r:$E2; g:$B8; b:$FF),(r:$F0; g:$C6; b:$FF),(r:$FE; g:$D4; b:$FF),(r:$FF; g:$E2; b:$FF),
(r:$1F; g:$1E; b:$9C),(r:$2D; g:$2C; b:$AA),(r:$3B; g:$3A; b:$B8),(r:$49; g:$48; b:$C6),
(r:$57; g:$56; b:$D4),(r:$65; g:$64; b:$E2),(r:$73; g:$72; b:$F0),(r:$81; g:$80; b:$FE),
(r:$8F; g:$8E; b:$FF),(r:$9D; g:$9C; b:$FF),(r:$AB; g:$AA; b:$FF),(r:$B9; g:$B8; b:$FF),
(r:$C7; g:$C6; b:$FF),(r:$D5; g:$D4; b:$FF),(r:$E3; g:$E2; b:$FF),(r:$F1; g:$F0; b:$FF),
(r:$07; g:$2E; b:$89),(r:$15; g:$3C; b:$97),(r:$23; g:$4A; b:$A5),(r:$31; g:$58; b:$B3),
(r:$3F; g:$66; b:$C1),(r:$4D; g:$74; b:$CF),(r:$5B; g:$82; b:$DD),(r:$69; g:$90; b:$EB),
(r:$77; g:$9E; b:$F9),(r:$85; g:$AC; b:$FF),(r:$93; g:$BA; b:$FF),(r:$A1; g:$C8; b:$FF),
(r:$AF; g:$D6; b:$FF),(r:$BD; g:$E4; b:$FF),(r:$CB; g:$F2; b:$FF),(r:$D9; g:$FF; b:$FF),
(r:$00; g:$3E; b:$65),(r:$03; g:$4C; b:$73),(r:$11; g:$5A; b:$81),(r:$1F; g:$68; b:$8F),
(r:$2D; g:$76; b:$9D),(r:$3B; g:$84; b:$AB),(r:$49; g:$92; b:$B9),(r:$57; g:$A0; b:$C7),
(r:$65; g:$AE; b:$D5),(r:$73; g:$BC; b:$E3),(r:$81; g:$CA; b:$F1),(r:$8F; g:$D8; b:$FF),
(r:$9D; g:$E6; b:$FF),(r:$AB; g:$F4; b:$FF),(r:$B9; g:$FF; b:$FF),(r:$C7; g:$FF; b:$FF),
(r:$00; g:$4B; b:$38),(r:$00; g:$59; b:$46),(r:$09; g:$67; b:$54),(r:$17; g:$75; b:$62),
(r:$25; g:$83; b:$70),(r:$33; g:$91; b:$7E),(r:$41; g:$9F; b:$8C),(r:$4F; g:$AD; b:$9A),
(r:$5D; g:$BB; b:$A8),(r:$6B; g:$C9; b:$B6),(r:$79; g:$D7; b:$C4),(r:$87; g:$E5; b:$D2),
(r:$95; g:$F3; b:$E0),(r:$A3; g:$FF; b:$EE),(r:$B1; g:$FF; b:$FC),(r:$BF; g:$FF; b:$FF),
(r:$00; g:$52; b:$09),(r:$00; g:$60; b:$17),(r:$0C; g:$6E; b:$25),(r:$1A; g:$7C; b:$33),
(r:$28; g:$8A; b:$41),(r:$36; g:$98; b:$4F),(r:$44; g:$A6; b:$5D),(r:$52; g:$B4; b:$6B),
(r:$60; g:$C2; b:$79),(r:$6E; g:$D0; b:$87),(r:$7C; g:$DE; b:$95),(r:$8A; g:$EC; b:$A3),
(r:$98; g:$FA; b:$B1),(r:$A6; g:$FF; b:$BF),(r:$B4; g:$FF; b:$CD),(r:$C2; g:$FF; b:$DB),
(r:$00; g:$53; b:$00),(r:$0B; g:$61; b:$00),(r:$19; g:$6F; b:$00),(r:$27; g:$7D; b:$0A),
(r:$35; g:$8B; b:$18),(r:$43; g:$99; b:$26),(r:$51; g:$A7; b:$34),(r:$5F; g:$B5; b:$42),
(r:$6D; g:$C3; b:$50),(r:$7B; g:$D1; b:$5E),(r:$89; g:$DF; b:$6C),(r:$97; g:$ED; b:$7A),
(r:$A5; g:$FB; b:$88),(r:$B3; g:$FF; b:$96),(r:$C1; g:$FF; b:$A4),(r:$CF; g:$FF; b:$B2),
(r:$13; g:$4E; b:$00),(r:$21; g:$5C; b:$00),(r:$2F; g:$6A; b:$00),(r:$3D; g:$78; b:$00),
(r:$4B; g:$86; b:$00),(r:$59; g:$94; b:$0B),(r:$67; g:$A2; b:$19),(r:$75; g:$B0; b:$27),
(r:$83; g:$BE; b:$35),(r:$91; g:$CC; b:$43),(r:$9F; g:$DA; b:$51),(r:$AD; g:$E8; b:$5F),
(r:$BB; g:$F6; b:$6D),(r:$C9; g:$FF; b:$7B),(r:$D7; g:$FF; b:$89),(r:$E5; g:$FF; b:$97),
(r:$2D; g:$43; b:$00),(r:$3B; g:$51; b:$00),(r:$49; g:$5F; b:$00),(r:$57; g:$6D; b:$00),
(r:$65; g:$7B; b:$00),(r:$73; g:$89; b:$01),(r:$81; g:$97; b:$0F),(r:$8F; g:$A5; b:$1D),
(r:$9D; g:$B3; b:$2B),(r:$AB; g:$C1; b:$39),(r:$B9; g:$CF; b:$47),(r:$C7; g:$DD; b:$55),
(r:$D5; g:$EB; b:$63),(r:$E3; g:$F9; b:$71),(r:$F1; g:$FF; b:$7F),(r:$FF; g:$FF; b:$8D),
(r:$46; g:$33; b:$00),(r:$54; g:$41; b:$00),(r:$62; g:$4F; b:$00),(r:$70; g:$5D; b:$00),
(r:$7E; g:$6B; b:$00),(r:$8C; g:$79; b:$0B),(r:$9A; g:$87; b:$19),(r:$A8; g:$95; b:$27),
(r:$B6; g:$A3; b:$35),(r:$C4; g:$B1; b:$43),(r:$D2; g:$BF; b:$51),(r:$E0; g:$CD; b:$5F),
(r:$EE; g:$DB; b:$6D),(r:$FC; g:$E9; b:$7B),(r:$FF; g:$F7; b:$89),(r:$FF; g:$FF; b:$97)
);

function yuv2rgb(y, u, v: Integer; out RR,GG,BB: Byte): Cardinal;
var R,G,B: Integer;
begin
  u := u - 128;
  v := v - 128;

  R := y + round(1.40200 * v);
  G := y - round(0.71414 * v + 0.34414 * u);
  B := y + round(1.77200 * u);

  if r < 0   then r := 0;
  if g < 0   then g := 0;
  if b < 0   then b := 0;
  if r > 255 then r := 255;
  if g > 255 then g := 255;
  if b > 255 then b := 255;

  RR := R;
  GG := G;
  BB := B;
end;

procedure Unrle_Psd(Src: TStream; UnpackedSize: Integer; Str: TStream);
var LengthOfLine: Word;
    Saved, Count: Integer;
    Temp,Val: Byte;
    i: Integer;
begin
  Saved := 0;
  repeat
    Temp := Src.ReadByte;

    If Temp >= 128 then begin
      Count := 256 - Temp;
      Val := Src.ReadByte;

      for i:=0 to Count do Str.Write(Val, 1);
      Inc(Saved, Count+1);
    end
    else begin
      Count := Temp;
      for i:=0 to Count do Str.Write(Src.ReadByte, 1);
      Inc(Saved, Count+1);
    end;
  until Saved >= UnpackedSize;
end;

procedure DecodeMulticolor(Bmp: TPV_Bitmap; Mem: TC64Mem);
const colodore: array[0..15] of Cardinal = (
    $000000,$ffffff,$96282e,$5bd6ce,
    $9f2dad,$41b936,$2724c4,$eff347,
    $9f4815,$5e3500,$da5f66,$474747,
    $787878,$91ff84,$6864ff,$aeaeae);
var Width, Height: Integer;
    i,j,k: Integer;
    x,y: Integer;
    pal: array[0..1999, 0..3] of Byte;
    R,G,B: Byte;
begin
  Bmp.SetSize(320, 200);

  //set C64 palette
  for i:=0 to 15 do begin
    B := colodore[i] and $FF;
    G := (colodore[i] shr 8) and $FF;
    R := (colodore[i] shr 16) and $FF;

    Bmp.AddPal(R,G,B,255);
  end;

  //screen ram
  for i:=0 to 999 do begin
    G := Mem.ScreenRAM[i];

    pal[i][1] := G shr 4;
    pal[i][2] := G and $F;
  end;

  //color ram
  for i:=0 to 999 do begin
    G := Mem.ColorRAM[i];

    pal[i][3] := G and $F;
    pal[i][0] := Mem.bgColor;
  end;

  i:= 0;
  for y:=0 to 24 do
  for x:=0 to 39 do
  for k:=0 to 7 do begin
    G := Mem.Bitmap[i];
    Inc(i);

    for j:=0 to 3 do begin
      B := GetBits(G, 6-2*j, 2);

      Bmp.SetPal(x*8 + 2*j, y*8 + k, pal[y*40 + x][B]);
      Bmp.SetPal(x*8 + 2*j+1, y*8 + k, pal[y*40 + x][B]);
    end;
  end;
end;

procedure DecodeHires(Bmp: TPV_Bitmap; Mem: TC64Mem);
const colodore: array[0..15] of Cardinal = (
    $000000,$ffffff,$96282e,$5bd6ce,
    $9f2dad,$41b936,$2724c4,$eff347,
    $9f4815,$5e3500,$da5f66,$474747,
    $787878,$91ff84,$6864ff,$aeaeae);
var Width, Height: Integer;
    i,j,k: Integer;
    x,y: Integer;
    pal: array[0..1999, 0..3] of Byte;
    R,G,B: Byte;
begin
  Bmp.SetSize(320, 200);

  //set C64 palette
  for i:=0 to 15 do begin
    B := colodore[i] and $FF;
    G := (colodore[i] shr 8) and $FF;
    R := (colodore[i] shr 16) and $FF;

    Bmp.AddPal(R,G,B,255);
  end;

  //screen ram
  for i:=0 to 999 do begin
    G := Mem.ScreenRAM[i];

    pal[i][1] := G shr 4;
    pal[i][0] := G and $F;
  end;

  i := 0;
  for y:=0 to 24 do
  for x:=0 to 39 do
  for k:=0 to 7 do begin
    G := Mem.Bitmap[i];
    Inc(i);

    for j:=0 to 7 do begin
      B := GetBits(G, 7-j, 1);

      Bmp.SetPal(x*8 + j, y*8 + k, pal[y*40 + x][B]);
    end;
  end;
end;


procedure UnGzip(Data: TStream; Result: TStream); //gzdecode from PHP
const BuffSize = 4096;
var Z: TDecompressionStream;
    Read: Integer;
    Buff: array of Byte;

    ID: Word;
    Method: Byte;
    Head: Word;
    Date: Cardinal;
    Flag, OS: Byte;
    Skip: Word;
    Tmp: String;
    Null: Integer;
begin
  ID     := Data.ReadWord;
  Method := Data.ReadByte;
  Head   := Data.ReadByte;
  Date   := Data.ReadDWord;
  Flag   := Data.ReadByte;
  OS     := Data.ReadByte;

  //FEXTRA
  if ((Head shr 2) and 1 = 1) then begin
    Skip := Data.ReadByte;
    Data.Position := Data.Position + Skip;
  end;

  //FNAME
  if ((Head shr 3) and 1 = 1) then begin
    SetLength(Tmp, 256);
    Data.Read(Tmp[1], 256);
    Null := Pos(chr(0), Tmp);
    Data.Position := Data.Position - Length(Tmp) + Null;
  end;

  //FCOMMENT
  if ((Head shr 4) and 1 = 1) then begin
    SetLength(Tmp, 256);
    Data.Read(Tmp[1], 256);
    Null := Pos(chr(0), Tmp);
    Data.Position := Data.Position - Length(Tmp) + Null;
  end;

  //FHCRC
  if ((Head shr 1) and 1 = 1) then begin
    Data.Position := Data.Position + Skip;
  end;

  //https://datatracker.ietf.org/doc/html/rfc1952#page-5
  Z := TDecompressionStream.Create(Data, True);
  SetLength(Buff, BuffSize);

  try
    repeat
      Read := Z.Read(Buff[0], BuffSize);
      Result.Write(Buff[0], Read);
    until Read<BuffSize-1;
  finally
    Z.Free;
  end;
end;


function LBM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Reader2: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,k,j: Integer;
    form, ilbm: String;
    size: Cardinal;
    bmWidth, bmHeight, bmX, bmY, bmPlanesCount, bmMasking, bmCompression,
    bmPad1, bmTransColor, bmXAspect, bmYAspect, bmPageWidth, bmPageHeight: Cardinal;
    chName: String;
    chSize: Integer;
    col: Byte;
    pal: array[0..255] of TPix;
    rr: array of Cardinal;
    gg: Byte;
    p: TPix;
    Mem: TMemoryStream;
begin
  Reader := TPV_Reader.Create(Str);

  form := Reader.getS(4);
  size := Reader.getMU4;
  ilbm := Reader.getS(4);

  if form <> 'FORM' then begin
    Reader.Free;
    Exit(False);
  end;

  if (ilbm <> 'ILBM') and (ilbm <> 'PBM ') then begin
    Reader.Free;
    Exit(False);
  end;

  while Reader.Offset < Reader.Size do begin
      chName := Reader.getS(4);   //LBM Chunk
      chSize := Reader.getMU4;

      if chName = 'BMHD' then begin
            bmWidth       := Reader.getMU2;
            bmHeight      := Reader.getMU2;
            bmX           := Reader.getMU2;
            bmY           := Reader.getMU2;
            bmPlanesCount := Reader.getU;
            bmMasking     := Reader.getU;
            bmCompression := Reader.getU;
            bmPad1        := Reader.getU;
            bmTransColor  := Reader.getMU2;
            bmXAspect     := Reader.getU;
            bmYAspect     := Reader.getU;
            bmPageWidth   := Reader.getMU2;
            bmPageHeight  := Reader.getMU2;

            Bmp.SetSize(bmWidth, bmHeight);
      end
      else if chName = 'CMAP' then begin //palette
            for i:=0 to ceil(chSize/3)-1 do begin

              pal[i].r := Reader.getU;
              pal[i].g := Reader.getU;
              pal[i].b := Reader.getU;
            end;
      end
      else if chName = 'BODY' then begin //image body

            Mem := TMemoryStream.Create;
            Str.Position := Reader.Offset;

            setLength(rr, bmWidth);

            if bmCompression = 1 then begin//RLE
              Unrle_LBM(Str, Mem, Str.Size-Str.Position);
              Mem.Position := 0;
              Reader2 := TPV_Reader.Create(Mem);
              Reader.Offset := Str.Size;
            end
            else begin
              Reader2 := TPV_Reader.Create(Str, chSize);
              Reader.Offset := Reader.Offset + chSize;
            end;

            for y:=0 to bmHeight-1 do begin
              for i:=0 to bmWidth-1 do rr[i] := 0;

              //showmessage(IntToStr(bmPlanesCount));

              for k:=0 to bmPlanesCount-1 do //for every bitplane
              for x:=0 to ceil(bmWidth/8)-1 do begin
                b := Reader2.getU;
                for j:=0 to 8-1 do begin
                  gg := getBits(b, (8-1)-j, 1);
                  rr[8*x + j] := rr[8*x + j] + (gg shl k);
                end
              end;

              if bmPlanesCount<9 then
                for x:=0 to bmWidth-1 do begin
                  gg := rr[x];
                  Bmp.SetRGBA(x, y, pal[gg].r, pal[gg].g, pal[gg].b, 255);
                end
              else
                for x:=0 to bmWidth-1 do begin
                  P.RGBA := rr[x];
                  Bmp.SetRGBA(x,y, p.B, p.G, p.R, 255);
                end;
            end;

            Mem.Free;
            Reader2.free;


          //  break; //TODO: remove

      end  //Other block- ignore
      else begin
            Reader.offset := Reader.offset + chSize;
      end
    end;

  Result := True;
  Reader.Free;
end;

function A4MI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari 4MI
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
pal: array[0..3] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  if (Str.size <> 244) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(16, 240);

  Reader.get(pal, 4);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do Bmp.Set32(x, y, atariPal[0]);

  for y:=0 to Bmp.Height-1 do begin
     g := Reader.getU;

    for x:=0 to 3 do begin
      a := getBits(g, 2*x, 2);

      if (a=2) or (a=3) then Bmp.Set32(4*x,   y, atariPal[pal[x]] );
      if (a=3) or (a=1) then Bmp.Set32(4*x+1, y, atariPal[pal[x]] );
    end;
  end;

  Result := True;
  Reader.Free;
end;

function A4PL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari 4PL
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    pal: array[0..3] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  if (Str.size <> 964) then begin
    Reader.Free;
    Exit(False);
  end;

  //Format sklada sie z 4 paskow pionowych. Kazdy pasek ma 8 pikseli (1bajt)
  //szerokosci i pomiedzy nimi sa 2 piksele "prozni"
  //Poniewaz wysokosc to 240 pikseli, wiec kazdy pasek zajmuje 240 bajtow
  //Pierwsze 4 bajty to paleta- po 1 bajcie dla kazdego paska
  //Ta paleta to tak naprawde indeks do palety atariPal

  Bmp.SetSize(40, 240);

  Reader.get(pal, 4);

  for i:=0 to 255 do
    Bmp.AddPal(atariPal[i].r, atariPal[i].g, atariPal[i].b, 255);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do Bmp.SetPal(x, y, 0);

  for x:=0 to 3 do
  for y:=0 to Bmp.Height-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);

        if a=1 then Bmp.SetPal(10*x + i, y, pal[x]);
      end;
  end;

  Result := True;
  Reader.Free;
end;


function PAM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Portable Arbitrary Map

const colors: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id, line, type1: String;
    version, type2: Byte;
    maxVal, depth: Integer;
    temp: TStringList;
    bpp: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getLn;
  version := strToInt(id[2]);

  if (id[1] <> 'P') or (version <> 7) then begin
    Reader.Free;
    Exit(False);
  end;

  //read header
  temp := TStringList.Create;

  for i:=0 to 99 do begin
    line := Reader.getLn;
    if line = '' then continue;
    if line[1] <> '#' then temp.add(line);
    if line = 'ENDHDR' then break;
  end;

  temp.text := StringReplace(temp.text, ' ', '=', [rfReplaceAll]);

  width  := strToInt(temp.Values['WIDTH']);
  height := strToInt(temp.Values['HEIGHT']);
  maxVal := strToInt(temp.Values['MAXVAL']); //255
  depth  := strToInt(temp.Values['DEPTH']); //3
  type1  := temp.Values['TUPLTYPE']; //RGB
  type1  := UpperCase(type1);

  if type1 = 'BLACKANDWHITE_ALPHA'  then type2 := 4
  else if type1 = 'BLACKANDWHITE'   then type2 := 1
  else if type1 = 'GRAYSCALE_ALPHA' then type2 := 5
  else if type1 = 'GRAYSCALE'       then type2 := 2
  else if type1 = 'RGB_ALPHA'       then type2 := 6
  else if type1 = 'RGB'             then type2 := 3
  else begin
    //Unsupported type: ' + type1
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to width-1 do
    case (type2) of
      1: begin
           g := Reader.getU;
           Bmp.Set32(x, y, colors[g]);
         end;
      2: begin
           g := Reader.getU;
           Bmp.SetRGBA(x, y, g, g, g, 255);
         end;
      3: begin
           r := Reader.getU;
           g := Reader.getU;
           b := Reader.getU;
           Bmp.SetRGBA(x, y, r, g, b, 255);
         end;
      4: begin
           g := Reader.getU;
           a := Reader.getU;
           Bmp.Set32(x, y, colors[g]);     //TODO: use alfa
         end;
      5: begin
           g := Reader.getU;
           a := Reader.getU;
           Bmp.SetRGBA(x, y, g, g, g, a);
         end;
      6: begin
           r := Reader.getU;
           g := Reader.getU;
           b := Reader.getU;
           a := Reader.getU;
           Bmp.SetRGBA(x, y, r, g, b, a);
         end;
    end;

  case (type2) of
    1,4: bpp := 1;
    2,5: bpp := 8;
    3,6: bpp := 24;
  end;


  Result := True;
  Reader.Free;
end;

function FITS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Flexible Image Transport System
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    temp: TStringList;
    p: Integer;
    line: String;
begin
  Reader := TPV_Reader.Create(Str);

  //read head
  temp := TStringList.Create;

  for i:=0 to 99 do begin
    line := Reader.getS(80);
    if pos('END', line) > 0 then break;
    if pos('HISTORY', line) = 1 then temp.add(line)
    else begin
      p := pos('=', line);
      temp.add(trim(copy(line, 1, p-1)) + '=' + trim(Copy(line, p+1)));
    end;
  end;

  if (temp.Values['SIMPLE'] = '') then begin
    Reader.Free;
    Exit(False);
  end;

  width  := strToInt(temp.values['NAXIS1']);
  height := strToInt(temp.values['NAXIS2']);
  Bmp.SetSize(width, height);

  //read the body
  Str.position := 2880;

  for y:=height-1 downto 0 do
  for x:=0 to width-1 do begin
    g := Reader.getU;
    Bmp.SetRGB(x, y, g, g, g);
  end;

  Result := True;
  Reader.Free;
end;


function G10_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari G10
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    pal: array[0..8] of TPal;
    Bmp2: TPV_Bitmap;
    half: Integer;
begin
  if Str.size <> 7689 then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp.SetSize(2*80, 2*48);
  Bmp2.SetSize(Bmp.Width, Bmp.Height);

  //read pallete- at the end of file
  Reader.offset := Reader.size - 9;

  for i:=0 to 8 do
    pal[i] := atariPal[Reader.getU];

  Reader.offset := 0;

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to (Bmp2.Width div 2)-1 do begin
      g := Reader.getU;

      for i:=0 to 1 do begin
        a := getBits(g, 4-4*i, 4);
        Bmp2.SetRGB(2*x + i, y, pal[a].R, pal[a].G, pal[a].B);
      end;
  end;

  //the picture in Bmp2 is in fact 2 pictures- one on the left, one on the right
  //we need to merge them
  Half := Bmp.Width div 2;
  for y:=0 to Bmp.Height-1 do
  for x:=0 to Half-1 do begin
    Bmp[2*x,    y] := Bmp2[x,      y];
    Bmp[2*x + 1,y] := Bmp2[x+Half, y];
  end;

  Bmp2.Free;

  Result := True;
  Reader.Free;
end;


function INP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari INP
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    GG: Byte;
    x,y: Integer;
    i,k: Integer;
    pal: array of Byte;
    palLen: Byte;
    buff,buff2: array[0..7999] of Byte;
    P: TPix;
    S: TPal;
begin
  Reader := TPV_Reader.Create(Str);  //Future: more strict recognition

  if (Str.size<16000) or (Str.size > 17000) then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.AtLeast := 160*200;

  Bmp.SetSize(160, 200);

  Reader.offset := 16000;

  palLen := Reader.size - Reader.offset;
  setLength(pal, palLen);
  Reader.get(pal[0], palLen);

  for i:=0 to palLen-1 do pal[i] := pal[i] - (pal[i] mod 2);

  for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do
      Bmp.SetRGB(x,y, 0,0,0);

 //read 2 images, each is 8000 bytes in size
 //then merge them so NewBmp[x,y] := (Bmp1[x,y] + Bmp2[x,y]) div 2;

 Reader.offset := 0;

 for k:=0 to 1 do begin
    for y:=0 to Bmp.Height-1 do
    for x:=0 to (Bmp.Width div 4)-1 do begin
        gg := Reader.getU;

        for i:=0 to 3 do begin
          a := getBits(gg, 6-2*i, 2);

          P := Bmp[4*x + i, y];
          S := atariPal[pal[a]];

          R := P.R + (S.R div 2);
          G := P.G + (S.G div 2);
          B := P.B + (S.B div 2);

          Bmp.SetRGB(4*x + i, y, R,G,B);
        end;
    end;
  end;

  Result := True;
  Reader.Free;
end;


function MIC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Movie Maker Background
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    pal: array[0..3] of Byte;
   size: Cardinal;
begin
  Bmp.SetSize(160, 192);

  size := Str.size mod 40;

  if (size <> 0) and (size <> 3) and (size <> 4) and (size <> 5) then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  case Str.size mod 40 of
    0,3: begin
           pal[0] := 0;
           pal[1] := 4;
           pal[2] := 8;
           pal[3] := 12;
         end;
    4  : begin
           Reader.offset := Reader.size - 4;
           Reader.get(pal, 4);
         end;
    5  : begin
           Reader.offset := Reader.size - 6;
           Reader.get(pal, 4);
         end;
  end;

  Reader.offset := 0;

  for y:=0 to 191 do
  for x:=0 to 39 do begin
    b := Reader.getU;
    for i:=0 to 3 do begin
      g := getBits(b, 6-(2*i), 2);
      Bmp.Set32(4*x+i, y, atariPal[pal[g]]);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function MIS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari MIS
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    index: Byte;
    buff: array[0..255] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  if (Str.size <> 241) and (Str.size <> 61) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(2, 240);

  index := Reader.getU;

  for i:=0 to 255 do Bmp.AddPal(atariPal[i].R, atariPal[i].G, atariPal[i].B, 255);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do Bmp.SetPal(x,y, 0);


  for y:=0 to (Bmp.Height div 4)-1 do begin
     g := Reader.getU;

    for i:=0 to 3 do begin
      a := getBits(g, 6-2*i, 2);

      if getBits(a, 1, 1) = 1 then Bmp.SetPal(0, 4*y+i, index);
      if getBits(a, 0, 1) = 1 then Bmp.SetPal(1, 4*y+i, index);
    end;
  end;

   Result := True;
  Reader.Free;
end;

function MTV_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//MTV / RayShade Image

var Reader: TPV_Reader;
    width, height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  width  := StrToIntDef(Reader.getLn(' '), 0);
  height := StrToIntDef(Reader.getLn(), 0);
  Reader.getU;

  if (width < 1) or (height < 1) then begin
    Reader.Free;
    Exit(False);
  end;

  if (width > 3000) or (height > 3000) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    r := Reader.getU;
    g := Reader.getU;
    b := Reader.getU;
    Bmp.SetRGB(x, y, r, g, b);
  end;

  Result := True;
  Reader.Free;
end;

function OTB_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Nokia Over The Air

const colors: array[0..1] of Cardinal = ($FF66CC66, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, j: Integer;
    buff, temp: String;
    id, colorss: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  buff := Reader.getS;

  if (copy(buff, 0, 2) = '00') then begin //must be hex file
    temp := StringReplace(buff, ' ', '', [rfReplaceAll]);
    buff := '';
    for i:=1 to system.length(temp) do
      buff := buff + chr(hexInt(copy(temp, i*2-1, 2)));
  end;

  id     := ord(buff[1]);
  width  := ord(buff[2]);
  height := ord(buff[3]);
  colorss:= ord(buff[4]);

  if (id <> 0) and (colorss <> 1) then begin
    Reader.Free;
    Exit(False);
  end;

  if (width>3000) or (height>3000) then begin
      Reader.Free;
      Exit(False);
    end;

  Bmp.SetSize(width, height);

  j := 5;
  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := ord(buff[j]);
    inc(j);
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.Set32(8*x+i, y, colors[g]);
    end;
  end;


  Result := True;
  Reader.Free;
end;

function PGX_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//PGX (JPEG 2000)

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    id: String;
    p: Integer;
    unk, w, h: String;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(6);

  if (id <> 'PG ML ') and (id <> 'PG LM ') then begin
     Reader.Free;
     Exit(False);
   end;

  unk := Reader.getLn(' ');
  w := Reader.getLn(' ');
  h := Reader.getLn;

  p := pos(' ', h);
  if (p > 0) then begin
    w := copy(h, 1, p-1);
    h := copy(h, p+1, 99);
  end;

  Width := StrToInt(w);
  Height := StrToInt(h);

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    g := Reader.getU;

    Bmp.SetRGBA(x, y, g, g, g, 255);
  end;

  Result := True;
  Reader.Free;
end;


function PI2_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Degas/Degas Elite
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    p: Integer;
    pp: array[0..1] of Integer;
    resolution: Integer;
    pal: array[0..15] of Tpix;
begin
  Reader := TPV_Reader.Create(Str);

  if (Str.size < 32000) or (Str.size > 35000) then begin
    Reader.Free;
    Exit(False);
  end;

  resolution := Reader.getMU2;

  for i:=0 to 15 do begin
    p := Reader.getMU2;

    pal[i].R := round(getBits(p, 8, 3)*36.25);
    pal[i].G := round(getBits(p, 4, 3)*36.25);
    pal[i].B := round(getBits(p, 0, 3)*36.25);
  end;

  Bmp.SetSize(640, 200);

  for y:=0 to bmp.height-1 do
  for x:=0 to 39 do  begin
    pp[0] := Reader.getMU2;
    pp[1] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 2*int16
      p := (getBits(pp[1], 15-i, 1) shl 1) +
           (getBits(pp[0], 15-i, 1)     );

      Bmp.SetRGBA(16*x+i, y, pal[p].r, pal[p].g, pal[p].b, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PI3_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Degas/Degas Elite
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    resolution: Integer;
    P: Integer;
    pal: array[0..15] of TPix;
begin
  if (Str.size <> 32034) and (Str.size <> 32066) then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  resolution := Reader.getU2; //MU2

  for i:=0 to 15 do begin
    p := Reader.getMU2;

    pal[i].R := round(getBits(p, 8, 3)*36.25);
    pal[i].G := round(getBits(p, 4, 3)*36.25);
    pal[i].B := round(getBits(p, 0, 3)*36.25);
  end;

  Bmp.SetSize(640, 400);

  for y:=0 to bmp.height-1 do
  for x:=0 to (bmp.width div 8)-1 do
  begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);

      Bmp.SetRGBA(8*x+i, y, pal[g].r, pal[g].g, pal[g].b, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PLA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari PLA
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    index: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  //This format is basically just 1 row from 4PL
  if (Str.size <> 241) then begin
  Reader.Free;
  Exit(False);
  end;

  Bmp.SetSize(8, 240);
  index := Reader.getU;

  for i:=0 to 255 do Bmp.AddPal(atariPal[i].R, atariPal[i].G, atariPal[i].B, 255);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do Bmp.SetPal(x,y, 0);

  for y:=0 to Bmp.Height-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);
        if a=1 then Bmp.SetPal(10*x + i, y, index);
      end;
  end;

   Result := True;
  Reader.Free;
end;


function PS1_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//ST-DEXL Pictures Set

const index: Integer = 0;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, num: Integer;
    pal: array[0..15] of TPix;
    count, c: Integer;
    p: array[0..3] of Integer;
begin
  Reader := TPV_Reader.Create(Str);

  count := Reader.getU2; //number of images

  for i:=0 to count-1 do begin
    num := Reader.getU2;

    if num <> i then begin
      Reader.Free;
      Exit(False);
    end;
  end;

  Reader.offset := 2+count*2 + index*32032;

  for i:=0 to 15 do begin
    c := Reader.getMU2;
    pal[i].r := getBits(c, 8, 3)*36;
    pal[i].g := getBits(c, 4, 3)*36;
    pal[i].b := getBits(c, 0, 3)*36;
  end;

  Bmp.SetSize(320, 200);

  for y:=0 to Bmp.height-1 do
  for x:=0 to 19 do  begin
    p[0] := Reader.getMU2;
    p[1] := Reader.getMU2;
    p[2] := Reader.getMU2;
    p[3] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
      b := (getBits(p[3], 15-i, 1) shl 3) +
           (getBits(p[2], 15-i, 1) shl 2) +
           (getBits(p[1], 15-i, 1) shl 1) +
           (getBits(p[0], 15-i, 1)     );

      Bmp.SetRGBA( 16*x+i, y, pal[b].r, pal[b].g, pal[b].b, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function SCT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//SciTex

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: String;
    bpp: Integer;
    buffR, buffG, buffB: array of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.getS(80);
  id := Reader.getS(2);

  if (id <> 'CT') then begin
    Reader.Free;
    Exit(False);
  end;


  Reader.offset:= Reader.offset + 942+32+1;
  height := strToInt(Reader.getS(11));

  Reader.offset := Reader.offset + 1;
  width := strToInt(Reader.getS(11));
  Reader.offset := Reader.offset + 968;

  Bmp.SetSize(width, height);

  if width*height*3 > Reader.size then bpp := 8
  else                                 bpp := 24;

  if bpp = 8 then begin
    setLength(buffG, width);

    for y:=0 to height-1 do begin
      Reader.get(buffG, width);

      for x:=0 to width-1 do begin
        g := buffG[x];
        Bmp.SetRGBA(x, y, g, g, g, 255);
      end;
    end;
  end
  else begin
    setLength(buffR, width);
    setLength(buffG, width);
    setLength(buffB, width);

    for y:=0 to height-1 do begin
      Reader.get(buffR, width);
      Reader.get(buffG, width);
      Reader.get(buffB, width);

      for x:=0 to width-1 do begin
        r := buffR[x];
        g := buffG[x];
        b := buffB[x];
        Bmp.SetRGBA(x, y, r, g, b, 255);
      end;
    end;
  end;

  Result := True;
  Reader.Free;
end;


function SHC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
//SAMAR Hi-res (Atari)
const colors: array[0..3] of Byte = (115, 87, 73, 45);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    index: Integer;
    buff: array[0..122880-1] of Byte;
begin
  Reader := TPV_Reader.Create(Str);  if Str.size <>  17920 then begin
    Reader.Free;
  Exit(False);
  end;

  for i:=0 to 15360-1 do begin
    g := Reader.getU;
    for j:=0 to 7 do
      buff[i*8 + j] := getBits(g, 7-j, 1);
  end;

  j := 0;
  Bmp.SetSize(320, 192);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin

    a := colors[ buff[j] + 2*buff[61440+j]];
    Bmp.SetRGBA(x, y, a,a,a,255);

    Inc(j);
  end;

  Result := True;
  Reader.Free;
end;


function SHP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Movie Maker Shape
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    pal: array[0..3] of TPal;
begin
  Reader := TPV_Reader.Create(Str);

  if Str.size <> 4384 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(160, 96);

  Str.position := Str.size - 16;

  for i:=0 to 3 do begin
    pal[i] := AtariPal[Reader.getU];
  end;

  Str.position := 528;
  for y:=0 to 95 do
  for x:=0 to 39 do begin
    b := Reader.getU;
    for i:=0 to 3 do begin
      g := getBits(b, 6-(2*i), 2);
      Bmp.SetRGBA(4*x+i, y, pal[g].r, pal[g].g, pal[g].b, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function SKA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Video renting box SKA

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    pal: array[0..255] of TPix;
    p: TPix;
begin
  Reader := TPV_Reader.Create(Str);

  width  := Reader.getU2;
  height := Reader.getU2;
  Reader.getU2;
  Reader.getU2;
  Reader.getI;

  if Str.size <> width*height +768+9 then begin
    Reader.Free;
    Exit(False);
  end;

  for i:=0 to 255 do begin
    pal[i].r := Reader.getU;
    pal[i].g := Reader.getU;
    pal[i].b := Reader.getU;
  end;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
   for x:=0 to width-1 do begin
    p := pal[Reader.getU];
    Bmp.SetRGBA(x, y, p.r, p.g, p.b, 255);
  end;

  Result := True;
  Reader.Free;
end;


function V2I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

  if (id <> '**TI92P*') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.offset := Reader.size - 3090 - 3;

  width := 240;
  height := 103;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.Set32(8*x+i, y, colors[g]);
    end;
  end;

  Result := True;
  Reader.Free;
end;



function PI4_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    Width, Height: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
    p: Word;
    pp: array[0..7] of Word;
    resolution: Integer;
    offset: Integer;
    color: cardinal;
begin
  if (Str.size <> 77824) and (Str.Size <> 154114) then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  case Str.Size of
    77824: height := 240;
    154114: height := 480;
  end;

  if height = 480 then begin
      resolution := Reader.getMU2;

      for i:=0 to 255 do begin
        p := Reader.getMU2;

        R := round(getBits(p, 8, 3)*36.25);
        G := round(getBits(p, 4, 3)*36.25);
        B := round(getBits(p, 0, 3)*36.25);

        Bmp.AddPal(R,G,B,255);
      end;
  end
  else begin

      for i:=0 to 255 do begin
        r := reader.getU;
        g := reader.getU;
        a := reader.getU;
        b := reader.getU;

        Bmp.AddPal(R,G,B,255);
      end;

  end;


  Reader.AtLeast := 320*240*2;

  Bmp.SetSize(320, height);

  Reader.offset := Reader.Size - 320*height;

  for y:=0 to bmp.height-1 do
  for x:=0 to 19 do  begin
    pp[0] := Reader.getMU2;
    pp[1] := Reader.getMU2;
    pp[2] := Reader.getMU2;
    pp[3] := Reader.getMU2;
    pp[4] := Reader.getMU2;
    pp[5] := Reader.getMU2;
    pp[6] := Reader.getMU2;
    pp[7] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
      G := (getBits(pp[7], 15-i, 1) shl 7) +
           (getBits(pp[6], 15-i, 1) shl 6) +
           (getBits(pp[5], 15-i, 1) shl 5) +
           (getBits(pp[4], 15-i, 1) shl 4) +
           (getBits(pp[3], 15-i, 1) shl 3) +
           (getBits(pp[2], 15-i, 1) shl 2) +
           (getBits(pp[1], 15-i, 1) shl 1) +
           (getBits(pp[0], 15-i, 1)     );

      Bmp.SetPal(16*x+i, y, G);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PI7_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    Width, Height: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
    p: Word;
    pp: array[0..7] of Word;
    resolution: Integer;
    offset: Integer;
    color: cardinal;
begin
  if (Str.size <> 308224) then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  case Str.Size of
    308224: height := 480;
  end;

  for i:=0 to 255 do begin
    r := reader.getU;
    g := reader.getU;
    a := reader.getU;
    b := reader.getU;

    Bmp.AddPal(R,G,B,255);
  end;


  Reader.AtLeast := 640*240*2;

  Bmp.SetSize(640, height);

  Reader.offset := Reader.Size - 640*height;

  for y:=0 to bmp.height-1 do
  for x:=0 to 39 do  begin
    pp[0] := Reader.getMU2;
    pp[1] := Reader.getMU2;
    pp[2] := Reader.getMU2;
    pp[3] := Reader.getMU2;
    pp[4] := Reader.getMU2;
    pp[5] := Reader.getMU2;
    pp[6] := Reader.getMU2;
    pp[7] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
      G := (getBits(pp[7], 15-i, 1) shl 7) +
           (getBits(pp[6], 15-i, 1) shl 6) +
           (getBits(pp[5], 15-i, 1) shl 5) +
           (getBits(pp[4], 15-i, 1) shl 4) +
           (getBits(pp[3], 15-i, 1) shl 3) +
           (getBits(pp[2], 15-i, 1) shl 2) +
           (getBits(pp[1], 15-i, 1) shl 1) +
           (getBits(pp[0], 15-i, 1)     );

      Bmp.SetPal(16*x+i, y, G);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PI8_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Mem: TMemoryStream;
    id: String;
    len: Integer;
    Buff: array of Byte;
begin

  if Str.Size <> 7685 then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(320, 192);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
    g := Reader.getU;

    for i:=0 to 7 do begin
      a := getBits(g, 7-i, 1);

      Bmp.SetMono(8*x + i, y, 1-a);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function PI9_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    Width, Height: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
    p: Word;
    pp: array[0..7] of Word;
    resolution: Integer;
    offset: Integer;
    color: cardinal;
begin
  if (Str.size <> 65024) and (Str.size <> 77824) then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  case Str.Size of
    65024: height := 200;
    77824: height := 240;
  end;

  for i:=0 to 255 do begin
    r := reader.getU;
    g := reader.getU;
    a := reader.getU;
    b := reader.getU;

    Bmp.AddPal(R,G,B,255);
  end;


  Reader.AtLeast := 320*240*2;

  Bmp.SetSize(320, height);

  Reader.offset := Reader.Size - 320*height;

  for y:=0 to bmp.height-1 do
  for x:=0 to 19 do  begin
    pp[0] := Reader.getMU2;
    pp[1] := Reader.getMU2;
    pp[2] := Reader.getMU2;
    pp[3] := Reader.getMU2;
    pp[4] := Reader.getMU2;
    pp[5] := Reader.getMU2;
    pp[6] := Reader.getMU2;
    pp[7] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
      G := (getBits(pp[7], 15-i, 1) shl 7) +
           (getBits(pp[6], 15-i, 1) shl 6) +
           (getBits(pp[5], 15-i, 1) shl 5) +
           (getBits(pp[4], 15-i, 1) shl 4) +
           (getBits(pp[3], 15-i, 1) shl 3) +
           (getBits(pp[2], 15-i, 1) shl 2) +
           (getBits(pp[1], 15-i, 1) shl 1) +
           (getBits(pp[0], 15-i, 1)     );

      Bmp.SetPal(16*x+i, y, G);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function PI5_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    Width, Height: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
    p: Word;
    pp: array[0..3] of Word;
    resolution: Integer;
begin
  if Str.size <> 153634 then begin
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Str);

  resolution := Reader.getMU2;

  for i:=0 to 15 do begin
    p := Reader.getMU2;

    R := round(getBits(p, 8, 3)*36.25);
    G := round(getBits(p, 4, 3)*36.25);
    B := round(getBits(p, 0, 3)*36.25);

    Bmp.AddPal(R,G,B,255);
  end;

  Reader.AtLeast := 640*480+100;

  Bmp.SetSize(640, 480);

  for y:=0 to bmp.height-1 do
  for x:=0 to 39 do  begin
    pp[0] := Reader.getMU2;
    pp[1] := Reader.getMU2;
    pp[2] := Reader.getMU2;
    pp[3] := Reader.getMU2;
    for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
      G := (getBits(pp[3], 15-i, 1) shl 3) +
           (getBits(pp[2], 15-i, 1) shl 2) +
           (getBits(pp[1], 15-i, 1) shl 1) +
           (getBits(pp[0], 15-i, 1)     );

      Bmp.SetPal(16*x+i, y, G);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function PGC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Mem: TMemoryStream;
    id: String;
    len: Integer;
    Buff: array of Byte;
begin
  Reader := TPV_Reader.Create(Str);
  id := Reader.getS(3);
  Reader.Free;

  if id <> 'PG'#01 then begin
    Exit(False);
  end;

  Str.Position := 3;
  Mem := TMemoryStream.Create;
  Unrle_PGC(Str, Mem, Str.Size-Str.Position); //TODO: brak funkcji
  Mem.Position := 0;

  Reader := TPV_Reader.Create(Mem);

  Bmp.SetSize(240, 64);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
    g := Reader.getU;

    for i:=0 to 7 do begin
      a := getBits(g, 7-i, 1);

      Bmp.SetMono(8*x + i, y, a);
    end;
  end;

  Result := True;
  Mem.Free;
  Reader.Free;
end;

function PAC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const BuffSize = 30;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Mem: TMemoryStream;
    gzip: TDecompressionStream;
    id: String;
    len: Integer;
    Buff: array of Byte;
    idByte, packByte, specialByte: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(4);

  if id <> 'pM86' then begin
    Reader.Free;
    Exit(False);
  end;

  idByte := Reader.getU;
  packByte := Reader.getU;
  specialByte := Reader.getU;

  Reader.Free;
  str.position := 7;

  Mem := TMemoryStream.Create;
  Unrle_PAC(Str, Mem, idByte, packByte, specialByte);
  Mem.Position := 0;


  Reader := TPV_Reader.Create(Mem);
  Reader.AtLeast := 640*100;

  Bmp.SetSize(640, 400);

  //DIFFERENT ORDER- first WIDTH then HEIGHT
  for x:=0 to (Bmp.width div 8)-1 do
    for y:=0 to Bmp.height-1 do  begin
      b := Reader.GetU;

      for i:=0 to 7 do begin
        g := getBits(b, 7-i, 1);
        Bmp.SetMono(8*x+i, y, g);
      end;
  end;

  Result := True;
  Mem.Free;
  Reader.Free;
end;

function CPR_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Mem: TMemoryStream;
    gzip: TDecompressionStream;
    id: Integer;
    len: Integer;
    Buff: array of Byte;
begin
  Reader := TPV_Reader.Create(Str);
  id := Reader.getU;
  Reader.Free;

  if id <> 2 then begin
    Exit(False);
  end;

  Str.Position := 1;
  Mem := TMemoryStream.Create;
  Unrle_CPR(Str, Mem, Str.Size-Str.Position); //TODO: brak funkcji
  Mem.Position := 0;

  if Mem.Size <> 7680 then begin
    Mem.Free;
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Mem);

  Bmp.SetSize(320, 192);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
    g := Reader.getU;

    for i:=0 to 7 do begin
      a := getBits(g, 7-i, 1);

      Bmp.SetMono(8*x + i, y, a);
    end;
  end;

  Result := True;
  Mem.Free;
  Reader.Free;
end;

function PC1_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A,GG: Byte;
    x,y: Integer;
    i,j, colors: Integer;
    Mem: TMemoryStream;
    resolution: Integer;
    len: Integer;
    Buff: array of Byte;
    p: array[1..4,0..39] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  resolution := Reader.getU2;

  if resolution <> 128 then begin
    Reader.Free;
    Exit(False);
  end;

  for i:=0 to 15 do begin
    gg := Reader.getMU2;
    r := getBits(gg, 8, 3)*36;
    g := getBits(gg, 4, 3)*36;
    b := getBits(gg, 0, 3)*36;

    Bmp.AddPal(r,g,b, 255);
  end;
  Reader.Free;

  Str.Position := 17*2;
  Mem := TMemoryStream.Create;
  Unrle_PSD(Str, Str.Size-Str.Position, Mem);
  Mem.Position := 0;

  Reader := TPV_Reader.Create(Mem);

  Bmp.SetSize(320, 200);

  j := 0;
  for y:=0 to Bmp.height-1 do begin
    Reader.Get(p[1], 40);
    Reader.Get(p[2], 40);
    Reader.Get(p[3], 40);
    Reader.Get(p[4], 40);

    for x:=0 to 39 do
    for i:=0 to 7 do begin

      g := (getBits(p[4][x], 7-i, 1) shl 3) +
           (getBits(p[3][x], 7-i, 1) shl 2) +
           (getBits(p[2][x], 7-i, 1) shl 1) +
           (getBits(p[1][x], 7-i, 1)     );

      Bmp.SetPal(8*x+i, y, g);
    end;
  end;

  Result := True;
  Mem.Free;
  Reader.Free;
end;

function NGF_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const BuffSize = 30;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Mem: TMemoryStream;
    gzip: TDecompressionStream;
    id: String;
    len: Integer;
    Buff: array of Byte;
begin

  Mem := TMemoryStream.Create;
  UnGzip(Str, Mem);
  Mem.Position := 0;

  Reader := TPV_Reader.Create(Mem);

  id := Reader.getS(5);
  Reader.skip(4);

  if (id <> 'NGF'#2#0) then begin
    Reader.Free;
    Mem.Free;
    Exit(False);
  end;

  for i:=0 to 39 do begin
    R := Reader.getU;
    G := Reader.getU;
    B := Reader.getU;

    Bmp.AddPal(R,G,B, 255);
  end;

  //thumb
  Bmp.SetSize(186, 131);

  for y:=0 to 130 do
    for x:=0 to 185 do begin
      b := Reader.getU-200;

      Bmp.SetPal(x,y, b);
  end;

//  if (thumb) then Exit;
  Bmp.ClearPalette;

  for i:=0 to 255 do begin
    R := Reader.getU;
    G := Reader.getU;
    B := Reader.getU;

    Bmp.AddPal(R,G,B, 255);
  end;

  Bmp.SetSize(640, 480);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    b := Reader.getU;

    Bmp.SetPal(x,y, b);
  end;

  Result := True;
  Mem.Free;
  Reader.Free;
end;

function MAC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    G,B: Byte;
    Magic: Cardinal;
    MacBinary: String;
    Padding: String;
    Offset: Integer;
    Mem: TMemoryStream;
    Height, Width: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.Skip(65);
  macBinary := Reader.getS(4);

  if macBinary = 'PNTG' then begin
    Reader.Skip(128);
    offset := 640;
  end
  else offset := 512;

  Reader.Skip(443);

  //image starts at 512 or 640 (if has MacBinary header)

  Width := 576;
  Height := 720;

  Mem := TMemoryStream.Create;

  for i:=0 to Height-1 do
    Unrle_Psd(Str, 72, Mem);

  Reader.Free;
  Mem.Position := 0;
  Reader := TPV_Reader.Create(Mem);

  if (Mem.size < 51840) or (Mem.size > 60000) then begin
    Reader.Free;
    Mem.Free;
    Exit(False);
  end;

  Bmp.SetSize(576, 720);

  Reader.AtLeast := Height * Ceil(Width/8);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Ceil(Bmp.Width/8)-1 do begin
      G := Reader.GetU;

      for i:=0 to 7 do begin
        B := (G shr (7-i)) and 1;

        Bmp.SetMono(8*X + i,Y, B);
      end;
    end;
  end;

  Reader.Free;
  Result := True;

  Mem.Free;
end;

procedure BOB_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);

//Bob Ray Tracer bitmap
var Writer: TPV_Writer;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    j: Integer;
    Bmp2: TPV_Bitmap;
    pal: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.ReduceColors(255, ddFloyd);

  Writer.putU2(Bmp2.Width);
  Writer.putU2(Bmp2.Height);

  for j:=0 to 255 do begin
    if j < Bmp2.PaletteLen then

    else
      Pal := MakePix(255,255,255,255);

    Writer.putU(pal.R);
    Writer.putU(pal.G);
    Writer.putU(pal.B);
  end;

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin
    G := Bmp2.GetPalIndex(x,y);

    Writer.putU(G);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure CM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);

//Puzzle image (X11)
var Writer: TPV_Writer;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
    Bmp2: TPV_Bitmap;
    Pal: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.ReduceColors(255, ddFloyd);

  Writer.putMU4(Bmp2.Width);
  Writer.putMU4(Bmp2.Height);
  Writer.putU(Bmp2.PaletteLen);


  for i:=0 to Bmp2.PaletteLen-1 do begin
    Pal := Bmp2.FPalette[i];

    Writer.putU(Pal.R);
    Writer.putU(Pal.G);
    Writer.putU(Pal.B);
  end;


  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin
    G := Bmp2.GetPalIndex(x,y);

    Writer.putU(G);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure DAP_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    pal: array[0..255] of TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(320, 240);
  Bmp2.ReduceColors(255, ddFloyd);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to Bmp2.width-1 do begin
    G := Bmp2.GetPalIndex(x,y);

    Writer.putU(G);
  end;

  //copy palette
  for i:=0 to Bmp2.PaletteLen-1 do
    pal[i] := Bmp2.FPalette[i];
  for i:=Bmp2.PaletteLen-1 to 255 do
    pal[i] := MakePix(0,0,0,0);

  for i:=0 to 255 do
    Writer.putU(pal[i].R);
  for i:=0 to 255 do
    Writer.putU(pal[i].G);
  for i:=0 to 255 do
    Writer.putU(pal[i].B);


  Bmp2.Free;
  Writer.Free;
end;

procedure MBG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
//Mad Designer
var Writer: TPV_Writer;
    i,j: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(512, 256);
  Bmp2.BlackWhite(ddFloyd);


  for y:=0 to 255 do
  for x:=0 to 63 do  begin
    g := 0;

    for j:=0 to 7 do begin
      b := Bmp2[8*x+j,y].R and 1;

      G := G + (B shl (7-j));
    end;

    Writer.PutU(not G);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure CHS_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
    B,G: Byte;
    i,j,k: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(256, 24);
  Bmp2.BlackWhite(ddFloyd);

  Writer.Skip(25);

  for y:=0 to 2 do
  for x:=0 to 31 do
  for j:=0 to 7 do begin
    G := 0;

    for i:=0 to 7 do begin
      B := Bmp2[8*x+i, 8*y+j].R and 1;

      G := G + (B shl (7-i));
    end;

    Writer.PutU(G);
  end;

  Writer.PutU(0);

  Bmp2.Free;
  Writer.Free;
end;

procedure A64C_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
    B,G: Byte;
    i,j,k: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(256, 24);
  Bmp2.BlackWhite(ddFloyd);

  Writer.Skip(2);

  for y:=0 to 2 do
  for x:=0 to 31 do
  for j:=0 to 7 do begin
    G := 0;

    for i:=0 to 7 do begin
      B := Bmp2[8*x+i, 8*y+j].R and 1;

      G := G + (B shl (7-i));
    end;

    Writer.PutU(G);
  end;

  Bmp2.Free;
  Writer.Free;
end;


procedure TIM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);

//TIM

var Writer: TPV_Writer;
    R,G,B,A: Byte;
    x,y: Integer;
    c: Word;
    P: TPix;
    id, version, empty, flags, size, x1,y1: Cardinal;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putU4(16); //id
  Writer.putU4(2);  //9=palette,2=16 bit RGB
  Writer.putU4(12); //offset
  Writer.putU2(0);
  Writer.putU2(0);
  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);


  for y:=0 to Bmp.height-1 do
  for x:=0 to Bmp.width-1 do begin
    P := Bmp[x,y];

    R := P.R shr 3;
    G := P.G shr 3;
    B := P.B shr 3;

    C := R + (G shl 5) + (B shl 10);

    Writer.putU2(C);
  end;

  Writer.Free;
end;


procedure SKP_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    B: Word;
    G: Byte;
    i,j: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(160, 192);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to (Bmp2.width div 8)-1 do begin
    b := 0;
    for i:=0 to 7 do begin
      g := Bmp2[8*x+i, y].R and 1;
      B := B + (G shl (14-2*i));
      B := B + (G shl (15-2*i));
    end;

    Writer.PutMU2(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;


procedure MBM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
//Psion Series 5 Bitmap
var Writer: TPV_Writer;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    P: TPix;
    id, typee, checkSum: String;
    appId: Integer;
    sizee, offset, xSize, ySize, bpp, type2, encoding: Integer;
begin

  Writer := TPV_Writer.Create(Str);

  Writer.putU4(268435511);
  Writer.putU4(268435522);
  Writer.putU4(0);
  Writer.putU4(1194943545);
  Writer.putU4(20+40 + Bmp.Width*Bmp.Height);


  Writer.putU4(40 + Bmp.width*Bmp.Height); //size - 28 bytes
  Writer.putU4(40);
  Writer.putU4(Bmp.Width);
  Writer.putU4(Bmp.Height);
  Writer.putU4(0);
  Writer.putU4(0);
  Writer.putU4(8);  //bpp
  Writer.putU4(0);  //colors=yes
  Writer.putU4(0);  //paletteLen
  Writer.putU4(0);  //packed=no

  for y:=0 to Bmp.height-1 do
   for x:=0 to Bmp.width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
  end;

  Writer.putU4(1); //num of pictures
  Writer.putU4($14);

  Writer.Free;
end;


function PCL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//PCL
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    temp1, temp2: String;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.getS(1);
  Reader.getLn(chr(27)); //E
  Reader.getLn(chr(27)); //&100
  Reader.getLn(chr(27)); //&10E
  Reader.getLn(chr(27)); //&12A
  Reader.getLn(chr(27)); //*p0x0Y
  Reader.getLn(chr(27)); //*t72R
  Reader.getLn('r');     //*r128s64T
  temp1 := Reader.getLn('s');
  temp2 := Reader.getLn('T');
  Reader.getLn(chr(27));
  Reader.getLn(chr(27)); //*t128h64V
  Reader.getLn(chr(27)); //*v6W......
  Reader.getLn(chr(27)); //*r2A
  Reader.getLn(chr(27)); //*b0M
  Reader.getLn('W');    //b384W

  width := strToInt(temp1);
  height := strToInt(temp2);

  Bmp.SetSize(Width, Height);

  if (width > 3000) or (height > 3000) then begin
    Reader.Free;
    Exit(False);
  end;

  for y:=Bmp.Height-1 downto 0 do begin
    for x:=0 to Bmp.Width-1 do begin
      r := Reader.getU;
      g := Reader.getU;
      b := Reader.getU;
      Bmp.SetRGBA(x, y, r, g, b, 255);
    end;
    Reader.getS(7);
  end;

  Result := True;
  Reader.Free;
end;

procedure BG9_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    j: Integer;
    B,C: Byte;
    Bmp2: TPV_Bitmap;
    G1, G2: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(160, 192);
  Bmp2.Grayscale(15, ddFloyd);


  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Ceil(Bmp2.Width/4)-1 do begin
    G1 := Bmp2[2*x   ,y].R shr 4;
    G2 := Bmp2[2*x +1,y].R shr 4;

    C := G2 + (G1 shl 4);

    Writer.PutU(C);
  end;

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Ceil(Bmp2.Width/4)-1 do begin
    G1 := Bmp2[80 + 2*x   ,y].R shr 4;
    G2 := Bmp2[80 + 2*x +1,y].R shr 4;

    C := G2 + (G1 shl 4);

    Writer.PutU(C);
  end;

  Bmp2.Free;
  Writer.Free;
end;


procedure MSP_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var x,y: Integer;
    j: Integer;
    Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    G,B: Byte;
    Checksum: Word;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.BlackWhite(ddFloyd);

  Writer.PutU4($4D6E6144); //v1

  Writer.PutU2(Bmp2.Width);
  Writer.PutU2(Bmp2.Height);
  Writer.PutU2(1);
  Writer.PutU2(1);
  Writer.PutU2(1);
  Writer.PutU2(1);
  Writer.PutU2(0);
  Writer.PutU2(0);
  Writer.PutU2(0);
  Writer.PutU2(0);

  Checksum := $4D6E xor $6144 xor Bmp2.Width xor Bmp2.Height xor 1 xor 1 xor 1 xor 1;

  Writer.PutU2(Checksum); // checksum of previous 24 bytes
  Writer.PutS(#0#0#0#0#0#0);


  for y:=0 to Bmp2.Height-1 do
    for x:=0 to Ceil(Bmp2.Width/8)-1 do begin
      B := 0;

      for j:=0 to 7 do begin
        G := Bmp2[8*x + j,y].R and 1;
        B := B + (G shl (7-j));
      end;

      Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure Unrle_Msp(Reader: TPV_Reader; PackedLen: Integer; Str: TStream);
var k,j: Integer;
    RunType, RunValue: Byte;
    RunCount: Integer;
begin
  k := 0;
  while k < PackedLen do begin
    RunType := Reader.GetU;
    Inc(k, 1);

    If RunType = 0 then begin
      RunCount := Reader.GetU;
      RunValue := Reader.GetU;

      for j:=0 to RunCount-1 do Str.Write(RunValue, 1);
      Inc(k, 2);
    end
    else begin
      RunCount := RunType;

      for j:=0 to RunCount-1 do Str.Write(Reader.GetU, 1);
      Inc(k, RunCount);
    end;
  end;
end;

function MSP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    G,B: Byte;
    Magic: Cardinal;
    Width, Height, XAspect, YAspect, XAspectPrint, YAspectPrint,
    PrinterWidth, PrinterHeight, XAspectCorrection, YAspectCorrection, Checksum: Integer;
    Padding: String;
    PackedLen: array of Word;
    Mem: TMemoryStream;
begin
  Reader := TPV_Reader.Create(Str);

  Magic := Reader.GetU4;

  if (Magic <> $4D6E6144) and (Magic <> $536E694C) then begin
    Reader.Free;
    Exit(False);
  end;

  Width := Reader.GetU2;
  Height := Reader.GetU2;
  XAspect := Reader.GetU2;
  YAspect := Reader.GetU2;
  XAspectPrint := Reader.GetU2;
  YAspectPrint := Reader.GetU2;
  PrinterWidth := Reader.GetU2;
  PrinterHeight := Reader.GetU2;
  XAspectCorrection := Reader.GetU2; //unused
  YAspectCorrection := Reader.GetU2; //unused
  Checksum := Reader.GetU2; // of previous 24 bytes   */
  Padding := Reader.GetS(6); //Unused padding

  Bmp.SetSize(Width, Height);

  if Magic = $536E694C then begin //RLE
    SetLength(PackedLen, Height);

    for i:=0 to Height-1 do
      PackedLen[i] := Reader.GetU2;

    Mem := TMemoryStream.Create;

    for i:=0 to Height-1 do
      Unrle_Msp(Reader, PackedLen[i], Mem);

    Reader.Free;
    Mem.Position := 0;
    Reader := TPV_Reader.Create(Mem);
  end;


  Reader.AtLeast := Height * Ceil(Width/8);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Ceil(Bmp.Width/8)-1 do begin
      G := Reader.GetU;

      for i:=0 to 7 do begin
        B := (G shr (7-i)) and 1;

        Bmp.SetMono(8*X + i,Y, 1-B);
      end;
    end;
  end;

  Reader.Free;
  Result := True;

  if Magic = $536E694C then Mem.Free;
end;

procedure CEL_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    i: Integer;
    Bmp2: TPV_Bitmap;
    Id, Unk1, Unk2, Bpp: Integer;
    R,G,B: Byte;
    Pal: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putU2(37145);
  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);
  Writer.putU2(0); //x offset
  Writer.putU2(0); //y offset
  Writer.putU2(8);
  Writer.PutU4(Bmp.Width*Bmp.Height + 32 + 256*3); //file size
  Writer.PutS(#0#0#0#0);
  Writer.PutS(#0#0#0#0);
  Writer.PutS(#0#0#0#0);
  Writer.PutS(#0#0#0#0);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.ReduceColors(255, ddFloyd);

  for i:=0 to 255 do begin

    if i < Bmp2.PaletteLen then
      Pal := Bmp2.FPalette[i]
    else
      Pal := MakePix(0,0,0,255);

    Writer.putU(Pal.R shr 2);
    Writer.putU(Pal.G shr 2);
    Writer.putU(Pal.B shr 2);
  end;

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin

    G := Bmp2.GetPalIndex(x,y);
    Writer.putU(G);
  end;

  Bmp2.Free;
  Writer.Free;
end;


function GFB_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari GFB
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    index: Integer;
    magic: String;
begin
  Reader := TPV_Reader.Create(Str);

  //might also be 320x200x 4bit, size the same-32 kB or even 64kB with 320x200x8bit

  magic := Reader.getS(4);

  if magic <> 'GF25' then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(640, 400);
  Reader.Skip(20);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);
        Bmp.SetMono(8*x + i, y, a);
      end;
  end;

  Result := True;
  Reader.Free;
end;

procedure PIC_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
    Unk1, Unk3: Integer;
    Unk2: String;
    Id: Integer;
    G: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Grayscale(255, ddNone);

  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);
  Writer.putU2(0);
  Writer.putS(#0#0#0#0);
  Writer.putS(#0#0#0#0);
  Writer.putU2(0);
  Writer.Skip(38);

  Writer.putU2(12345);
  Writer.Skip(20);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin
    G := Bmp2[x,y].R;

    Writer.putU(G);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure KRO_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
//Autopano KOLOR Raw
var Writer: TPV_Writer;
    x,y: Integer;
    P: TPix;
    Head: String;
    One, Empty0, Empty1, Unk0, Unk1, Empty2, Empty3, BitsPerPlane, BytesPerPixel: Integer;
    R,G,B,A: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putS('KRO');
  Writer.putU(1); //version

  Writer.putMU4(Bmp.Width);
  Writer.putMU4(Bmp.Height);
  Writer.putMU4(8); //bpp
  Writer.putMU4(4); //Bytes per pixel


  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
    Writer.putU(P.A);
  end;

  Writer.Free;
end;

procedure SGI_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var x,y: Integer;
    i: Integer;
    Writer: TPV_Writer;
    R,G,B,A: Byte;
    Magic, Width, Height,Bpp,Dimension,Channels,PixMin,PixMax,Ignored,PalType: Integer;
    FName: String;
    Pix: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putMU2(474); //ID=474
  Writer.putU(0);   //0=unpacked, 1=RLE
  Writer.putU(2);   //Bytes per pixel, 1 or 2
  Writer.putMU2(2); //dimension, 2 or 1 should be fine
  Writer.putMU2(Bmp.Width);
  Writer.putMU2(Bmp.Height);
  Writer.putMU2(4); //channels
  Writer.putMU4(0); //Pixmin
  Writer.putMU4(0); //Pixmax
  Writer.putMU4(0);
  Writer.putS('Filename.sgi');
  Writer.Skip(80-12);
  Writer.putMU4(0); //0=normal

  Writer.Skip(404) ;//zeroes


  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do
      Writer.PutMU2(Bmp[x,y].B shl 8);

  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do
      Writer.PutMU2(Bmp[x,y].G shl 8);

  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do
      Writer.PutMU2(Bmp[x,y].R shl 8);

  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do
      Writer.PutMU2(Bmp[x,y].A shl 8);

  Writer.Free;
end;

procedure WBMP_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var x,y: Integer;
    j: Integer;
    Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    G,B: Byte;
    Typ,Fix,Extender: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.BlackWhite(ddFloyd);

  Writer.putV(0); //image type; 0=mono
  Writer.putU(1);

  Writer.putV(Bmp2.Width);
  Writer.putV(Bmp2.Height);

  for y:=0 to Bmp2.Height-1 do
    for x:=0 to Ceil(Bmp2.Width/8)-1 do begin
      B := 0;

      for j:=0 to 7 do begin
        G := Bmp2[8*x + j,y].R and 1;
        B := B + (G shl (7-j));
      end;

      Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure NLM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var x,y: Integer;
    B,G: Byte;
    j: Integer;
    Writer: TPV_Writer;
    Id: String;
    Bmp2: TPV_Bitmap;
    Unk0, Unk1, Unk2: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(255, 255);
  Bmp2.BlackWhite(ddFloyd);

  Writer.putS('NLM '); //id
  Writer.putU(0); //TODO: unsure
  Writer.putU2(0); //TODO: unsure
  Writer.putU(Bmp2.Width);
  Writer.putU(Bmp2.Height);
  Writer.putU(0); //TODO: unsure


  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Ceil(Bmp2.Width/8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(not B);
  end;

  Bmp2.Free;
  Writer.Free;
end;


function GP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Atari GP
const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    index: Integer;
    id: Byte;
    pal: array[0..8] of Byte;
begin
  Reader := TPV_Reader.Create(Str);
  id := Reader.getU;

  if (id <> 8) and (id <> 9) and (id <> 10) and (id <> 15) then begin
    Reader.Free;
    Exit(False);
  end;

  case id of
    8 : begin
          //Str.position := 10;
          Reader.Skip(9);

          Bmp.SetSize(320, 192);
          for y:=0 to Bmp.Height-1 do
          for x:=0 to Ceil(Bmp.Width/ 8)-1 do begin
              g := Reader.getU;

              for i:=0 to 7 do begin
                a := getBits(g, 7-i, 1);
                Bmp.Set32(8*x + i, y, colors[a]);
              end;
          end;
        end;
    9 : begin
          //palette here, perhaps
          //Str.position := 10;
          Reader.Skip(9);

          Bmp.SetSize(80, 192);
          for y:=0 to Bmp.Height-1 do
          for x:=0 to (Bmp.Width div 2)-1 do begin
              g := Reader.getU;

              for i:=0 to 1 do begin
                a := getBits(g, 4-4*i, 4);
                Bmp.Set32(2*x + i, y, atariPal[a]);
              end;
          end;
        end;
    10 : begin
          //palette here, perhaps
          //Str.position := 10;
          Reader.Skip(9);

          Bmp.SetSize(160, 192);
          for y:=0 to Bmp.Height-1 do
          for x:=0 to (Bmp.Width div 4)-1 do begin
              g := Reader.getU;

              for i:=0 to 3 do begin
                a := getBits(g, 6-2*i, 2);
                Bmp.Set32(4*x + i, y, atariPal[a]);
              end;
          end;
        end;
    15 : begin
          //palette here, perhaps
          //Str.position := 10;
          Reader.Skip(9);

          Bmp.SetSize(160, 192);
          for y:=0 to Bmp.Height-1 do
          for x:=0 to (Bmp.Width div 4)-1 do begin
              g := Reader.getU;

              for i:=0 to 3 do begin
                a := getBits(g, 6-2*i, 2);
                Bmp.Set32(4*x + i, y, atariPal[a]);
              end;
          end;
        end;
  end;

  Result := True;
  Reader.Free;
end;

procedure AAI_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    P: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putU4(Bmp.Width);
  Writer.putU4(Bmp.Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.B);
    Writer.putU(P.G);
    Writer.putU(P.R);
    Writer.putU(0);
  end;

  Writer.Free;
end;

procedure GHG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    j: Integer;
    Bmp2: TPV_Bitmap;
    G,C,B: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(320, 200); //max: 320x200
  Bmp2.BlackWhite(ddFloyd);

  Writer.putU2(Bmp2.Width-2);
  Writer.putU(Bmp2.Height);

  for y:=0 to Bmp2.Height-1 do
    for x:=0 to Ceil(Bmp2.Width/8)-1 do begin
      B := 0;

      for j:=0 to 7 do begin
        G := Bmp2[8*x + j,y].R and 1;
        B := B + (G shl (7-j));
      end;

      Writer.PutU(not B);
    end;

  Bmp2.Free;
  Writer.Free;
end;

procedure GR9_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
    j: Integer;
    A,B,C: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(80, 192);
  Bmp2.Grayscale(15, ddFloyd);


  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Ceil(Bmp2.Width/2)-1 do begin
    A := Bmp2[2*x  , y].R shr 4;
    B := Bmp2[2*x+1, y].R shr 4;

    C := B + (A shl 4);

    Writer.PutU(C);
  end;

  Bmp2.free;
  Writer.Free;
end;

procedure AVS_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    P: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putMU4(Bmp.Width);
  Writer.putMU4(Bmp.Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    Writer.putU(not P.A);
    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
  end;

  Writer.Free;
end;

procedure RWL_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(640, 400);
  Bmp2.Grayscale(255, ddNone);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to Bmp2.width-1 do begin
    Writer.putU(255-Bmp2[x,y].R);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure PAT_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    P: TPix;
    Name: String;
    R,G,B: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Name := 'Pattern';

  Writer.putMU4(24 + Length(Name));
  Writer.putMU4(1); //version
  Writer.putMU4(Bmp.Width);
  Writer.putMU4(Bmp.Height);
  Writer.putMU4(4); //1=gray, 2=G+A, 3=RGB, 4=RGBA
  Writer.putS('GPAT');
  Writer.putS(Name+#0);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
  end;

  Writer.Free;
end;

procedure DRG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    j: Integer;
    x,y: Integer;
    B,G: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(320, 160);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to (Bmp2.Width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure VOLLABEL_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    G: Byte;
    id: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Grayscale(255, ddNone);

  Writer.putU(1); //=1
  Writer.putMU2(Bmp2.Width);
  Writer.putMU2(Bmp2.Height);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin
    Writer.putU(Bmp2[x,y].R);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure XGA_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    P: TPix;
    c: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(384, 480);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to Bmp2.Width-1 do begin
    P := Bmp2[x,y];

    B := P.B shr 3;
    G := P.G shr 2;
    R := P.R shr 3;

    C := B + (G shl 5) + (R shl 11);

    Writer.putMU2(C);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure KFX_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(56, 60);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to (Bmp2.width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure DOO_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    buff: array of Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(640, 400);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to (Bmp2.Width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(not B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure DA4_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(640, 800);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to (Bmp2.width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(not B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure B_W_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Grayscale(255, ddNone);

  Writer.putS('B&W256');
  Writer.putMU2(Bmp2.Width);
  Writer.putMU2(Bmp2.Height);


  for y:=0 to Bmp2.height-1 do
  for x:=0 to Bmp2.width-1 do begin
    Writer.PutU(Bmp2[x,y].R);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure VST_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    P: TPix;
    R,G,B,A: Byte;
    x,y: Integer;
    bpp: Integer;
    id: String;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putS(#0#0#0#0);
  Writer.putS(#0#0#0#0);
  Writer.putS(#0#0#0#0);
  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);
  Writer.putU(24); //bpp
  Writer.putS(#0#0#0);
  Writer.putS('IGCH');
  Writer.putS(#0#0#0#0);
  Writer.putS(#0#0#0#0);
  Writer.putS(#0#0#0#0);

  for y:=Bmp.height-1 downto 0 do
  for x:=0 to Bmp.width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
  end;

  Writer.Free;
end;

procedure VBM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    R,G,B,A: Byte;
    x,y: Integer;
    j: Integer;
    id: String;
    version: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(640, 400);
  Bmp2.BlackWhite(ddFloyd);

  Writer.putS('BM'#$CB);
  Writer.putU(3); //version, 2 or 3

  Writer.putMU2(Bmp2.Width);
  Writer.putMU2(Bmp2.Height);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to (Bmp2.Width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;


procedure PSA_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    i,j: Integer;
    R,G,B,A: Byte;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putMU2(0); //TODO: check

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(88, 52);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to bmp2.height-1 do
  for x:=0 to (bmp2.width div 8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(not B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure PGF_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    i,j: Integer;
    R,G,B,A: Byte;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(240, 64);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to (Bmp2.Width div 8)-1 do begin
      B := 0;

      for j:=0 to 7 do begin
        G := Bmp2[8*x + j,y].R and 1;
        B := B + (G shl (7-j));
      end;

      Writer.PutU(not B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure NGG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    G: Byte;
    x,y: Integer;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.BlackWhite(ddFloyd);

  Writer.putS('NGG'+#0);

  Writer.putU2(1);
  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);
  Writer.putU2(1);
  Writer.putU2(1);
  Writer.putU2(88);

  for y:=0 to Bmp2.height-1 do
  for x:=0 to Bmp2.width-1 do begin
    G := Bmp2[x,y].R and 1;
    G := 1 - G;

    Writer.putS(IntToStr(G));
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure NPM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    G,B: Byte;
    x,y: Integer;
    j: Integer;
    comment: String;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(72, 28);
  Bmp2.BlackWhite(ddFloyd);

  Writer.putS('NPM');
  Writer.putU(0);

  comment := 'Lazzy Image Viewer';

  Writer.PutU(Length(Comment));
  Writer.putS(Comment);

  Writer.putU(0);

  Writer.putU(Bmp2.Width);
  Writer.putU(Bmp2.Height);

  Writer.putU(1);
  Writer.putU(1);
  Writer.putU(89);

  for y:=0 to Bmp2.Height-1 do
  for x:=0 to ceil(Bmp2.Width/8)-1 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x + j,y].R and 1;
      B := B + (G shl j);
    end;

    Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure IPI_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    P: TPix;
    x,y: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putMU2(Bmp.Width);
  Writer.putMU2(Bmp.Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
  end;

  Writer.Free;
end;

procedure GR8_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    Bmp2: TPV_Bitmap;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(320, 192);
  Bmp2.BlackWhite(ddFloyd);

  for y:=0 to 191 do
  for x:=0 to 39 do begin
    B := 0;

    for j:=0 to 7 do begin
      G := Bmp2[8*x+j,y].R and 1;
      B := B + (G shl (7-j));
    end;

    Writer.PutU(B);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure IIM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    P: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putS('IS_IMAGE');

  Writer.putMU2(5); //type
  Writer.putMU2(72); //dpi

  Writer.putMU2(Bmp.Width);
  Writer.putMU2(Bmp.Height);

  for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      P := Bmp[x,y];

      Writer.putU(P.A);
      Writer.putU(P.R);
      Writer.putU(P.G);
      Writer.putU(P.B);
    end;

  Writer.Free;
end;

procedure GBR_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    P: TPix;
    x,y: Integer;
    BrushName: String;
begin
  Writer := TPV_Writer.Create(Str);

  BrushName := 'Brush'#0;

  Writer.putMU4(28 + Length(BrushName));
  Writer.putMU4(2); //version
  Writer.putMU4(Bmp.Width);
  Writer.putMU4(Bmp.Height);
  Writer.putMU4(4); //4=RGBA, 1=gray
  Writer.putS('GIMP');
  Writer.putMU4(100); //spacing
  Writer.putS(BrushName);

  for y:=0 to Bmp.height-1 do
   for x:=0 to Bmp.width-1 do begin
    P := Bmp[x,y];

    Writer.putU(P.R);
    Writer.putU(P.G);
    Writer.putU(P.B);
    Writer.putU(P.A);
  end;

  Writer.Free;
end;

procedure DIS_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);
  Writer.putU2(0);

  for y:=0 to Bmp.height-1 do begin
    for x:=0 to Bmp.width-1 do
      Writer.PutU(Bmp[x,y].R);

    for x:=0 to Bmp.width-1 do
      Writer.PutU(Bmp[x,y].G);

    for x:=0 to Bmp.width-1 do
      Writer.PutU(Bmp[x,y].B);

    if (y <> Bmp.height-1) then
      Writer.PutU2(0);
  end;

  Writer.Free;
end;

procedure CSV_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    G: Byte;
    Line: String;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Grayscale(255, ddNone);

  for y:=0 to bmp2.height-1 do begin
    Line := '';

    for x:=0 to bmp2.width-1 do begin
      G := Bmp2[x,y].G;

      Line := Line + IntToStr(G) + ',';
    end;

    Line := Copy(Line, 1, Length(Line)-1); //cut , from end
    Writer.PutS(Line + #13#10);
  end;

  Bmp2.Free;
  Writer.Free;
end;

procedure A565_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    R,G,B,A: Byte;
    x,y: Integer;
    c: Word;
    P: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.putS('C565');
  Writer.putU2(Bmp.Width);
  Writer.putU2(Bmp.Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    P := Bmp[x,y];

    B := P.B shr 3;
    G := P.G shr 2;
    R := P.R shr 3;

    C := B + (G shl 5) + (R shl 11);

    Writer.putU2(C);
  end;

  Writer.Free;
end;

function DD_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 9218) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function HIR_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 8002) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  FillChar(Mem.ScreenRAM, 1000, 1);
  Mem.ScreenRAM[0] := 0;

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function FGS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 8002) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  FillChar(Mem.ScreenRAM, 1000, 1);
  Mem.ScreenRAM[0] := 0;

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function GCD_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size < 8002) or (Str.Size > 10000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  FillChar(Mem.ScreenRAM, 1000, 1);
  Mem.ScreenRAM[0] := 0;

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function GIH_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 8002) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  FillChar(Mem.ScreenRAM, 1000, 1);
  Mem.ScreenRAM[0] := 0;

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function HED_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 9218) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function ISH_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 9194) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function MON_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size < 8000) or (Str.Size > 12000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  FillChar(Mem.ScreenRAM, 1000, 1);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  Mem.ScreenRAM[0] := 0;

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function IPH_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
begin
  if (Str.Size <> 9002) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function A64_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10242) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $27FF + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $2000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2400 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function CDU_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10277) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2821 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $2051 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2439 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0111 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function CHE_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size < 19432) or (Str.size > 50000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $4BE8 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $4200 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $4800 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function CWG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10007) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2711 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function DOL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10242) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $07E8 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $400 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0800 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function DRZ_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10051) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2740 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0800 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function FPT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10004) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2712 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function GIG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10003) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2710 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function IPT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10003) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2710 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function ISM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10218) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $27E7 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $2400 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function MIL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10022) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $07E4 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $0014 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $03FC + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $07E4 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function OCP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10018) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2329 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2338 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function P64_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10050) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $07FF + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0800 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function PI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10242) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $1F80 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $2000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2400 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function PMG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
    val: Byte;
begin
  if (Str.Size < 9000) or (Str.Size > 12000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $1FB2 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $2072 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $1FB5 + 2;
  val := Reader.getU;
  FillChar(Mem.ColorRAM, 1000, val);

  Reader.Offset := $0072 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function RP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size < 9000) or (Str.Size > 12000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $23FF + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2400 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;


function SXS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//SXS font
//TODO: ugly implementation

const colors: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    buff: array[0..1023] of Byte;
    aa,bb,cc,dd,ee: Integer;
    offset: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  if Str.size <> 1030 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(256, 32);

  Reader.getS(6);

  fillChar(buff, 1024, 0);
  for i:=0 to 1023 do begin
    aa := getBits(i, 9, 1) shl 9;
    bb := getBits(i, 4, 1) shl 8;
    cc := getBits(i, 0, 3) shl 5;
    dd := getBits(i, 5, 4) shl 1;
    ee := getBits(i, 3, 1);

    offset := aa + bb + cc + dd + ee;
    buff[offset] := Reader.getU;
  end;

  i := 0;
  for y:=0 to Bmp.height-1 do
  for x:=0 to (Bmp.width div 8)-1 do begin
    bb := buff[i];
    inc(i);
    for j:=0 to 7 do begin
      g := getBits(bb, 7-j, 1);

      Bmp.Set32(8*x+j, y, colors[g]);
    end;
  end;

  Result := True;
  Reader.Free;
end;

function RPM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
 var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10006) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $2710 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function SAR_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size < 10000) or (Str.Size > 12000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := $03F0 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2400 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.Bitmap, 8000);

  //uncertain fix
  FillChar(Mem.ColorRAM, 1000, 0);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function KOA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
begin
  if (Str.Size <> 10003) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.Offset := 10002;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := 8002;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := 9002;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Reader.Free;
  Result := True;
end;

function JJ_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    bgColor: Byte;
    Mem: TC64Mem;
    Memo: TMemoryStream;
begin
  Memo := TMemoryStream.Create;
  Unrle_GG(Str, Memo, Str.Size);
  Memo.Position := 0;

  if (Memo.Size < 8000) or (Memo.Size > 10000) then begin
    Memo.Free;
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Memo);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $0400 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeHires(Bmp, Mem);

  Memo.Free;
  Reader.Free;
  Result := True;
end;

function GG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
    Memo: TMemoryStream;
begin
  Memo := TMemoryStream.Create;
  Unrle_GG(Str, Memo, Str.Size);
  Memo.Position := 0;

  if (Memo.Size < 10003) or (Memo.Size > 11000) then begin
    Memo.Free;
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Memo);

  Reader.Offset := $2710 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Memo.Free;
  Reader.Free;
  Result := True;
end;

function AMI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Mem: TC64Mem;
    Memo: TMemoryStream;
begin
  Memo := TMemoryStream.Create;
  Unrle_AMI(Str, Memo, Str.Size);
  Memo.Position := 0;

  if (Memo.Size <> 10259) then begin
    Memo.Free;
    Exit(False);
  end;

  Reader := TPV_Reader.Create(Memo);

  Reader.Offset := $2710 + 2;
  Mem.BgColor := Reader.GetU;

  Reader.Offset := $1F40 + 2;
  Reader.get(Mem.ScreenRAM, 1000);

  Reader.Offset := $2328 + 2;
  Reader.get(Mem.ColorRAM, 1000);

  Reader.Offset := $0000 + 2;
  Reader.get(Mem.Bitmap, 8000);

  DecodeMulticolor(Bmp, Mem);

  Memo.Free;
  Reader.Free;
  Result := True;
end;

function A565_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//OLPC 565

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
id: String;
    c: Word;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(4);

  if (id <> 'C565') then begin
    Reader.Free;
    Exit(False);
  end;

  width := Reader.getU2;
  height := Reader.getU2;


  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    c := Reader.getU2;

    b := 8* getBits(c, 0, 5);
    g := 4* getBits(c, 5, 6);
    r := 8* getBits(c, 11, 5);

    Bmp.SetRGB(x,y, r,g,b);
  end;

  Result := True;
  Reader.Free;
end;


function A73I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Ti-73

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

  if (id <> '**TI73**') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := Str.size - 756 - 2;

  width := 96;
  height := 63;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;



function A82I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Ti-82



const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

 if (id <> '**TI82**') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := str.size - 756 - 2;

  width := 96;
  height := 63;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function A83I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Ti-83 [83i]
//TI-83+ [8xi]



const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

  if (id <> '**TI83**') and (id <> '**TI83F*') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := str.size - 756 - 2;

  width := 96;
  height := 63;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function A85I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//T-85

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

   if (id <> '**TI85**') then begin
    Reader.Free;
    Exit(False);
  end;

 Reader.Offset := str.size - 1008 - 2;
//  f.position := 92;

  width := 128;
  height := 63;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;




function A86I_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Ti-86

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(8);

  if (id <> '**TI86**') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := str.size - 1008 - 2;

  width := 128;
  height := 63;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;





function BKG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const pal: array[0..3] of TPal = ( (r:45;  g:45;  b:45),
                                    (r:126; g:107; b:0),
                                    (r:241; g:241; b:241),
                                    (r:192; g:94;  b:140)  );
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    p: TPix;
begin
  if Str.size <> 3856  then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.AddPalette(pal, 4);

  Bmp.SetSize(160, 96);

  for y:=0 to 95 do
  for x:=0 to 39 do begin
    b := Reader.getU;
    for i:=0 to 3 do begin
      // p := pal[getBits(b, 6-(2*i), 2)];
      //Bmp.SetRGB(4*x+i, y, p.r, p.g, p.b);
      G := getBits(b, 6-(2*i), 2);
      Bmp.SetPal(4*x+i, y, G);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function BOB_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    j: Integer;
    pal: array[0..255] of TPix;
begin
  Reader := TPV_Reader.Create(Str);

  Width := Reader.getU2;
  Height := Reader.getU2;

  if Str.size <> width*height+768+4 then begin
    Reader.Free;
    Exit(False);
  end;

  for j:=0 to 255 do begin
    R := Reader.getU;
    G := Reader.getU;
    B := Reader.getU;

    Bmp.AddPal(R,G,B, 255);
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    g := Reader.getU;

    Bmp.SetPal(x,y, g);
  end;

  Result := True;
  Reader.Free;
end;


function BRU_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const colors: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
begin
  if (Str.size <> 64) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  width := 8;
  height := 8;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    b := Reader.getU;

    Bmp.SetMono(x, y, b);
  end;

  Result := True;
  Reader.Free;
end;


function CM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i, colors: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.getMU2;
  Width := Reader.getMU2;
  Reader.getMU2;
  Height := Reader.getMU2;
  colors := Reader.getU;

  if (Str.size <> width*height + 9 + colors*3) then begin
    Reader.Free;
    Exit(False);
  end;

  for i:=0 to colors-1 do begin
    R := Reader.getU;
    G := Reader.getU;
    B := Reader.getU;

    Bmp.AddPal(R,G,B, 255);
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    g := Reader.getU;

    Bmp.SetPal(x,y, g);
  end;

  Result := True;
  Reader.Free;
end;


function CSV_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Comma-Separated Value, grayscale
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    lines,row: TStringList;
    AllText: String;
    ch: Char;
    i: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  lines := TStringList.Create;
  row   := TStringList.Create;

  AllText := Reader.getS(Str.Size);
  lines.text := AllText;
  row.commaText := lines[0];

  for i:=1 to Length(AllText) do begin
    ch := AllText[i];

    if ch in ['0'..'9'] then continue;
    if ch in [',', #10, #13] then continue;

    Reader.Free;
    Exit(False);
  end;

  width := row.count;
  height := lines.count;

  if (width > 3000) or (height > 3000) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  for y:=0 to bmp.height-1 do begin
    row.commaText := lines[y];
    for x:=0 to bmp.width-1 do begin
      g := strToIntDef(row[x], 0);

      Bmp.SetRGBA(x, y, g, g, g, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function DDS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    j,i: Integer;
    p: TPix;
    magicNumber: String;
    surfSize, surfFlags, surfHeight, surfWidth, surfPitchOrLinearSize,
    surfDepth, surfMipMaps, surfPixSize, surfPixFlags, surfPixBpp, surfPixRedMask,
    surfPixGreenMask, surfPixBlueMask, surfPixAlphaMask, surfCap1, surfCap2,
    surfCapReserved1, surfCapReserved2, surfReserved: Cardinal;
    surfPixFourCC, surfReserved1: String;
    buff, buff2: array[1..4] of Word;
    color, color2: array[0..3] of TPix;
    pix, pix2: array[0..15] of TPix;
    alpha: array[0..7] of Byte;
    alf: array[0..3] of Cardinal;
begin
  Reader := TPV_Reader.Create(Str);

  magicNumber           := Reader.getS(4);

  if (magicNumber <> 'DDS ') then begin
    Reader.Free;
  Exit(False);
  end;



  surfSize              := Reader.getU4;
  surfFlags             := Reader.getU4;
  surfHeight            := Reader.getU4;
  surfWidth             := Reader.getU4;
  surfPitchOrLinearSize := Reader.getU4;
  surfDepth             := Reader.getU4;
  surfMipMaps           := Reader.getU4;
  surfReserved1         := Reader.getS(44);
  surfPixSize           := Reader.getU4;
  surfPixFlags          := Reader.getU4;
  surfPixFourCC         := Reader.getS(4);
  surfPixBpp            := Reader.getU4;
  surfPixRedMask        := Reader.getU4;
  surfPixGreenMask      := Reader.getU4;
  surfPixBlueMask       := Reader.getU4;
  surfPixAlphaMask      := Reader.getU4;
  surfCap1              := Reader.getU4;
  surfCap2              := Reader.getU4;
  surfCapReserved1      := Reader.getU4;
  surfCapReserved2      := Reader.getU4;
  surfReserved          := Reader.getU4;

  Bmp.SetSize(surfWidth, surfHeight);

  if (surfPixFourCC = 'DXT1') then begin
    for y:=0 to ceil(surfHeight / 4)-1  do
    for x:=0 to ceil(surfWidth / 4)-1 do begin
      buff[1] := Reader.getU2;
      buff[2] := Reader.getU2;
      buff[3] := Reader.getU2;
      buff[4] := Reader.getU2;
      color[0].r := getBits(buff[1], 0, 5)*8;
      color[0].g := getBits(buff[1], 5, 6)*4;
      color[0].b := getBits(buff[1], 11, 5)*8;

      color[1].r := getBits(buff[2], 0, 5)*8;
      color[1].g := getBits(buff[2], 5, 6)*4;
      color[1].b := getBits(buff[2], 11, 5)*8;

      color[2].r := round(2/3*color[0].r + 1/3*color[1].r);
      color[2].g := round(2/3*color[0].g + 1/3*color[1].g);
      color[2].b := round(2/3*color[0].b + 1/3*color[1].b);

      color[3].r := round(1/3*color[0].r) + round(2/3*color[1].r);
      color[3].g := round(1/3*color[0].g) + round(2/3*color[1].g);
      color[3].b := round(1/3*color[0].b) + round(2/3*color[1].b);

      pix[0] := color[getBits(buff[3], 0, 2)];
      pix[1] := color[getBits(buff[3], 2, 2)];
      pix[2] := color[getBits(buff[3], 4, 2)];
      pix[3] := color[getBits(buff[3], 6, 2)];

      pix[4] := color[getBits(buff[3], 8, 2)];
      pix[5] := color[getBits(buff[3], 10, 2)];
      pix[6] := color[getBits(buff[3], 12, 2)];
      pix[7] := color[getBits(buff[3], 14, 2)];

      pix[8]  := color[getBits(buff[4], 0, 2)];
      pix[9]  := color[getBits(buff[4], 2, 2)];
      pix[10] := color[getBits(buff[4], 4, 2)];
      pix[11] := color[getBits(buff[4], 6, 2)];

      pix[12] := color[getBits(buff[4], 8, 2)];
      pix[13] := color[getBits(buff[4], 10, 2)];
      pix[14] := color[getBits(buff[4], 12, 2)];
      pix[15] := color[getBits(buff[4], 14, 2)];

      for j:=0 to 3 do
      for i:=0 to 3 do begin
        p := pix[4*j+i];
        Bmp.SetRGBA(4*x+i, 4*y+j, p.b, p.g, p.r, 255);
      end;
    end;
  end
else if surfPixFourCC = 'DXT3' then begin
    for y:=0 to (surfHeight div 4)-1  do
    for x:=0 to (surfWidth div 4)-1 do begin
      alf[0] := Reader.getU2;
      alf[1] := Reader.getU2;
      alf[2] := Reader.getU2;
      alf[3] := Reader.getU2;

      buff2[1] := Reader.getU2;
      buff2[2] := Reader.getU2;
      buff2[3] := Reader.getU2;
      buff2[4] := Reader.getU2;

      color2[0].r := getBits(buff2[1], 0, 5)*8;
      color2[0].g := getBits(buff2[1], 5, 6)*4;
      color2[0].b := getBits(buff2[1], 11, 5)*8;

      color2[1].r := getBits(buff2[2], 0, 5)*8;
      color2[1].g := getBits(buff2[2], 5, 6)*4;
      color2[1].b := getBits(buff2[2], 11, 5)*8;

      color2[2].r := round(2/3*color2[0].r + 1/3*color2[1].r);
      color2[2].g := round(2/3*color2[0].g + 1/3*color2[1].g);
      color2[2].b := round(2/3*color2[0].b + 1/3*color2[1].b);

      color2[3].r := round(1/3*color2[0].r) + round(2/3*color2[1].r);
      color2[3].g := round(1/3*color2[0].g) + round(2/3*color2[1].g);
      color2[3].b := round(1/3*color2[0].b) + round(2/3*color2[1].b);

      pix2[0] := color2[getBits(buff2[3], 0, 2)];
      pix2[1] := color2[getBits(buff2[3], 2, 2)];
      pix2[2] := color2[getBits(buff2[3], 4, 2)];
      pix2[3] := color2[getBits(buff2[3], 6, 2)];

      pix2[4] := color2[getBits(buff2[3], 8, 2)];
      pix2[5] := color2[getBits(buff2[3], 10, 2)];
      pix2[6] := color2[getBits(buff2[3], 12, 2)];
      pix2[7] := color2[getBits(buff2[3], 14, 2)];

      pix2[8]  := color2[getBits(buff2[4], 0, 2)];
      pix2[9]  := color2[getBits(buff2[4], 2, 2)];
      pix2[10] := color2[getBits(buff2[4], 4, 2)];
      pix2[11] := color2[getBits(buff2[4], 6, 2)];

      pix2[12] := color2[getBits(buff2[4], 8, 2)];
      pix2[13] := color2[getBits(buff2[4], 10, 2)];
      pix2[14] := color2[getBits(buff2[4], 12, 2)];
      pix2[15] := color2[getBits(buff2[4], 14, 2)];

      pix2[0].a  := getBits(alf[0],  0, 4);
      pix2[1].a  := getBits(alf[0],  4, 4);
      pix2[2].a  := getBits(alf[0],  8, 4);

      pix2[3].a  := getBits(alf[0], 12, 4);
      pix2[4].a  := getBits(alf[1],  0, 4);
      pix2[5].a  := getBits(alf[1],  4, 4);

      pix2[6].a  := getBits(alf[1],  8, 4);
      pix2[7].a  := getBits(alf[1], 12, 4);

      pix2[8].a  := getBits(alf[2],  0, 4);
      pix2[9].a  := getBits(alf[2],  4, 4);
      pix2[10].a := getBits(alf[2],  8, 4);

      pix2[11].a := getBits(alf[2], 12, 4);
      pix2[12].a := getBits(alf[3],  0, 4);
      pix2[13].a := getBits(alf[3],  4, 4);

      pix2[14].a := getBits(alf[3],  8, 4);
      pix2[15].a := getBits(alf[3], 12, 4);

      for j:=0 to 3 do
      for i:=0 to 3 do begin
        p := pix2[4*j+i];
        Bmp.SetRGBA(4*x+i, 4*y+j, p.b, p.g, p.r, p.a + p.a shl 4);
      end;
    end;
  end
  else if surfPixFourCC = 'DXT5' then begin
    for y:=0 to (surfHeight div 4)-1  do
    for x:=0 to (surfWidth div 4)-1 do begin
      alpha[0] := Reader.getU;
      alpha[1] := Reader.getU;

      if (alpha[0] > alpha[1]) then  begin
        // 6 linearly interpolated alpha values.
        alpha[2] := round(6/7*alpha[0] + 1/7*alpha[1]);
        alpha[3] := round(5/7*alpha[0] + 2/7*alpha[1]);
        alpha[4] := round(4/7*alpha[0] + 3/7*alpha[1]);
        alpha[5] := round(3/7*alpha[0] + 4/7*alpha[1]);
        alpha[6] := round(2/7*alpha[0] + 5/7*alpha[1]);
        alpha[7] := round(1/7*alpha[0] + 6/7*alpha[1]);
      end
      else begin
        // 4 interpolated alpha values.
        alpha[2] := round(4/5*alpha[0] + 1/5*alpha[1]);
        alpha[3] := round(3/5*alpha[0] + 2/5*alpha[1]);
        alpha[4] := round(2/5*alpha[0] + 3/5*alpha[1]);
        alpha[5] := round(1/5*alpha[0] + 4/5*alpha[1]);
        alpha[6] := 0;
        alpha[7] := 255;
      end;

      alf[0] := Reader.getU3;
      alf[1] := Reader.getU3;

      buff2[1] := Reader.getU2;
      buff2[2] := Reader.getU2;
      buff2[3] := Reader.getU2;
      buff2[4] := Reader.getU2;

      color2[0].r := getBits(buff2[1], 0, 5)*8;
      color2[0].g := getBits(buff2[1], 5, 6)*4;
      color2[0].b := getBits(buff2[1], 11, 5)*8;

      color2[1].r := getBits(buff2[2], 0, 5)*8;
      color2[1].g := getBits(buff2[2], 5, 6)*4;
      color2[1].b := getBits(buff2[2], 11, 5)*8;

      color2[2].r := round(2/3*color2[0].r + 1/3*color2[1].r);
      color2[2].g := round(2/3*color2[0].g + 1/3*color2[1].g);
      color2[2].b := round(2/3*color2[0].b + 1/3*color2[1].b);

      color2[3].r := round(1/3*color2[0].r) + round(2/3*color2[1].r);
      color2[3].g := round(1/3*color2[0].g) + round(2/3*color2[1].g);
      color2[3].b := round(1/3*color2[0].b) + round(2/3*color2[1].b);

      pix2[0] := color2[getBits(buff2[3], 0, 2)];
      pix2[1] := color2[getBits(buff2[3], 2, 2)];
      pix2[2] := color2[getBits(buff2[3], 4, 2)];
      pix2[3] := color2[getBits(buff2[3], 6, 2)];

      pix2[4] := color2[getBits(buff2[3], 8, 2)];
      pix2[5] := color2[getBits(buff2[3], 10, 2)];
      pix2[6] := color2[getBits(buff2[3], 12, 2)];
      pix2[7] := color2[getBits(buff2[3], 14, 2)];

      pix2[8]  := color2[getBits(buff2[4], 0, 2)];
      pix2[9]  := color2[getBits(buff2[4], 2, 2)];
      pix2[10] := color2[getBits(buff2[4], 4, 2)];
      pix2[11] := color2[getBits(buff2[4], 6, 2)];

      pix2[12] := color2[getBits(buff2[4], 8, 2)];
      pix2[13] := color2[getBits(buff2[4], 10, 2)];
      pix2[14] := color2[getBits(buff2[4], 12, 2)];
      pix2[15] := color2[getBits(buff2[4], 14, 2)];

      pix2[0].a  := alpha[getBits(alf[0], 0, 3)];
      pix2[1].a  := alpha[getBits(alf[0], 3, 3)];
      pix2[2].a  := alpha[getBits(alf[0], 6, 3)];

      pix2[3].a  := alpha[getBits(alf[0], 9, 3)];
      pix2[4].a  := alpha[getBits(alf[0], 12, 3)];
      pix2[5].a  := alpha[getBits(alf[0], 15, 3)];

      pix2[6].a  := alpha[getBits(alf[0], 18, 3)];
      pix2[7].a  := alpha[getBits(alf[0], 21, 3)];

      pix2[8].a  := alpha[getBits(alf[1], 0, 3)];
      pix2[9].a  := alpha[getBits(alf[1], 3, 3)];
      pix2[10].a := alpha[getBits(alf[1], 6, 3)];

      pix2[11].a := alpha[getBits(alf[1], 9, 3)];
      pix2[12].a := alpha[getBits(alf[1], 12, 3)];
      pix2[13].a := alpha[getBits(alf[1], 15, 3)];

      pix2[14].a := alpha[getBits(alf[1], 18, 3)];
      pix2[15].a := alpha[getBits(alf[1], 21, 3)];

      for j:=0 to 3 do
      for i:=0 to 3 do begin
        p := pix2[4*j+i];
        Bmp.SetRGBA(4*x+i, 4*y+j, p.b, p.g, p.r, p.a);
      end;
    end;
  end;

  Result := True;
  Reader.Free;
end;


function DIS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//DKB Trace / Qrt raytrace


var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    buffR, buffG, buffB: array of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  width  := Reader.getU2;
  height := Reader.getU2;
  Reader.getU2;

  if Str.size <> (width*3+2)*height + 4 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);
  setLength(buffR, width);
  setLength(buffG, width);
  setLength(buffB, width);

  Reader.AtLeast := width*4*height;

  for y:=0 to height-1 do begin
    Reader.get(buffR[0], width);
    Reader.get(buffG[0], width);
    Reader.get(buffB[0], width);

    for x:=0 to width-1 do begin
      Bmp.SetRGBA( x, y, buffR[x], buffG[x], buffB[x], 255);
    end;
    if (y <> height-1) then begin
      Reader.Offset := Reader.Offset + 2;
    end;
  end;

  Result := True;
  Reader.Free;
end;


function ESM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    id: String;
    Bpp: Integer;
    i: Integer;
    headSize, typ, deepRed, deepGreen, deepBlue, deepBlack, Version, Xdpi, Ydpi,
    FileHeight, startLine, endLine, mask: Integer;

    palR, palB, palG: array[0..255] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(4);

  if (id <> 'TMS'#0) then begin
    Reader.Free;
    Exit(False);
  end;

  headSize := Reader.getMU2;
  Width := Reader.getMU2;
  Height := Reader.getMU2;
  Bpp := Reader.getMU2;
  Typ := Reader.getMU2;
  deepRed := Reader.getMU2;
  deepGreen := Reader.getMU2;
  deepBlue := Reader.getMU2;
  deepBlack := Reader.getMU2;
  Version := Reader.getMU2;
  Xdpi := Reader.getMU2;
  Ydpi := Reader.getMU2;
  FileHeight := Reader.getMU2;
  startLine := Reader.getMU2;
  endLine := Reader.getMU2;
  mask := Reader.getMU2;

  Reader.Get(palR, 256);
  Reader.Get(palG, 256);
  Reader.Get(palB, 256);

  for i:=0 to 255 do begin
    R := palR[i];
    G := palG[i];
    B := palB[i];

    Bmp.AddPal(R,G,B,255);
  end;

  Reader.Skip(8);

  Bmp.SetSize(Width, Height);

  Reader.AtLeast:= Width*Height*3;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    g := Reader.getU;

    Bmp.SetPal(x, y, g);
  end;


 {
  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    r := Reader.getU;
    g := Reader.getU;
    b := Reader.getU;

    Bmp.SetRGBA(x, y, r,g,b, 255);
  end;
 }
  Result := True;
  Reader.Free;
end;


function FMV_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    id : String;
begin
  Reader := TPV_Reader.Create(Str);  id := Reader.getS(16);

  if id <> '<MakerVector5.0>' then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := 114;

  Width := Reader.getU4;
  Height := Reader.getU4;

  Reader.Offset := Str.size - Width*Height*3 - 19; //60+78+1;

  Bmp.SetSize(Width, Height);

  for y:=Bmp.Height-1 downto 0 do
  for x:=0 to Bmp.Width-1 do begin
    r := Reader.getU;
    g := Reader.getU;
    b := Reader.getU;
    Bmp.SetRGBA(x, y, r, g, b, 255);
  end;

  Result := True;
  Reader.Free;
end;


function FNT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i,k: Integer;
    index: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  if (Str.size < 1024) or (Str.size > 1030) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(256, 32);
//  f.position := f.position + 1556;

  //This image is an array of chars- every char is 8 bytes long
  //and is 8x8 px
  //So we read char by char and then we build and image from it

  for r:=0 to 3 do //for every row of chars
  for x:=0 to 31 do //for every char in a row
  for y:=0 to 7 do begin //for every row of a char
    g := Reader.getU;

    for i:=0 to 7 do begin //for every pixel in a char's row
      a := getBits(g, 7-i, 1);
      Bmp.SetMono(8*x+i, 8*r+ y, 1-a);
    end;
  end;


  Result := True;
  Reader.Free;
end;


function GBR_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//GIMP Brush
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    bpp: Integer;
    soft, id: String;
    headLen, version, spacing: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  headLen := Reader.getMU4;
  version := Reader.getMU4;
  width  := Reader.getMU4;
  height := Reader.getMU4;
  bpp    := Reader.getMU4;
  soft   := Reader.getS(4);
  spacing := Reader.getMU4;
  id     := Reader.getS(headLen-28);

  if soft <> 'GIMP' then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  if bpp = 1 then begin
    for y:=0 to height-1 do
     for x:=0 to width-1 do begin
      g := Reader.getU;
      Bmp.SetRGBA(x, y, g, g, g, 255);
    end;
  end
  else if bpp = 3 then begin
    for y:=0 to height-1 do
     for x:=0 to width-1 do begin
      r := Reader.getU;
      g := Reader.getU;
      b := Reader.getU;
      Bmp.SetRGBA(x, y, r, g, b, 255);
    end;
  end
  else if bpp = 4 then begin
    for y:=0 to height-1 do
     for x:=0 to width-1 do begin
      r := Reader.getU;
      g := Reader.getU;
      b := Reader.getU;
      Reader.getU;
      Bmp.SetRGBA(x, y, r, g, b, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function GR7_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
i: Integer;
    pal: array[0..3] of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  if Str.size > 12000 then begin  //FUTURE: think of something
    Reader.Free;
    Exit(False);
  end;

  Bmp.AddPalette(AtariPal, 256);

  height := floor(Str.size/40);
  Bmp.SetSize(160, height);

  Reader.Offset := Str.size - 4;
  pal[0] := Reader.getU;
  pal[1] := Reader.getU;
  pal[2] := Reader.getU;
  pal[3] := Reader.getU;
  Reader.Offset := 0;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 4)-1 do begin
      g := Reader.getU;

      for i:=0 to 3 do begin
        a := getBits(g, 6-2*i, 2);
        Bmp.SetPal(4*x +i, y, pal[a]);
      end;
  end;

   Result := True;
  Reader.Free;
end;



function GR8_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
//GR8
var Reader: TPV_Reader;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;

const colors: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
begin
  Reader := TPV_Reader.Create(Str);

  if Str.size <> 7680 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(320, 192);

  for y:=0 to 191 do
  for x:=0 to 39 do begin
    g := Reader.getU;
    for j:=0 to 7 do begin
      b := getBits(g, 7-j, 1);

      Bmp.SetMono(8*x+j, y, 1-b);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function GRFX_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    id: String;
    unk, hlen, colors: Word;
    pal: array[0..15] of TPix;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(4);

  if id <> 'GRFX' then begin
   //Error('Not a GRFX image');
   Reader.Free;
   Exit(False);
  end;

  unk := Reader.getU2; //unknown, usually $01$01
  hlen := Reader.getU2; //header length, usually 8

  //block
  unk := Reader.getU4; //unknown, usually 0
  hlen := Reader.getU2; //len of this block, always 78
  unk := Reader.getU2;
  unk := Reader.getU2;

  Width := Reader.getU2;
  Height := Reader.getU2;

  unk := Reader.getU2;
  unk := Reader.getU2;

  colors := Reader.getU2; //number of colors, always 16
  unk := Reader.getU;

  id := Reader.getS(3);

  if id <> 'IMA' then Exit(False); //Not a GRFX image //AMI in reverse

  for j:=0 to colors-1 do begin
    R := Reader.getU shl 4;
    G := Reader.getU shl 4;
    B := Reader.getU shl 4;
    A := 255;

    Bmp.AddPal(R,G,B,A);
  end;

  unk := Reader.getU2;

  id := Reader.getS(4);

  //if (id <> 'SIMA') and (id <> 'LIMA') then Error('Not a GRX image3');
  //but also 'GLMS'
  //SIMA = AMI small, LIMA = AMI large


  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to ceil(Bmp.Width/2)-1 do begin
    g := Reader.getU;

    a := getBits(g, 4, 4);
    b := getBits(g, 0, 4);

    Bmp.SetPal(2*x  , y, a);
    Bmp.SetPal(2*x+1, y, b);
  end;

  Result := True;
  Reader.Free;
end;



function IIM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: String;
    Dpi, Typ: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(8);

  if (id <> 'IS_IMAGE') then begin
   Reader.Free;
   Exit(False);
  end;

  Typ := Reader.getMU2;
  Dpi := Reader.getMU2; //usually 72

  Width := Reader.getMU2;
  Height := Reader.getMU2;

  Bmp.SetSize(Width, Height);

  if Typ = 4 then begin
      for y:=0 to Bmp.Height-1 do
      for x:=0 to Bmp.Width-1 do begin
        r := Reader.getU;
        g := Reader.getU;
        b := Reader.getU;

        Bmp.SetRGBA(x, y, r, g, b, 255);
      end;
  end
  else if Typ = 5 then begin
    for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      a := Reader.getU;
      r := Reader.getU;
      g := Reader.getU;
      b := Reader.getU;

      Bmp.SetRGBA(x, y, r, g, b, a);
    end;
  end
  else if Typ = 1 then begin
    for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      g := Reader.getU;

      Bmp.SetRGB(x,y, g,g,g);
    end;
  end
  else if Typ = 0 then begin
    for y:=0 to Bmp.Height-1 do
    for x:=0 to Ceil(Bmp.Width/8)-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        B := GetBits(G, 7-i, 1);

        Bmp.SetMono(8*x +i,y, B);
      end;
    end;
  end;

  Result := True;
  Reader.Free;
end;

function IPI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    Width, Height: Integer;
    x,y: Integer;
    A,R,G,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Width := Reader.getMU2;
  Height := Reader.getMU2;

  if Str.size <> width*3*height+4 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    r := Reader.getU;
    g := Reader.getU;
    b := Reader.getU;

    Bmp.SetRGBA(x, y, r, g, b, 255);
  end;

  Result := True;
  Reader.Free;
end;

function IPL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
//add other modes
const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    tag,id,version: String;
    size, size2, channels, zDepth, yDepth, dataType: Cardinal;
begin
  Reader := TPV_Reader.Create(Str);

  tag     := Reader.getS(4);
  size    := Reader.getU4;
  version := Reader.getS(4);
  id      := Reader.getS(4);
  size2   := Reader.getU4;
  width   := Reader.getU4;
  height  := Reader.getU4;
  channels:= Reader.getU4;
  zDepth  := Reader.getU4;
  yDepth  := Reader.getU4;
  dataType:= Reader.getU4;

  if (tag <> 'iiii') or (id <> 'data') then begin
  Reader.Free;
  Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  {switch dataType
            case 0
                dt = 'uint8';
            case 1
                dt = 'int16';
            case 2
                dt = 'uint16';
            case 3
                dt = 'int32';
            case 4
                dt = 'float32';
            case 5
                dt = 'Color24';
            case 6
                dt = 'Color48';
            case 7
                dt = '(reserved)';
            case 8
                dt = '(reserved)';
            case 9
                dt = '(reserved)';
            case 10
                dt = 'float64';
            otherwise
                dt = 'unknown';
        end
    }
  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    r := Reader.getU;

    if r = 255 then g := 0
    else            g := 1;

    Bmp.SetMono(x,y, g);
  end;

  Result := True;
  Reader.Free;
end;


function JGP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    aa,bb,cc,dd: Integer;
    offset: Integer;
    buff: array[0..2048] of Byte;
begin
  if Str.size <> 2054 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(128,64);

  Reader.getS(6);
  fillchar(buff, 2048, 0);

  for i:=0 to 2047 do begin
    aa := getBits(i, 8, 2)  shl 9;
    bb := getBits(i, 10, 1) shl 8;
    cc  := getBits(i, 0, 3)  shl 5;
    dd  := getBits(i, 3, 5);

    offset := aa + bb + cc + dd;
    buff[offset] := Reader.getU;
  end;

  i := 0;
  for y:=0 to Bmp.height-1 do
  for x:=0 to (Bmp.width div 4)-1 do begin
    b := buff[i];
    inc(i);

    for j:=0 to 3 do begin
      g := getBits(b, 6-2*j, 2)*64;
      Bmp.SetRGBA(4*x+j, y, g, g, g, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function MBG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i,j: Integer;
    x,y: Integer;
    A,R,G,B: Byte;

const colors: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
begin
  if Str.size <> 16384  then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(512, 256);

  for y:=0 to 255 do
  for x:=0 to 63 do  begin
    g := Reader.getU;
    for j:=0 to 7 do begin
      b := getBits(g, 7-j, 1);

      Bmp.SetMono(8*x+j, y, b);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function MBM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const pal: array[0..255, 0..2] of Byte = (
(0,0,0),(51,0,0),(102,0,0),(153,0,0),(204,0,0),(255,0,0),(0,51,0),(51,51,0),(102,51,0),
(153,51,0),(204,51,0),(255,51,0),(0,102,0),(51,102,0),(102,102,0),(153,102,0),
(204,102,0),(255,102,0),(0,153,0),(51,153,0),(102,153,0),(153,153,0),(204,153,0),
(255,153,0),(0,204,0),(51,204,0),(102,204,0),(153,204,0),(204,204,0),(255,204,0),
(0,255,0),(51,255,0),(102,255,0),(153,255,0),(204,255,0),(255,255,0),(0,0,51),
(51,0,51),(102,0,51),(153,0,51),(204,0,51),(255,0,51),(0,51,51),(51,51,51),
(102,51,51),(153,51,51),(204,51,51),(255,51,51),(0,102,51),(51,102,51),(102,102,51),
(153,102,51),(204,102,51),(255,102,51),(0,153,51),(51,153,51),(102,153,51),
(153,153,51),(204,153,51),(255,153,51),(0,204,51),(51,204,51),(102,204,51),
(153,204,51),(204,204,51),(255,204,51),(0,255,51),(51,255,51),(102,255,51),
(153,255,51),(204,255,51),(255,255,51),(0,0,102),(51,0,102),(102,0,102),(153,0,102),
(204,0,102),(255,0,102),(0,51,102),(51,51,102),(102,51,102),(153,51,102),(204,51,102),
(255,51,102),(0,102,102),(51,102,102),(102,102,102),(153,102,102),(204,102,102),
(255,102,102),(0,153,102),(51,153,102),(102,153,102),(153,153,102),(204,153,102),
(255,153,102),(0,204,102),(51,204,102),(102,204,102),(153,204,102),(204,204,102),
(255,204,102),(0,255,102),(51,255,102),(102,255,102),(153,255,102),(204,255,102),
(255,255,102),(17,17,17),(34,34,34),(68,68,68),(85,85,85),(119,119,119),(17,0,0),
(34,0,0),(68,0,0),(85,0,0),(119,0,0),(0,17,0),(0,34,0),(0,68,0),(0,85,0),(0,119,0),
(0,0,17),(0,0,34),(0,0,68),(0,0,85),(0,0,119),(0,0,136),(0,0,170),(0,0,187),
(0,0,221),(0,0,238),(0,136,0),(0,170,0),(0,187,0),(0,221,0),(0,238,0),(136,0,0),
(170,0,0),(187,0,0),(221,0,0),(238,0,0),(136,136,136),(170,170,170),(187,187,187),
(221,221,221),(238,238,238),(0,0,153),(51,0,153),(102,0,153),(153,0,153),(204,0,153),
(255,0,153),(0,51,153),(51,51,153),(102,51,153),(153,51,153),(204,51,153),
(255,51,153),(0,102,153),(51,102,153),(102,102,153),(153,102,153),(204,102,153),
(255,102,153),(0,153,153),(51,153,153),(102,153,153),(153,153,153),(204,153,153),
(255,153,153),(0,204,153),(51,204,153),(102,204,153),(153,204,153),(204,204,153),
(255,204,153),(0,255,153),(51,255,153),(102,255,153),(153,255,153),(204,255,153),
(255,255,153),(0,0,204),(51,0,204),(102,0,204),(153,0,204),(204,0,204),(255,0,204),
(0,51,204),(51,51,204),(102,51,204),(153,51,204),(204,51,204),(255,51,204),
(0,102,204),(51,102,204),(102,102,204),(153,102,204),(204,102,204),(255,102,204),
(0,153,204),(51,153,204),(102,153,204),(153,153,204),(204,153,204),(255,153,204),
(0,204,204),(51,204,204),(102,204,204),(153,204,204),(204,204,204),(255,204,204),
(0,255,204),(51,255,204),(102,255,204),(153,255,204),(204,255,204),(255,255,204),
(0,0,255),(51,0,255),(102,0,255),(153,0,255),(204,0,255),(255,0,255),(0,51,255),
(51,51,255),(102,51,255),(153,51,255),(204,51,255),(255,51,255),(0,102,255),
(51,102,255),(102,102,255),(153,102,255),(204,102,255),(255,102,255),(0,153,255),
(51,153,255),(102,153,255),(153,153,255),(204,153,255),(255,153,255),(0,204,255),
(51,204,255),(102,204,255),(153,204,255),(204,204,255),(255,204,255),(0,255,255),
(51,255,255),(102,255,255),(153,255,255),(204,255,255),(255,255,255)
);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id, typee, checkSum: String;
    appId: Integer;
    sizee, offset, xSize, ySize, bpp, palSize, isColor, packedd: Integer;
    padding: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  id       := Reader.getS(4);
  typee    := Reader.getS(4);
  appId    := Reader.getU4;
  checkSum := Reader.getS(4);
  Reader.getU4;

  if (id <> #$37#0#0#$10) or (typee <> #$42#0#0#$10) then begin
    Reader.Free;
    Exit(False);
  end;

  sizee     := Reader.getU4;
  offset    := Reader.getU4;
  width     := Reader.getU4;
  height    := Reader.getU4;
  xSize     := Reader.getU4;
  ySize     := Reader.getU4;
  bpp       := Reader.getU4;
  isColor   := Reader.getU4;
  palSize   := Reader.getU4;
  packedd   := Reader.getU4;

  if packedd <> 0 then begin
    Reader.Free; //rle not supported
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  if (bpp = 8) and (isColor = 1) then begin
    for i:=0 to 255 do
      Bmp.AddPal(pal[i][0], pal[i][1], pal[i][2], 255);
  end;

  if (bpp = 8) and (isColor = 0) then
        for y:=0 to height-1 do
         for x:=0 to width-1 do begin
          g := Reader.getU;

          Bmp.SetRGBA(x, y, g, g, g, 255);
        end;

  if (bpp = 8) and (isColor = 1) then
        for y:=0 to height-1 do
         for x:=0 to width-1 do begin
          g := Reader.getU;

          Bmp.SetPal(x,y, g);
        end;

  if (bpp = 4) and (isColor = 0) then
        for y:=0 to height-1 do begin
         for x:=0 to Ceil(width/2)-1 do begin
          g := Reader.getU;

          a := byte(g and $F) shl 4;
          b := byte(g shr 4) shl 4;

          Bmp.SetRGBA(2*x  , y, a, a, a, 255);
          Bmp.SetRGBA(2*x+1, y, b, b, b, 255);
        end;

        padding := 8*Ceil(Ceil(width/2)/8) - Ceil(width/2);
        if padding>0 then
          Reader.Skip(padding);
      end;

  if (bpp = 2) and (isColor = 0) then
        for y:=0 to height-1 do begin
         for x:=0 to Ceil(width/4)-1 do begin
          g := Reader.getU;

          for i:=0 to 3 do begin
            b := getBits(G, 2*i, 2);
            a := b shl 6;

            Bmp.SetRGBA(4*x+i, y, a, a, a, 255);
          end;
        end;

        padding := 4*Ceil(Ceil(width/4)/4) - Ceil(width/4);
        if padding>0 then
          Reader.Skip(padding);
      end;

  Result := True;
  Reader.Free;
end;


function MGP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    j,i: Integer;
    index: Integer;
    pal: array[0..4] of Byte;
begin
  if Str.size <> 3845 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(160, 96);

  Bmp.AddPalette(AtariPal, 256);

  pal[1] := Reader.getU;
  pal[2] := Reader.getU;
  pal[3] := Reader.getU;
  pal[0] := Reader.getU;

  Reader.getU;
  index := Reader.getU;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 4)-1 do begin
      if index<4 then pal[index] := 16 +y;
      g := Reader.getU;

      for i:=0 to 3 do begin
        a := getBits(g, 6-2*i, 2);
        Bmp.SetPal(4*x +i, y, pal[a]);
      end;
  end;

  Result := True;
  Reader.Free;
end;


function NEO_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    j,i: Integer;
    flag, resolution, xx, yy, colorLimits, colorSpeed, colorSteps: Integer;
    pal: array[0..15] of TPix;
    gg: Integer;
    p: TPix;
    filename, reserved: String;
    pp: array[0..3] of Integer;
    animStart: Byte;
    animStop: Byte;
    isAnim: Byte;
    index: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  flag       := Reader.getMU2;
  resolution := Reader.getMU2;

  if (Str.size <> 32128) or (flag <> 0) or (resolution > 2) then begin
    Reader.Free;
    Exit(False);
  end;

  for i:=0 to 15 do begin
    gg := Reader.getMU2;

    R := round(getBits(gg, 8, 3)*36.25);
    G := round(getBits(gg, 4, 3)*36.25);
    B := round(getBits(gg, 0, 3)*36.25);
    A := 255;

    Bmp.AddPal(R,G,B,A);
  end;

  filename   := Reader.getS(12);
  colorLimits:= Reader.getMU2;
  colorSpeed := Reader.getMU2;
  colorSteps := Reader.getMU2;
  xx         := Reader.getMU2;
  yy         := Reader.getMU2;
  width      := Reader.getMU2;   //usually 0
  height     := Reader.getMU2;
  reserved   := Reader.getS(66);

  animStop  := getBits(colorLimits, 0, 4);
  animStart := getBits(colorLimits, 4, 4);
  isAnim    := getBits(colorLimits, 15, 1);

  index := 0;  //frame Number
  {
  if isAnim = 1 then begin
    p := pal[animStart];

    i := index + animStart;
    pal[i] := pal[i+1];

    pal[animStop]  := p;
  end;
  }

  //0 = 320,200, 16 colors
  //1 = 640,200, 4 colors
  //2 = 640x400, 2 colors

  if resolution = 0 then begin
      Bmp.SetSize(320, 200);

      for y:=0 to Bmp.height-1 do
      for x:=0 to 19 do  begin
        pp[0] := Reader.getMU2;
        pp[1] := Reader.getMU2;
        pp[2] := Reader.getMU2;
        pp[3] := Reader.getMU2;
        for i:=0 to 15 do begin //make 16 Bmp.Pixels from 4*int16
          gg:= (getBits(pp[3], 15-i, 1) shl 3) +
               (getBits(pp[2], 15-i, 1) shl 2) +
               (getBits(pp[1], 15-i, 1) shl 1) +
               (getBits(pp[0], 15-i, 1)     );

          Bmp.SetPal(16*x +i, y, gg);
        end;
      end;
  end
  else if resolution = 1 then begin
      Bmp.SetSize(640, 200);

      for y:=0 to Bmp.height-1 do
      for x:=0 to 39 do  begin
        pp[0] := Reader.getMU2;
        pp[1] := Reader.getMU2;
        for i:=0 to 15 do begin //make 16 Bmp.Pixels from 2*int16
          gg:= (getBits(pp[1], 15-i, 1) shl 1) +
               (getBits(pp[0], 15-i, 1)     );

          Bmp.SetPal(16*x +i, y, gg);
        end;
      end;
  end
  else if resolution = 2 then begin
      Bmp.SetSize(640, 400);

      for y:=0 to Bmp.height-1 do
      for x:=0 to 39 do  begin
        pp[0] := Reader.getMU2;
        for i:=0 to 15 do begin //make 16 Bmp.Pixels from int16
          gg:= (getBits(pp[0], 15-i, 1)     );

          Bmp.SetPal(16*x +i, y, gg);
        end;
      end;
  end;

  Result := True;
  Reader.Free;
end;


function NGG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Nokia Group Graphics

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(4);

  if id <> 'NGG'+#0 then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.getU2;
  width  := Reader.getU2;
  height := Reader.getU2;
  Reader.getU2;
  Reader.getU2;
  Reader.getU2;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    g := strToInt(Reader.getS(1));

    Bmp.SetMono(x, y, g);
  end;

  Result := True;
  Reader.Free;
end;


function NPM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Nokia Picture Message

const Colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    id, comment: String;
    len: Integer;
begin
  Reader := TPV_Reader.Create(Str);


  id := Reader.getS(3);

  if (id <> 'NPM') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.getU;
  len := Reader.getU;

  comment := Reader.getS(len);

  Reader.getU;

  width := Reader.getU;
  height := Reader.getU;

  if (width <> 72) or (height <> 28) then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.getU;
  Reader.getU;
  Reader.getU;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to ceil(Bmp.Width/8)-1 do begin
    g := Reader.getU;

    for j:=0 to 7 do begin
      b := getBits(g, 7-j, 1);
      Bmp.SetMono(8*x + j, y, b);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function NSR_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const pal: array[0..1] of Cardinal = ($FF000000, $FFFFFFFF);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    id: String;
    offsetX, offsetY: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.getMU2; //starting address
  Reader.getMU2; //ending address

  offsetY := Reader.getU;
  height := 1+ Reader.getU - offsetY;

  offsetX := Reader.getU;
  width := 1+ Reader.getU - offsetX;

  Reader.Offset := 23;
  id := Reader.getS(8);

  if id <> 'NEWSROOM' then begin
   Reader.Free;
   Exit(False);
  end;

  Reader.Offset := 39;

//  Width := 125;
//  Height := 119;


  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to ceil(Bmp.Width/8)-1 do begin
    r := Reader.getU;

    for j:=0 to 7 do begin
      g := getBits(r, 7-j, 1);

      Bmp.SetMono(8*x + j,y, g);
    end;

  end;

  Result := True;
  Reader.Free;
end;


function PA3_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: string;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(20);

  if (id <> 'PABLO PACKED PICTURE') then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Offset := Reader.Offset + 79-20;

  Bmp.SetSize(640, 400);

  for y:=0 to Bmp.height-1 do
  for x:=0 to (Bmp.width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);

      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PDB_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
type TPDBRec = record
       offset: Integer;
       attr: Byte;
       uniq: String;
     end;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    name: String;
    attr, version, createTime, modTime, backupTime,
    modNumber, appInfo, sortInfo, typee, creator,
    uniq, nextRec, recCount: Integer;
    rec: array of TPDBRec;
begin
  Reader := TPV_Reader.Create(Str);

  name       := Reader.getS(32);

  if (Pos('.pdb', name) < 1) and (Pos('.PDB', name) < 1) then begin
    Reader.Free;
    Exit(False);
  end;

  attr       := Reader.getMU2;
  version    := Reader.getMU2;
  createTime := Reader.getMU4;
  modTime    := Reader.getMU4;
  backupTime := Reader.getMU4;
  modNumber  := Reader.getMU4;
  appInfo    := Reader.getMU4;
  sortInfo   := Reader.getMU4;
  typee      := Reader.getMU4;
  creator    := Reader.getMU4;
  uniq       := Reader.getMU4;
  nextRec    := Reader.getMU4;
  recCount   := Reader.getMU2;

  setLength(rec, recCount);
  for i:=0 to recCount-1 do begin
    rec[i].offset := Reader.getMU4;
    rec[i].attr   := Reader.getU;
    rec[i].uniq   := Reader.getS(3);
  end;

  name       := Reader.getS(32);
  Reader.getS(18);
  Reader.getS(4);
  width      := Reader.getMU2;
  height     := Reader.getMU2;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 4)-1 do begin
    g := Reader.getU;
    for i:=0 to 3 do begin
      a := getBits(g, 6-2*i, 2);
      a := a * 85;
      a := 255-a;

      Bmp.SetRGBA(4*x+i, y, a, a, a, 255);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PGF_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Portfolio Graphics

var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    i,j: Integer;
    R,G,B,A: Byte;

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
begin
  if Str.size <> 1920 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(240, 64);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
      g := Reader.getU;

      for j:=0 to 7 do begin
        a := getBits(g, 7-j, 1);

        Bmp.SetMono(8*x + j, y, a);
      end;
  end;

  Result := True;
  Reader.Free;
end;


function PNT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const colors: array[0..29] of Cardinal = (
  $7f000000, $ffff0000, $ff00ff00, $ff0000ff, $ff020202,
  $ff000000, $ff2e1e0f, $ff01ffff, $ff002600, $ff000000,
  $ff000000, $ffff3f00, $ffffff7f, $ffff33c2, $ff5300bf,
  $ff000000, $ff530073, $ffbfbfbf, $ff7aaaff, $ffffd873,
  $ff6c2105, $fffdfdfd, $ffffff00, $ffff00ff, $ff841f27,
  $fffea620, $ff2b2722, $ff000080, $ff808000, $ff778899);
{
const pal: array[0..24] of TPal = (
(r:255;g:0;b:0),
(r:0;g:255;b:0),
(r:0;g:0;b:255),
(r:28;g:28;b:28),
(r:117;g:96;b:70),
(r:0;g:255;b:255),
(r:0;g:107;b:0),
(r:255;g:136;b:0),
(r:255;g:255;b:186),
(r:255;g:123;b:225),
(r:123;g:0;b:224),
(r:123;g:0;b:168),
(r:224;g:224;b:224),
(r:186;g:212;b:255),
(r:255;g:237;b:178),
(r:173;g:101;b:43),
(r:254;g:254;b:254),
(r:255;g:255;b:0),
(r:231;g:28;b:217),
(r:148;g:50;b:28),
(r:255;g:154;b:0),
(r:70;g:59;b:43),
(r:50;g:50;b:107),
(r:186;g:186;b:89),
(r:89;g:89;b:89)
);
      }
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    pix: TPix;
    Version, Revision, BufSize: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Version := Reader.GetU4;

  Pix.R := 0;
  Pix.G := 0;
  Pix.B := 0;
  Pix.A := 255;

  Width := Reader.getU4;
  Height := Reader.getU4;

  if Str.size <> width*height+20 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  Revision := Reader.GetU4;
  BufSize := Reader.GetU4;

 // Bmp.AddPalette(Pal, 25);

  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    b := Reader.getU;


    if b = 0 then Bmp[x,y] := Pix
    else          Bmp.SetPixel(x,y, TPix(colors[b]));

    //Bmp.SetPal(x,y, b);
  end;

  Result := True;
  Reader.Free;
end;


function PSA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

// Print Shop

var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    i,j: Integer;
    R,G,B,A: Byte;

const colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
begin
  if (Str.size <> 272) and (Str.size <> 574) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Reader.getMU2;

  case Str.size of
    272:  Bmp.SetSize(48, 45);
    574:  Bmp.SetSize(88, 52);
  end;

  for y:=0 to bmp.height-1 do
  for x:=0 to (bmp.width div 8)-1 do begin
    g := Reader.getU;
    for j:=0 to 7 do begin
      a := getBits(g, 7-j, 1);

      Bmp.setMono(8*x+j, y, a);
    end;
  end;

  Result := True;
  Reader.Free;
end;


function PTX_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
//V.Flash PTX
//used by The Amazing Spider-Man: Countdown To Doom game for the VTech V.Flash Game Console
//and possibly other games for the same system.
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i,j: Integer;
    offset, bitsPerPixel: Integer;
    c: Word;
    width2, height2: Cardinal;
begin
  Reader := TPV_Reader.Create(Str);

  offset := Reader.getU2;          // offset to where the image data starts, 0x2c (44)
  Reader.getU4;     // all zero
  Reader.getU2;       // = 0x01e0 (480)
  width  := Reader.getU2;
  height := Reader.getU2;
  bitsPerPixel:= Reader.getU2;

  for j:=0 to 8 do
    Reader.getU2;      // all zero

  width2 := Reader.getU2;          // duplicate of width (why?)
  height2 := Reader.getU2;        // duplicate of height (why?)
  Reader.getU2;        // 0x0000 (0)
  Reader.getU2;        // 0x0004 (4)
  Reader.getU2;        // possibly foreground color? mostly 0x0008
  Reader.getU2;        // possibly background color? varies between 0x0686 and 0x0688

  if (offset <> 44) or (width2 <> width) or (height2 <> height) then begin
  Reader.Free;
  Exit(False);
  end;


  Bmp.SetSize(width, height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin

    c := Reader.getU2;  //BGR555 in little-endian order.
    r := 8*getBits(c, 0, 5);
    g := 8*getBits(c, 5, 5);
    b := 8*getBits(c, 10, 5);
    a := 255*getBits(c, 15, 1);

    Bmp.SetRGBA(x, y, r,g,b,255-a);
  end;

  Result := True;
  Reader.Free;
end;


function TIM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    c: Word;
    i,j: Integer;
    id, typ, offset, size, palLen, palNum, x1,y1: Cardinal;
begin
  Reader := TPV_Reader.Create(Str);

  id      := Reader.getU4;
  typ     := Reader.getU4;
  offset  := Reader.getU4;
  x1      := Reader.getU2;
  y1      := Reader.getU2;

  if id <> 16 then begin
    Reader.Free;
    Exit(False);
  end;

  if (typ <> 2) and (typ <> 3) and (typ <> 8) and (typ <> 9) then begin
    Reader.Free;
    Exit(False);
  end;

  if (typ <> 2) and (typ <> 3) then begin
    palLen := Reader.getU2;
    palNum := Reader.getU2;

    for i:=0 to palLen-1 do begin
      c := Reader.getU2;

      b := getBits(c, 10, 5)*8;
      g := getBits(c, 5, 5)*8;
      r := getBits(c, 0, 5)*8;

      Bmp.AddPal(R,G,B,255);
    end;
    //skip other palettes
    for i:=1 to palNum-1 do begin
      for j:=0 to palLen-1 do
        Reader.Skip(2);
    end;

    size    := Reader.getU4;
    x1      := Reader.getU2;
    y1      := Reader.getU2;
  end;

  width   := Reader.getU2;
  height  := Reader.getU2;

  if typ = 9 then width := width * 2
  else if typ = 8 then width := width * 4
  else if typ = 3 then width := ceil(2*width/3);

  Bmp.SetSize(width, height);

  case typ of
    2:  for y:=0 to height-1 do
        for x:=0 to width-1 do begin
          c := Reader.getU2;

          b := getBits(c, 10, 5)*8;
          g := getBits(c, 5, 5)*8;
          r := getBits(c, 0, 5)*8;

          Bmp.SetRGBA(x,y, r,g,b,255);
        end;
    3:  for y:=0 to height-1 do
        for x:=0 to width-1 do begin
          r := Reader.getU;
          g := Reader.getU;
          b := Reader.getU;

          Bmp.SetRGBA(x,y, r,g,b,255);
        end;
   8:   for y:=0 to height-1 do
        for x:=0 to Ceil(width/2)-1 do begin
          c := Reader.getU;

          B := C shr 4;
          A := C and $F;

          Bmp.SetPal(2*x   ,y, A);
          Bmp.SetPal(2*x +1,y, B);
        end;
   9:   for y:=0 to height-1 do
        for x:=0 to width-1 do begin
          c := Reader.getU;

          Bmp.SetPal(x,y, C);
        end;
  end;



  Result := True;
  Reader.Free;
end;


function VBM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//VBM (VDC BitMap)

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    id: String;
    version: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getS(3);

  if id <> 'BM'#$CB then begin
   Reader.Free;
   Exit(False);
  end;

  version := Reader.getU;

  width := Reader.getMU2;
  height := Reader.getMU2;

  Bmp.SetSize(width, height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);

        Bmp.SetMono(8*x + i, y, a);
      end;
  end;

  Result := True;
  Reader.Free;
end;


function VIFF_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    id, typee, release, version, machineDep: Integer;
    padding, comment: String;
    i, widHei: Integer;

    xx, yy, subrowLen, locationType, locationDim, numerOfImages,
    numberOfBands, dataStorageType, dataEncodingScheme,
    mapScheme, mapStorageType, mapRowSize, mapColumnSize,
    mapSubrowSize, mapEnable, mapsPerCycle, colorSpaceModel,
    iSpare1, iSpare2: Integer;

    xPixelSize, yPixelSize, fSpare1, fSpare2: Double;

    buffR, buffG, buffB: array of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id         := Reader.getU;
  typee      := Reader.getU;
  release    := Reader.getU;
  version    := Reader.getU;
  machineDep := Reader.getU;
  padding    := Reader.getS(3);
  comment    := Reader.getS(512);

  if (id <> $AB) or (typee <> 1) then begin
   Reader.Free;
   Exit(False);
  end;

  if machineDep = 2 then begin //Motorola (big endian, high-order byte, most-significant byte first)
    width          := Reader.getMU4;
    height         := Reader.getMU4;
    subrowLen      := Reader.getMU4;
    xx             := Reader.getMU4;
    yy             := Reader.getMU4;
    xPixelSize     := Reader.getMF;
    yPixelSize     := Reader.getMF;
    locationType   := Reader.getMU4;
    locationDim    := Reader.getMU4;
    numerOfImages  := Reader.getMU4;
    numberOfBands  := Reader.getMU4;
    dataStorageType:= Reader.getMU4;
    dataEncodingScheme:= Reader.getMU4;
    mapScheme      := Reader.getMU4;
    mapStorageType := Reader.getMU4;
    mapRowSize     := Reader.getMU4;
    mapColumnSize  := Reader.getMU4;
    mapSubrowSize  := Reader.getMU4;
    mapEnable      := Reader.getMU4;
    mapsPerCycle   := Reader.getMU4;
    colorSpaceModel:= Reader.getMU4;
    iSpare1        := Reader.getMU4;
    iSpare2        := Reader.getMU4;
    fSpare1        := Reader.getMF;
    fSpare2        := Reader.getMF;
  end
  else begin
    width          := Reader.getU4;
    height         := Reader.getU4;
    subrowLen      := Reader.getU4;
    xx             := Reader.getU4;
    yy             := Reader.getU4;
    xPixelSize     := Reader.getF;
    yPixelSize     := Reader.getF;
    locationType   := Reader.getU4;
    locationDim    := Reader.getU4;
    numerOfImages  := Reader.getU4;
    numberOfBands  := Reader.getU4;
    dataStorageType:= Reader.getU4;
    dataEncodingScheme:= Reader.getU4;
    mapScheme      := Reader.getU4;
    mapStorageType := Reader.getU4;
    mapRowSize     := Reader.getU4;
    mapColumnSize  := Reader.getU4;
    mapSubrowSize  := Reader.getU4;
    mapEnable      := Reader.getU4;
    mapsPerCycle   := Reader.getU4;
    colorSpaceModel:= Reader.getU4;
    iSpare1        := Reader.getU4;
    iSpare2        := Reader.getU4;
    fSpare1        := Reader.getF;
    fSpare2        := Reader.getF;
  end;

  Reader.Offset := Reader.Offset + 404;

  Bmp.SetSize(width, height);

  Reader.AtLeast := width*4*height;

  widHei := width*height;

  setLength(buffR, widHei);
  setLength(buffG, widHei);
  setLength(buffB, widHei);

  Reader.get(buffR[0], widHei);
  Reader.get(buffG[0], widHei);
  Reader.get(buffB[0], widHei);

  i := 0;

  for y:=0 to height-1 do
   for x:=0 to width-1 do begin
    r := buffR[i];
    g := buffG[i];
    b := buffB[i];
    Bmp.SetRGBA(x, y, r, g, b, 255);
    inc(i);
  end;

  Result := True;
  Reader.Free;
end;

function VST_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//Vista
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    bpp: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.getS(12);
  width  := Reader.getU2;
  height := Reader.getU2;
  bpp    := Reader.getU;
  Reader.getS(3);
  id     := Reader.getS(4);
  Reader.getS(12);

  if (id <> 'IGCH') then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(width, height);

  for y:=height-1 downto 0 do
  for x:=0 to width-1 do begin
    r := Reader.getU;
    g := Reader.getU;
    b := Reader.getU;
    Bmp.SetRGBA(x, y, r, g, b, 255);
  end;

  Result := True;
  Reader.Free;
end;

function XWD_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;

    headerSize, fileVersion, windowNumColors: Cardinal;
    displayType, displayPlanes: Cardinal;
    pixmapFormat, pixmapDepth, xOffset, byteOrder, bitmapUnit,
    bitmapBitOrder, bitmapPad, bpp, bytesPerLine, visualClass,
    redMask, greenMas, blueMask, bitsPerRGB, numberOfColors, colorMapEntries,
    windowWidth, windowHeight, windowX, windowY, windowBorderWidth: Cardinal;
begin
  Reader := TPV_Reader.Create(Str);
  headerSize  := Reader.getMU4;
  fileVersion := Reader.getMU4;

  if (fileVersion <> 6) and (fileVersion <> 7) then begin
    Reader.Free;
    Exit(False);
  end;

  case fileVersion of
    6 : begin //X10
          displayType   := Reader.getMU4;
          displayPlanes := Reader.getMU4;
          pixmapFormat  := Reader.getMU4;
          width         := Reader.getMU4;
          height        := Reader.getMU4;
          windowWidth   := Reader.getMU2;
          windowHeight  := Reader.getMU2;
          windowX       := Reader.getMU2;
          windowY       := Reader.getMU2;
          windowBorderWidth:= Reader.getMU2;
          windowNumColors  := Reader.getMU2;
          bpp           := displayPlanes * 8;
        end;
    7:  begin //X11
           pixmapFormat := Reader.getMU4;
           pixmapDepth  := Reader.getMU4;
           width        := Reader.getMU4;
           height       := Reader.getMU4;
           xOffset      := Reader.getMU4;
           byteOrder    := Reader.getMU4;
           bitmapUnit   := Reader.getMU4;
           bitmapBitOrder:= Reader.getMU4;
           bitmapPad    := Reader.getMU4;
           bpp          := Reader.getMU4;
           bytesPerLine := Reader.getMU4;
           visualClass  := Reader.getMU4;
           redMask      := Reader.getMU4;
           greenMas     := Reader.getMU4;
           blueMask     := Reader.getMU4;
           bitsPerRGB   := Reader.getMU4;
           numberOfColors:= Reader.getMU4;
           colorMapEntries:= Reader.getMU4;
           windowWidth  := Reader.getMU4;
           windowHeight := Reader.getMU4;
           windowX      := Reader.getMU4;
           windowY      := Reader.getMU4;
           windowBorderWidth:= Reader.getMU4;
        end;
  end;

  if (bpp = 1) then begin
    Reader.Free;
    Exit(False);
    //Error('Error: Monochrome images are not supported');
  end;

  if (width > 3000) or (height > 3000) then begin
    Reader.Free; //Result := 4;
    Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
   for x:=0 to Bmp.Width-1 do begin
    if bpp =32 then a := Reader.getU
    else            a := 255;

    b := Reader.getU;

    if bpp = 8 then g := b
    else            g := Reader.getU;

    if bpp = 8 then r := b
    else            r := Reader.getU;

    Bmp.SetRGBA(x, y, r, g, b, 255);
  end;

  Result := True;
  Reader.Free;
end;


function B_W_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    id: String;
begin
  Reader := TPV_Reader.Create(Str);
  id := Reader.getS(6);
  width := Reader.getMU2;
  height := Reader.getMU2;


  if (id <> 'B&W256') then begin
    Reader.Free;
    Exit(False);
  end;


  Bmp.SetSize(width, height);

  for y:=0 to Bmp.height-1 do
  for x:=0 to Bmp.width-1 do begin
    g := Reader.getU;

    Bmp.SetRGB(x, y, g,g,g);
  end;

  Reader.Free;
  Result := True;
end;

function DA4_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
begin
  if (Str.Size <> 64000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(640, 800);

  for y:=0 to Bmp.height-1 do
  for x:=0 to (Bmp.width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function DAP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    pal: array[0..255] of TPix;
begin

  if (Str.Size <> 77568) then Exit(False);

  Reader := TPV_Reader.Create(Str);
  Bmp.SetSize(320, 240);

  Reader.Offset := 320*240;

  for i:=0 to 255 do
    pal[i].R := Reader.getU;
  for i:=0 to 255 do
    pal[i].G := Reader.getU;
  for i:=0 to 255 do
    pal[i].B := Reader.getU;

  for i:=0 to 255 do Bmp.AddPal(pal[i].R, pal[i].G, pal[i].B, 255);

  Reader.Offset := 0;

  for y:=0 to Bmp.height-1 do
  for x:=0 to Bmp.width-1 do begin
    g := Reader.getU;


    Bmp.SetPal(x, y, g);

  end;

  Reader.Free;
  Result := True;
end;

function DOO_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    buff: array of Byte;
begin
  if (Str.Size <> 32000) then Exit(False);

  Reader := TPV_Reader.Create(Str);
  Bmp.SetSize(640, 400);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do Bmp.SetRGB(x,y, 255,255,255);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);
        Bmp.SetMono(8*x+i, y, a);
      end;
  end;

  Reader.Free;
  Result := True;
end;

function KFX_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
begin
  if (Str.Size <>  420) then Exit(False);

  Reader := TPV_Reader.Create(Str);
  width := 56;
  height := 60;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      g := getBits(b, 7-i, 1);
      Bmp.SetMono(8*x+i, y, 1-g);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function PG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    id: String;
    buff: array of Byte;
    count: Word;
    k: Integer;
    pos: Integer;
    offset: Integer;
    res: Integer;
const bitmask: array[0..7] of Byte = ($80,$40,$20,$10,$08,$04,$02,$01);
      colors: array[0..1] of Cardinal = ($FFFFFFFF, $FF000000);
begin
  Reader := TPV_Reader.Create(Str);
  id       := Reader.getS(1);

  if (id <> 'G') and (id <> 'B') and (id <> 'P') then begin
    Reader.Free;
    Exit(False);
  end;

  setLength(buff, 65000);
  pos := 0;

  if id = 'G' then begin        //.BG
    Bmp.SetSize(640, 400);
    while (Reader.Offset < Str.Size) do begin
      b := Reader.getU;

	    if b = 155 then begin
	      count := Reader.getU2;
	      g := Reader.getU;

	      for k:=0 to count-1 do begin
	        buff[pos] := g;
		      Inc(pos);
	      end;
	   end
	   else begin
	     buff[pos] := b;
  	   Inc(pos);
  	 end;
    end;
  end
  else if id = 'B' then begin    //.BS

    Bmp.SetSize(320, 200);
    while (Reader.Offset < Str.Size) do begin
      b := Reader.getU;

	    if b = 155 then begin
	      count := Reader.getU2;
	      g := Reader.getU;

	      for k:=0 to count-1 do begin
	        buff[pos] := g;
		      Inc(pos);
	      end;
	   end
	   else begin
	     buff[pos] := b;
  	   Inc(pos);
  	 end;
    end;
  end
  else if id = 'P' then begin    //.PG
    g := Reader.getU;
    b := Reader.getU;

    for k:=0 to (2+g*4)-1 do Reader.getU;   //skip ID and outline info

    Bmp.SetSize(b*8, g*8);

    while (Reader.Offset < Str.Size) do begin
      b := Reader.getU;

	    if b = 155 then begin
	      count := Reader.getU;
	      g := Reader.getU;
        if  count = 0 then  count := 256;

	      for k:=0 to count-1 do begin
	        buff[pos] := g;
		      Inc(pos);
	      end;
	   end
	   else begin
	     buff[pos] := b;
  	   Inc(pos);
  	 end;
    end;
  end;
  {
  else if id = chr(0) then begin    //.??
    g := Reader.getU;
   // b := Reader.getU;

   // for k:=0 to (2+g*4)-1 do Reader.getU;   //skip ID and outline info

    //Bmp.SetSize(b*8, g*8);
    bmp.setsize(320,200);

    while (f.position < Str.Size) do begin
      b := Reader.getU;

	    buff[pos] := b;
  	  Inc(pos);
    end;
  end;
  }
//

  k := 0;

	for y:=0 to Bmp.height-1 do
	for x:=0 to Bmp.width-1 do begin

    offset :=  (y mod 8) + (x div 8)*8 + (y div 8) * Bmp.width;
    res := buff[offset] and bitmask[x mod 8];


    if (res <> 0) then Bmp.SetMono(x, y, 1)
    else               Bmp.SetMono(x, y, 0);
	end;

  Reader.Free;
  Result := True;
end;

function XGA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;

    c: Integer;
begin

  if (Str.Size <> 368640) and (Str.Size <> 153600) then Exit(False);

  Reader := TPV_Reader.Create(Str);
  if Str.Size = 153600 then begin
    Width := 320;
    Height := 240;
  end
  else begin
    Width := 384;
    Height := 480;
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    c := Reader.getMU2;

    b := 8* getBits(c, 0, 5);
    g := 4* getBits(c, 5, 6);
    r := 8* getBits(c, 11, 5);

    Bmp.SetRGB(x, y, r, g, b);
  end;

  Reader.Free;
  Result := True;
end;


function SXG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
const tab: array[0..24] of Byte = (0, 10, 21, 31, 42, 53, 63, 74, 85, 95, 106,
          117, 127, 138, 149, 159, 170, 181, 191, 202, 213, 223, 234, 245, 255);
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;

    pal: array[0..255] of TPix;
    id: String;
    id2: Byte;
    num: Byte;
    palOffset: Word;
    imgOffset: Word;
    palLen: Integer;
    w: Word;
    version, background, pack, sxgFormat: Byte;
    bad: Byte;
begin
  Reader := TPV_Reader.Create(Str);
  id2 := Reader.getU;
  id := Reader.getS(3);

 if (id <> 'SXG') or (id2 <> 127) then begin
    Reader.Free;
    Exit(False);
  end;

  version := Reader.getU;
  background := Reader.getU;
  pack := Reader.getU;
  sxgFormat := Reader.getU;

  width := Reader.getU2;
  height := Reader.getU2;

  palOffset := Reader.getU2;
  imgOffset := Reader.getU2;

  Reader.Offset := 14 + palOffset;

  Bmp.SetSize(width, height);

  palLen := (imgOffset-palOffset + 2) div 2;

  BAD := 0;
  for i:=0 to palLen-1 do begin
    w := Reader.getU2;

    if getBits(w, 15, 1) = 0 then begin

      R := tab[getBits(w,  0, 5) ];
      G := tab[getBits(w,  5, 5) ];
      B := tab[getBits(w, 10, 5) ];
    end
    else begin         BAD := 1;
      R := getBits(w,  0, 5) shl 3;
      G := getBits(w,  5, 5) shl 3;
      B := getBits(w, 10, 5) shl 3;
    end;

    Bmp.AddPal(B,G,R,255);
  end;


  Reader.Offset := 16 + imgOffset;

  if sxgFormat = 1 then begin //means 16 colors

    for y:=0 to Bmp.height-1 do
    for x:=0 to Ceil(Bmp.width/2)-1 do begin
      g := Reader.getU;

      a := getBits(g, 0, 4);
      b := getBits(g, 4, 4);

      Bmp.SetPal(2*x  , y, b);
      Bmp.SetPal(2*x+1, y, a);
    end;
  end
  else begin //sxgFormat = 2 means 256 colors

    for y:=0 to Bmp.height-1 do
    for x:=0 to Bmp.width-1 do begin
      g := Reader.getU;
      Bmp.SetPal(x,y, g);
    end;

  end;

  Reader.Free;
  Result := True;
end;

function SKP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
begin
 if (Str.Size <>  7680 ) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  width := 320;
  height := 192;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to (width div 8)-1 do begin
    b := Reader.getU;
    for i:=0 to 7 do begin
      if i mod 2 = 1 then          //every 2 bit is ignored and copied previous one
        g := getBits(b, 7-i, 1);

        Bmp.SetMono(8*x+i, y, g);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function SC8_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i,j: Integer;
    id: Cardinal;
    id2: Word;
    id3: Byte;
begin
  //3:3:2 RGB
  if (Str.Size <  54279 ) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  id := Reader.getU;
  Reader.getS(3);
  id2 := Reader.getU;    //found 2=12 in .SC8 and .SR8, =211 in .GE8 but perhaps there are more
  Reader.getS(2);

  if (id <> 254) then begin
    Reader.Free;
    Exit(False);
  end;

  width := 256;
  height := 212;

  Bmp.SetSize(width, height);

  for y:=0 to height-1 do
  for x:=0 to width-1 do begin
    a := Reader.getU;

    b := getBits(a, 0, 2)*85;
    r := getBits(a, 2, 3)*36;
    g := getBits(a, 5, 3)*36;

    Bmp.SetRGB(x, y, r,g,b);
  end;

  Reader.Free;
  Result := True;
end;

function RWL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    R,G,B,A: Byte;
    i: Integer;
begin
  if (Str.Size <> 256000) and (Str.Size <> 64000) then Exit(False);

  Reader := TPV_Reader.Create(Str);

  if Str.Size = 256000 then Bmp.SetSize(640, 400)
  else if  Str.Size = 64000 then Bmp.SetSize(320, 200);


  for y:=0 to Bmp.height-1 do
  for x:=0 to Bmp.width-1 do begin
    g := 255-Reader.getU;

    Bmp.SetRGB(x, y, g,g,g);

  end;

  Reader.Free;
  Result := True;
end;

function VOLLABEL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    Width, Height: Integer;
    x,y: Integer;
    G: Byte;
    id: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id     := Reader.getU; //=1
  Width  := Reader.getMU2;
  Height := Reader.getMU2;

  if (id <> 1) or (Str.size <> width*height+5) then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    g := Reader.getU;

    Bmp.SetRGB(x, y, g, g, g);
  end;

  Result := True;
  Reader.Free;
end;


function LUM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    j: Integer;
    Width, Height: Integer;
    id: Byte;
    A,G,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  id := Reader.getU;

  if (id <> 4) or (Str.size <> 4766) then begin
   Reader.Free;
   Exit(False);
  end;

  Reader.Offset := 6;

  Width := 80;
  Height := 119;

  Bmp.SetSize(Width, Height);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to ceil(Bmp.Width/2)-1 do begin
    G := Reader.getU;

    for j:=0 to 1 do begin
      A := 16*getBits(g, 4 -4*j, 4);
      Bmp.SetRGB(2*x + j, y, a,a,a);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function DRG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    i: Integer;
    index: Integer;
    x,y: Integer;
    A,G: Byte;
begin
  if Str.size <> 6400 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(320, 160);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to (Bmp.Width div 8)-1 do begin
      g := Reader.getU;

      for i:=0 to 7 do begin
        a := getBits(g, 7-i, 1);
        Bmp.SetMono(8*x + i, y, 1-a);
      end;
  end;

  Reader.Free;
  Result := True;
end;

function CHS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    B,G: Byte;
    i,j,k: Integer;
begin
  if Str.Size <> 794 then Exit(False);
  Reader := TPV_Reader.Create(Str);

  Reader.AtLeast := 256*3;

  Reader.Skip(25);

  Bmp.SetSize(256, 24);

  for y:=0 to 2 do
  for x:=0 to 31 do
  for j:=0 to 7 do begin
    G := Reader.GetU;

    for i:=0 to 7 do begin
      B := GetBits(G, 7-i, 1);
      Bmp.SetMono(8*x + i, 8*y + j, 1-B);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function A64C_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    B,G: Byte;
    i,j,k: Integer;
begin
  if Str.Size <> 770 then Exit(False);
  Reader := TPV_Reader.Create(Str);

  Reader.AtLeast := 256*3;

  Reader.Skip(2);

  Bmp.SetSize(256, 24);

  for y:=0 to 2 do
  for x:=0 to 31 do
  for j:=0 to 7 do begin
    G := Reader.GetU;

    for i:=0 to 7 do begin
      B := GetBits(G, 7-i, 1);
      Bmp.SetMono(8*x + i, 8*y + j, 1-B);
    end;
  end;

  Reader.Free;
  Result := True;
end;

function VZI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    R,G,B: Byte;
    i: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Reader.AtLeast := 80*200;

  Bmp.SetSize(320,200);

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Ceil(Bmp.Width/8)-1 do begin
    G := Reader.GetU;

    B := (G shr 4) shl 4;
    R := (G and $F) shl 4;

    for i:=0 to 3 do
    Bmp.SetRGB(8*x + i,y, B,B,B);

    for i:=0 to 3 do
    Bmp.SetRGB(8*x + 4+i  ,y, R,R,R);
  end;

  Reader.Free;
  Result := True;
end;


function PAT_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    Unk1, Unk2, Bpp: Integer;
    Id, Name: String;
    R,G,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Unk1   := Reader.GetMU4;
  Unk2   := Reader.GetMU4;
  Width  := Reader.GetMU4;
  Height := Reader.GetMU4;
  Bpp    := Reader.GetMU4;
  Id     := Reader.GetS(4);
  Name   := Reader.GetS(8);
  //Pattern'#0

  if Id <> 'GPAT' then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.AtLeast := Height*Width*3;

  Bmp.SetSize(Width, Height);

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin
    R := Reader.GetU;
    G := Reader.GetU;
    B := Reader.GetU;

    Bmp.SetRGB(x,y, R,G,B);
  end;

  Reader.Free;
  Result := True;
end;


function CEL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    i: Integer;
    Width, Height: Integer;
    Id, Unk1, Unk2, Bpp: Integer;
    R,G,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Id     := Reader.GetU2;
  Width  := Reader.GetU2;
  Height := Reader.GetU2;
  Unk1   := Reader.GetU2;
  Unk2   := Reader.GetU2;
  Bpp    := Reader.GetU2;
  Reader.Skip(20);


  if Id <> 37145 then begin
    Reader.Free;
    Exit(False);
  end;

  for i:=0 to 255 do begin

    R := Reader.GetU;
    G := Reader.GetU;
    B := Reader.GetU;

    Bmp.AddPal(4*R, 4*G, 4*B, 255);
  end;

  Bmp.SetSize(Width, Height);

  Reader.AtLeast := Height*Width;

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin

    G := Reader.GetU;
    Bmp.SetPal(x,y, G);
  end;

  Reader.Free;
  Result := True;
end;

function AVS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    A,R,G,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Width := Reader.GetMU4;
  Height := Reader.GetMU4;

  if Str.Size <> 4*Width*Height+8 then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  Reader.AtLeast := Height*Width*4;

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin

    A := Reader.GetU;
    R := Reader.GetU;
    G := Reader.GetU;
    B := Reader.GetU;

    Bmp.SetRGBA(x,y, R,G,B, not A);
  end;

  Reader.Free;
  Result := True;
end;

function PIC_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    Unk1, Unk3: Integer;
    Unk2: String;
    Id: Integer;
    G: Byte;
begin
  Reader := TPV_Reader.Create(Str);


  Width  := Reader.GetU2;
  Height := Reader.GetU2;
  Unk1   := Reader.GetU2;
  Unk2   := Reader.GetS(8);
  Unk3   := Reader.GetU2;
  Reader.Skip(38);

  Id     := Reader.GetU2;
  Reader.Skip(20);

  if Id <> 12345 then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.AtLeast := Height*Width;

  Bmp.SetSize(Width, Height);

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin
    G := Reader.GetU;
    Bmp.SetRGB(x,y, G,G,G);
  end;

  Reader.Free;
  Result := True;
end;


function KRO_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    Head: String;
    One, Empty0, Empty1, Unk0, Unk1, Empty2, Empty3, BitsPerPlane, BytesPerPixel: Integer;
    R,G,B,A: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Head          := Reader.GetS(3);
  One           := Reader.GetMU2;
  Empty0        := Reader.GetU;
  Width         := Reader.GetMU2;
  Unk0          := Reader.GetMU2;
  Height        := Reader.GetMU2;
  Unk1          := Reader.GetMU2;
  Empty1        := Reader.GetU;
  BitsPerPlane  := Reader.GetU;
  Empty2        := Reader.GetMU2;
  Empty3        := Reader.GetU;
  BytesPerPixel := Reader.GetU;

  if Head <> 'KRO' then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.AtLeast := Height*Width*BytesPerPixel;

  Bmp.SetSize(Width, Height);

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin

    if BytesPerPixel = 4 then begin

      R := Reader.GetU;
      G := Reader.GetU;
      B := Reader.GetU;
      A := not Reader.GetU;
    end
    else begin

      R := Reader.GetU;
      G := Reader.GetU;
      B := Reader.GetU;
      A := 0;
    end;
    Bmp.SetRGBA(x,y, R,G,B, not A);
  end;

  Reader.Free;
  Result := True;
end;


function GR9_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    j: Integer;
    B,C: Byte;
begin
  if Str.Size <> 40*192 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(80, 192);

  Reader.AtLeast := 40*192;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Ceil(Bmp.Width/2)-1 do begin

    B := Reader.GetU;
    for j:=0 to 1 do begin
      C := Getbits(B, 4-4*j, 4)*16;
      Bmp.SetRGB(2*x + j, y, C,C,C);
    end;
  end;

  Reader.Free;
  Result := True;
end;


function BG9_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    j: Integer;
    B,C: Byte;
    Width, Height: Integer;
begin
  if Str.Size <> 15360 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(160, 192);

  Reader.AtLeast := 80*192;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Ceil(Bmp.Width/4)-1 do begin

    b := Reader.GetU;
    for j:=0 to 1 do begin

      c := getbits(b, 4-4*j, 4)*16;
      Bmp.SetRGB(2*x + j, y, C,C,C);
    end;
  end;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Ceil(Bmp.Width/4)-1 do begin

    b := Reader.GetU;
    for j:=0 to 1 do begin

      c := getbits(b, 4-4*j, 4)*16;
      Bmp.SetRGB(80 + 2*x + j, y, C,C,C);
    end;
  end;


  Reader.Free;
  Result := True;
end;


function GHG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    j: Integer;
    Width, Height: Integer;
    C,B: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Width  := Reader.GetU2;
  Height := Reader.GetU;

  Inc(Width, 2);
  Bmp.SetSize(Width, Height); //max: 320x200

  Reader.AtLeast := Height*Ceil(Width/8);

  for y:=0 to Height-1 do
    for x:=0 to Ceil(Width/8)-1 do begin

      C := Reader.GetU;

      for j:=0 to 7 do begin
        B := Getbits(C, 7-j, 1);
        Bmp.SetMono(8*x + j, y, B);
      end;
    end;

  Reader.Free;
  Result := True;
end;


function AAI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x,y: Integer;
    Width, Height: Integer;
    B,G,R,Null: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Width  := Reader.GetU4;
  Height := Reader.GetU4;

  if Width*Height*4+8 <> Str.Size then begin
    Reader.Free;
    Exit(False);
  end;

  Bmp.SetSize(Width, Height);

  Reader.AtLeast := Height*Width*4;

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin

    B := Reader.GetU;
    G := Reader.GetU;
    R := Reader.GetU;
    Null := Reader.GetU;

    Bmp.SetRGB(x,y, R,G,B);
  end;

  Reader.Free;
  Result := True;
end;

procedure A411_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    x,y: Integer;
    j: Integer;
    UU,VV: Integer;
    P: TPix;
    YY: array[0..3] of Byte;
    YYY,UUU,VVV: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(64, 48);

  for y:=0 to 47 do
  for x:=0 to 15 do begin

    UU := 0;
    VV := 0;

    for j:=0 to 3 do begin
      P := Bmp2[4*x + j, y];

      rgb2yuv(P.R, P.G, P.B, YYY,UUU,VVV);

      YY[j] := YYY;
      UU := UU + UUU;
      VV := VV + VVV;
   end;

    UU := Clip(UU/4);
    VV := Clip(VV/4);

    Writer.PutU(YY[0]);
    Writer.PutU(YY[1]);
    Writer.PutU(YY[2]);
    Writer.PutU(YY[3]);
    Writer.PutU(UU);
    Writer.PutU(VV);
  end;

  Bmp2.Free;
  Writer.Free;
end;

function A411_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Reader: TPV_Reader;
    x, y: Integer;
    RGB: Cardinal;
    R,G,B: Byte;
    j: Integer;
    YY: array[0..3] of Byte;
    U, V: Byte;
begin
  if Str.Size <> 4608 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(64, 48);

  Reader.AtLeast := 96*48;

  for y:=0 to 47 do
    for x:=0 to 15 do begin

      YY[0] := Reader.GetU;
      YY[1] := Reader.GetU;
      YY[2] := Reader.GetU;
      YY[3] := Reader.GetU;
      U     := Reader.GetU;
      V     := Reader.GetU;

      for j:=0 to 3 do begin

        yuv2rgb(YY[j], U, V, R,G,B);

        Bmp.SetRGB(4*x + j,y, R,G,B);
      end;
    end;

    Reader.Free;
    Result := True;
end;

function NLM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    C,G: Byte;
    i: Integer;
    Reader: TPV_Reader;
    Id: String;
    Unk0, Unk1, Unk2, Width, Height: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Id     := Reader.GetS(4);
  Unk0   := Reader.GetU;
  Unk1   := Reader.GetU2;
  Width  := Reader.GetU;
  Height := Reader.GetU;
  Unk2   := Reader.GetU;

  Bmp.SetSize(Width, Height);
  Reader.AtLeast := Height * Ceil(Width/8);

  Bmp.AddPal($66, $CC, $66, $FF);
  Bmp.AddPal(0,0,0, $FF);

  for y:=0 to Height-1 do
  for x:=0 to Ceil(Width/8)-1 do begin

    G := Reader.GetU;
    for i:=0 to 7 do begin

      C := Getbits(G, 7-i, 1);
      Bmp.SetPal(8*x + i, y, C);
    end;
  end;

  Reader.Free;
  Result := True;
end;


function NOL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    G: Byte;
    Reader: TPV_Reader;
    Id: String;
    Unk0, Unk1, Unk2, Unk3, Unk4, Unk5, Unk6, Width, Height: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Id     := Reader.GetS(4);
  Unk0   := Reader.GetU2;
  Unk1   := Reader.GetU;
  Unk2   := Reader.GetU;
  Unk3   := Reader.GetU2;
  Width  := Reader.GetU2;
  Height := Reader.GetU2;
  Unk4   := Reader.GetU2;
  Unk5   := Reader.GetU2;
  Unk6   := Reader.GetU2;

  Bmp.SetSize(Width, Height);
  Reader.AtLeast := Height * Width;

  for y:=0 to Height-1 do
  for x:=0 to Width-1 do begin
    G := Reader.GetU;
    Bmp.SetMono(x,y, 1-(G and 1));
  end;

  Reader.Free;
  Result := True;
end;

function SGI_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    R,G,B,A: Byte;
    Magic, Width, Height,Bpp,Dimension,Compression,Channels,PixMin,PixMax,Ignored,PalType: Integer;
    FName: String;
    Pix: TPix;
    Pixels: array[0..2] of array of array of Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Magic       := Reader.GetMU2; //=474
  Compression := Reader.GetU;   //0=unpacked, 1=RLE
  Bpp         := Reader.GetU;   //Bytes per pixel, 1 or 2
  Dimension   := Reader.GetMU2; //2 or 1 should be fine
  Width       := Reader.GetMU2;
  Height      := Reader.GetMU2;
  Channels    := Reader.GetMU2;
  PixMin      := Reader.GetMU4;
  PixMax      := Reader.GetMU4;
  Ignored     := Reader.GetMU4;
  FName       := Reader.GetS(80);
  PalType     := Reader.GetMU4; //0=normal

  if (Magic <> 474) or (Compression <> 0) then begin
    Reader.Free;
    Exit(False);
  end;

  Reader.Skip(404) ;//zeroes

  Bmp.SetSize(Width, Height);
  Reader.AtLeast := Height * Width*4;

  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do begin
      Bmp.SetB(x,y, Reader.GetMU2 shr 8);
    end;
  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do begin
      Bmp.SetG(x,y, Reader.GetMU2 shr 8);
    end;
  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do begin
      Bmp.SetR(x,y, Reader.GetMU2 shr 8);
    end;
  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do begin
      Bmp.SetA(x,y, Reader.GetMU2 shr 8);
    end;

  Reader.Free;
  Result := True;
end;

procedure RAS_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    x,y: Integer;
    Pix: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutMU4($59A66A95); //Magic
  Writer.PutMU4(Bmp.Width);
  Writer.PutMU4(Bmp.Height);
  Writer.PutMU4(32); //Bpp
  Writer.PutMU4(Bmp.Width*Bmp.Height*4); //ImageSize
  Writer.PutMU4(1); //Typ
  Writer.PutMU4(0); //PalType
  Writer.PutMU4(0); //PalLen

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[x,y];

      Writer.PutU(Pix.B);
      Writer.PutU(Pix.G);
      Writer.PutU(Pix.R);
      Writer.PutU(Pix.A);
    end;
  end;

  Writer.Free;
end;

function RAS_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    R,G,B,A: Byte;
    Magic, Width, Height,Bpp,ImageSize,Typ,PalType,PalLen: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Magic     := Reader.GetMU4;
  Width     := Reader.GetMU4;
  Height    := Reader.GetMU4;
  Bpp       := Reader.GetMU4;
  ImageSize := Reader.GetMU4;
  Typ       := Reader.GetMU4;
  PalType   := Reader.GetMU4;
  PalLen    := Reader.GetMU4;

  //Magic = 59 a6 6a 95 //TODO

  Reader.AtLeast := Height * Width*(Bpp div 8);
  Bmp.SetSize(Width, Height);

  case Bpp of
    24: for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            B := Reader.GetU;
            G := Reader.GetU;
            R := Reader.GetU;

            Bmp.SetRGB(x,y, R,G,B);
          end;
    32: for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            B := Reader.GetU;
            G := Reader.GetU;
            R := Reader.GetU;
            A := Reader.GetU;

            Bmp.SetRGBA(x,y, R,G,B, 255-A);
          end;
  end;

  Reader.Free;
  Result := True;
end;

function WBMP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    G,B: Byte;
    Width, Height, Typ,Fix,Extender: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Typ := Reader.GetV;
  Fix := Reader.GetU;

  if Typ and 1 = 1 then
  Extender := Reader.GetV;

  Width := Reader.GetV;
  Height := Reader.GetV;

  Bmp.SetSize(Width, Height);
  Reader.AtLeast := Height * Ceil(Width/8);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Ceil(Bmp.Width/8)-1 do begin
      G := Reader.GetU;

      for i:=0 to 7 do begin
        B := (G shr (7-i)) and 1;

        Bmp.SetMono(8*X + i,Y, 1-B);
      end;
    end;
  end;

  Reader.Free;
  Result := True;
end;

function BMP_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var BufSize: Integer;
    x,y: Integer;
    i: Integer;
    Reader: TPV_Reader;
    R,G,B,A: Byte;
    GG: Word;
    Bpp: Byte;
    PalSize: Integer;
    PadSize: Integer;
    RowSize: Integer;
    DoFlip: Boolean;
    Magic: String;
    FileSize, Reserved, Offset, HeadSize, Width, Height, Planes, Bits,
    Compression, SizeImage, DpiX, DpiY, Colors, ColorsUsed: Integer;
begin
  Reader := TPV_Reader.Create(Str);

  Magic    := Reader.GetS(2);
  FileSize := Reader.GetU4;
  Reserved := Reader.GetU4;
  Offset   := Reader.GetU4;

  if Magic <> 'BM' then begin
    Reader.Free;
    Exit(False);
  end;

  HeadSize    := Reader.GetU4; //40
  Width       := Reader.GetI4;
  Height      := Reader.GetI4;
  Planes      := Reader.GetU2; //always=1
  Bits        := Reader.GetU2; //32
  Compression := Reader.GetU4; //0
  SizeImage   := Reader.GetU4;
  DpiX        := Reader.GetU4;
  DpiY        := Reader.GetU4;
  Colors      := Reader.GetU4;
  ColorsUsed  := Reader.GetU4;

  Bpp := Bits div 8;;

  if Height < 0 then DoFlip := False
  else               DoFlip := True;

  BufSize := Width*Bpp;
  Height := abs(Height);

  RowSize := Ceil(Width*Bits/8);
  PadSize := 4*Ceil(RowSize/4) - RowSize;

  //TODO: read 565 values BI_BITFIELDS

  Bmp.SetSize(Width, Height);
  Reader.AtLeast := Height * (Width*Bpp+4);  //+4 for row padding

  //read palettes
  if Bits < 10 then begin
    PalSize := 1 shl Bits; //2^Power

    for i:=0 to PalSize-1 do begin
      B := Reader.GetU;
      G := Reader.GetU;
      R := Reader.GetU;
      A := Reader.GetU;

      Bmp.AddPal(R,G,B,255);
    end;
  end;
  Reader.Offset := Offset;

  //========== 32 bits
  if Bits = 32 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Bmp.Width-1 do begin
        B := Reader.GetU;
        G := Reader.GetU;
        R := Reader.GetU;
        A := Reader.GetU;

        Bmp.SetRGBA(x,y, R,G,B,A);
      end;
    end;
  end
  //========== 24 bits
  else if Bits = 24 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Bmp.Width-1 do begin
        B := Reader.GetU;
        G := Reader.GetU;
        R := Reader.GetU;

        Bmp.SetRGB(x,y, R,G,B);
      end;
      Reader.Skip(PadSize);
    end;
  end
  //========== 16 bits
  else if Bits = 16 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Bmp.Width-1 do begin
        GG := Reader.GetU2;

        B := GG and $1F;
        G := (GG shr 5) and $3F;
        R := GG shr 11;

        Bmp.SetRGB(X,Y, R shl 3, G shl 2, B shl 3);
      end;
      Reader.Skip(PadSize);
    end;
  end
  //========== 8 bits
  else if Bits = 8 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Bmp.Width-1 do begin
        G := Reader.GetU;

        Bmp.SetPal(X,Y, G);
      end;
      Reader.Skip(PadSize);
    end;
  end
  //========== 4 bits
  else if Bits = 4 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Ceil(Bmp.Width/2)-1 do begin
        G := Reader.GetU;

        Bmp.SetPal(2*X  ,Y, G shr 4);
        Bmp.SetPal(2*X+1,Y, G and $F);
      end;
      Reader.Skip(PadSize);
    end;
  end
  //========== 2 bits
  else if Bits = 2 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Ceil(Bmp.Width/4)-1 do begin
        G := Reader.GetU;

        for i:=0 to 3 do begin
          B := (G shr (6-2*i)) and 3;

          Bmp.SetPal(4*X + i,Y, B);
        end;
      end;
      Reader.Skip(PadSize);
    end;
  end
  //========== 1 bits
  else if Bits = 1 then begin
    for y:=0 to Bmp.Height-1 do begin
      for x:=0 to Ceil(Bmp.Width/8)-1 do begin
        G := Reader.GetU;

        for i:=0 to 7 do begin
          B := (G shr (7-i)) and 1;

          Bmp.SetPal(8*X + i,Y, B);
        end;
      end;
      Reader.Skip(PadSize);
    end;
  end;

  if DoFlip then Bmp.FlipV;

  Reader.Free;
  Result := True;
end;

function HRZ_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var x,y: Integer;
    Reader: TPV_Reader;
    R,G,B: Byte;
begin
  if Str.Size <> 184320 then Exit(False);

  Reader := TPV_Reader.Create(Str);

  Bmp.SetSize(256, 240);
  Reader.AtLeast := 256*240*3;

  for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      R := Reader.GetU;
      G := Reader.GetU;
      B := Reader.GetU;

      Bmp.SetRGB(x,y, R shl 2,G shl 2,B shl 2);
    end;

  Reader.Free;
  Result := True;
end;

procedure HRZ_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var x,y: Integer;
    Writer: TPV_Writer;
    Bmp2: TPV_Bitmap;
    Pix: TPix;
begin
  Writer := TPV_Writer.Create(Str);

  Bmp2 := TPV_Bitmap.Create;
  Bmp2.CopyFrom(Bmp);
  Bmp2.Resize(256, 240);

  for y:=0 to Bmp2.Height-1 do
    for x:=0 to Bmp2.Width-1 do begin
      Pix := Bmp2.Pixel[x,y];

      Writer.PutU(Pix.R shr 2);
      Writer.PutU(Pix.G shr 2);
      Writer.PutU(Pix.B shr 2);
    end;

  Writer.Free;
  Bmp2.Free;
end;

procedure GIF_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Laz: TLazIntfImage;
    Writer: TFPWriterGIF;
    Bmp2: TBitmap;
begin
  Writer := TFPWriterGIF.Create;

  Laz := TLazIntfImage.Create(0,0);

  try
    Bmp2 := Bmp.ToBitmap;
    Laz.LoadFromBitmap(Bmp2.Handle, Bmp2.MaskHandle);
    Bmp2.Free;
    Laz.SaveToStream(Str, Writer);

  finally
    Writer.Free;
    Laz.Free;
  end;
end;

procedure BMP_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var y,x: Integer;
    Pix: TPix;
    Writer: TPV_Writer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('BM'); //Magic
  Writer.PutU4(54+Bmp.Width*Bmp.Height*4); //File Size
  Writer.PutU4(0); //Reserved
  Writer.PutU4(54); //Offset

  Writer.PutU4(40);          //HeadSize
  Writer.PutI4(Bmp.Width);   //Width
  Writer.PutI4(-Bmp.Height); //Height
  Writer.PutU2(1);    //Planes
  Writer.PutU2(32);   //Bits
  Writer.PutU4(0);    //Compression
  Writer.PutU4(0);    //SizeImage
  Writer.PutU4(2834); //DpiX
  Writer.PutU4(2834); //DpiY
  Writer.PutU4(0);    //Colors
  Writer.PutU4(0);    //ColorsUsed

  for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[x,y];

      Writer.PutU(Pix.B);
      Writer.PutU(Pix.G);
      Writer.PutU(Pix.R);
      Writer.PutU(Pix.A);
    end;

  Writer.Free;
end;


procedure TIFF_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
const Tiff_Long = 4;
      Tiff_Short = 3;

var Writer: TPV_Writer;
    Offset: Integer;
    x,y: Integer;
    Pix: TPix;
    R,G,B: Byte;


    procedure PutTag(Id, Typ, DataCount, Data: Cardinal);
    begin
      Writer.PutU2(Id);
      Writer.PutU2(Typ);
      Writer.PutU4(DataCount);
      Writer.PutU4(Data);
    end;


begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('II');
  Writer.PutU2($2A);
  Writer.PutU4(8);
  Writer.PutU2(13); //DirEntries

  Offset := 10 + 13*12 + 4 + 6; //Header size + (DirEntries * 12Bytes) + NextIFDOffset + Bpp

  PutTag(256, Tiff_Long,  1, Bmp.Width);
  PutTag(257, Tiff_Long,  1, Bmp.Height);
  PutTag(258, Tiff_Short, 3, 10 + 13*12 + 4); //Bpp Offset
  PutTag(259, Tiff_Short, 1, 1); //Compression
  PutTag(262, Tiff_Short, 1, 2);
  PutTag(273, Tiff_Short, 1, Offset); //StripOffsets
  PutTag(277, Tiff_Short, 1, 3); //SamplesPerPixel
  PutTag(278, Tiff_Long,  1, Bmp.Height); //RowsPerStrip
  PutTag(279, Tiff_Long,  1, Bmp.Width*3*Bmp.Height); //StripByteCounts
  PutTag(282, Tiff_Long,  1, 300);
  PutTag(283, Tiff_Long,  1, 300);
  PutTag(284, Tiff_Short, 1, 1);
  PutTag(296, Tiff_Short, 1, 2);

  //270 (description)

  Writer.PutU4(0); //NextIFDOffset

  //BPP
  Writer.PutU2(8);
  Writer.PutU2(8);
  Writer.PutU2(8);

  for y:=0 to Bmp.Height-1 do
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[X,Y];

      Writer.PutU(Pix.R);
      Writer.PutU(Pix.G);
      Writer.PutU(Pix.B);
  end;

  Writer.Free;
end;

procedure PNG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    Pix: TPix;
    x,y: Integer;
    Deflate: TCompressionStream;
    Mem: TMemoryStream;
    Writer2: TPV_Writer;
    CRC: TPV_CRC32Stream;
    Len: Integer;
begin
  CRC := TPV_CRC32Stream.Create(Str);
  Writer := TPV_Writer.Create(CRC);

  Writer.PutU(137);
  Writer.PutS('PNG');
  Writer.PutU(13);
  Writer.PutU(10);
  Writer.PutU(26);
  Writer.PutU(10);

  Writer.PutMU4(13);

  Writer.Flush;
  CRC.Clear;

  Writer.PutS('IHDR');
  Writer.PutMU4(Bmp.Width);
  Writer.PutMU4(Bmp.Height);
  Writer.PutU(8);
  Writer.PutU(6);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.Flush;

  Writer.PutMU4(CRC.Final); //CRC

  Mem := TMemoryStream.Create;
  Deflate := TCompressionStream.Create(zstream.clmax, Mem, False);

  Writer2 := TPV_Writer.Create(Deflate);

  for y:=0 to Bmp.Height-1 do begin
    Writer2.PutU(0);

    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[X,Y];

      Writer2.PutU(Pix.R);
      Writer2.PutU(Pix.G);
      Writer2.PutU(Pix.B);
      Writer2.PutU(Pix.A);
    end;
  end;

  Writer2.Free;
  Deflate.Free;
  Mem.Position := 0;


  Writer.PutMU4(Mem.Size);
  Writer.Flush;
  CRC.Clear;

  Writer.PutS('IDAT');
  Writer.CopyFrom(Mem, Mem.Size);
  Writer.Flush;

  Writer.PutMU4(CRC.Final); //CRC
  Mem.Free;

  Writer.PutMU4(0);
  Writer.PutS('IEND');
  Writer.PutMU4($AE426082); //CRC

  Writer.Free;
  CRC.Free;
end;

procedure PCX_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
type THead = packed record
  Identifier: Byte;
  Version: Byte;
  Encoding: Byte;
  BitsPerPixel: Byte;
  XStart: Word;
  YStart: Word;
  XEnd: Word;
  YEnd: Word;
  HorzResolution: Word;
  VertResolution: Word;
  Palette: array[0..47] of Byte;
  Reserved1: Byte;
  NumBitPlanes: Byte;
  BytesPerLine: Word;
  PaletteType: Word;
  HorzScreenSize: Word;
  VertScreenSize: Word;
  Reserved2: array[0..53] of Byte;
end;

var Head: THead;
    Writer: TPV_Writer;
    Pix: TPix;
    R,G,B: Byte;
    x,y: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Head.Identifier     := 10;
  Head.Version        := 5;
  Head.Encoding       := 0;
  Head.BitsPerPixel   := 8;
  Head.XStart         := 0;
  Head.YStart         := 0;
  Head.XEnd           := Bmp.Width-1;
  Head.YEnd           := Bmp.Height-1;
  Head.HorzResolution := 300;
  Head.VertResolution := 300;

  Head.Reserved1      := 0;
  Head.NumBitPlanes   := 3;
  Head.BytesPerLine   := Bmp.Width;
  Head.PaletteType    := 1;
  Head.HorzScreenSize := 0;
  Head.VertScreenSize := 0;

  FillChar(Head.Palette, 48, 0);
  FillChar(Head.Reserved2, 54, 0);

  Writer.Put(Head, SizeOf(Head));

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Bmp.Width-1 do begin
      R := Bmp.Pixel[X,Y].R;
      Writer.PutU(R);
    end;

    for x:=0 to Bmp.Width-1 do begin
      G := Bmp.Pixel[X,Y].G;
      Writer.PutU(G);
    end;

    for x:=0 to Bmp.Width-1 do begin
      B := Bmp.Pixel[X,Y].B;
      Writer.PutU(B);
    end;
  end;

  Writer.Free;
end;

procedure TGA_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
type THead = packed record
  IdLen: Byte;
  ColorMap: Byte;
  ImageType: Byte;
  FirstIndex: Word;
  ColorCount: Word;
  ColorBits: Byte;
  LeftCornerX: Word;
  LeftCornerY: Word;
  Width: Word;
  Height: Word;
  BytesPerPixel: Byte;
  Flags: Byte;
end;

var Head: THead;
    Writer: TPV_Writer;
    Pix: TPix;
    x,y: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Head.IdLen        := 0;
  Head.ColorMap     := 0;
  Head.ImageType    := 2;
  Head.FirstIndex   := 0;
  Head.ColorCount   := 0;
  Head.ColorBits    := 0;
  Head.LeftCornerX  := 0;
  Head.LeftCornerY  := 0;
  Head.Width        := Bmp.Width;
  Head.Height       := Bmp.Height;
  Head.BytesPerPixel:= 32;
  Head.Flags        := 32;

  Writer.Put(Head, SizeOf(Head));

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    Pix := Bmp.Pixel[X,Y];

    Writer.PutU(Pix.R);
    Writer.PutU(Pix.G);
    Writer.PutU(Pix.B);
    Writer.PutU(Pix.A);
  end;

  Writer.Free;
end;

procedure PSD_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Writer: TPV_Writer;
    R,G,B: Byte;
    x,y: Integer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('8BPS');

  Writer.PutMU2(1);
  Writer.PutS(chr(0)+chr(0)+chr(0)+chr(0)+chr(0)+chr(0));
  Writer.PutMU2(3);
  Writer.PutMU4(Bmp.Height);
  Writer.PutMU4(Bmp.Width);
  Writer.PutMU2(8);
  Writer.PutMU2(3);
  Writer.PutMU4(0);
  Writer.PutMU4($1C);

  Writer.PutS('8BIM');
  Writer.PutU(3);
  Writer.PutU($ED);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.PutU(0);
  Writer.PutU($10);
  Writer.PutU(0);
  Writer.PutU($48);

  Writer.PutMU2(1);
  Writer.PutMU2(1);
  Writer.PutMU2(1);
  Writer.PutMU2($48);
  Writer.PutMU2(1);
  Writer.PutMU2(1);
  Writer.PutMU2(1);
  Writer.PutMU2(0);
  Writer.PutMU2(0);
  Writer.PutMU2(0);


  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    R := Bmp.Pixel[X,Y].R;
    Writer.PutU(R);
  end;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    G := Bmp.Pixel[X,Y].G;
    Writer.PutU(G);
  end;

  for y:=0 to Bmp.Height-1 do
  for x:=0 to Bmp.Width-1 do begin
    B := Bmp.Pixel[X,Y].B;
    Writer.PutU(B);
  end;

  Writer.Free;
end;


procedure PGM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var y,x: Integer;
    Pix: TPix;
    G: Byte;
    Writer: TPV_Writer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('P5'+#10);
  Writer.PutS(IntToStr(Bmp.Width));
  Writer.PutS(' ');
  Writer.PutS(IntToStr(Bmp.Height));
  Writer.PutS(#10+'255'+#10);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[X,Y];

      G := (Pix.R + Pix.G + Pix.B) div 3;

      Writer.PutU(G);
    end;
  end;

  Writer.Free;
end;

procedure PPM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var y,x: Integer;
    Pix: TPix;
    Writer: TPV_Writer;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('P6'+#10);
  Writer.PutS(IntToStr(Bmp.Width));
  Writer.PutS(' ');
  Writer.PutS(IntToStr(Bmp.Height));
  Writer.PutS(#10+'255'+#10);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[X,Y];

      Writer.PutU(Pix.R);
      Writer.PutU(Pix.G);
      Writer.PutU(Pix.B);
    end;
  end;

  Writer.Free;
end;

procedure PBM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var y,x,i: Integer;
    Pix: TPix;
    Writer: TPV_Writer;
    R,G: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('P4'+#10);
  Writer.PutS(IntToStr(Bmp.Width));
  Writer.PutS(' ');
  Writer.PutS(IntToStr(Bmp.Height));
  Writer.PutS(#10);

  for y:=0 to Bmp.Height-1 do begin
    for x:=0 to Ceil(Bmp.Width/8)-1 do begin

      G := 0;
      for i:=0 to 7 do begin
        R := Bmp.Pixel[8*X + i,Y].R and 1;
        G := G + (R shl (7-i));
      end;

      Writer.PutU(not G);
    end;
  end;

  Writer.Free;
end;

procedure PFM_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var y,x,i: Integer;
    Pix: TPix;
    Writer: TPV_Writer;
    R,G: Byte;
begin
  Writer := TPV_Writer.Create(Str);

  Writer.PutS('PF'+#10);
  Writer.PutS(IntToStr(Bmp.Width));
  Writer.PutS(' ');
  Writer.PutS(IntToStr(Bmp.Height));
  Writer.PutS(#10);
  Writer.PutS('1.0');
  Writer.PutS(#10);

  for y:=Bmp.Height-1 downto 0 do
    for x:=0 to Bmp.Width-1 do begin
      Pix := Bmp.Pixel[x,y];

      Writer.PutMF(Pix.R / 255);
      Writer.PutMF(Pix.G / 255);
      Writer.PutMF(Pix.B / 255);
    end;

  Writer.Free;
end;

function Clip(V: Single): Byte;
var Temp: Integer;
begin
  V := V * 255;

  if V > 254 then Result := 255
  else if V < 1 then Result := 0
  else Result := Round(V);
end;

function PPM_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Magic1: Char;
    Magic2: Char;
    Width,Height,MaxVal,MinVal: Integer;
    x,y,i: Integer;
    Reader: TPV_Reader;
    R,G,B,A: Byte;
begin
  Reader := TPV_Reader.Create(Str);

  Magic1 := Reader.GetC;
  Magic2 := Reader.GetC;

  if Magic1 <> 'P' then Exit(False);
  if not (Magic2 in ['1'..'6','F']) then Exit(False);

  Reader.GetWhite;

  Width := Reader.GetNum;
  Reader.GetWhite;

  Height := Reader.GetNum;
  Reader.GetWhite;

  if not (Magic2 in ['1','4']) then begin
    MaxVal := Reader.GetNum;
    Reader.GetU;
  end;

  if Magic2 in ['F'] then begin
    MinVal := Reader.GetNum;
    Reader.GetU;
  end;

  Bmp.SetSize(Width, Height);

  case Magic2 of
   //PBM
   '1': for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            G := Reader.GetNum; Reader.GetU;

            Bmp.SetMono(x,y, G and 1);
          end;
   '4': for y:=0 to Bmp.Height-1 do
          for x:=0 to Ceil(Bmp.Width/8)-1 do begin
            G := Reader.GetU;

            for i:=0 to 7 do begin
              B := G shr (7-i);

              Bmp.SetMono(8*x+i,y, B and 1);
            end;
          end;
   //PGM
   '2': for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            G := Reader.GetNum; Reader.GetU;

            Bmp.SetRGB(x,y, G,G,G);
          end;
   '5': for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            G := Reader.GetU;

            Bmp.SetRGB(x,y, G,G,G);
          end;
   //PPM
   '3': for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            R := Reader.GetNum; Reader.GetU;
            G := Reader.GetNum; Reader.GetU;
            B := Reader.GetNum; Reader.GetU;

            Bmp.SetRGB(x,y, R,G,B);
          end;
   '6': for y:=0 to Bmp.Height-1 do
          for x:=0 to Bmp.Width-1 do begin
            R := Reader.GetU;
            G := Reader.GetU;
            B := Reader.GetU;

            Bmp.SetRGB(x,y, R,G,B);
          end;
   //PFM
   'F': for y:=Bmp.Height-1 downto 0 do
          for x:=0 to Bmp.Width-1 do begin
            R := Clip(Reader.GetMF);
            G := Clip(Reader.GetMF);
            B := Clip(Reader.GetMF);

            Bmp.SetRGB(x,y, R,G,B);
          end;
  end;

  Reader.Free;
  Result := True;
end;

function PSD_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPCustomImage;
    Reader: TFPReaderPSD;
begin
  Result := False;
  Reader := TFPReaderPSD.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);

    //TODO Bmp.FBitmap.SetSize(Laz.Width, Laz.Height);
    //TODO Bmp.FBitmap.Assign(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;
{
function XWD_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPCustomImage;
    Reader: TFPReaderXWD;
begin
  Result := False;
  Reader := TFPReaderXWD.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;
 }
function GIF_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderGIF;
begin
  Result := False;
  Reader := TFPReaderGIF.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;

function TIFF_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderTIFF;
begin
  Result := False;
  Reader := TFPReaderTIFF.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;

function PNG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderPNG;
begin
  Result := False;
  Reader := TFPReaderPNG.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;

function TGA_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderTarga;
begin
  Result := False;
  Reader := TFPReaderTarga.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;

function PCX_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderPCX;
begin
  Result := False;
  Reader := TFPReaderPCX.Create;
  Laz := TFPMemoryImage.Create(1, 1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;

procedure JPG_Write(Bmp: TPV_Bitmap; Str: TStream; Compression: Byte);
var Laz: TLazIntfImage;
    Writer: TFPWriterJPEG;
begin
  Writer := TFPWriterJPEG.Create;
  Writer.CompressionQuality := Compression;

  Laz := TLazIntfImage.Create(0,0);

  try
    //TODO Laz.LoadFromBitmap(Bmp.FBitmap.Handle, Bmp.FBitmap.MaskHandle);
    Laz.SaveToStream(Str, Writer);

  finally
    Writer.Free;
    Laz.Free;
  end;
end;

function JPG_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;
var Laz: TFPMemoryImage;
    Reader: TFPReaderJPEG;
begin
  Result := False;
  Reader := TFPReaderJPEG.Create;
  Laz := TFPMemoryImage.Create(1,1);

  try
    Laz.LoadFromStream(Str, Reader);
    Bmp.CopyFrom(Laz);
  finally
    Reader.Free;
    Laz.Free;
    Result := True;
  end;
end;


function WZL_Read(Bmp: TPV_Bitmap; Str: TStream): Boolean;

//256 bytes xored with 13 except that a Windows Bitmap

var Reader: TPV_Reader;
    Width, Height: Integer;
    R,G,B,A: Byte;
    x,y: Integer;
    i: Integer;
    P: Byte;
    id: Word;
    Str2: TMemoryStream;
    Buff: array of Byte;
    Len: Integer;
begin
  Str.Read(Id, 2);

  if (id <> 16463) then Exit(False);

  SetLength(Buff, 256);

  Str2 := TMemoryStream.Create;

  Str.Position := 0;
  Str.Read(Buff[0], 256);

  for i:=0 to 255 do begin
    Buff[i] := Buff[i] xor 13;
  end;

  Str2.Write(Buff[0], 256);

  while Str.Position < Str.Size do begin
    Len := Str.Read(Buff[0], 256);

    Str2.Write(Buff[0], Len);
  end;

  Str2.Position := 0;

  Result := BMP_Read(Bmp, Str2);
  Str2.Free;
end;

//{$INCLUDE MORE.pas}
//{$INCLUDE OKAY.pas}

initialization

//  BitmapFormats.Add('all', @ALL_Read, nil, '');
//  BitmapFormats.Add('cip', @CIP_Read, nil, '');
//  BitmapFormats.Add('hgr', @HGR_Read, nil, '');
//  BitmapFormats.Add('kps', @KPS_Read, nil, '');
//  BitmapFormats.Add('mch', @MCH_Read, nil, '');
//  BitmapFormats.Add('ozb', @OZB_Read, nil, '');
//  BitmapFormats.Add('ozt', @OZT_Read, nil, '');
//  BitmapFormats.Add('pgx2', @PGX2_Read, nil, '');

  BitmapFormats.Add('lbm', @LBM_Read, nil, 'Amiga LBM');
  BitmapFormats.Add('iff', @LBM_Read, nil, 'Amiga LBM');

  BitmapFormats.Add('pi4', @PI4_Read, nil, 'Degas Extended');
  BitmapFormats.Add('pi5', @PI5_Read, nil, 'Degas Extended');  //medium
  BitmapFormats.Add('pi7', @PI7_Read, nil, 'Degas Extended');
  BitmapFormats.Add('pi8', @PI8_Read, nil, 'Degas Extended');
  BitmapFormats.Add('pi9', @PI9_Read, nil, 'Degas Extended');

  BitmapFormats.Add('4mi', @A4MI_Read, nil, 'Atari 4MI');
  BitmapFormats.Add('4pl', @A4PL_Read, nil, 'Atari 4PL');
  BitmapFormats.Add('fts', @FITS_Read, nil, 'Flexible Image Transport System');
  BitmapFormats.Add('fit', @FITS_Read, nil, 'Flexible Image Transport System');
  BitmapFormats.Add('fits', @FITS_Read, nil, 'Flexible Image Transport System');
  BitmapFormats.Add('g10', @G10_Read, nil, 'Atari G10');
  BitmapFormats.Add('gfb', @GFB_Read, nil, 'Atari GFB');
  BitmapFormats.Add('gp',  @GP_Read, nil, 'Atari GP');
  BitmapFormats.Add('mic', @MIC_Read, nil, 'Movie Maker Background');
  BitmapFormats.Add('mis', @MIS_Read, nil, 'Atari Missile');
  BitmapFormats.Add('mtv', @MTV_Read, nil, 'MTV / RayShade Image ');
  BitmapFormats.Add('otb', @OTB_Read, nil, 'Nokia Over The Air ');
  BitmapFormats.Add('pam', @PAM_Read, nil, 'Portable Arbitrary Map ');
  BitmapFormats.Add('pcl', @PCL_Read, nil, 'PCL');
  BitmapFormats.Add('pgx', @PGX_Read, nil, 'PGX (JPEG 2000)');
  BitmapFormats.Add('pi2', @PI2_Read, nil, 'Degas/Degas Elite ');      //medium res
  BitmapFormats.Add('pi3', @PI3_Read, nil, 'Degas/Degas Elite ');      //high-res
  BitmapFormats.Add('pla', @PLA_Read, nil, 'Atari PLA ');
  BitmapFormats.Add('shc', @SHC_Read, nil, 'SAMAR Hi-res');
  BitmapFormats.Add('v2i', @V2I_Read, nil, 'Voyage 200 ');
  BitmapFormats.Add('ska', @SKA_Read, nil, 'Video renting box SKA');
  BitmapFormats.Add('sxs', @SXS_Read, nil, 'SXS font ');
  BitmapFormats.Add('shp', @SHP_Read, nil, 'Movie Maker Shape');
  BitmapFormats.Add('inp', @INP_Read, nil, 'Atari INP');


  BitmapFormats.Add('ps1', @PS1_Read, nil, 'ST-DEXL Pictures Set'); //no sample
  BitmapFormats.Add('sct', @SCT_Read, nil, 'SciTex');            //bad

//  BitmapFormats.Add('wrl', @WRL_Read, nil, '');






  BitmapFormats.Add('pc1', @PC1_Read, nil, 'Degas Elite');
  BitmapFormats.Add('pac', @PAC_Read, nil, 'Stad PAC');
  BitmapFormats.Add('cpr', @CPR_Read, nil, 'Atari CPR');
  BitmapFormats.Add('pgc', @PGC_Read, nil, 'Atari PGC');
  BitmapFormats.Add('ngf', @NGF_Read, nil, 'Ninell Graphic Format');
  BitmapFormats.Add('mac', @MAC_Read, nil, 'MacPaint');
  BitmapFormats.Add('msp', @MSP_Read, @MSP_Write, 'Microsoft Paint'); //Paint v.1, v.2

  BitmapFormats.Add('vzi', @VZI_Read, nil, 'Atari VZI');
  BitmapFormats.Add('wzl', @WZL_Read, nil, 'Winzle Puzzle');

  BitmapFormats.Add('jj',  @JJ_Read,  nil, 'Doodle, packed');
  BitmapFormats.Add('gg',  @GG_Read,  nil, 'Koala Painter 2, packed');
  BitmapFormats.Add('ami', @AMI_Read, nil, 'Amica Paint, packed');

  BitmapFormats.Add('koa', @KOA_Read, nil, 'Koala Painter');
  BitmapFormats.Add('sar', @SAR_Read, nil, 'Saracen Paint');
  BitmapFormats.Add('drz', @DRZ_Read, nil, 'Drazpaint');
  BitmapFormats.Add('rpm', @RPM_Read, nil, 'Run Paint');
  BitmapFormats.Add('rp',  @RP_Read,  nil, 'Rainbow Painter');
  BitmapFormats.Add('pmg', @PMG_Read, nil, 'Paint Magic');
  BitmapFormats.Add('pi',  @PI_Read,  nil, 'Blazing Paddles');
  BitmapFormats.Add('p64', @P64_Read, nil, 'Picasso 64');
  BitmapFormats.Add('ocp', @OCP_Read, nil, 'Advanced Art Studio');
  BitmapFormats.Add('mil', @MIL_Read, nil, 'Micro Illustrator');
  BitmapFormats.Add('ism', @ISM_Read, nil, 'Image System');
  BitmapFormats.Add('ipt', @IPT_Read, nil, 'Interpaint Multicolor');
  BitmapFormats.Add('gig', @GIG_Read, nil, 'Gigapaint Multicolor');
  BitmapFormats.Add('fpt', @FPT_Read, nil, 'Face Painter');
  BitmapFormats.Add('dol', @DOL_Read, nil, 'Dolphin Ed');
  BitmapFormats.Add('cwg', @CWG_Read, nil, 'Create with Garfield');
  BitmapFormats.Add('che', @CHE_Read, nil, 'Cheese');
  BitmapFormats.Add('cdu', @CDU_Read, nil, 'CDU-Paint');
  BitmapFormats.Add('a64', @A64_Read, nil, 'Artist 64');

  BitmapFormats.Add('iph', @IPH_Read, nil, 'Interpaint');
  BitmapFormats.Add('mon', @MON_Read, nil, 'Mono Magic');
  BitmapFormats.Add('ish', @ISH_Read, nil, 'Image System');
  BitmapFormats.Add('hed', @HED_Read, nil, 'Hi-Eddi');
  BitmapFormats.Add('hir', @HIR_Read, nil, 'Hires-Bitmap');
  BitmapFormats.Add('hbm', @HIR_Read, nil, 'Hires-Bitmap');
  BitmapFormats.Add('gih', @GIH_Read, nil, 'Gigapaint-Hires');
  BitmapFormats.Add('gcd', @GCD_Read, nil, 'Gigacad');
  BitmapFormats.Add('fgs', @FGS_Read, nil, 'Fun Graphics Machine');
  BitmapFormats.Add('dd',  @DD_Read,  nil, 'Doodle');

  BitmapFormats.Add('565', @A565_Read, @A565_Write, 'OLPC 565');
  BitmapFormats.Add('73i', @A73I_Read, nil, 'Texas Instruments');
  BitmapFormats.Add('82i', @A82I_Read, nil, 'Texas Instruments');
  BitmapFormats.Add('83i', @A83I_Read, nil, 'Texas Instruments');
  BitmapFormats.Add('85i', @A85I_Read, nil, 'Texas Instruments');
  BitmapFormats.Add('86i', @A86I_Read, nil, 'Texas Instruments');
  //BitmapFormats.Add('9xi', @A9XI_Read, nil, 'Texas Instruments');
  //BitmapFormats.Add('92i', @A92I_Read, nil, 'Texas Instruments'); //Ti-92 Bitmap file

  BitmapFormats.Add('bkg', @BKG_Read, nil, 'Movie Maker Background');
  BitmapFormats.Add('bob', @BOB_Read, @BOB_Write, 'Bob Ray Tracer bitmap');
  BitmapFormats.Add('bru', @BRU_Read, nil, 'DEGAS Elite brush');
  BitmapFormats.Add('cm',  @CM_Read, @CM_Write, 'Puzzle Image'); //Puzzle image (X11)
  BitmapFormats.Add('csv', @CSV_Read, @CSV_Write, 'Comma-Separated Value');    //grayscale
  BitmapFormats.Add('dds', @DDS_Read, nil, 'Direct Show Surface');

  BitmapFormats.Add('dis', @DIS_Read, @DIS_Write, 'DKB Trace');    //DKB Trace / Qrt raytrace
  BitmapFormats.Add('esm', @ESM_Read, nil, 'Enhanced Simplex');
  BitmapFormats.Add('fmv', @FMV_Read, nil, 'FrameMaker');
  BitmapFormats.Add('fnt', @FNT_Read, nil, 'Atari Font');
  BitmapFormats.Add('gbr', @GBR_Read, @GBR_Write, 'GIMP Brush'); //v2
  BitmapFormats.Add('gr7', @GR7_Read, nil, 'Atari GR7');
  BitmapFormats.Add('gr8', @GR8_Read, @GR8_Write, 'Atari GR8');
  BitmapFormats.Add('grfx', @GRFX_Read, nil, 'AMI BIOS Splash');
  BitmapFormats.Add('iim', @IIM_Read, @IIM_Write, 'InShape IIM');
  BitmapFormats.Add('ipi', @IPI_Read, @IPI_Write, 'The Complete Morph');
  BitmapFormats.Add('ipl', @IPL_Read, nil, 'Scanalytics IPLab');
  BitmapFormats.Add('jgp', @JGP_Read, nil, 'Jet Graphics Planner');
  BitmapFormats.Add('mbg', @MBG_Read, @MBG_Write, 'Mad Designer');
  BitmapFormats.Add('mbm', @MBM_Read, @MBM_Write, 'Psion Series 5 Bitmap');
  BitmapFormats.Add('mgp', @MGP_Read, nil, 'Magic Painter');
  BitmapFormats.Add('neo', @NEO_Read, nil, 'NeoChrome');
  BitmapFormats.Add('ngg', @NGG_Read, @NGG_Write, 'Nokia Group Graphics');
  BitmapFormats.Add('npm', @NPM_Read, @NPM_Write, 'Nokia Picture Message');
  BitmapFormats.Add('nsr', @NSR_Read, nil, 'Newsroom');
  BitmapFormats.Add('pa3', @PA3_Read, nil, 'Pablo Paint 2.5');
  BitmapFormats.Add('pdb', @PDB_Read, nil, 'Palm Pilot Image');
  BitmapFormats.Add('pgf', @PGF_Read, @PGF_Write, 'Portfolio Graphics');
  BitmapFormats.Add('pnt', @PNT_Read, nil, 'Ark: Survival Evolved');
  BitmapFormats.Add('psa', @PSA_Read, @PSA_Write, 'Print Shop');
  BitmapFormats.Add('ptx', @PTX_Read, nil, 'V.Flash PTX');
  BitmapFormats.Add('tim', @TIM_Read, @TIM_Write, 'Playstation TIM');
  BitmapFormats.Add('vbm', @VBM_Read, @VBM_Write, 'VDC BitMap');   //VBM (VDC BitMap) from C64 operating system ACE
  BitmapFormats.Add('viff', @VIFF_Read, nil, 'Khoros Visualization');  //Khoros Visualization Image File Format
  BitmapFormats.Add('vst', @VST_Read, @VST_Write, 'Vista');
  BitmapFormats.Add('xwd', @XWD_Read, nil, 'X Window Dump');

  BitmapFormats.Add('b_w', @B_W_Read, @B_W_Write, 'ImageLab');
  BitmapFormats.Add('chs', @CHS_Read, @CHS_Write, 'Oric');
  BitmapFormats.Add('64c', @A64C_Read, @A64C_Write, 'C-64 font');
  BitmapFormats.Add('drg', @DRG_Read, @DRG_Write , 'Atari CAD');
  BitmapFormats.Add('lum', @LUM_Read, nil, 'Technicolor Dream');
  BitmapFormats.Add('vollabel', @VOLLABEL_Read, @VOLLABEL_Write , 'Apple Volume Label');
  BitmapFormats.Add('sc8', @SC8_Read, nil, 'MSX SCreen 8');
  BitmapFormats.Add('skp', @SKP_Read, @SKP_Write, 'Sketch-PadDles');
  BitmapFormats.Add('sxg', @SXG_Read, nil, 'Speccy eXtended Graphics');
  BitmapFormats.Add('xga', @XGA_Read, @XGA_Write , 'Atari Falcon');
  BitmapFormats.Add('rwl', @RWL_Read, @RWL_Write , 'IMG Scan');
  BitmapFormats.Add('pg', @PG_Read, nil, 'Printfox/Pagefox');
  BitmapFormats.Add('kfx', @KFX_Read, @KFX_Write , 'Atari KFX'); //Atari 8-bit
  BitmapFormats.Add('doo', @DOO_Read, @DOO_Write , 'Atari DOO');
  BitmapFormats.Add('dap', @DAP_Read, @DAP_Write, 'Atari Slideshow'); //Atari 8-bit SlideShow for VBXE
  BitmapFormats.Add('da4', @DA4_Read, @DA4_Write , 'PaintShop');
  BitmapFormats.Add('nol', @NOL_Read, nil, 'Nokia Operator Logo');
  BitmapFormats.Add('nlm', @NLM_Read, @NLM_Write , 'Nokia Logo Manager');
  BitmapFormats.Add('411', @A411_Read, @A411_Write, 'Sony Mavica'); //YUV 4:1:1
  BitmapFormats.Add('aai', @AAI_Read, @AAI_Write , 'Dune HD');

  BitmapFormats.Add('hrz', @HRZ_Read, @HRZ_Write, 'Slow-scan Television');




  BitmapFormats.Add('pat', @PAT_Read, @PAT_Write, 'GIMP Pattern'); //version 1
  BitmapFormats.Add('cel', @CEL_Read, @CEL_Write, 'Autodesk Animator');
  BitmapFormats.Add('gr9', @GR9_Read, @GR9_Write, 'Atari GR9');
  BitmapFormats.Add('ghg', @GHG_Read, @GHG_Write, 'Gephard Hires Graphics');
  BitmapFormats.Add('bg9', @BG9_Read, @BG9_Write, 'Atari BG9');
  BitmapFormats.Add('avs', @AVS_Read, @AVS_Write, 'Stardent AVS X');
  BitmapFormats.Add('kro', @KRO_Read, @KRO_Write, 'AutoPano RAW');
  BitmapFormats.Add('pic', @PIC_Read, @PIC_Write, 'Bio-Rad');

  BitmapFormats.Add('sgi', @SGI_Read, @SGI_Write, 'Silicon Graphics Image');
  BitmapFormats.Add('ras', @RAS_Read, @RAS_Write, 'Sun Raster');
  BitmapFormats.Add('wbmp', @WBMP_Read, @WBMP_Write, 'Wireless Protocol Bitmap');

  BitmapFormats.Add('bmp', @BMP_Read, @BMP_Write, 'Windows Bitmap');
  BitmapFormats.Add('2bp', @BMP_Read, @BMP_Write, 'Windows CE Bitmap');

  BitmapFormats.Add('pcx', @PCX_Read, @PCX_Write, 'ZSoft PiCture eXchange');
  BitmapFormats.Add('jpg', @JPG_Read, @JPG_Write, 'Joint Pictures Expert Group');
  BitmapFormats.Add('jpeg', @JPG_Read, @JPG_Write, 'Joint Pictures Expert Group');
  BitmapFormats.Add('jpe', @JPG_Read, @JPG_Write, 'Joint Pictures Expert Group');
  BitmapFormats.Add('tga', @TGA_Read, @TGA_Write, 'Truevision Targa');
  BitmapFormats.Add('tif', @TIFF_Read, @TIFF_Write, 'Thousands of Incompatible File Formats');
  BitmapFormats.Add('tiff', @TIFF_Read, @TIFF_Write, 'Thousands of Incompatible File Formats');
  BitmapFormats.Add('png', @PNG_Read, @PNG_Write, 'Portable Network Graphic');
  BitmapFormats.Add('pnm', @PPM_Read, @PPM_Write, 'Portable aNyMap');
  BitmapFormats.Add('ppm', @PPM_Read, @PPM_Write, 'Portable PixMap');
  BitmapFormats.Add('pgm', @PPM_Read, @PGM_Write, 'Portable GrayMap');
  BitmapFormats.Add('pbm', @PPM_Read, @PBM_Write, 'Portable BitMap');
  BitmapFormats.Add('pfm', @PPM_Read, @PFM_Write, 'Portable Float Map');
  BitmapFormats.Add('gif', @GIF_Read, @GIF_Write, 'Graphics Interchange Format');
  BitmapFormats.Add('psd', @PSD_Read, @PSD_Write, 'Adobe Photoshop Document');
  //BitmapFormats.Add('xwd', @XWD_Read, nil);

end.
