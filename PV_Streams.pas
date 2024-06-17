unit PV_Streams;

//Lazzy Image Viewer
//github.com/PascalVault
//License: MIT

interface

uses Classes, SysUtils, Dialogs;

type
   { TPV_Reader }

   TPV_Reader = class
   private
     FStream: TStream;
     FSize: Integer;
     Buf: array of Byte;
     FPos: Integer;

     procedure SetOffset(Offset: Integer);
     function GetOffset: Integer;
     procedure SetAtLeast(Amount: Integer);
   public
     function GetU: Byte; inline;
     function GetU2: Word; inline;
     function GetU3: Cardinal; inline;
     function GetU4: Cardinal; inline;

     function GetMU2: Word; inline;
     function GetMU4: Cardinal; inline;

     function GetI: ShortInt; inline;
     function GetI2: Smallint; inline;
     function GetI4: LongInt; inline;
     function GetMI2: Smallint; inline;
     function GetMI4: LongInt; inline;

     function GetF: Single; inline;
     function GetMF: Single; inline; //Single
     function GetV: Int64; inline; //variable-length integer

     property Offset: Integer read GetOffset write SetOffset;
     property AtLeast: Integer write SetAtLeast;

     function Get(var Buffer; Count: Longint): Longint;
     function GetC: Char; inline;
     function GetNum: Integer;
     function GetWhite: String;
     function GetS(Count: Integer): String;
     procedure Skip(Count: Integer);
     constructor Create(Str: TStream);
   end;

   { TPV_Writer }
   TPV_Writer = class
   private
     FStream: TStream;
     Buf: array of Byte;
     FPos: Integer;
     FSize: Integer;
   public
     procedure Flush;

     procedure Put(const Buffer; Count: Integer);
     procedure PutU(V: Byte); inline;
     procedure PutU2(V: Word); inline;
     procedure PutU4(V: Cardinal); inline;
     procedure PutMU2(V: Word); inline;
     procedure PutMU4(V: Cardinal); inline;

     procedure PutI(V: ShortInt); inline;
     procedure PutI2(V: Smallint); inline;
     procedure PutI4(V: LongInt); inline;
     procedure PutMI2(V: Smallint); inline;
     procedure PutMI4(V: LongInt); inline;

     procedure PutMF(V: Single); inline;
     procedure PutF(V: Single); inline;

     procedure PutV(V: Word); inline;

     procedure Skip(Len: Integer);

     procedure PutS(S: String); inline;
     procedure CopyFrom(Str: TStream; Count: Integer);

     constructor Create(Str: TStream);
     destructor Destroy; override;
   end;

   function Getbits(Val: Word; Index, Count: Integer): Word;

implementation

function Getbits(Val: Word; Index, Count: Integer): Word;
var Res: Word;
begin
  Res := Val shr Index;
  case Count of
    0: Result := 0;
    1: Result := Res and 1;
    2: Result := Res and 3;
    3: Result := Res and 7;
    4: Result := Res and 15;
    5: Result := Res and 31;
    6: Result := Res and 63;
    7: Result := Res and 127;

    8: Result := Res and 255;
    9: Result := Res and 511;
    10: Result := Res and 1023;
    11: Result := Res and 2047;
    12: Result := Res and 4095;
    13: Result := Res and 8191;
    14: Result := Res and 16383;
    15: Result := Res and 32767;
  end;
end;

{ TPV_Reader }

procedure TPV_Reader.SetOffset(Offset: Integer);
begin
  FPos := Offset;
end;

function TPV_Reader.GetOffset: Integer;
begin
  Result := FPos;
end;

procedure TPV_Reader.SetAtLeast(Amount: Integer);
begin
  if FSize-FPos < Amount then SetLength(Buf, FPos+Amount);
end;

function TPV_Reader.GetU: Byte;
begin
  Result := Buf[FPos];
  Inc(FPos);
end;

function TPV_Reader.GetU2: Word;
begin
  Move(Buf[FPos], Result, 2);
  Inc(FPos, 2);
end;

function TPV_Reader.GetU3: Cardinal;
begin
  Move(Buf[FPos], Result, 3);
  Inc(FPos, 3);
end;

function TPV_Reader.GetU4: Cardinal;
begin
  Move(Buf[FPos], Result, 4);
  Inc(FPos, 4);
end;

function TPV_Reader.GetMU2: Word;
begin
  Move(Buf[FPos], Result, 2);

  Result := SwapEndian(Result);
  Inc(FPos, 2);
end;

function TPV_Reader.GetMU4: Cardinal;
begin
  Move(Buf[FPos], Result, 4);

  Result := SwapEndian(Result);
  Inc(FPos, 4);
end;

function TPV_Reader.GetI: ShortInt;
begin
  Move(Buf[FPos], Result, 1);
  Inc(FPos);
end;

function TPV_Reader.GetI2: Smallint;
begin
  Move(Buf[FPos], Result, 2);
  Inc(FPos, 2);
end;

function TPV_Reader.GetI4: LongInt;
begin
  Move(Buf[FPos], Result, 4);
  Inc(FPos, 4);
end;

function TPV_Reader.GetMI2: Smallint;
begin
  Move(Buf[FPos], Result, 2);

  Result := SwapEndian(Result);
  Inc(FPos, 2);
end;

function TPV_Reader.GetMI4: LongInt;
begin
  Move(Buf[FPos], Result, 4);

  Result := SwapEndian(Result);
  Inc(FPos, 4);
end;

function TPV_Reader.GetF: Single;
var Temp: Cardinal absolute Result;
begin
  Move(Buf[FPos], Temp, 4);

  Inc(FPos, 4);
end;

function TPV_Reader.GetMF: Single;
var Temp: Cardinal absolute Result;
begin
  Move(Buf[FPos], Temp, 4);

  Temp := SwapEndian(Temp);

  Inc(FPos, 4);
end;

function TPV_Reader.GetV: Int64;
var i: Integer;
    Val,Cont: Byte;
    V: Byte;
begin
  Result := 0;

  while True do begin
    V := Buf[FPos];
    Inc(FPos);

    Cont := V shr 7;
    Val  := V and $7F;

    Result := (Result shl 7) + Val;

    if Cont=0 then Exit;
  end;
end;

function TPV_Reader.GetC: Char;
begin
  Result := chr(Buf[FPos]);
  Inc(FPos);
end;

function TPV_Reader.Get(var Buffer; Count: Longint): Longint;
var Count2: Integer;
    i: Integer;
begin
  Count2 := FSize-FPos;
  if Count2 < Count then Count := Count2;

  Move(Buf[FPos], Buffer, Count);

  Result := Count;
  Inc(FPos, Count);
end;

function TPV_Reader.GetNum: Integer;
var Res: String;
begin
  Res := '';

  while FPos < FSize do begin
    if Buf[FPos] in [48..57] then Res := Res + chr(Buf[FPos])
    else break;

    Inc(FPos);
  end;

  Result := StrToInt64Def(Res, 0);
end;

function TPV_Reader.GetWhite: String;
begin
  Result := '';

  while FPos < FSize do begin
    if Buf[FPos] in [32,13,10,09] then Result := Result + chr(Buf[FPos])
    else break;

    Inc(FPos);
  end;
end;

function TPV_Reader.GetS(Count: Integer): String;
begin
  SetLength(Result, Count);
  Move(Buf[FPos], Result[1], Count);
  Inc(FPos, Count);
end;

procedure TPV_Reader.Skip(Count: Integer);
begin
  Inc(FPos, Count);
end;

constructor TPV_Reader.Create(Str: TStream);
begin
  FStream := Str;

  FSize := Str.Size;
  SetLength(Buf, FSize);
  Str.Read(Buf[0], FSize);

  FPos := 0;
end;

{ TPV_Writer }

procedure TPV_Writer.Flush;
begin
  if FPos < 1 then Exit;

  FStream.Write(Buf[0], FPos);
  FPos := 0;
end;

procedure TPV_Writer.Put(const Buffer; Count: Integer);
begin
  if FPos+Count > FSize then Flush;

  Move(Buffer, Buf[FPos], Count);
  Inc(FPos, Count);
end;

procedure TPV_Writer.PutU(V: Byte);
begin
  if FPos+1 > FSize then Flush;

  Buf[FPos] := V;
  Inc(FPos);
end;

procedure TPV_Writer.PutU2(V: Word);
begin
  if FPos+2 > FSize then Flush;

  Move(V, Buf[FPos], 2);
  Inc(FPos, 2);
end;

procedure TPV_Writer.PutU4(V: Cardinal);
begin
  if FPos+4 > FSize then Flush;

  Move(V, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutMU2(V: Word);
begin
  if FPos+2 > FSize then Flush;

  V := SwapEndian(V);

  Move(V, Buf[FPos], 2);
  Inc(FPos, 2);
end;

procedure TPV_Writer.PutMU4(V: Cardinal);
begin
  if FPos+4 > FSize then Flush;

  V := SwapEndian(V);

  Move(V, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutI(V: ShortInt);
begin
  if FPos+1 > FSize then Flush;

  Move(V, Buf[FPos], 1);
  Inc(FPos, 1);
end;

procedure TPV_Writer.PutI2(V: Smallint);
begin
  if FPos+2 > FSize then Flush;

  Move(V, Buf[FPos], 2);
  Inc(FPos, 2);
end;

procedure TPV_Writer.PutI4(V: LongInt);
begin
  if FPos+4 > FSize then Flush;

  Move(V, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutMI2(V: Smallint);
begin
  if FPos+2 > FSize then Flush;

  V := SwapEndian(V);

  Move(V, Buf[FPos], 2);
  Inc(FPos, 2);
end;

procedure TPV_Writer.PutMI4(V: LongInt);
begin
  if FPos+4 > FSize then Flush;

  V := SwapEndian(V);

  Move(V, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutMF(V: Single);
var VV: Cardinal;
begin
  if FPos+4 > FSize then Flush;

  Move(V, VV, 4);

  VV := SwapEndian(VV);

  Move(VV, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutF(V: Single);
var VV: Cardinal;
begin
  if FPos+4 > FSize then Flush;

  Move(V, VV, 4);

  Move(VV, Buf[FPos], 4);
  Inc(FPos, 4);
end;

procedure TPV_Writer.PutV(V: Word);
var A,B: Byte;
begin
  //not really variable-length
  A := (V shr 7) + $80;
  B := (V and $7F);

  PutU(A);
  PutU(B);
end;

procedure TPV_Writer.Skip(Len: Integer);
var i: Integer;
begin
  for i:=0 to Len-1 do
    PutU(0);
end;

procedure TPV_Writer.PutS(S: String);
var Len: Integer;
begin
  Len := Length(S);
  if FPos+Len > FSize then Flush;

  Move(S[1], Buf[FPos], Len);
  Inc(FPos, Len);
end;

procedure TPV_Writer.CopyFrom(Str: TStream; Count: Integer);
var Buff: array of Byte;
    BuffSize: Integer;
    Len: Integer;
begin
  Flush;
  FPos := 0;

  BuffSize := 40960;

  if BuffSize > Count then BuffSize := Count;
  SetLength(Buff, BuffSize);

  while Count >0 do begin
    Len := Str.Read(Buff[0], BuffSize);

    FStream.Write(Buff[0], Len);
    Dec(Count, Len);
  end;
end;

constructor TPV_Writer.Create(Str: TStream);
begin
  FStream := Str;
  FPos := 0;
  FSize := 409600;
  SetLength(Buf, FSize);
end;

destructor TPV_Writer.Destroy;
begin
  Flush;

  inherited Destroy;
end;

end.
