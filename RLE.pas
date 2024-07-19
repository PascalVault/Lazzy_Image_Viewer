{


procedure UnRle4BT(source: TQFile; out dest: TQFile; packedSize: Integer);
const unitSize = 1;
var i, k: Integer;
    count: Integer;
    val: Byte;
begin
	i := 0;
	while (i<packedSize-1) do begin
		count := source.readU;
		if (count <> 173) then begin //uncompressed
      dest.writeU(count);
      inc(i, unitSize);
		end
		else begin
      count := source.readU;
			val := source.readU;
      for k:=0 to count-1 do dest.writeU(val);
      inc(i, unitSize+1);
		end;
  end;
end;


procedure UnRleBMP8(source: TQFile; out dest: TQFile; packedSize: Integer; width, height: Integer);
var i: Integer;
    x,y: Integer;
    count, marker: Integer;
    byt: Byte;

    procedure moveXY(newX, newY: Integer);
    begin
      dest.position := (newX - x) + (newY - y)*width;
      x := newX;
      y := newY;
    end;

begin
	i := 0;
  x := 0;
  y := 0;

  dest.writeRepeat(0, width*height);
  dest.position := 0;

	while (i<packedSize-1) do begin

		count := source.readU;
		if (count > 0) then begin //RLE-compressed
      dest.copyRepeat(source, 1, count);
      inc(i, 2);
      inc(x, count);
		end
		else begin                 //uncompressed
      marker := source.readU;

      if marker = 0 then begin //end of scanline
        moveXY(0, y+1);
        inc(i, 1);
      end
      else if marker = 1 then begin //EOF
        break;
      end
      else if marker = 2 then begin //move X,Y
        moveXY(x+source.readU, y+source.readU);
      end
      else begin //uncompressed
        dest.writeRepeat(byt, marker);  //TODO: skad byt?

        if marker mod 2 = 1 then source.readU; //padding byte
      end;
		end;
  end;
end;
}


procedure UnRle_LBM(src: TStream; dest: TStream; packedSize: Integer);
const unitSize = 1;
var i,j: Integer;
    count: Byte;
    count2: ShortInt absolute count;
    buff: array of Byte;
begin
  setLength(Buff, unitSize);

  i := 0;
  while (i<packedSize-1) do begin
    count := src.ReadByte;

    if (count2 >= 0) then begin //uncompressed
      count2 := count2+1;
      dest.copyFrom(src, unitSize*count2);
      inc(i, unitSize*count2+1);
    end
    else if count2 = -128 then begin
      inc(i, 1);
    end
    else begin
      count2 := -count2+1;

      Src.Read(buff[0], unitSize);
      for j:=0 to count2-1 do
        Dest.Write(Buff[0], unitSize);

      inc(i, unitSize+1);
    end;
  end;
end;

procedure UnRle_PGC(src: TStream; dest: TStream; packedSize: Integer);
const unitSize = 1;
var i,j: Integer;
    count: Integer;
    buff: array of Byte;
begin
  setLength(Buff, unitSize);

  i := 0;
  while (i<packedSize-1) do begin
    count := src.ReadByte;

    if (count < 128) then begin //uncompressed
      count := count;
      dest.copyFrom(src, unitSize*count);
      inc(i, unitSize*count+1);
    end
    else begin
      count := count-128;

      Src.Read(buff[0], unitSize);
      for j:=0 to count-1 do
        Dest.Write(Buff[0], unitSize);

      inc(i, unitSize+1);
    end;
  end;
end;

procedure UnRle_CPR(src: TStream; dest: TStream; packedSize: Integer);
const unitSize = 1;
var i,j: Integer;
    count: Integer;
    buff: array of Byte;
begin
  setLength(Buff, unitSize);

  i := 0;
  while (i<packedSize-1) do begin
    if i > 9000 then break;  //TODO: rather uncecessray

    count := src.ReadByte;
    if (count < 128) then begin
      count := count;
      if count = 0 then begin
        count := SwapEndian(src.ReadWord);
        inc(i, 2);
      end;

      Src.Read(buff[0], unitSize);
      for j:=0 to count-1 do
        Dest.Write(Buff[0], unitSize);

      inc(i, unitSize+1);
    end
    else begin                 //uncompressed
      count := count-128;
      if count = 0 then begin
        count := SwapEndian(src.ReadWord);
        inc(i, 2);
      end;

      dest.copyFrom(src, unitSize*count);
      inc(i, unitSize*count+1);
    end;
  end;
end;


procedure Unrle_PAC(Src: TStream; Dest: TStream; idByte, packByte, specialByte: Byte);
var count: Integer;
    i: Integer;
    b: Byte;
begin
  while (Src.position < Src.size) do begin
    b := Src.ReadByte;

    if b = idByte then begin
      count := Src.ReadByte;

      for i:=0 to count do begin
        Dest.Write(packByte, 1);
      end;

    end
    else if b = specialByte then begin
      b := Src.ReadByte;
      count := Src.ReadByte;

      for i:=0 to count do begin   //or maybe count-1
        Dest.Write(b, 1);
      end;
    end
    else begin
      Dest.Write(b, 1);
    end;
  end;
end;

procedure Unrle_TGA(Src: TStream; Dest: TStream; packedSize: Integer; unitSize: Integer);
var i,j,k: Integer;
    A,B,C: Byte;
    count: Integer;
    Buff: array of Byte;
begin
  i := 0;
  SetLength(Buff, unitSize);

  while i<packedSize-1 do begin
    count := Src.ReadByte;

    if count < 128 then begin //uncompressed
      count := count+1;

      Src.Read(Buff[0], unitSize);

      for j:=0 to count-1 do
        Dest.Write(Buff[0], unitSize);

      Inc(i, unitSize*count+1);
    end
    else begin
      count := count-128+1;

      for k:=0 to count-1 do begin
        Src.Read(Buff[0], unitSize);
        Dest.Write(Buff[0], unitSize);
      end;
        Inc(i, unitSize+1);
      end;
    end;
end;

procedure Unrle_RGB(Src: TStream; Dest: TStream; packedSize: Integer);
var i,j,k: Integer;
    A,B,C: Byte;
    unitSize: Integer;
    count: Integer;
    Buff: array of Byte;
begin
  i := 0;
  unitSize := 1;
  SetLength(Buff, unitSize);

  while i<packedSize-1 do begin
    count := Src.ReadByte;

    if count > 128 then begin //uncompressed

      count := count-128; //+1

      for k:=0 to count-1 do begin
        Src.Read(Buff[0], unitSize);
        Dest.Write(Buff[0], unitSize);
      end;

      Inc(i, unitSize*count+1);
    end
    else begin
      count := count;//+1
      Src.Read(Buff[0], unitSize);

      for k:=0 to count-1 do begin
        Dest.Write(Buff[0], unitSize);
      end;
      Inc(i, unitSize+1);
    end;
  end;
end;

procedure Unrle_PCX(Src: TStream; Dest: TStream; packedSize: Integer);
var i,j,k: Integer;
    A,B,C: Byte;
    buff: Byte;
    count: Integer;
begin
  i := 0;
  j := 0;

  while i<packedSize-1 do begin
    count := Src.ReadByte;

    if ((count and $C0) = $C0) then begin //compressed
      count := (count and $3F);
      buff := Src.ReadByte;
      Inc(i, 2);
    end
    else begin
      buff := count;
      count := 1;
      Inc(i);
    end;
    for k:=0 to count-1 do Dest.Write(Buff, 1);
  end;
end;

procedure Unrle_CUT(Src: TStream; Dest: TStream; packedSize: Integer; unitSize: Integer);
var i,j,k: Integer;
    A,B,C: Byte;
    count: Integer;
    Buff: array of Byte;
begin
  i := 0;
  SetLength(Buff, unitSize);

  while i<packedSize-1 do begin

    count := Src.ReadByte;
    if (count < 128) then begin//uncompressed

      count := count+1;

      for k:=0 to count-1 do begin
        Src.Read(Buff[0], unitSize);
        Dest.Write(Buff[0], unitSize);
      end;

      Inc(i, unitSize*count+1);
    end
    else begin
      count := count-128+1;

      Src.Read(Buff[0], unitSize);

      for k:=0 to count-1 do begin
        Dest.Write(Buff[0], unitSize);
      end;

      Inc(i, unitSize+1);
    end;
  end;
end;

procedure Unrle_GG(Src: TStream; Dest: TStream; packedSize: Integer);
var i,j: Integer;
    A,B,C: Byte;
begin
  i := 0;
  while i<packedSize do begin
    A := Src.ReadByte;
    Inc(i);

    if A = 254 then begin
      B := Src.ReadByte;
      C := Src.ReadByte;

      for j:=0 to C-1 do Dest.Write(B, 1);
      Inc(i, 2);
    end
    else begin //uncompressed
      dest.Write(A, 1);
    end;
  end;
end;

procedure Unrle_AMI(Src: TStream; Dest: TStream; packedSize: Integer);
var i,j: Integer;
    A,B,C: Byte;
begin
  i := 0;
  while i<packedSize do begin
    A := Src.ReadByte;
    Inc(i);

    if A = 194 then begin
      B := Src.ReadByte;
      if B = 0 then Exit; //EOF

      C := Src.ReadByte;

      for j:=0 to B-1 do Dest.Write(C, 1);

      Inc(i, 2);
    end
    else begin //uncompressed
      Dest.Write(A, 1);
    end;
  end;
end;

procedure Unrle_DLP(Src: TStream; Dest: TStream; packedSize: Integer; escapeByte: Byte);
var i,j: Integer;
    A,B,C: Byte;
begin
  i := 0;
  while i<packedSize do begin
    A := Src.ReadByte;
    Inc(i);

    if A = escapeByte then begin
      B := Src.ReadByte;
      C := Src.ReadByte;

      for j:=0 to B-1 do Dest.Write(C, 1);

      Inc(i, 2);
    end
    else begin //uncompressed
      Dest.Write(A, 1);
    end;
  end;
end;

procedure Unrle_PSD(Src: TStream; Dest: TStream; packedSize: Integer; unitSize: Integer = 1); //psd,mac
var i,j,k: Integer;
    A,B,C: Byte;
    Buff: array of Byte;
    count: Integer;
begin
  i := 0;
  SetLength(Buff, unitSize);

  while i<packedSize-1 do begin
    count := Src.ReadByte;
    if count > 127 then count := count - 256; //convert UInt8 to Int8;

    if count = -128 then Inc(i)
    else if count >=0 then begin //uncompressed
      count := count+1;

      for k:=0 to count-1 do begin
        Src.Read(Buff[0], unitSize);
        Dest.Write(Buff[0], unitSize);
      end;

      Inc(i, unitSize*count+1);
    end
    else begin
      count := -1*count+1;

      Src.Read(Buff[0], unitSize);

      for k:=0 to count-1 do begin
        Dest.Write(Buff[0], unitSize);
      end;
      Inc(i, unitSize+1);
    end;
  end;
end;

function hexInt(hex: String): Integer;
begin
  Result := StrToInt64Def('$' + hex, 0);
end;

