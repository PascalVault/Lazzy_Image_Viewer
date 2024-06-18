unit dlg_formats;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  PV_Bitmap, PV_BitmapFormats;

type

  { TFormatsDlg }

  TFormatsDlg = class(TForm)
    Panel1: TPanel;
    SG: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormatsDlg: TFormatsDlg;

implementation

uses Unit1;

{$R *.lfm}

{ TFormatsDlg }

function ListSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr( List[Index1] , List[Index2] );
end;

procedure TFormatsDlg.FormCreate(Sender: TObject);
var i: Integer;
    Ext, AName: String;
    R: TPV_BitmapReader;
    W: TPV_BitmapWriter;
    Reader,Writer: String;
    SumR,SumW: Integer;
    Temp: TStringList;
begin
  SG.ColWidths[0] := 250;
  SG.Rows[0].CommaText := 'Format,Read,Write';

  SG.RowCount := BitmapFormats.Count+1;
  SumR := 0;
  SumW := 0;

  Temp := TStringList.Create;

  for i:=0 to BitmapFormats.Count-1 do begin
    BitmapFormats.Item(i, Ext, AName, R, W);

    if R = nil then Reader := ''
    else            Reader := '+';

    if W = nil then Writer := ''
    else            Writer := '+';

    if R <> nil then Inc(SumR);
    if W <> nil then Inc(SumW);

    Temp.Add( '"' + AName + ' (.' + Ext + ')",' + Reader + ',' + Writer );
  end;

  Temp.CustomSort(@ListSort);

  for i:=0 to Temp.Count-1 do
    SG.Rows[i+1].CommaText := Temp[i];

  Temp.Free;

  Panel1.Caption := 'Read: ' + IntToStr(SumR) + ', Write: ' + IntToStr(SumW);
end;

end.

