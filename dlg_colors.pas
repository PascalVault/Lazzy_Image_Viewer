unit dlg_colors;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, PV_Bitmap;

type

  { TColorsDlg }

  TColorsDlg = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioGroup1: TRadioGroup;
    UpDown1: TUpDown;
    procedure Button1Click(Sender: TObject);
  private

  public
    ColorMode: Integer;
    procedure Show(Num: Integer); overload;
  end;

var
  ColorsDlg: TColorsDlg;

implementation

uses Unit1;

{$R *.lfm}

{ TColorsDlg }

procedure TColorsDlg.Button1Click(Sender: TObject);
var i: Integer;
    ColorCount: Integer;
    Dither: TDither;
begin
  for i:=0 to GroupBox1.ControlCount - 1 do
    if (GroupBox1.Controls[i] is TRadioButton) and TRadioButton(GroupBox1.Controls[i]).Checked then begin
      ColorCount := GroupBox1.Controls[i].Tag;
      break;
    end;

  if ColorCount = 1 then ColorCount := UpDown1.Position;

  case RadioGroup1.ItemIndex of
   0 : Dither := ddNone;
   1 : Dither := ddFloyd;
   2 : Dither := ddBurkes;
   3 : Dither := ddStucki;
   4 : Dither := ddJarvis;
   5 : Dither := ddAtkinson;
   6 : Dither := ddSierra2;
   7 : Dither := ddSierra3;
   8 : Dither := ddSierra4;
  end;

  Form1.SaveUndo;

  if ColorMode = 256 then
    Form1.GetBmp.ReduceColors(ColorCount-1, Dither)
  else if ColorMode = -256 then
    Form1.GetBmp.Grayscale(ColorCount-1, Dither)
  else if ColorMode = 2 then
    Form1.GetBmp.BlackWhite(Dither);

  Form1.Redraw;

  Close;
end;

procedure TColorsDlg.Show(Num: Integer);
begin
  ColorMode := Num;

  if Num = 2 then begin
    RadioButton1.Checked := True;
  end
  else RadioButton5.Checked := True;

  Show;
end;

end.

