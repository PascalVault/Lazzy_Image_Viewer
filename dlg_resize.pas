unit dlg_resize;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, PV_Filters;

type

  { TResizeDlg }

  TResizeDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RG: TRadioGroup;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  ResizeDlg: TResizeDlg;

implementation

uses Unit1;

{$R *.lfm}

{ TResizeDlg }

procedure TResizeDlg.Button1Click(Sender: TObject);
begin
  Form1.SaveUndo;

  case RG.ItemIndex of
    0: Form1.GetBmp.Resize(UpDown3.Position, UpDown4.Position);
    1: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfBox);
    2: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfBilinear);
    3: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfBell);
    4: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfHermite);
    5: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfLanczos3);
    6: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfMitchell);
    7: Form1.GetBmp.Resample(UpDown3.Position, UpDown4.Position, rfSpline);
  end;

  Form1.Redraw;
  Close;
end;

procedure TResizeDlg.Button2Click(Sender: TObject);
begin
  Form1.SaveUndo;

  case RG.ItemIndex of
    0: Form1.GetBmp.ResizePercent(UpDown5.Position, UpDown6.Position);
    1: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfBox);
    2: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfBilinear);
    3: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfBell);
    4: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfHermite);
    5: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfLanczos3);
    6: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfMitchell);
    7: Form1.GetBmp.ResamplePercent(UpDown5.Position, UpDown6.Position, rfSpline);
  end;

  Form1.Redraw;
  Close;
end;

end.

