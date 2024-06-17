unit dlg_params;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TParamsDlg }

  TParamsDlg = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    procedure Button1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private

  public
    TheMin, TheMax, TheVal: Integer;
    procedure Setup;
  end;

var
  ParamsDlg: TParamsDlg;

implementation

{$R *.lfm}

{ TParamsDlg }

procedure TParamsDlg.ScrollBar1Change(Sender: TObject);
begin
  TheVal := ScrollBar1.Position;

  Label1.Caption := IntToStr(TheVal);
end;

procedure TParamsDlg.Setup;
begin
  ScrollBar1.Max := TheMax;
  ScrollBar1.Min := TheMin;

  ScrollBar1.Position := TheVal;
  Label1.Caption := IntToStr(TheVal);
end;

procedure TParamsDlg.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

