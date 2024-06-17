unit dlg_about;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TAboutDlg }

  TAboutDlg = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
  private

  public

  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.lfm}

end.

