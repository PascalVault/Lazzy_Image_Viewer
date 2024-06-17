program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, unit1, dlg_colors, dlg_params, dlg_resize, dlg_about,
  dlg_info
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Lazzy Image Viewer';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TColorsDlg, ColorsDlg);
  Application.CreateForm(TParamsDlg, ParamsDlg);
  Application.CreateForm(TResizeDlg, ResizeDlg);
  Application.CreateForm(TAboutDlg, AboutDlg);
  Application.CreateForm(TInfoDlg, InfoDlg);
  Application.Run;
end.

