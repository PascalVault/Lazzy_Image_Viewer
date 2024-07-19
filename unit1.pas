unit Unit1;

//Lazzy Image Viewer
//github.com/PascalVault
//License: GNU/GPL

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Grids,
  StdCtrls, Buttons, Menus, LCLIntf, LCLType, Clipbrd, Math, {$IFDEF WINDOWS}ShellApi,{$ENDIF}
  PV_Bitmap, PV_BitmapFormats, PV_Filters, Printers, PrintersDlgs, Types,
  FileUtil, dlg_colors, dlg_resize, dlg_params, dlg_about, dlg_info, dlg_formats,
  IntfGraphics, AsyncProcess, FPImage, FPReadJPEG;

const Levels: array of Extended = (0.1, 0.15, 0.20, 0.25, 0.3, 0.5, 0.7, 1, 1.5, 2, 3, 4, 5, 6, 7, 8);

type
  TFileInfo = record
    Name: String;
    Size: Int64;
    Ext: String;
    Width: Integer;
    Height: Integer;
    Date: TDateTime;
    Thumb: TBitmap;
  end;

  { TFileList }

  TFileList = class
  private
    FList: array of TFileInfo;
    FSize: Integer;
    FPos: Integer;
  public
    constructor Create;
    procedure Clear;
    procedure Add(Name: String; Size: Int64; Date: TDateTime);
    procedure Edit(Index: Integer; Width,Height: Integer; Thumb: TPV_Bitmap);
    function GetName(Index: Integer): String;
    function Get(Index: Integer; out Info: TFileInfo): Boolean;
  end;

  { TTabSheet }

  TTabSheet = class(ComCtrls.TTabSheet)
  public
    Bmp: TPV_Bitmap;
    Zoom: Extended;
    Filename: String;
    UndoPos: Integer;
    UndoList: array[0..9] of TPV_Bitmap;
    ScrollbarV: TScrollbar;
    ScrollbarH: TScrollbar;
    List: TFileList;
    ListPos: Integer;
    DG: TDrawGrid;
    Panel: TPanel;
    Split: TSplitter;
    GridActive: Boolean;
    procedure Close;
    procedure ListFiles(AFilename: String);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    BoxBlur_PopEl: TMenuItem;
    Emboss_PopEl: TMenuItem;
    Contrast_PopEl: TMenuItem;
    Brightness_PopEl: TMenuItem;
    Desaturate_PopEl: TMenuItem;
    Formats_MenuEl: TMenuItem;
    Negate_PopEl: TMenuItem;
    RotateLeft_PopEl: TMenuItem;
    RotateRight_PopEl: TMenuItem;
    Rotate180_PopEl: TMenuItem;
    FlipH_PopEl: TMenuItem;
    FlipV_PopEl: TMenuItem;
    Gauss_PopEl: TMenuItem;
    Palette_PopEl: TMenuItem;
    Grayscale_PopEl: TMenuItem;
    Hicolor15_PopEl: TMenuItem;
    BlackWhite_PopEl: TMenuItem;
    Hicolor16_PopEl: TMenuItem;
    Edit_MenuEl: TMenuItem;
    Undo_MenuEl: TMenuItem;
    File_MenuEl: TMenuItem;
    Open_MenuEl: TMenuItem;
    Close_MenuEl: TMenuItem;
    Gauss5_PopEl: TMenuItem;
    Exit_MenuEl: TMenuItem;
    Help_MenuEl: TMenuItem;
    About_MenuEl: TMenuItem;
    Copy_MenuEl: TMenuItem;
    Paste_MenuEl: TMenuItem;
    SaveAs_MenuEl: TMenuItem;
    View_MenuEl: TMenuItem;
    Fullscreen_MenuEl: TMenuItem;
    Next_MenuEl: TMenuItem;
    Prev_MenuEl: TMenuItem;
    CloseTab_PopEl: TMenuItem;
    CloseOther_PopEl: TMenuItem;
    CloseAll_PopEl: TMenuItem;
    Next_PopEl: TMenuItem;
    Prev_PopEl: TMenuItem;
    Copy_PopEl: TMenuItem;
    Fullscreen_PopEl: TMenuItem;
    Trim_PopEl: TMenuItem;
    Delete_MenuEl: TMenuItem;
    Info_MenuEl: TMenuItem;
    Countcolors_MenuEl: TMenuItem;
    Fileinfo_MenuEl: TMenuItem;
    SwitchChannel_PopEl: TMenuItem;
    BGR_PopEl: TMenuItem;
    GBR_PopEl: TMenuItem;
    RBG_PopEl: TMenuItem;
    BRG_PopEl: TMenuItem;
    GRB_PopEl: TMenuItem;
    ExtractChannel_PopEl: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    Rename_MenuEl: TMenuItem;
    ZoomIn_MenuEl: TMenuItem;
    Zoom100_MenuEl: TMenuItem;
    ZoomOut_MenuEl: TMenuItem;
    BestFit_MenuEl: TMenuItem;
    CMYK_PopEl: TMenuItem;
    ExtractC_PopEl: TMenuItem;
    ExtractM_PopEl: TMenuItem;
    ExtractY_PopEl: TMenuItem;
    ExtractK_PopEl: TMenuItem;
    RemoveAlpha_PopEl: TMenuItem;
    Opaque_PopEl: TMenuItem;
    Separator9: TMenuItem;
    Separator8: TMenuItem;
    Separator7: TMenuItem;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    PopupTab: TPopupMenu;
    PopupImage: TPopupMenu;
    Separator1: TMenuItem;
    SmarterBlur_PopEl: TMenuItem;
    Unsharp_PopEl: TMenuItem;
    Sharpen_PopEl: TMenuItem;
    Sepia_PopEl: TMenuItem;
    AddNoise_PopEl: TMenuItem;
    FindEdges_PopEl: TMenuItem;
    OpenDialog1: TOpenDialog;
    PG: TPageControl;
    Panel1: TPanel;
    PopupFilter: TPopupMenu;
    PopupRotate: TPopupMenu;
    PopupFlip: TPopupMenu;
    PopupColors: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    Open_Button: TSpeedButton;
    Colors_Button: TSpeedButton;
    Flip_Button: TSpeedButton;
    ZoomIn_Button: TSpeedButton;
    ZoomOut_Button: TSpeedButton;
    Zoom100_Button: TSpeedButton;
    Browse_Button: TSpeedButton;
    Paste_Button: TSpeedButton;
    Filter_Button: TSpeedButton;
    SaveAs_Button: TSpeedButton;
    Rotate_Button: TSpeedButton;
    Print_Button: TSpeedButton;
    Copy_Button: TSpeedButton;
    Screenshot_Button: TSpeedButton;
    Resize_Button: TSpeedButton;
    Fullscreen_Button: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure Formats_MenuElClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Emboss_PopElClick(Sender: TObject);
    procedure Contrast_PopElClick(Sender: TObject);
    procedure Brightness_PopElClick(Sender: TObject);
    procedure Desaturate_PopElClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Negate_PopElClick(Sender: TObject);
    procedure RotateLeft_PopElClick(Sender: TObject);
    procedure RotateRight_PopElClick(Sender: TObject);
    procedure Rotate180_PopElClick(Sender: TObject);
    procedure FlipH_PopElClick(Sender: TObject);
    procedure FlipV_PopElClick(Sender: TObject);
    procedure BoxBlur_PopElClick(Sender: TObject);
    procedure Palette_PopElClick(Sender: TObject);
    procedure Grayscale_PopElClick(Sender: TObject);
    procedure Hicolor15_PopElClick(Sender: TObject);
    procedure BlackWhite_PopElClick(Sender: TObject);
    procedure Hicolor16_PopElClick(Sender: TObject);
    procedure Undo_MenuElClick(Sender: TObject);
    procedure Open_MenuElClick(Sender: TObject);
    procedure Close_MenuElClick(Sender: TObject);
    procedure Gauss_PopElClick(Sender: TObject);
    procedure Exit_MenuElClick(Sender: TObject);
    procedure About_MenuElClick(Sender: TObject);
    procedure Copy_MenuElClick(Sender: TObject);
    procedure Paste_MenuElClick(Sender: TObject);
    procedure SaveAs_MenuElClick(Sender: TObject);
    procedure Fullscreen_MenuElClick(Sender: TObject);
    procedure Next_MenuElClick(Sender: TObject);
    procedure Prev_MenuElClick(Sender: TObject);
    procedure Gauss5_PopElClick(Sender: TObject);
    procedure CloseTab_PopElClick(Sender: TObject);
    procedure CloseOther_PopElClick(Sender: TObject);
    procedure CloseAll_PopElClick(Sender: TObject);
    procedure Next_PopElClick(Sender: TObject);
    procedure Prev_PopElClick(Sender: TObject);
    procedure Copy_PopElClick(Sender: TObject);
    procedure Fullscreen_PopElClick(Sender: TObject);
    procedure Trim_PopElClick(Sender: TObject);
    procedure Delete_MenuElClick(Sender: TObject);
    procedure SmarterBlur_PopElClick(Sender: TObject);
    procedure Countcolors_MenuElClick(Sender: TObject);
    procedure Fileinfo_MenuElClick(Sender: TObject);
    procedure BGR_PopElClick(Sender: TObject);
    procedure GBR_PopElClick(Sender: TObject);
    procedure RBG_PopElClick(Sender: TObject);
    procedure BRG_PopElClick(Sender: TObject);
    procedure GRB_PopElClick(Sender: TObject);
    procedure MenuItem59Click(Sender: TObject);
    procedure Unsharp_PopElClick(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure MenuItem61Click(Sender: TObject);
    procedure MenuItem62Click(Sender: TObject);
    procedure Rename_MenuElClick(Sender: TObject);
    procedure ZoomIn_MenuElClick(Sender: TObject);
    procedure Zoom100_MenuElClick(Sender: TObject);
    procedure ZoomOut_MenuElClick(Sender: TObject);
    procedure BestFit_MenuElClick(Sender: TObject);
    procedure ExtractC_PopElClick(Sender: TObject);
    procedure Sharpen_PopElClick(Sender: TObject);
    procedure ExtractM_PopElClick(Sender: TObject);
    procedure ExtractY_PopElClick(Sender: TObject);
    procedure ExtractK_PopElClick(Sender: TObject);
    procedure RemoveAlpha_PopElClick(Sender: TObject);
    procedure Opaque_PopElClick(Sender: TObject);
    procedure Sepia_PopElClick(Sender: TObject);
    procedure AddNoise_PopElClick(Sender: TObject);
    procedure FindEdges_PopElClick(Sender: TObject);
    procedure PGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Colors_ButtonClick(Sender: TObject);
    procedure Flip_ButtonClick(Sender: TObject);
    procedure ZoomIn_ButtonClick(Sender: TObject);
    procedure ZoomOut_ButtonClick(Sender: TObject);
    procedure Zoom100_ButtonClick(Sender: TObject);
    procedure Browse_ButtonClick(Sender: TObject);
    procedure Paste_ButtonClick(Sender: TObject);
    procedure Filter_ButtonClick(Sender: TObject);
    procedure Open_ButtonClick(Sender: TObject);
    procedure SaveAs_ButtonClick(Sender: TObject);
    procedure Rotate_ButtonClick(Sender: TObject);
    procedure Print_ButtonClick(Sender: TObject);
    procedure Copy_ButtonClick(Sender: TObject);
    procedure Screenshot_ButtonClick(Sender: TObject);
    procedure Resize_ButtonClick(Sender: TObject);
    procedure Fullscreen_ButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure OnPanelPaint(Sender: TObject);
    procedure OnGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OnGridDrawCellThumb(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OnGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OnGridSelectCellThumb(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OnGridResize(Sender: TObject);
  public
    IsFullscreen: Boolean;
    WinSize: TRect;
    procedure OpenImage(Filename: String);
    function ReOpenImage(Filename: String; ASheet: TTabSheet; DoList: Boolean = True): Boolean;
    procedure DrawBmp(Image: TImage; Bmp: TPV_Bitmap);
    function GetBmp: TPV_Bitmap;
    function GetSheet: TTabSheet;
    function NoImage: Boolean;
    procedure Redraw;
    function ZoomIn(Val: Extended): Extended;
    function ZoomOut(Val: Extended): Extended;
    procedure SaveUndo;
    procedure Undo;
    procedure FixScrollbars;
    procedure EnterFullscreen;
    procedure LeaveFullscreen;
    procedure ShowInfo;
    procedure ShowZoom;
    procedure ErrorBox(Msg: String);
    procedure AddSaveFormats;
    procedure SelectInGrid(Row: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function HumanSize(V: Int64): String;
begin
  if V < 1024 then Exit(IntToStr(V) + ' B');
  if V < 1048576 then Exit(Format('%.02f', [V/1024]) + ' kB');
  if V < 1073741824 then Exit(Format('%.02f', [V/1048576]) + ' MB');
  Exit(Format('%.02f', [V/1073741824]) + ' GB');
end;

{ TFileList }

constructor TFileList.Create;
begin
  inherited Create;
  FPos := 0;
  FSize := 1000;
  SetLength(FList, FSize);
end;

procedure TFileList.Clear;
begin
  FPos := 0;
end;

procedure TFileList.Add(Name: String; Size: Int64; Date: TDateTime);
begin
  if FPos = FSize then begin
    Inc(FSize, 1000);
    SetLength(FList, FSize);
  end;

  FList[FPos].Name := Name;
  FList[FPos].Ext  := Copy(ExtractFileExt(Name), 2);
  FList[FPos].Size := Size;
  FList[FPos].Date := Date;
  FList[FPos].Width  := 0;
  FList[FPos].Height := 0;
  Inc(FPos);
end;

procedure TFileList.Edit(Index: Integer; Width, Height: Integer; Thumb: TPV_Bitmap);
begin
 FList[Index].Width := Width;
 FList[Index].Height := Height;
 FList[Index].Thumb := Thumb.ToBitmap;
end;

function TFileList.GetName(Index: Integer): String;
begin
  Result := FList[Index].Name;
end;

function TFileList.Get(Index: Integer; out Info: TFileInfo): Boolean;
begin
  if (Index < 0) or (Index > FPos) then Exit(False);

  Info := FList[Index];
  Result := True;
end;

{ TTabSheet }

procedure TTabSheet.Close;
begin
  Bmp.Free;
  List.Free;
  Self.Free;
end;

procedure TTabSheet.ListFiles(AFilename: String);
 var sr: TSearchRec;
     Dir: String;
     FilenameLow: String;
     k: Integer;
 begin
  Dir := ExtractFilePath(AFilename);
  List.Clear;
  FilenameLow := LowerCase(AFilename);
  ListPos := -1;

  if FindFirst(Dir + '*', faAnyFile - faDirectory, sr) <> 0 then Exit;

  k := 0;
  repeat

    List.Add(Dir + sr.Name, sr.Size, sr.TimeStamp);

    if LowerCase(Dir + sr.Name) = FilenameLow then ListPos := k;

    Inc(k);

    if k mod 100 = 0 then Application.ProcessMessages;
  until FindNext(sr) <> 0;

  FindClose(sr);

  Self.DG.RowCount := 1;
end;

{ TForm1 }

procedure TForm1.OnGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const Cols: array of String = ('Name','Size','Format','Resolution','Date');
var Sheet: TTabSheet;
    Txt: String;
    FInfo: TFileInfo;
    FFormat, Resolution: String;
    Wid,Hei,Topp: Integer;
begin
  Sheet := GetSheet as TTabSheet;

  if Sheet.List = nil then Exit;

  if ARow > Sheet.List.FPos then Exit;

  if ARow = 0 then begin //header
    Txt := Cols[ACol];

    Hei := Sheet.DG.Canvas.TextHeight(Txt);
    Topp := (ARect.Height - Hei) div 2;

    Sheet.DG.Canvas.TextOut(ARect.Left+2, ARect.Top+Topp, Txt);
    Exit;
  end;

  Dec(ARow);

  Sheet.List.Get(ARow, FInfo);

  FFormat := BitmapFormats.FindName(FInfo.Ext);
  if FInfo.Width > 0 then Resolution := Format('%d x %d', [FInfo.Width, FInfo.Height])
  else Resolution := '';

  if (not (gdSelected in aState)) and (FFormat <> '') then
    Sheet.DG.Canvas.Brush.Color := $FFE3E3;
  Sheet.DG.Canvas.FillRect(ARect);

  case ACol of
    0: Txt := ExtractFileName(FInfo.Name);
    1: Txt := HumanSize(FInfo.Size);
    2: Txt := FFormat;
    3: Txt := Resolution;
    4: Txt := FormatDateTime('YYYY-MM-DD HH:NN:SS', FInfo.Date);
  end;

  Hei := Sheet.DG.Canvas.TextHeight(Txt);
  Wid := Sheet.DG.Canvas.TextWidth(Txt);
  Topp := (ARect.Height - Hei) div 2;

  Sheet.DG.Canvas.TextOut(ARect.Left+2, ARect.Top+Topp, Txt);
end;

procedure TForm1.OnGridDrawCellThumb(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var Sheet: TTabSheet;
    Txt: String;
    FInfo: TFileInfo;
    FFormat, Resolution: String;
    Wid,Hei,Topp: Integer;
    Bmp: TPV_Bitmap;
    Index: Integer;
    Grid: TDrawGrid;
    L: Integer;
begin
  Sheet := GetSheet as TTabSheet;

  if Sheet.List = nil then Exit;

  Grid := Sheet.DG;

  Index := ARow * Grid.ColCount + ACol;

  if Index > Sheet.List.FPos-1 then Exit;

  Sheet.List.Get(Index, FInfo);

  L := (ARect.Width - FInfo.Thumb.Width) div 2;

  Sheet.DG.Canvas.Draw(ARect.Left+L, ARect.Top+5, FInfo.Thumb);

  Txt := ExtractFileName(FInfo.Name);

  L := (ARect.Width - Sheet.DG.Canvas.TextWidth(Txt)) div 2;

  Sheet.DG.Canvas.TextOut(ARect.Left+L, ARect.Top + 130, Txt);
end;

procedure TForm1.OnGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var Sheet: TTabSheet;
    FInfo: TFileInfo;
begin
  Sheet := GetSheet;

  if Sheet.GridActive = False then Exit;

  if Sheet.List = nil then Exit;

  if ARow > Sheet.List.FPos then Exit;

  Sheet.List.Get(ARow-1, FInfo);

  if Sheet.Filename <> FInfo.Name then
    ReOpenImage(FInfo.Name, Sheet, False);

  Sheet.ListPos := ARow-1;
end;

procedure TForm1.OnGridSelectCellThumb(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var Sheet: TTabSheet;
    FInfo: TFileInfo;
    Index: Integer;
    Grid: TDrawGrid;
begin
  Sheet := GetSheet;

  if Sheet.GridActive = False then Exit;

  if Sheet.List = nil then Exit;

  Grid := Sheet.DG;

  Index := ARow * Grid.ColCount + ACol;

  if Index > Sheet.List.FPos-1 then Exit;

  Sheet.List.Get(Index, FInfo);

  if Sheet.Filename <> FInfo.Name then
    ReOpenImage(FInfo.Name, Sheet, False);

  Sheet.ListPos := Index;
end;

procedure TForm1.OnGridResize(Sender: TObject);
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
var i: Integer;
begin
  IsFullScreen := False;

  for i:=1 to ParamCount do
    OpenImage(ParamStr(i));

  AddSaveFormats;

  {$IFNDEF WINDOWS}
  //deleting to recycle bin doesn't work on Linux
  Delete_MenuEl.Free;
  Separator5.Free;
  {$ENDIF}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin

end;

procedure TForm1.DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin

end;

procedure TForm1.Formats_MenuElClick(Sender: TObject);
begin
  FormatsDlg.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var i: Integer;
begin
  for i:=PG.PageCount-1 downto 0 do
    TTabSheet(PG.Pages[i]).Close;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var i: Integer;
begin
  for i:=0 to High(Filenames) do OpenImage(Filenames[i]);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_ESCAPE then LeaveFullscreen;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  ZoomOut_Button.Click;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  ZoomIn_Button.Click;
end;

procedure TForm1.Emboss_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.Emboss;
  Redraw;
end;

procedure TForm1.Contrast_PopElClick(Sender: TObject);
begin
  with ParamsDlg do begin
    TheMin := 0;
    TheMax := 100;
    TheVal := 10;
    Setup;
    ShowModal;
  end;

  SaveUndo;
  GetBmp.Contrast(ParamsDlg.TheVal);
  Redraw;
end;

procedure TForm1.Brightness_PopElClick(Sender: TObject);
begin
  with ParamsDlg do begin
    TheMin := -100;
    TheMax := 100;
    TheVal := 0;
    Setup;
    ShowModal;
  end;

  SaveUndo;
  GetBmp.Brightness(ParamsDlg.TheVal);
  Redraw;
end;

procedure TForm1.Desaturate_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.Grayscale(256, ddNone);
  Redraw;
end;

procedure TForm1.FormResize(Sender: TObject);
begin

end;

procedure TForm1.Negate_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.Negate;
  Redraw;
end;

procedure TForm1.RotateLeft_PopElClick(Sender: TObject);
begin
  if NoImage then Exit;

  SaveUndo;
  GetBmp.Rotate90;
  Redraw;
end;

procedure TForm1.RotateRight_PopElClick(Sender: TObject);
begin
  if NoImage then Exit;

  SaveUndo;
  GetBmp.Rotate270;
  Redraw;
end;

procedure TForm1.Rotate180_PopElClick(Sender: TObject);
begin
  if NoImage then Exit;

  SaveUndo;
  GetBmp.Rotate180;
  Redraw;
end;

procedure TForm1.FlipH_PopElClick(Sender: TObject);
begin
  if NoImage then Exit;

  SaveUndo;
  GetBmp.FlipH;
  Redraw;
end;

procedure TForm1.FlipV_PopElClick(Sender: TObject);
begin
  if NoImage then Exit;

  SaveUndo;
  GetBmp.FlipV;
  Redraw;
end;

procedure TForm1.BoxBlur_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.BoxBlur;
  Redraw;
end;

procedure TForm1.Palette_PopElClick(Sender: TObject);
begin
  ColorsDlg.Show(256);
end;

procedure TForm1.Grayscale_PopElClick(Sender: TObject);
begin
  ColorsDlg.Show(-256);
end;

procedure TForm1.Hicolor15_PopElClick(Sender: TObject);
begin
  GetBmp.Highcolor(15);
end;

procedure TForm1.BlackWhite_PopElClick(Sender: TObject);
begin
  ColorsDlg.Show(2);
end;

procedure TForm1.Hicolor16_PopElClick(Sender: TObject);
begin
  GetBmp.Highcolor(16);
end;

procedure TForm1.Undo_MenuElClick(Sender: TObject);
begin
  Undo;
end;

procedure TForm1.Open_MenuElClick(Sender: TObject);
begin
  Open_Button.Click;
end;

procedure TForm1.Close_MenuElClick(Sender: TObject);
var Sheet: TTabSheet;
begin
  Sheet := GetSheet;
  if Sheet = nil then Exit;
  Sheet.Close;
end;

procedure TForm1.Gauss_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.GaussBlur;
  Redraw;
end;

procedure TForm1.Exit_MenuElClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About_MenuElClick(Sender: TObject);
begin
  AboutDlg.Show;
end;

procedure TForm1.Copy_MenuElClick(Sender: TObject);
begin
  Copy_Button.Click;
end;

procedure TForm1.Paste_MenuElClick(Sender: TObject);
begin
  Paste_Button.Click;
end;

procedure TForm1.SaveAs_MenuElClick(Sender: TObject);
begin
  SaveAs_Button.Click;
end;

procedure TForm1.Fullscreen_MenuElClick(Sender: TObject);
begin
  EnterFullscreen;
end;

procedure TForm1.Next_MenuElClick(Sender: TObject);
var Sheet: TTabSheet;
    Pos: Integer;
    i: Integer;
begin
 if NoImage then Exit;
 Sheet := GetSheet;

  for i:=1 to 200 do begin // try 200 times
    Pos := Sheet.ListPos + i;
    if Pos > Sheet.List.FPos-1 then Exit;

    if ReOpenImage(Sheet.List.GetName(Pos), Sheet, False) then begin
      SelectInGrid(Pos+1);
      break;
    end;

    Sheet.ListPos := Pos;
  end;

  Redraw;
end;

procedure TForm1.Prev_MenuElClick(Sender: TObject);
var Sheet: TTabSheet;
    Pos: Integer;
    i: Integer;
begin
 if NoImage then Exit;
 Sheet := GetSheet;

  for i:=1 to 200 do begin // try 200 times
    Pos := Sheet.ListPos - i;
    if Pos < 0 then Exit;

    if ReOpenImage(Sheet.List.GetName(Pos), Sheet, False) then begin
      SelectInGrid(Pos+1);
      break;
    end;

    Sheet.ListPos := Pos;
  end;

  Redraw;
end;

procedure TForm1.Gauss5_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.GaussBlur5;
  Redraw;
end;

procedure TForm1.CloseTab_PopElClick(Sender: TObject);
var Cur: Integer;
begin
  Cur := PopupTab.Tag;
  TTabSheet(PG.Pages[Cur]).Close;
end;

procedure TForm1.CloseOther_PopElClick(Sender: TObject);
var i: Integer;
    Cur: Integer;
begin
 Cur := PopupTab.Tag;

  for i:=PG.PageCount-1 downto 0 do begin
    if i <> Cur then TTabSheet(PG.Pages[i]).Close;
  end;
end;

procedure TForm1.CloseAll_PopElClick(Sender: TObject);
var i: Integer;
    Cur: Integer;
begin
  Cur := PopupTab.Tag;

  for i:=PG.PageCount-1 downto 0 do begin
    TTabSheet(PG.Pages[i]).Close;
  end;
end;

procedure TForm1.Next_PopElClick(Sender: TObject);
begin
  Next_MenuEl.Click;
end;

procedure TForm1.Prev_PopElClick(Sender: TObject);
begin
  Prev_MenuEl.Click;
end;

procedure TForm1.Copy_PopElClick(Sender: TObject);
begin
 Copy_Button.Click;
end;

procedure TForm1.Fullscreen_PopElClick(Sender: TObject);
begin
  Fullscreen_Button.Click;
end;

procedure TForm1.Trim_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.Trim(0,0,0);
 FixScrollbars;
 Redraw;
end;

procedure TForm1.Delete_MenuElClick(Sender: TObject);
{$IFDEF WINDOWS}
var fileOpStruct: TSHFileOpStruct;
    Sheet: TTabSheet;
begin
  if NoImage then Exit;

  fileOpStruct.Wnd := 0;
  fileOpStruct.wFunc := FO_DELETE;
  fileOpStruct.pFrom := PChar(TTabSheet(PG.ActivePage).Filename + #0 + #0);
  fileOpStruct.fFlags := FOF_ALLOWUNDO + FOF_NOCONFIRMATION;
  SHFileOperation( fileOpStruct );

  Sheet := GetSheet;
  Sheet.Close;
{$ENDIF}
end;

procedure TForm1.SmarterBlur_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.SmarterBlur;
  Redraw;
end;

procedure TForm1.Countcolors_MenuElClick(Sender: TObject);
begin
  if NoImage then Exit;

  ShowMessage('There are ' + IntToStr(GetBmp.CountColors) + ' unique colors');
end;

procedure TForm1.Fileinfo_MenuElClick(Sender: TObject);
var Bmp: TPV_Bitmap;
    Bpp: Integer;
begin
 if NoImage then Exit;

 Bmp := GetBmp;
 Bpp := Bmp.CountColors;

 if Bpp > 256 then Bpp := 24
 else if Bpp > 16 then Bpp := 8
 else if Bpp > 4 then Bpp := 4
 else if Bpp > 1 then Bpp := 2
 else Bpp := 1;

 with InfoDlg do begin
   Edit1.Text := Bmp.FormatName;
   Edit2.Text := IntToStr(Bmp.Width);
   Edit3.Text := IntToStr(Bmp.Height);
   Edit4.Text := IntToStr(FileUtil.Filesize(GetSheet.Filename));
   Edit5.Text := IntToStr(Bpp);
 end;

 InfoDlg.Show;
end;

procedure TForm1.BGR_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.BGR;
  Redraw;
end;

procedure TForm1.GBR_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.GBR;
 Redraw;
end;

procedure TForm1.RBG_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.RBG;
 Redraw;
end;

procedure TForm1.BRG_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.BRG;
 Redraw;
end;

procedure TForm1.GRB_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.GRB;
  Redraw;
end;

procedure TForm1.MenuItem59Click(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractRed;
 Redraw;
end;

procedure TForm1.Unsharp_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.Unsharp;
  Redraw;
end;

procedure TForm1.MenuItem60Click(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractGreen;
 Redraw;
end;

procedure TForm1.MenuItem61Click(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractBlue;
 Redraw;
end;

procedure TForm1.MenuItem62Click(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractAlpha;
 Redraw;
end;

procedure TForm1.Rename_MenuElClick(Sender: TObject);
var FName: String;
    OldName, NewName: String;
    Res: Boolean;
begin
 if NoImage then Exit;

 OldName := GetSheet.Filename;
 FName := ExtractFileName(OldName);

 FName := InputBox('Change filename', 'New filename', FName);

 NewName := ExtractFilePath(OldName) + FName;

 Res := RenameFile(OldName, NewName);

 if Res then begin
   GetSheet.Filename := NewName;
   GetSheet.Caption := FName;
 end;

end;

procedure TForm1.ZoomIn_MenuElClick(Sender: TObject);
begin
  ZoomIn_Button.Click;
end;

procedure TForm1.Zoom100_MenuElClick(Sender: TObject);
begin
  Zoom100_Button.Click;
end;

procedure TForm1.ZoomOut_MenuElClick(Sender: TObject);
begin
  ZoomOut_Button.Click;
end;

procedure TForm1.BestFit_MenuElClick(Sender: TObject);
var Sheet: TTabSheet;
    ZoomX, ZoomY: Extended;
begin
  if NoImage then Exit;

  Sheet := PG.ActivePage as TTabSheet;

  ZoomY := Sheet.Height/Sheet.Bmp.Height;
  ZoomX := Sheet.Width/Sheet.Bmp.Width;

  Sheet.Zoom := Min(ZoomX, ZoomY);
  FixScrollbars;
  ShowZoom;
  Redraw;
end;

procedure TForm1.ExtractC_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractCyan;
 Redraw;
end;

procedure TForm1.Sharpen_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.Sharpen;
  Redraw;
end;

procedure TForm1.ExtractM_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractMagenta;
 Redraw;
end;

procedure TForm1.ExtractY_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractYellow;
 Redraw;
end;

procedure TForm1.ExtractK_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.ExtractBlack;
 Redraw;
end;

procedure TForm1.RemoveAlpha_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.RemoveAlpha;
  Redraw;
end;

procedure TForm1.Opaque_PopElClick(Sender: TObject);
begin
 SaveUndo;
 GetBmp.Opaque;
 Redraw;
end;

procedure TForm1.Sepia_PopElClick(Sender: TObject);
begin
  with ParamsDlg do begin
    TheMin := 0;
    TheMax := 100;
    TheVal := 20;
    Setup;
    ShowModal;
  end;

  SaveUndo;
  GetBmp.Sepia(ParamsDlg.TheVal);
  Redraw;
end;

procedure TForm1.AddNoise_PopElClick(Sender: TObject);
begin
  with ParamsDlg do begin
    TheMin := 0;
    TheMax := 100;
    TheVal := 20;
    Setup;
    ShowModal;
  end;

  SaveUndo;
  GetBmp.AddNoise(ParamsDlg.TheVal);
  Redraw;
end;

procedure TForm1.FindEdges_PopElClick(Sender: TObject);
begin
  SaveUndo;
  GetBmp.FindEdges;
  Redraw;
end;

procedure TForm1.PGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Tabindex: Integer;
    P: TPoint;
begin
  if button = mbright then begin
    Tabindex := TPageControl(Sender).IndexOfTabAt(X, Y);
    if Tabindex >= 0 then begin
      P.X := X;
      P.Y := Y;
      P := TPageControl(Sender).ClientToScreen(P);

      PopupTab.Tag := TabIndex;
      PopupTab.PopUp(P.X, P.Y);
    end;
  end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Redraw;
end;

procedure TForm1.Colors_ButtonClick(Sender: TObject);
var P: TPoint;
    Button: TSpeedButton;
begin
  if NoImage then Exit;
  Button := Sender as TSpeedButton;

  P := Point(Button.Left, Button.Height);
  P := Form1.ClientToScreen(P);

  PopupColors.Popup(P.X, P.Y);
end;

procedure TForm1.Flip_ButtonClick(Sender: TObject);
var P: TPoint;
    Button: TSpeedButton;
begin
  if NoImage then Exit;
  Button := Sender as TSpeedButton;

  P := Point(Button.Left, Button.Height);
  P := Form1.ClientToScreen(P);

  PopupFlip.Popup(P.X, P.Y);
end;

procedure TForm1.ZoomIn_ButtonClick(Sender: TObject);
var Sheet: TTabSheet;
begin
  if NoImage then Exit;

  Sheet := PG.ActivePage as TTabSheet;

  Sheet.Zoom := ZoomIn(Sheet.Zoom);
  FixScrollbars;
  ShowZoom;
  Redraw;
end;

procedure TForm1.ZoomOut_ButtonClick(Sender: TObject);
var Sheet: TTabSheet;
begin
  if NoImage then Exit;

  Sheet := PG.ActivePage as TTabSheet;

  Sheet.Zoom := ZoomOut(Sheet.Zoom);
  FixScrollbars;
  ShowZoom;
  Redraw;
end;

procedure TForm1.Zoom100_ButtonClick(Sender: TObject);
var Sheet: TTabSheet;
begin
  if NoImage then Exit;

  Sheet := PG.ActivePage as TTabSheet;

  Sheet.Zoom := 1;
  FixScrollbars;
  ShowZoom;
  Redraw;
end;

procedure TForm1.Browse_ButtonClick(Sender: TObject);
var Sheet: TTabSheet;
begin
  if NoImage then begin
    ErrorBox('Open an image first');
    Exit;
  end;

  Sheet := GetSheet;

  Sheet.DG.RowCount := Sheet.List.FPos;
  Sheet.DG.Visible := not Sheet.DG.Visible;

  if Sheet.DG.Visible then begin
    Sheet.Split.Align := alNone;
    Sheet.DG.Align := alRight;
    Sheet.Split.Align := alRight;
  end;
end;

procedure TForm1.Paste_ButtonClick(Sender: TObject);
var Bmp: TBitmap;
begin
  if NoImage then Exit;

  if not Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then Exit;

  Bmp := TBitmap.Create;
  Bmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));

  GetBmp.CopyFrom(Bmp);

  Redraw;

  Bmp.Free;
end;

procedure TForm1.Filter_ButtonClick(Sender: TObject);
var P: TPoint;
    Button: TSpeedButton;
begin
  if NoImage then Exit;
  Button := Sender as TSpeedButton;

  P := Point(Button.Left, Button.Height);
  P := Form1.ClientToScreen(P);

  PopupFilter.Popup(P.X, P.Y);
end;

procedure TForm1.Open_ButtonClick(Sender: TObject);
var i: Integer;
begin
  if not OpenDialog1.Execute then Exit;

  for i:=0 to OpenDialog1.Files.Count-1 do
    OpenImage(OpenDialog1.Files[i]);
end;

procedure TForm1.SaveAs_ButtonClick(Sender: TObject);
begin
  if NoImage then Exit;

  if not SaveDialog1.Execute then Exit;

  GetBmp.SaveToFile(SaveDialog1.Filename);
end;

procedure TForm1.Rotate_ButtonClick(Sender: TObject);
var P: TPoint;
    Button: TSpeedButton;
begin
  if NoImage then Exit;
  Button := Sender as TSpeedButton;

  P := Point(Button.Left, Button.Height);
  P := Form1.ClientToScreen(P);

  PopupRotate.Popup(P.X, P.Y);
end;

procedure TForm1.Print_ButtonClick(Sender: TObject);
var Dest: TRect;
    Wid, Hei: Integer;
    Bmp: TBitmap;
begin
  if NoImage then Exit;

  if not PrintDialog1.Execute then Exit;

  Bmp := GetBmp.ToBitmap;

  Wid := Printer.PageWidth;
  Hei := Round(Wid / (Bmp.Width/Bmp.Height));

  Dest := Rect(0,0, Wid, Hei);

  Printer.BeginDoc;

  Printer.Canvas.StretchDraw(Dest, Bmp);

  Printer.EndDoc;

  Bmp.Free;
end;

procedure TForm1.Copy_ButtonClick(Sender: TObject);
var Bmp: TBitmap;
begin
  if NoImage then Exit;

  Bmp := GetBmp.ToBitmap;
  Clipboard.Assign(Bmp);
  Bmp.Free;
end;

procedure TForm1.Screenshot_ButtonClick(Sender: TObject);
var ScreenDC: HDC;
    Bitmap: TBitmap;
begin
  OpenImage('');

  Hide;
  Sleep(300);
  Application.ProcessMessages;
  Bitmap := TBitmap.Create;
  ScreenDC := GetDC(0);
  try
    Bitmap.LoadFromDevice(ScreenDC);

    GetBmp.SetSize(Bitmap.Width, Bitmap.Height);
    GetBmp.CopyFrom(Bitmap);
    Redraw;

  finally
    Bitmap.Free;
    ReleaseDC(0, ScreenDC);
    Show;
  end;
end;

procedure TForm1.Resize_ButtonClick(Sender: TObject);
begin
  if NoImage then Exit;

  ResizeDlg.UpDown3.Position := GetBmp.Width;
  ResizeDlg.UpDown4.Position := GetBmp.Height;
  ResizeDlg.Show;
end;

procedure TForm1.Fullscreen_ButtonClick(Sender: TObject);
begin
  if IsFullScreen then LeaveFullscreen
  else EnterFullscreen;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var Sheet: TTabSheet;
    i: Integer;
    Bmp: TPV_Bitmap;
    FInfo: TFileInfo;
    AllDone: Boolean;
begin
  Sheet := GetSheet;

  if Sheet.List = nil then Exit;

  Bmp := TPV_Bitmap.Create;

  AllDone := True;

  for i:=0 to Sheet.List.FPos-1 do begin
    Sheet.List.Get(i, FInfo);

    if FInfo.Width = 0 then begin

      if not Bmp.LoadFromFile(Sheet.List.GetName(i)) then continue;

      Bmp.Resize(180,120);

      Sheet.List.Edit(i, Bmp.Width, Bmp.Height, Bmp);
      AllDone := False;

      break;
    end;
  end;

  if AllDone then Timer1.Enabled := False;

  Bmp.Free;
end;

procedure TForm1.EnterFullscreen;
var i: Integer;
begin
  for i:=0 to PG.PageCount-1 do
    PG.Pages[i].TabVisible := False;

  WinSize := Rect(Left, Top, Left+Width, Top+Height);

  Form1.BorderStyle := bsNone;
  Panel1.Visible := False;
  StatusBar1.Visible := False;
  Form1.Menu := nil;

  PG.TabIndex := 0;
  IsFullScreen := True;

  Form1.BoundsRect := Rect(0,0, Screen.Width, Screen.Height);
end;

procedure TForm1.LeaveFullscreen;
var i: Integer;
begin
  for i:=0 to PG.PageCount-1 do
    PG.Pages[i].TabVisible := True;

  Form1.BorderStyle := bsSingle;
  Panel1.Visible := True;
  StatusBar1.Visible := True;
  Form1.WindowState := wsNormal;
  Form1.Menu := MainMenu1;

  PG.TabIndex := 0;
  IsFullScreen := False;

  Form1.BoundsRect := WinSize;
end;

procedure TForm1.ShowInfo;
begin
  StatusBar1.Panels[0].Text := Format('%d x %d', [GetBmp.Width, GetBmp.Height]);
end;

procedure TForm1.ShowZoom;
begin
  StatusBar1.Panels[1].Text := IntToStr(Round(GetSheet.Zoom * 100)) + ' %';
end;

procedure TForm1.ErrorBox(Msg: String);
begin
  MessageDlg('Error', Msg, mtError, [mbOK], 0);
end;

function Glue(Delimiter: String; List: TStrings): String;
var i: Integer;
begin
  Result := '';

  if List.Count = 0 then Exit;

  Result := List[0];

  for i:=1 to List.Count-1 do
    Result := Result + Delimiter + List[i];
end;

procedure TForm1.AddSaveFormats;
var List: TStringList;
begin
 List := TStringList.Create;
 List.Add('TGA - Truevision Targa|*.tga');
 List.Add('TIFF - Thousands of Incompatible File Formats|*.tif');
 List.Add('PSD - Adobe Photoshop Document|*psd');
 List.Add('PPM - Portable PixMap|*.ppm');
 List.Add('PGM - Portable GrayMap|*.pgm');
 List.Add('PBM - Portable BitMap|*.pbm');
 List.Add('HRZ - Slow Scan Television|*.hrz');
 List.Add('RAS - Sun Raster|*.ras');
 List.Add('PFM - Portable FloatMap|*.pfm');
 List.Add('PCX - ZSoft PiCture eXchange|*.pcx');
 List.Add('565 - OLPC 565|*.565');
 List.Add('CSV - Comma Separated Values|*.csv');
 List.Add('DIS - DKB Trace|*.dis');
 List.Add('GBR - GIMP Brush|*.gbr');
 List.Add('IIM - InShape|*.iim');
 List.Add('GR8 - GR8|*.gr8');
 List.Add('IPI - IPI|*.ipi');
 List.Add('NPM - Nokia Picture Message|*.npm');
 List.Add('NGG - Nokia Group Graphics|*.ngg');
 List.Add('PGF - Portfolio Graphics|*.pgf');
 List.Add('PSA - Print Shop|*.psa');
 List.Add('VBM - VDC BitMap|*.vbm');
 List.Add('VST - Vista|*.vst');
 List.Add('B_W - ImageLab|*.b_w');
 List.Add('DA4 - PaintShop|*.da4');
 List.Add('DOO - Atari DOO |*.doo');
 List.Add('KFX - Atari KFX|*.kfx');
 List.Add('XGA - Falcon XGA|*.xga');
 List.Add('VOLLABEL - Apple Volume Label|*.vollabel');
 List.Add('DRG - Atari CAD|*.drg');
 List.Add('PAT - GIMP Pattern|*.pat');
 List.Add('RWL - Img Scan|*.rwl');
 List.Add('AVS - Stardent|*.avs');
 List.Add('GR9 - Atari GR9|*.gr9');
 List.Add('GHG - Gephard Hires|*.ghg');
 List.Add('AAI - Dune HD|*.aai');
 List.Add('NLM - Nokia Logo Manager|*.nlm');
 List.Add('WBMP - Wireless Protocol Bitmap|*.wbmp');
 List.Add('SGI - Silicon Graphics Image|*.sgi');
 List.Add('KRO - AutoPano RAW|*.kro');
 List.Add('PIC - Bio-Rad|*.pic');
 List.Add('CEL - Autodesk Animator|*.cel');
 List.Add('MSP - Microsoft Paint 1|*.msp');
 List.Add('411 - Sony Mavica|*.411');
 List.Add('BOB - Bob Ray Tracer|*.bob');

 List.Add('CM - Puzzle Image|*.cm');
 List.Add('MBG - Mad Designer|*.mbg');
 List.Add('DAP - Atari Slideshow|*.dap');
 List.Add('64C - C-64 font|*.64c');
 List.Add('CHS - Oric|*.chs');
 List.Add('TIM - Playstation TIM|*.tim');
 List.Add('BG9 - Atari BG9|*.bg9');
 List.Add('MBM - Psion Series 5 Bitmap|*.mbm');
 List.Add('SKP - Sketch-PadDles|*.skp');

 List.Sort;

 //List.DelimitedText adds quotes

 SaveDialog1.Filter := 'PNG - Portable Network Graphic|*.png|'+
                       'BMP - Windows Bitmap|*.bmp|'+
                       'JPEG - Joint Pictures Expert Group|*.jpg|'+
                       'GIF - Graphics Interchange Format|*.gif|' + Glue('|', List);
 List.Free;

end;

procedure TForm1.SelectInGrid(Row: Integer);
var Sheet: TTabSheet;
    Rect: TGridRect;
begin
  Sheet := GetSheet;

  Rect.Top := Row;
  Rect.Left := 0;
  Rect.Bottom := Row;
  Rect.Right := 4;

  //Sheet.DG.Selection := Rect;
 // Sheet.DG.Row := Row;
end;

procedure TForm1.OpenImage(Filename: String);
var Sheet: TTabSheet;
    Panel: TPanel;
    Image: TImage;
    Res: Boolean;
    Grid: TDrawGrid;
    Split: TSplitter;
begin
  Sheet := TTabSheet.Create(PG);
  Sheet.Parent := PG;
  Sheet.GridActive := False;

  Sheet.Bmp := TPV_Bitmap.Create;

  if Filename = '' then begin
    Sheet.Caption := 'Untitled';
    Res := True;
  end
  else
    Res := Sheet.Bmp.LoadFromFile(Filename);

  if not Res then begin
    ErrorBox('Could not read file "' + ExtractFilename(Filename) + '"');
    Sheet.Close;
    Exit;
  end;

  Sheet.Zoom := 1;
  Sheet.Caption := ExtractFileName(Filename);

  Sheet.List := nil;

  Grid := TDrawGrid.Create(Sheet);
  Grid.Parent := Sheet;
  Grid.Align := alRight;
  Grid.FixedCols := 0;
  //Grid.FixedRows := 0;
  Grid.OnDrawCell := @OnGridDrawCell;
  Grid.OnSelectCell := @OnGridSelectCell;
  Grid.OnResize := @OnGridResize;
  //Grid.DefaultDrawing := False;
  Grid.Options := Grid.Options + [goRowSelect, goColSizing];
  Grid.Enabled := True;
  Grid.Visible := False;
  Sheet.DG := Grid;

  Split := TSplitter.Create(Sheet);
  Split.Parent := Sheet;
  Split.Align := alRight;
  Sheet.Split := Split;

  Panel := TPanel.Create(Sheet);
  Panel.Parent := Sheet;
  Panel.Align := alClient;
  Panel.Color := clBtnShadow;
  Panel.OnPaint := @OnPanelPaint;
  Panel.BevelOuter := bvNone;
  Sheet.Panel := Panel;

  Sheet.ScrollbarV := TScrollBar.Create(Sheet);
  with Sheet.ScrollbarV do begin
    Parent := Panel;
    Kind := sbVertical;
    Align := alRight;
    Width := 20;
    OnChange := @ScrollBar1Change;
  end;

  Sheet.ScrollbarH := TScrollBar.Create(Sheet);
  with Sheet.ScrollbarH do begin
    Parent := Panel;
    Align := alBottom;
    Height := 20;
    OnChange := @ScrollBar1Change;
  end;

  Sheet.Filename := Filename;
  Sheet.UndoPos := 0;
  PG.ActivePageIndex := PG.PageCount-1;

  FixScrollbars;
  ShowInfo;

  Sheet.List := TFileList.Create;
  Sheet.ListFiles(Filename);

  SelectInGrid(Sheet.ListPos);

  Timer1.Enabled := True;

  Sheet.GridActive := True;
 end;

function TForm1.ReOpenImage(Filename: String; ASheet: TTabSheet; DoList: Boolean = True): Boolean;
begin
  Result := ASheet.Bmp.LoadFromFile(Filename);

  if not Result then Exit;

  ASheet.Zoom := 1;
  ASheet.Caption := ExtractFileName(Filename);
  ASheet.Filename := Filename;
  ASheet.UndoPos := 0;

  FixScrollbars;
  ShowInfo;

  if DoList then
  ASheet.ListFiles(Filename);

  ASheet.Repaint;
end;


procedure TForm1.DrawBmp(Image: TImage; Bmp: TPV_Bitmap);
var Panel: TPanel;
begin
  Panel := PG.ActivePage.Controls[0] as TPanel;
end;

function TForm1.NoImage: Boolean;
begin
  Result := (PG.ActivePage = nil);
end;

procedure TForm1.Redraw;
var Sheet: TTabSheet;
begin
 Sheet := GetSheet;
 Sheet.Panel.Repaint;
end;

function TForm1.ZoomIn(Val: Extended): Extended;
var i: Integer;
begin
  for i:=0 to High(Levels) do
    if Levels[i] > Val then Exit(Levels[i]);

  Result := Levels[High(Levels)];
end;

function TForm1.ZoomOut(Val: Extended): Extended;
var i: Integer;
begin
  for i:=High(Levels) downto 0 do
    if Levels[i] < Val then Exit(Levels[i]);

  Result := Levels[0];
end;

procedure TForm1.SaveUndo;
var i: Integer;
begin
  if GetSheet.UndoPos = 10 then begin
    GetSheet.UndoList[0].Free;

    for i:=0 to 9 do
      GetSheet.UndoList[i] := GetSheet.UndoList[i+1];
  end;

  GetSheet.UndoList[GetSheet.UndoPos] := TPV_Bitmap.Create;
  GetSheet.UndoList[GetSheet.UndoPos].CopyFrom(GetBmp);

  Inc(GetSheet.UndoPos);
end;

procedure TForm1.Undo;
begin
  if GetSheet.UndoPos < 1 then Exit;

  GetBmp.CopyFrom( GetSheet.UndoList[GetSheet.UndoPos-1] );
  Dec(GetSheet.UndoPos);
  Redraw;
end;

procedure TForm1.FixScrollbars;
var Sheet: TTabSheet;
    Bmp: TPV_Bitmap;
    Wid, Hei: Integer;
begin
  Sheet := GetSheet;
  Bmp   := GetBmp;

  Sheet.ScrollBarV.Position := 0;
  Sheet.ScrollBarH.Position := 0;

  Wid := Bmp.Width - Round(PG.Width / Sheet.Zoom);

  Hei := Bmp.Height - Round(PG.Height / Sheet.Zoom);

  Sheet.ScrollBarH.Max := Max(0, Wid);
  Sheet.ScrollBarV.Max := Max(0, Hei);

  Sheet.ScrollBarV.Enabled := Hei > 0;
  Sheet.ScrollBarH.Enabled := Wid > 0;
end;

procedure TForm1.OnPanelPaint(Sender: TObject);
var Panel: TPanel;
    Sheet: TTabSheet;
    Bmp: TPV_Bitmap;
    Dest: TRect;
    Piece: TBitmap;
    Wid, Hei: Integer;
    DestWid, DestHei, DestLeft, DestTop: Integer;
    SheetWid, SheetHei: Integer;
begin
  Panel := Sender as TPanel;
  Sheet := TTabSheet(Panel.Parent);

  if Sheet.Zoom = 0 then Exit;

  Bmp := Sheet.Bmp;

  SheetWid := Panel.Width - 20;
  SheetHei := Panel.Height - 20;

  Wid := Round(SheetWid  / Sheet.Zoom);
  Hei := Round(SheetHei / Sheet.Zoom);

  Wid := Min(Wid, Bmp.Width);
  Hei := Min(Hei, Bmp.Height);

  Piece := Bmp.GetPiece(Sheet.ScrollBarH.Position, Sheet.ScrollBarV.Position, Wid, Hei);

  //Piece.SaveToFile('__piece.bmp');

  DestWid := Round(Wid*Sheet.Zoom);
  DestHei := Round(Hei*Sheet.Zoom);
  DestLeft := (SheetWid- DestWid) div 2;
  DestTop  := (SheetHei - DestHei) div 2;

  Dest := Rect(DestLeft, DestTop, DestLeft+DestWid, DestTop+DestHei);

  Panel.Canvas.Brush.Color := Panel.Color;
  Panel.Canvas.FillRect(0,0, Panel.Width, Panel.Height);
  Panel.Canvas.StretchDraw(Dest, Piece);
  Piece.Free;
end;

function TForm1.GetBmp: TPV_Bitmap;
var Sheet: TTabSheet;
begin
  if NoImage then Exit(nil);

  Sheet := PG.ActivePage as TTabSheet;

  Result := Sheet.Bmp;
end;

function TForm1.GetSheet: TTabSheet;
begin
  Result := PG.ActivePage as TTabSheet;
end;

end.

