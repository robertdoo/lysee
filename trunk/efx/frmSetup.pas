unit frmSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Printers, ComCtrls, WinSpool;

type
  TPageSetupInfo = record
    Left: integer;           {<--左边距: 毫米}
    Top: integer;            {<--上边距: 毫米}
    Right: integer;          {<--右边距: 毫米}
    Bottom: integer;         {<--下边距: 毫米}
    HorzCenter: boolean;     {<--是否水平居中}
    VertCenter: boolean;     {<--是否垂直居中}
  end;
  PPageSetupInfo = ^TPageSetupInfo;

  TPageSetupForm = class(TForm)
    GroupBox1: TGroupBox;
    eLeft: TLabeledEdit;
    eRight: TLabeledEdit;
    eTop: TLabeledEdit;
    eBottom: TLabeledEdit;
    eHorzCenter: TCheckBox;
    eVertCenter: TCheckBox;
    GroupBox2: TGroupBox;
    lsPrinter: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    udLeft: TUpDown;
    udRight: TUpDown;
    udTop: TUpDown;
    udBottom: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    lsPaper: TComboBox;
    bPortrait: TRadioButton;
    bLandscape: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure lsPrinterChange(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    FPrinterIndex: integer;
  end;

var
  PageSetupForm: TPageSetupForm;
  PageInfo: TPageSetupInfo = (
    Left               :15;
    Top                :15;
    Right              :15;
    Bottom             :15;
    HorzCenter         :false;
    VertCenter         :false;
  );

function PageSetup: boolean;

function SetPaper(Paper: integer; Landscape: boolean): boolean;
function GetPaper(var Paper: integer; var Landscape: boolean): boolean;

procedure SavePageInfo;
procedure RestorePageInfo;

implementation

uses
  lse_efx, lse_devcaps, Math;

function MsgBox(const Msg, Title: string; Flags: Integer): integer;
begin
	Result := Application.MessageBox(PChar(Msg), PChar(Title), Flags);
end;

function MsgErr(const Msg: string): boolean;
begin
	Result := MsgBox(Msg, '对不起', MB_ICONERROR + MB_OK) = IDOK;
end;

function PageSetup: boolean;
begin
  Result := HasPrinter;
  if not Result then
  begin
    MsgErr('未找到打印机！');
    Exit;
  end;
  with TPageSetupForm.Create(Application) do
  try
    Result := (ShowModal = mrOK);
  finally
    Release;
  end;
end;

function SetPaper(Paper: integer; Landscape: boolean): boolean;
var
  dev, drv, Port : array[0..255] of char;
  hDMode: THandle;
  pDMode: PDEVMODE;
begin
  Result := HasPrinter;
  if not Result then
  begin
    MsgErr('未找到打印机！');
    Exit;
  end;
  Printer.PrinterIndex := Printer.PrinterIndex;
  Printer.GetPrinter(dev, drv, Port, hDMode);
  PDMode := GlobalLock(hDMode);
  if pDMode^.dmPaperSize <> Paper then
  begin
    pDMode^.dmPaperSize := Paper;
    pDMode^.dmFields := pDMode^.dmFields or dm_PaperSize;
  end;
  GlobalUnlock(hDMode);
  Printer.PrinterIndex := Printer.PrinterIndex;
  if Landscape then
    Printer.Orientation := poLandscape else
    Printer.Orientation := poPortrait;
  set_capsrec(Printer.Handle, Printercap);
end;

function GetPaper(var Paper: integer; var Landscape: boolean): boolean;
var
  dev, drv, Port : array[0..255] of char;
  hDMode: THandle;
  pDMode: PDEVMODE;
begin
  Result := HasPrinter;
  if not Result then
  begin
    MsgErr('未找到打印机！');
    Exit;
  end;
  Printer.PrinterIndex := Printer.PrinterIndex;
  Printer.GetPrinter(dev, drv, Port, hDMode);
  PDMode := GlobalLock(hDMode);
  Paper := pDMode^.dmPaperSize;
  GlobalUnlock(hDMode);
  Printer.PrinterIndex := Printer.PrinterIndex;
  Landscape := (Printer.Orientation = poLandscape);
end;

function SetPageInfo(Source, Desti: PPageSetupInfo): PPageSetupInfo;
begin
  Move(Source^, Desti^, sizeof(TPageSetupInfo));
  Result := Desti;
end;

var
  TmpPageInfo: TPageSetupInfo;
  TmpPaper: integer;
  TmpLandscape: boolean;

procedure SavePageInfo;
begin
  if HasPrinter then
  begin
    SetPageInfo(Addr(PageInfo), Addr(TmpPageInfo));
    GetPaper(TmpPaper, TmpLandscape);
  end;
end;

procedure RestorePageInfo;
begin
  if HasPrinter then
  begin
    SetPageInfo(Addr(TmpPageInfo), Addr(PageInfo));
    SetPaper(TmpPaper, TmpLandscape);
  end;
end;

{$R *.dfm}

procedure TPageSetupForm.FormCreate(Sender: TObject);
begin
  lsPrinter.OnChange := nil;
  FPrinterIndex := Printer.PrinterIndex;
  lsPrinter.Items.Assign(Printer.Printers);
  lsPrinter.ItemIndex := FPrinterIndex;
  lsPrinter.OnChange := lsPrinterChange;
  lsPrinterChange(nil);
  udLeft.Position := PageInfo.Left;
  udRight.Position := PageInfo.Right;
  udTop.Position := PageInfo.Top;
  udBottom.Position := PageInfo.Bottom;
  eHorzCenter.Checked := PageInfo.HorzCenter;
  eVertCenter.Checked := PageInfo.VertCenter;
end;

type
  TName = array[0..63] of Char;
  TNameList = array[1..1024] of TName;
  PNameList = ^TNameList;
  TWordList = array[1..1024] of word;
  PWordList = ^TWordList;

procedure TPageSetupForm.lsPrinterChange(Sender: TObject);
var
  Dev, Drv, Port: array [0..255] of Char;
  Buf: Pointer;
  hDMode: THandle;
  pDMode: PDEVMODE;
  A, X, Papers: Integer;
  PNames: PNameList;
  PWords: PWordList;
begin
  Printer.PrinterIndex := lsPrinter.ItemIndex;
  try
    lsPaper.Clear;
    Printer.GetPrinter(Dev, Drv, Port, hDMode);
    Papers := WinSpool.DeviceCapabilities(Dev, Port, DC_PAPERNAMES, Nil, Nil);
    GetMem(Buf, Papers * Sizeof(TName));
    try
      // 1. list all papers supported
      WinSpool.DeviceCapabilities(Dev, Port, DC_PAPERNAMES, Buf, Nil);
      PNames := Buf;
      for A := 1 to Papers do
        lsPaper.Items.Add(PNames^[A]);
      WinSpool.DeviceCapabilities(Dev, Port, DC_PAPERS, Buf, Nil);
      PWords := Buf;
      X := 0;
      for A := 1 to Papers do
      begin
        lsPaper.Items.Objects[A - 1] := TObject(PWords^[A]);
        if PWords^[A] = DMPAPER_USER then
          X := A;
      end;
      if X > 0 then
        lsPaper.Items.Delete(X - 1);

      // 2. select current pager
      pDMode := GlobalLock(hDMode);
      if pDMode <> nil then
      begin
        X := pDMode^.dmPaperSize;
        GlobalUnlock(hDMode);
      end
      else X := DMPAPER_USER;
      lsPaper.ItemIndex := lsPaper.Items.IndexOfObject(TObject(X));
      if lsPaper.ItemIndex < 0 then
        lsPaper.ItemIndex := 0;

      // 3. set paper direction
      if Printer.Orientation = poPortrait then
        bPortrait.Checked := true else
        bLandscape.Checked := true;
    finally
      FreeMem(Buf);
    end;
  finally
    Printer.PrinterIndex := FPrinterIndex;
  end;
end;

procedure TPageSetupForm.bOKClick(Sender: TObject);
var
  dev, drv, Port : array[0..255] of char;
  Paper: integer;
  hDMode: THandle;
  pDMode: PDEVMODE;
begin
  Printer.PrinterIndex := lsPrinter.ItemIndex;
  Paper := integer(lsPaper.Items.Objects[lsPaper.ItemIndex]);
  Printer.GetPrinter(dev, drv, Port, hDMode);
  PDMode := GlobalLock(hDMode);
  if pDMode^.dmPaperSize <> Paper then
  begin
    pDMode^.dmPaperSize := Paper;
    pDMode^.dmFields := pDMode^.dmFields or dm_PaperSize;
  end;
  GlobalUnlock(hDMode);
  Printer.PrinterIndex := lsPrinter.ItemIndex;
  if bPortrait.Checked then
    Printer.Orientation := poPortrait else
    Printer.Orientation := poLandscape;
  PageInfo.Left := udLeft.Position;
  PageInfo.Right := udRight.Position;
  PageInfo.Top := udTop.Position;
  PageInfo.Bottom := udBottom.Position;
  PageInfo.HorzCenter := eHorzCenter.Checked;
  PageInfo.VertCenter := eVertCenter.Checked;
  set_capsrec(Printer.Handle, Printercap);
  ModalResult := mrOK;
end;

end.
