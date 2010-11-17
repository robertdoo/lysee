unit efxPreview;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ComCtrls, ToolWin, ExtCtrls, lse_efx, StdCtrls;

type
  TEfxEditBook = procedure(Sender: TObject; EasyForm: TEasyForm) of object;
  TEfxSaveBook = procedure(Sender: TObject; var fname: string) of object;

  TPreviewFrame = class(TFrame)
    ClientPanel: TPanel;
    ViewBox: TScrollBox;
    pnView: TPanel;
    pbView: TPaintBox;
    pnLeft: TPanel;
    pnTop: TPanel;
    pbTop: TPaintBox;
    pnBottom: TPanel;
    pbBottom: TPaintBox;
    pnRight: TPanel;
    ToolBar: TToolBar;
    btnFirst: TToolButton;
    btnPrev: TToolButton;
    btnNext: TToolButton;
    btnLast: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnSetup: TToolButton;
    btnPrint: TToolButton;
    btnEdit: TToolButton;
    btnSave: TToolButton;
    btnClose: TToolButton;
    StatusBar: TStatusBar;
    PrintMenu: TPopupMenu;
    pmiPrint: TMenuItem;
    pmiPrintCurrent: TMenuItem;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    cbxRatio: TComboBox;
    procedure OnButtonClick(Sender: TObject);
    procedure pbViewPaint(Sender: TObject);
    procedure pbTopPaint(Sender: TObject);
    procedure pbBottomPaint(Sender: TObject);
    procedure cbxRatioChange(Sender: TObject);
  private
    FZoomRatio: integer;
    FRatio: integer;
    FSource: TEasyForm;
    FForm: TEasyForm;
    FPage: TEasyForm;
    FIndex: integer;
    FPixelsPerCM: integer;
    FSavingCount: integer;
    FSavingIndex: integer;
    FOnClose: TNotifyEvent;
    FOnEdit: TEfxEditBook;
    FOnSave: TEfxSaveBook;
    FOnPrint: TNotifyEvent;
    FOnSetup: TNotifyEvent;
    FOnChangePage: TNotifyEvent;
    FOnZoom: TNotifyEvent;
    FOnBeginSave: TNotifyEvent;
    FOnEndSave: TNotifyEvent;
    function GetPageCount: integer;
    function GetPage(Index: integer): TEasyForm;
    procedure SetPageIndex(Value: integer);
    procedure SetRatio(Value: integer);
    function GetShowToolBar: boolean;
    procedure SetShowToolBar(Value: boolean);
    function GetShowStatusBar: boolean;
    procedure SetShowStatusBar(Value: boolean);
    procedure SetZoomRatio(Value: integer);
    function GetFlat: boolean;
    procedure SetFlat(Value: boolean);
    procedure SetOnEdit(Value: TEfxEditBook);
    procedure SetSource(Value: TEasyForm);
    procedure SetOnSave(Value: TEfxSaveBook);
    procedure SetOnClose(Value: TNotifyEvent);
    procedure SetOnPrint(Value: TNotifyEvent);
    procedure SetOnSetup(Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure First;
    procedure Prev;
    procedure Next;
    procedure Last;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure Setup;
    procedure Print;
    procedure PrintCurrent;
    procedure Edit;
    procedure Save;
    procedure SaveToFile(const fname: string);
    procedure Close;
    procedure SetStatusText(Index: integer; const Text: string);
    procedure ResizePagePanel;
    procedure ShowPagerAndRatio;
    property PageCount: integer read GetPageCount;
    property Page[Index: integer]: TEasyForm read GetPage;
    property PageIndex: integer read FIndex write SetPageIndex;
    property ShowToolBar: boolean read GetShowToolBar write SetShowToolBar;
    property ShowStatusBar: boolean read GetShowStatusBar write SetShowStatusBar;
    property Flat: boolean read GetFlat write SetFlat;
    property Ratio: integer read FRatio write SetRatio;
    property Source: TEasyForm read FSource write SetSource;
    property ZoomRatio: integer read FZoomRatio write SetZoomRatio;
    property OnSetup: TNotifyEvent read FOnSetup write SetOnSetup;
    property OnPrint: TNotifyEvent read FOnPrint write SetOnPrint;
    property OnEdit: TEfxEditBook read FOnEdit write SetOnEdit;
    property OnSave: TEfxSaveBook read FOnSave write SetOnSave;
    property OnClose: TNotifyEvent read FOnClose write SetOnClose;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnBeginSave: TNotifyEvent read FOnBeginSave write FOnBeginSave;
    property OnEndSave: TNotifyEvent read FOnEndSave write FOnEndSave;
  end;

function PreviewOn(Ctrl: TWinControl; book: TEasyForm; Index: integer): TPreviewFrame;

implementation

uses
  lse_devcaps, frmSetup, lse_msgbox, Math;

{$R *.dfm}

function PreviewOn(Ctrl: TWinControl; book: TEasyForm; Index: integer): TPreviewFrame;
begin
  Result := TPreviewFrame.Create(Ctrl);
  Result.Parent := Ctrl;
  Result.Source := book;
  Result.PageIndex := 0;
  Result.Visible := true;
end;

{ TPreviewFrame }

procedure TPreviewFrame.Close;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

constructor TPreviewFrame.Create(AOwner: TComponent);
begin
  inherited;
  FPixelsPerCM := mtop_y(Screencap, 5);
  FRatio := 100;
  FZoomRatio := 3;
  pnLeft.Visible := HasPrinter;
  pnTop.Visible := HasPrinter;
  pnRight.Visible := HasPrinter;
  pnBottom.Visible := HasPrinter;
  SetOnSetup(FOnSetup);
  SetOnPrint(FOnPrint);
  SetOnEdit(FOnEdit);
  SetOnSave(FOnSave);
  SetOnClose(FOnClose);
end;

destructor TPreviewFrame.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

procedure TPreviewFrame.First;
begin
  SetPageIndex(0);
end;

function TPreviewFrame.GetPage(Index: integer): TEasyForm;
begin
  Result := FForm;
end;

function TPreviewFrame.GetPageCount: integer;
begin
  if FForm <> nil then
    Result := 1 else
    Result := 0;
end;

function TPreviewFrame.GetShowStatusBar: boolean;
begin
  Result := StatusBar.Visible;
end;

function TPreviewFrame.GetShowToolBar: boolean;
begin
  Result := ToolBar.Visible;
end;

procedure TPreviewFrame.Last;
begin
  SetPageIndex(PageCount - 1);
end;

procedure TPreviewFrame.OnButtonClick(Sender: TObject);
begin
  if Sender = btnFirst then First else
  if Sender = btnPrev then Prev else
  if Sender = btnNext then Next else
  if Sender = btnLast then Last else
  if Sender = btnZoomIn then ZoomIn else
  if Sender = btnZoomOut then ZoomOut else
  if Sender = btnSetup then Setup else
  if Sender = btnPrint then Print else
  if Sender = pmiPrint then Print else
  if Sender = pmiPrintCurrent then PrintCurrent else
  if Sender = btnEdit then Edit else
  if Sender = btnSave then Save else
  if Sender = btnClose then Close;
end;

procedure TPreviewFrame.Next;
begin
  SetPageIndex(Min(PageCount - 1, FIndex + 1));
end;

procedure TPreviewFrame.pbBottomPaint(Sender: TObject);
var
  P: TPoint;
  R: TRect;
begin
  with pbBottom do
  begin
    R := ClientRect;
    Canvas.FillRect(R);
    Canvas.Pen.Style := psDot;
    P := Point(pnLeft.Width - 1, 0);
    Canvas.MoveTo(P.X - FPixelsPerCM, P.Y);
    Canvas.LineTo(P.X, P.Y);
    Canvas.LineTo(P.X, P.Y + FPixelsPerCM);
    P.X := pnRight.Left;
    Canvas.MoveTo(P.X + FPixelsPerCM, P.Y);
    Canvas.LineTo(P.X, P.Y);
    Canvas.LineTo(P.X, P.Y + FPixelsPerCM);
  end;
end;

procedure TPreviewFrame.pbTopPaint(Sender: TObject);
var
  P: TPoint;
  R: TRect;
begin
  with pbTop do
  begin
    R := ClientRect;
    Canvas.FillRect(R);
    Canvas.Pen.Style := psDot;
    P := Point(pnLeft.Width - 1, R.Bottom - 1);
    Canvas.MoveTo(P.X - FPixelsPerCM, P.Y);
    Canvas.LineTo(P.X, P.Y);
    Canvas.LineTo(P.X, P.Y - FPixelsPerCM);
    P.X := pnRight.Left;
    Canvas.MoveTo(P.X + FPixelsPerCM, P.Y);
    Canvas.LineTo(P.X, P.Y);
    Canvas.LineTo(P.X, P.Y - FPixelsPerCM);
  end;
end;

procedure TPreviewFrame.pbViewPaint(Sender: TObject);
var
  P: TPoint;
begin
  pbView.Canvas.FillRect(pbView.ClientRect);
  if Assigned(FPage) then
  begin
    P := Point(0, 0);
    if PageInfo.HorzCenter then
      P.X := (pbView.Width - FPage.Width) div 2;
    if PageInfo.VertCenter then
      P.Y := (pbView.Height - FPage.Height) div 2;
    FPage.Draw(pbView.Canvas, P);
  end;
end;

procedure TPreviewFrame.Prev;
begin
  SetPageIndex(Max(0, FIndex - 1));
end;

procedure TPreviewFrame.ResizePagePanel;
begin
  if HasPrinter then
  begin
    pnLeft.Width := mtop_x(Screencap, PageInfo.Left);
    pnRight.Width := mtop_x(Screencap, PageInfo.Right);
    pnTop.Height := mtop_y(Screencap, PageInfo.Top);
    pnBottom.Height := mtop_y(Screencap, PageInfo.Bottom);
    pnView.Width := mtop_x(Screencap, mm_width(Printercap));
    pnView.Height := mtop_y(Screencap, mm_height(Printercap));
    pbTop.Invalidate;
    pbBottom.Invalidate;
  end
  else
  if (FPage <> nil) and (FIndex >= 0) then
  begin
    pnView.Height := FPage.Height;
    pnView.Width := FPage.Width;
  end;
  pbView.Invalidate;
end;

procedure TPreviewFrame.SetPageIndex(Value: integer);
begin
  FPage := GetPage(Value);
  FIndex := Value;
  ShowPagerAndRatio;
  btnFirst.Enabled := FIndex > 0;
  btnPrev.Enabled := FIndex > 0;
  btnNext.Enabled := FIndex < PageCount - 1;
  btnLast.Enabled := FIndex < PageCount - 1;
  ResizePagePanel;
  if Assigned(FOnChangePage) then
    FOnChangePage(Self);
end;

procedure TPreviewFrame.SetRatio(Value: integer);
{var
  C: TEasyForm;
  S: TEasyForm;
  R: double;
  A: integer;
  X: integer;
  ratio: string;}
begin
{ FRatio := Max(1, Value);
  btnZoomout.Enabled := FRatio > FZoomRatio;
  R := FRatio / 100;
  if FSource = nil then Exit;

  for X := 0 to PageCount - 1 do
  begin
    S := FSource;
    C := FForm;
    for A := 0 to S.ColCount - 1 do
    	C.ColWidths[A] := Round(S.ColWidths[A] * R);

    for A := 0 to S.RowCount - 1 do
    	C.RowHeights[A] := Round(S.RowHeights[A] * R);

    for A := 0 to S.ItemCount - 1 do
    begin
    	C.Items[A].FontSize := Round(S.Items[A].FontSize * R);
      C.Items[A].LineDistance := Round(S.Items[A].LineDistance * R);
    end;
  end;

  if HasPrinter then
    pbView.Invalidate else
    ResizePagePanel;

  if Assigned(FOnZoom) then
    FOnZoom(Self);

  ratio := IntToStr(FRatio);
  X := cbxRatio.Items.IndexOf(ratio);
  cbxRatio.OnChange := nil;
  try
    if X < 0 then
    begin
      X := 0;
      while X < cbxRatio.Items.Count do
      begin
        if StrToInt(cbxRatio.Items[X]) > FRatio then Break;
        Inc(X);
      end;
      cbxRatio.Items.Insert(X, ratio);
    end;
    cbxRatio.ItemIndex := X;
  finally
    cbxRatio.OnChange := cbxRatioChange;
  end;

  ShowPagerAndRatio;}
end;

procedure TPreviewFrame.SetShowStatusBar(Value: boolean);
begin
  StatusBar.Visible := Value;
end;

procedure TPreviewFrame.SetShowToolBar(Value: boolean);
begin
  ToolBar.Visible := Value;
end;

procedure TPreviewFrame.SetStatusText(Index: integer; const Text: string);
begin
  StatusBar.Panels[Index].Text := Text;
  StatusBar.Refresh;
end;

procedure TPreviewFrame.SetZoomRatio(Value: integer);
begin
  if (Value <> FZoomRatio) and (Value > 0) and (Value <= 33) then
    FZoomRatio := Value;
end;

procedure TPreviewFrame.ShowPagerAndRatio;
begin
  SetStatusText(0, Format('页码:%d/%d', [FIndex + 1, PageCount]));
  SetStatusText(1, Format('打印比率:%d%%', [FRatio]));
  if FPage <> nil then
    SetStatusText(2, Format('W:%d / H:%d', [FPage.Width, FPage.Height])) else
    SetStatusText(2, '');
end;

procedure TPreviewFrame.ZoomIn;
begin
  SetRatio(FRatio + FZoomRatio);
end;

procedure TPreviewFrame.ZoomOut;
begin
  SetRatio(FRatio - FZoomRatio);
end;

procedure TPreviewFrame.Setup;
begin
  if Assigned(FOnSetup) then
  begin
    FOnSetup(Self);
    ResizePagePanel;
  end
  else if PageSetup then
    ResizePagePanel;
end;

procedure TPreviewFrame.Print;
begin
  if Assigned(FOnPrint) then
    FOnPrint(Self) else
  if HasPrinter then
    FForm.Print;
end;

procedure TPreviewFrame.PrintCurrent;
begin
  if HasPrinter then FPage.Print;
end;

procedure TPreviewFrame.Edit;
begin
  if Assigned(FOnEdit) then FOnEdit(Self, FSource);
end;

procedure TPreviewFrame.Save;
var
  fname: string;
begin
  if Assigned(FOnSave) then
  begin
    fname := '';
    FOnSave(Self, fname);
    if fname <> '' then
      SaveToFile(fname);
  end;
end;

procedure TPreviewFrame.SaveToFile(const fname: string);

  procedure BeginSave;
  begin
    if Assigned(FOnBeginSave) then
      FOnBeginSave(Self);
    lock_cursor;
    ToolBar.Enabled := false;
    FSavingCount := 1024;
    FSavingIndex := 0;
    SetStatusText(2, ' 正在保存 ...');
  end;

  procedure EndSave;
  begin
    ToolBar.Enabled := true;
    SetStatusText(2, '');
    unlock_cursor;
    if Assigned(FOnEndSave) then
      FOnEndSave(Self);
  end;

begin
  BeginSave;
  try
    FSource.SaveToFile(fname);
  finally
    EndSave;
  end;
end;

function TPreviewFrame.GetFlat: boolean;
begin
  Result := (ViewBox.BorderStyle = bsNone);
end;

procedure TPreviewFrame.SetFlat(Value: boolean);
begin
  if not Value then
    ViewBox.BorderStyle := bsSingle else
    ViewBox.BorderStyle := bsNone;
end;

procedure TPreviewFrame.SetOnEdit(Value: TEfxEditBook);
begin
  FOnEdit := Value;
  btnEdit.Enabled := Assigned(FOnEdit);
  btnEdit.Visible := Assigned(FOnEdit);
end;

procedure TPreviewFrame.SetSource(Value: TEasyForm);
{var
  B: integer;
  P: TEasyForm;}
begin
  if Value <> FSource then
  begin
    FreeAndNil(FForm);
    FSource := Value;
    FPage := nil;
    if FSource <> nil then
    begin
      FForm := FSource.Clone;
{     P := FForm;
      for B := 0 to P.ItemCount - 1 do
        P.Items[B].BackColor := DefColor;}
      SetRatio(FRatio);
    end;
  end;
  ShowPagerAndRatio;
end;

procedure TPreviewFrame.SetOnSave(Value: TEfxSaveBook);
begin
  FOnSave := Value;
  btnSave.Enabled := Assigned(FOnSave);
  btnSave.Visible := Assigned(FOnSave);
end;

procedure TPreviewFrame.SetOnClose(Value: TNotifyEvent);
begin
  FOnClose := Value;
  btnClose.Enabled := Assigned(FOnClose);
  btnClose.Visible := Assigned(FOnClose);
end;

procedure TPreviewFrame.SetOnPrint(Value: TNotifyEvent);
begin
  FOnPrint := Value;
  btnPrint.Enabled := Assigned(Value) or HasPrinter;
  btnPrint.Visible := Assigned(Value) or HasPrinter;
  pmiPrint.Enabled := Assigned(Value) or HasPrinter;
  pmiPrintCurrent.Enabled := HasPrinter;
end;

procedure TPreviewFrame.SetOnSetup(Value: TNotifyEvent);
begin
  FOnSetup := Value;
  btnSetup.Enabled := Assigned(Value) or HasPrinter;
  btnSetup.Visible := Assigned(Value) or HasPrinter;
end;

procedure TPreviewFrame.cbxRatioChange(Sender: TObject);
begin
  if FSource <> nil then
    SetRatio(StrToInt(cbxRatio.Text));
end;

end.
