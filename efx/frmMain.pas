unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Math, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, lse_efx, ExtCtrls, ComCtrls, ToolWin, ImgList,
  StdCtrls, ActnList, ExtDlgs, Tabs, efxPreview;

const
  AppTitle = 'Easy Form Editor';

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    miEditSize: TMenuItem;
    miFileOpen: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    miFileSaveAs: TMenuItem;
    miFileSave: TMenuItem;
    miFile: TMenuItem;
    N1: TMenuItem;
    miPage: TMenuItem;
    miFileNew: TMenuItem;
    miView: TMenuItem;
    miViewGrid: TMenuItem;
    miViewRule: TMenuItem;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileSave: TAction;
    miFilePreview: TMenuItem;
    N5: TMenuItem;
    acFilePreview: TAction;
    acInsertColumn: TAction;
    acAddColumn: TAction;
    acDeleteColumn: TAction;
    acInsertRow: TAction;
    acAddRow: TAction;
    acDeleteRow: TAction;
    acFilePrint: TAction;
    dlgColor: TColorDialog;
    N4: TMenuItem;
    miViewRefresh: TMenuItem;
    opdImage: TOpenPictureDialog;
    miFilePrint: TMenuItem;
    miEditForward: TMenuItem;
    miEditBackward: TMenuItem;
    miEditImage: TMenuItem;
    N3: TMenuItem;
    miEditAutoSize: TMenuItem;
    miEditSpacing: TMenuItem;
    miViewMove: TMenuItem;
    miEditTitle: TMenuItem;
    miEditNew: TMenuItem;
    miEditRemove: TMenuItem;
    acEditUndo: TAction;
    acEditCopy: TAction;
    acEditCut: TAction;
    acEditPaste: TAction;
    acViewHorz: TAction;
    H1: TMenuItem;
    acViewMode: TAction;
    acViewRule: TAction;
    acViewGrid: TAction;
    acViewRefresh: TAction;
    acMergeTarget: TAction;
    acDeleteTarget: TAction;
    N8: TMenuItem;
    miCell: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    miEditDelete: TMenuItem;
    acCancelMerge: TAction;
    M1: TMenuItem;
    sdExcel: TSaveDialog;
    acLyseeAnalyzer: TAction;
    miAnalyzer: TMenuItem;
    N10: TMenuItem;
    acLyseeMacro: TAction;
    miMacros: TMenuItem;
    acViewError: TAction;
    L1: TMenuItem;
    acLyseeGet: TAction;
    acLyseeCalc: TAction;
    acLyseeCheck: TAction;
    imsIndex: TImageList;
    acEditAddPage: TAction;
    acEditDeletePage: TAction;
    miFileExit: TMenuItem;
    sttDesign: TStatusBar;
    pnClient: TPanel;
    pnEdit: TPanel;
    pnToolbar: TPanel;
    acHelpAbout: TAction;
    H2: TMenuItem;
    A1: TMenuItem;
    miEditWrap: TMenuItem;
    N13: TMenuItem;
    acEditValues: TAction;
    A2: TMenuItem;
    acPickStyle: TAction;
    pnEditText: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton23: TToolButton;
    ToolButton12: TToolButton;
    ToolButton25: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton19: TToolButton;
    ToolButton24: TToolButton;
    ToolButton26: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    pnText: TPanel;
    eText: TEdit;
    pnRange: TPanel;
    pnContainer: TPanel;
    P2: TMenuItem;
    acNoZero: TAction;
    N6: TMenuItem;
    N01: TMenuItem;
    acPageSize: TAction;
    ToolButton30: TToolButton;
    miEditReadonly: TMenuItem;
    ToolButton31: TToolButton;
    JJJ1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acViewGridExecute(Sender: TObject);
    procedure acFilePreviewExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acMergeTargetExecute(Sender: TObject);
    procedure acDeleteTargetExecute(Sender: TObject);
    procedure acInsertColumnExecute(Sender: TObject);
    procedure acAddColumnExecute(Sender: TObject);
    procedure acInsertRowExecute(Sender: TObject);
    procedure acAddRowExecute(Sender: TObject);
    procedure acDeleteColumnExecute(Sender: TObject);
    procedure acDeleteRowExecute(Sender: TObject);
    procedure acViewRuleExecute(Sender: TObject);
    procedure acViewRefreshExecute(Sender: TObject);
    procedure acFilePrintExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
    procedure acCancelMergeExecute(Sender: TObject);
    procedure acLyseeAnalyzerExecute(Sender: TObject);
    procedure acViewErrorExecute(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure PreviewerbtnFirstClick(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure acPickStyleExecute(Sender: TObject);
    procedure pnContainerResize(Sender: TObject);
    procedure eTextChange(Sender: TObject);
    procedure acPageSizeExecute(Sender: TObject);
    procedure JJJ1Click(Sender: TObject);
  private
    FView: TEasyView;
    FUpdating: boolean;
    FFile: string;
    FPath: string;
    FPreview: TPreviewFrame;
    procedure LoadFromFile(const fname: string);
    procedure SaveToFile(const fname: string);
    procedure SetButtons(Sender: TObject);
    procedure ShowModified;
    function QuerySave: boolean;
    procedure UndoStatus(Sender: TObject);
    procedure ResizingRow(Sender: TObject; Row, NewHeight: integer);
    procedure ResizingColumn(Sender: TObject; Col, NewWidth: integer);
    procedure ErrorListClose(Sender: TObject);
    procedure ErrorListShow(Sender: TObject);
    procedure ClosePreview(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure SetStatusText(Index: integer; const Msg: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  frmPreview, frmSize, frmInputBox, lse_devcaps, frmText, ShellAPI, lseu,
  lse_msgbox, frmRunning, frmError, lse_lml,
  frmAbout, frmStyle;

{$R *.DFM}

const
  UndoRecSize = sizeof(RUndoRec);
	EdgeBase    = 13;                    {<--start edge icon index}
	SizingFmt   = '%3d / %-3d';

var
  arg_delete: boolean = false;
  arg_file  : string;
  
procedure TMainForm.FormCreate(Sender: TObject);
var
	A: integer;
  S: string;
begin
  Application.Title := AppTitle;
  Caption := AppTitle;

  SetupEfxModule(Application.ExeName);

  FView := TEasyView.Create(Self);
  FView.Parent := pnEdit;
  FView.Align := alClient;
  FView.OnSelect := acViewRefreshExecute;
  FView.OnChange := SetButtons;
  FView.OnUndoStatus := UndoStatus;
  FView.OnResizingRow := ResizingRow;
  FView.OnResizingColumn := ResizingColumn;
  FView.Visible := true;
  FView.SelectRange(Point(0, 0), Point(0, 0));
  pnClient.Align := alClient;

  acFilePrint.Enabled := HasPrinter;
  acEditUndo.Enabled := false;
  SetButtons(nil);

  FPath := ExtractFilePath(Application.ExeName);

  ErrorList.View := FView;
  ErrorList.OnHideList := ErrorListClose;
  ErrorList.OnShowList := ErrorListShow;
  HideErrorList;

  Application.OnHint := ShowHint;

  eText.Height := Canvas.TextHeight('高度');
  eText.Top := 4;
  eText.Left := 4;
  pnEditText.Height := eText.Height + 8;
  pnContainer.Color := clWindow;

  for A := 1 to ParamCount() do
  begin
    S := ParamStr(A);
    if AnsiSameText(S, '--delete') or AnsiSameText(S, '-d') then
      arg_delete := true else
    begin
      arg_file := ExpandFileName(S);
      if not FileExists(arg_file) then
      begin
        lse_msgbox.MsgErrFmt('文件“%s”不存在！', [S]);
        arg_file := '';
        Break;
      end;
    end;
  end;
end;

procedure TMainForm.JJJ1Click(Sender: TObject);
var
  R, C: integer;
begin
  FView.BeginUpdate;
  try
    FView.Form.ColCount := 16;
    FView.Form.RowCount := 32;
    for R := 0 to 31 do
      for C := 0 to 15 do
        FView.Form.Force(C, R).Text := IntToStr(R * 32 + C + 1);
    FView.Form.FixedColCount := 2;
    FView.Form.FixedRowCount := 2;
  finally
    FView.EndUpdate;
  end;
end;

{文件}

procedure TMainForm.acFileNewExecute(Sender: TObject);
begin
	if not QuerySave then Exit;
  Caption := AppTitle;
	FFile := '';
  FView.BeginUpdate;
  try
    FView.UndoListClear;
    FView.Clear;
    FView.Modified := false;
  finally
    FView.EndUpdate;
  end;
  SetButtons(nil);
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
begin
	with OpenDialog do if Execute then
		LoadFromFile(FileName);
end;

procedure TMainForm.LoadFromFile(const fname: string);
begin
	if QuerySave then
  begin
    Caption := AppTitle;
    FView.LoadFromFile(fname);
    SetButtons(nil);
    FFile := fname;
    Caption := Format('%s  [%s]', [AppTitle, FFile]);
    ShowModified;
  end;
end;

procedure TMainForm.acFileSaveExecute(Sender: TObject);
begin
	if FFile = '' then
		miFileSaveAsClick(nil) else
		SaveToFile(FFile);
end;

procedure TMainForm.miFileSaveAsClick(Sender: TObject);
begin
	with SaveDialog do
	begin
    if FFile <> '' then
      FileName := FFile;
		if Execute then
			SaveToFile(FileName);
	end;
end;

procedure TMainForm.acViewGridExecute(Sender: TObject);
begin
  FView.ShowGridLine := not FView.ShowGridLine;
  acViewGrid.Checked := FView.ShowGridLine;
end;

procedure TMainForm.acFilePreviewExecute(Sender: TObject);
begin
  lse_msgbox.lock_cursor;
  try
    pnClient.Visible := false;
    Menu := nil;
    if FPreview = nil then
    begin
      FPreview := TPreviewFrame.Create(Self);
      FPreview.Parent := Self;
      FPreview.Align := alClient;
      FPreview.OnClose := ClosePreview;
    end;
    FPreview.Source := FView.Form;
    FPreview.Visible := true;
  finally
    lse_msgbox.unlock_cursor;
  end;
end;

procedure TMainForm.SaveToFile(const fname: string);
begin
  lock_cursor;
  try
    FView.Sort;
    FView.SaveToFile(fname);
    FView.Form.Modified := false;
    FFile := fname;
    Caption := Format('%s  [%s]', [AppTitle, FFile]);
    ShowModified;
  finally
    unlock_cursor;
  end;
end;

function TMainForm.QuerySave: boolean;
const
	B = MB_YESNOCANCEL + MB_ICONQUESTION + MB_DEFBUTTON1;
begin
	Result := not FView.Modified;
	if not Result then
		if AnsYes('本表格已经被修改过, 是否进行存盘 ?') then
		begin
			acFileSaveExecute(nil);
			Result := true;
		end
		else Result := true;
end;

procedure TMainForm.ResizingColumn(Sender: TObject; Col, NewWidth: integer);
begin
  SetStatusText(0, IntToStr(NewWidth));
end;

procedure TMainForm.ResizingRow(Sender: TObject; Row, NewHeight: integer);
begin
  SetStatusText(0, IntToStr(NewHeight));
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := QuerySave;
  if CanClose then
  begin
    FView.OnSelect := nil;
    FView.OnChange := nil;
  end;
end;

procedure TMainForm.acMergeTargetExecute(Sender: TObject);
begin
  if not FUpdating and FView.Targeted then
  begin
    FView.MergeTarget;
    SetButtons(nil);
  end;
end;

procedure TMainForm.acPageSizeExecute(Sender: TObject);
begin
  ModalSize(FView.Form);
end;

procedure TMainForm.acPickStyleExecute(Sender: TObject);
var
  S: TEasyStyle;
  M: TEasyItem;
begin
  M := FView.Selected;
  if M <> nil then
    S := M.Style else
    S := FView.Form.Styles.Find('');
  if PickStyle(S, FView.Form.Styles) then
  begin
    FView.SetTargetStyle(S);
    SetButtons(nil);
  end;
end;

procedure TMainForm.acDeleteTargetExecute(Sender: TObject);
begin
  if not FUpdating and FView.Targeted then
  begin
    FView.DeleteTarget(true);
    SetButtons(nil);
  end;
end;

procedure TMainForm.acInsertColumnExecute(Sender: TObject);
begin
	if not FUpdating and FView.Targeted then
	begin
    FView.InsertColumnBeforeTarget;
    SetButtons(nil);
  end;
end;

procedure TMainForm.acAddColumnExecute(Sender: TObject);
begin
  FView.Form.AppendColumn;
  SetButtons(nil);
end;

procedure TMainForm.acInsertRowExecute(Sender: TObject);
begin
	if not FUpdating and FView.Targeted then
	begin
    FView.InsertRowBeforeTarget;
    SetButtons(nil);
  end;
end;

procedure TMainForm.acAddRowExecute(Sender: TObject);
begin
  FView.Form.AppendRow;
  SetButtons(nil);
end;

procedure TMainForm.acDeleteColumnExecute(Sender: TObject);
begin
	if not FUpdating and FView.Targeted then
	begin
    FView.UndoSaveChange;
    FView.DeleteTargetColumns;
    SetButtons(nil);
  end;
end;

procedure TMainForm.acDeleteRowExecute(Sender: TObject);
begin
	if not FUpdating and FView.Targeted then
	begin
    FView.UndoSaveChange;
    FView.DeleteTargetRows;
    SetButtons(nil);
  end;
end;

procedure TMainForm.acViewRuleExecute(Sender: TObject);
begin
  FView.ShowRule := not FView.ShowRule;
  acViewRule.Checked := FView.ShowRule;
end;

procedure TMainForm.acViewRefreshExecute(Sender: TObject);
begin
  SetButtons(nil);
  FView.Refresh;
end;

procedure TMainForm.acFilePrintExecute(Sender: TObject);
begin
  FView.Print;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if (arg_file <> '') and FileExists(arg_file) then
  begin
    LoadFromFile(arg_file);
    if arg_delete then
    begin
	    FFile := '';
      Caption := AppTitle;
      SysUtils.DeleteFile(arg_file);
      ShowModified;
    end;
  end;
  FView.SetFocus;
end;

procedure TMainForm.ShowModified;
begin
  if FView.Modified then
    SetStatusText(1, 'MODIFIED') else
    SetStatusText(1, '');
  acFileSave.Enabled := (FFile = '') or FView.Modified;
end;

const
  MoveHint = '将当前页由第%d页改为第%d页，肯定吗？';

procedure TMainForm.SetButtons(Sender: TObject);
var
  P: TEasyForm;
	N: TEasyItem;
  F: boolean;
begin
	FUpdating := true;
	try
    P := FView.Form;
    F := FView.Targeted;
		miEditAutoSize.Enabled := F;
    miEditAutoSize.Checked := false;
    miEditWrap.Enabled     := F;
    miEditWrap.Checked     := false;
		miEditSpacing.Enabled  := F;
    miEditSpacing.Checked  := false;
  	acInsertColumn.Enabled := F;
    acAddColumn.Enabled    := F;
  	acDeleteColumn.Enabled := F;
  	acInsertRow.Enabled    := F;
    acAddRow.Enabled       := F;
  	acDeleteRow.Enabled    := F;
		acMergeTarget.Enabled  := F;
		acDeleteTarget.Enabled := F;
    eText.ReadOnly         := not F;
    miEditTitle.Enabled    := F;
    miEditTitle.Checked    := false;
    miEditReadonly.Enabled := F;
    miEditReadonly.Checked := false;
    miEditImage.Enabled    := F;
    acEditCut.Enabled      := F;
    acEditCopy.Enabled     := F;
    acEditPaste.Enabled    := F;
		if F then
		begin
			N := FView.Selected;
			if N = nil then
			begin
        eText.Text := '';
			end
			else
			begin
        if eText.Tag = 0 then
        begin
          eText.OnChange := nil;
          eText.Text := N.Text;
          eText.OnChange := eTextChange;
        end;
			end;
		end
    else
    begin
      eText.OnChange := nil;
      eText.Text := '';
      eText.OnChange := eTextChange;
    end;
    if FView.Targeted then
      pnRange.Caption := FView.TargetedRangeID else
      pnRange.Caption := '';
    ShowModified;

    SetStatusText(2, Format('W:%d / H:%d', [P.Width, P.Height]));
	finally
		FUpdating := false;
	end;
end;

procedure TMainForm.pnContainerResize(Sender: TObject);
begin
  eText.Width := pnContainer.Width - 8;
end;

procedure TMainForm.acEditUndoExecute(Sender: TObject);
begin
  if FView.Undo then
    acViewRefreshExecute(nil);
end;

procedure TMainForm.UndoStatus(Sender: TObject);
begin
  acEditUndo.Enabled := FView.UndoCount > 0;
end;

procedure TMainForm.acEditCopyExecute(Sender: TObject);
begin
  FView.CopyToClipboard;
end;

procedure TMainForm.acEditCutExecute(Sender: TObject);
begin
  FView.CutToClipboard;
end;

procedure TMainForm.acEditPasteExecute(Sender: TObject);
begin
  FView.PasteFromClipboard;
end;

procedure TMainForm.btnTextClick(Sender: TObject);
var
  M: TEasyItem;
  S: string;
begin
  if not FView.Targeted then Exit;
  M := FView.Selected;
  if M = nil then S := '' else S := M.Text;
  if not EditCommaText('单元文本', S) then Exit;
  FView.SetFocus;
  FView.SetTargetText(S, true);
end;

procedure TMainForm.eTextChange(Sender: TObject);
begin
  if not FUpdating and FView.Targeted and eText.Focused then
  begin
    eText.Tag := 1;
    try
      FView.SetTargetText(eText.Text, true);
      SetButtons(nil);
    finally
      eText.Tag := 0;
    end;
  end;
end;

procedure TMainForm.acCancelMergeExecute(Sender: TObject);
begin
  FView.UnmergeTarget;
end;

procedure TMainForm.acLyseeAnalyzerExecute(Sender: TObject);
begin
//  frmScript.ModalScript(FView.Form, true, false);
end;

procedure TMainForm.acViewErrorExecute(Sender: TObject);
begin
  if ErrorList.Visible then
    HideErrorList else
    ShowErrorList;
end;

procedure TMainForm.ClosePreview(Sender: TObject);
begin
  FPreview.Visible := false;
  FPreview.Source := nil;
  Menu := MainMenu1;
  pnClient.Visible := true;
end;

procedure TMainForm.miFileExitClick(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TMainForm.ErrorListClose(Sender: TObject);
begin
  acViewError.Checked := false;
end;

procedure TMainForm.ErrorListShow(Sender: TObject);
begin
  acViewError.Checked := true;
end;

procedure TMainForm.PreviewerbtnFirstClick(Sender: TObject);
begin
  FPreview.OnButtonClick(Sender);
end;

procedure TMainForm.acHelpAboutExecute(Sender: TObject);
begin
  lse_msgbox.ModalShow(TAboutBox);
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  SetStatusText(3, GetLongHint(Application.Hint));
end;

procedure TMainForm.SetStatusText(Index: integer; const Msg: string);
begin
  sttDesign.Panels[Index].Text := Msg;
end;

end.
