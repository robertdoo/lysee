{==============================================================================}
{        UNIT: lysee_pad_fpc                                                   }
{ DESCRIPTION: main form of lysee_pad_fpc (FPC)                                }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2012/01/03                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_pad_fpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ComCtrls, ExtCtrls, StdCtrls, Buttons, SynEdit, SynMemo,
  SynHighlighterJava, SynEditTypes, SynExportHTML, lseu, lse_synedit, LCLType;

type

  { TLspadForm }

  TLspadForm = class(TForm)
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileSave: TAction;
    acFileExit: TAction;
    acEditUndo: TAction;
    acEditRedo: TAction;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditCopyHTML: TAction;
    acEditPaste: TAction;
    acEditSelectAll: TAction;
    acEditFind: TAction;
    acEditReplace: TAction;
    acRunCheck: TAction;
    acRunRun: TAction;
    acHelpAbout: TAction;
    acEditF3: TAction;
    acFileAnsiToUTF8: TAction;
    acFileUTF8ToAnsi: TAction;
    ActionList: TActionList;
    btnReplace: TSpeedButton;
    btnReplaceAll: TSpeedButton;
    chkWholeWord: TCheckBox;
    chkCaseSensitive: TCheckBox;
    edtFindText: TEdit;
    edtReplaceText: TEdit;
    ImageList: TImageList;
    lblFindText: TLabel;
    lblReplaceText: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pnBench: TPanel;
    pnFindReplace: TPanel;
    pmiReplace: TMenuItem;
    pmiFind: TMenuItem;
    pmiSelectAll: TMenuItem;
    pmiPaste: TMenuItem;
    pmiCopyHTML: TMenuItem;
    pmiCopy: TMenuItem;
    pmiCut: TMenuItem;
    pmiRedo: TMenuItem;
    pmiUndo: TMenuItem;
    miFileUTF8ToAnsi: TMenuItem;
    miFileAnsiToUTF8: TMenuItem;
    miFileConvert: TMenuItem;
    miFile_2: TMenuItem;
    miFile_1: TMenuItem;
    miEditReplace: TMenuItem;
    miRunCheck: TMenuItem;
    miRunRun: TMenuItem;
    miHelpAbout: TMenuItem;
    miEdit_1: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditCopyHTML: TMenuItem;
    miEditPaste: TMenuItem;
    miEditSelectAll: TMenuItem;
    miEdit_2: TMenuItem;
    miEditFind: TMenuItem;
    miEditRedo: TMenuItem;
    miEditUndo: TMenuItem;
    miFileExit: TMenuItem;
    miFileSave: TMenuItem;
    miFileOpen: TMenuItem;
    miFileNew: TMenuItem;
    miFile: TMenuItem;
    miEdit: TMenuItem;
    miRun: TMenuItem;
    miHelp: TMenuItem;
    dlgSave: TSaveDialog;
    popEdit: TPopupMenu;
    btnFind: TSpeedButton;
    btnClose: TSpeedButton;
    StatusBar: TStatusBar;
    smLysee: TSynMemo;
    expHTML: TSynExporterHTML;
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCopyHTMLExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditF3Execute(Sender: TObject);
    procedure acEditFindExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acEditReplaceExecute(Sender: TObject);
    procedure acEditSelectAllExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acFileAnsiToUTF8Execute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acFileUTF8ToAnsiExecute(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure acRunCheckExecute(Sender: TObject);
    procedure acRunRunExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure chkCaseSensitiveChange(Sender: TObject);
    procedure chkWholeWordChange(Sender: TObject);
    procedure edtFindTextChange(Sender: TObject);
    procedure edtFindTextKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miRunClick(Sender: TObject);
    procedure pnFindReplaceResize(Sender: TObject);
    procedure smLyseeChange(Sender: TObject);
    procedure smLyseeSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure smLyseeStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FFileName: string;
    FSynLysee: TLyseeSyn;
    FTempFile: string;
    FPath: string;
    FProgram: string;
    FProgramExist: boolean;
    FModified: boolean;
    FErrorRow: integer;
    FEngine: TLseEngine;
    FReplace: boolean;
    FOptions: TSynSearchOptions;
    procedure ResetCaption;
    procedure SetPanelText(Index: integer; const AText: string);
    procedure ResetSyntaxHilighter;
    procedure ClearError;
    procedure OpenNew(const fname: string);
    procedure ExecOpen(const ExeName, Options, FileName: string);
    function PromptSave: boolean;
    function SameFile(const F1, F2: string): boolean;
  end;

var
  LspadForm: TLspadForm;

implementation

uses
  Process, Clipbrd, lse_msgbox, lse_about_fpc, lse_pad_open_fpc;

{ TLspadForm }

procedure TLspadForm.FormCreate(Sender: TObject);
var
  target: string;
begin
  Application.Title := 'Lysee PAD';

  FPath := ExtractFilePath(Application.ExeName);
  FTempFile := FPath + IntToHex(Handle, 8) + '.ls';

  {$IFDEF WINDOWS}
  FProgram := FPath + 'lysee.exe';
  {$ELSE}
  FProgram := FPath + 'lysee';
  {$ENDIF}
  FProgramExist := FileExists(FProgram);

  if ParamCount > 0 then
  try
    target := ExpandFileName(Trim(ParamStr(1)));
    if FileExists(target) then
    begin
      {$IFDEF WINDOWS}
      target := AnsiToUTF8(target);
      {$ENDIF}
      smLysee.Lines.LoadFromFile(target);
      FFileName := target;
    end;
  except
    MsgErr(lse_exception_str);
    Application.Terminate;
  end;

  lse_startup;
  lse_set_program_file(Application.ExeName);
  SetKeywords(lse_keywords);
  FEngine := TLseEngine.Create(nil);

  ResetCaption;
  FOptions := [];
  ResetSyntaxHilighter;
end;

procedure TLspadForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEngine);
  lse_cleanup;
end;

procedure TLspadForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    btnCloseClick(nil);
end;

procedure TLspadForm.miRunClick(Sender: TObject);
begin
  acRunRun.Enabled := FProgramExist and not FModified and (FFileName <> '');
end;

procedure TLspadForm.pnFindReplaceResize(Sender: TObject);
begin
  btnClose.Left := pnFindReplace.Width - btnClose.Width;
end;

procedure TLspadForm.smLyseeChange(Sender: TObject);
begin
  FModified := true;
  ClearError;
end;

procedure TLspadForm.smLyseeSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  Special := (FErrorRow > 0) and (FErrorRow = Line);
  if Special then
  begin
    FG := clWhite;
    BG := clRed;
  end;
end;

procedure TLspadForm.smLyseeStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  SetPanelText(0, Format('%d,%d ', [smLysee.CaretY, smLysee.CaretX]));
  acEditCut.Enabled := smLysee.SelText <> '';
  acEditCopy.Enabled := acEditCut.Enabled;
  acEditCopyHTML.Enabled := acEditCut.Enabled;
  acEditUndo.Enabled := smLysee.CanUndo;
  acEditRedo.Enabled := smLysee.CanRedo;
  acEditPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TLspadForm.acFileNewExecute(Sender: TObject);
begin
  OpenNew('');
end;

procedure TLspadForm.acFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TLspadForm.acEditUndoExecute(Sender: TObject);
begin
  smLysee.Undo;
end;

procedure TLspadForm.acFileAnsiToUTF8Execute(Sender: TObject);
begin
  smLysee.Lines.Text := AnsiToUTF8(smLysee.Lines.Text);
end;

procedure TLspadForm.acEditRedoExecute(Sender: TObject);
begin
  smLysee.Redo;
end;

procedure TLspadForm.acEditReplaceExecute(Sender: TObject);
begin
  pnFindReplace.Height := 56;
  pnFindReplace.Visible := true;
  if edtFindText.Text <> '' then
    edtReplaceText.SetFocus else
    edtFindText.SetFocus;
  FReplace := true;
end;

procedure TLspadForm.acEditSelectAllExecute(Sender: TObject);
begin
  smLysee.SelectAll;
end;

procedure TLspadForm.acEditCutExecute(Sender: TObject);
begin
  smLysee.CutToClipboard;
end;

procedure TLspadForm.acEditF3Execute(Sender: TObject);
begin
  if FReplace then
    acEditReplaceExecute(nil) else
  if edtFindText.Text = '' then
    acEditFindExecute(nil) else
    btnFindClick(nil);
end;

procedure TLspadForm.acEditFindExecute(Sender: TObject);
begin
  pnFindReplace.Height := 26;
  pnFindReplace.Visible := true;
  edtFindText.SetFocus;
  FReplace := false;
end;

procedure TLspadForm.acEditPasteExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_TEXT) then
    smLysee.PasteFromClipboard;
end;

procedure TLspadForm.acEditCopyExecute(Sender: TObject);
begin
  smLysee.CopyToClipboard;
end;

procedure TLspadForm.acEditCopyHTMLExecute(Sender: TObject);
var
  S: TStringStream;
  H: string;
  X: integer;
begin
  S := TStringStream.Create('');
  try
    expHTML.ExportRange(smLysee.Lines, smLysee.BlockBegin, smLysee.BlockEnd);
    expHTML.SaveToStream(S);
    expHTML.Clear;

    H := S.DataString;

    X := Pos('</pre>', H);
    if X > 0 then
      H := Copy(H, 1, X + 5) else
      H := '';

    X := Pos('<pre>', H);
    if X > 0 then
      H := Copy(H, X, Length(H)) else
      H := '';

    Clipboard.AsText := H;
  finally
    S.Free;
  end;
end;

procedure TLspadForm.acFileOpenExecute(Sender: TObject);
var
  F: string;
begin
  F := '';
  if GetOpenFile(F) then
    if (FFileName = '') or SameFile(FFileName, F) then
    begin
      if FFileName = '' then PromptSave;
      smLysee.Lines.LoadFromFile(F);
      FFileName := F;
      smLyseeChange(nil);
      FModified := false;
      ResetCaption;
      ResetSyntaxHilighter;
    end
    else OpenNew(F);
end;

procedure TLspadForm.acFileSaveExecute(Sender: TObject);
begin
  if FFileName = '' then
  begin
    if not dlgSave.Execute then Exit;
    FFileName := dlgSave.FileName;
    ResetSyntaxHilighter;
    ResetCaption;
  end;
  smLysee.Lines.SaveToFile(FFileName);
  FModified := false;
end;

procedure TLspadForm.acFileUTF8ToAnsiExecute(Sender: TObject);
begin
  smLysee.Lines.Text := UTF8ToAnsi(smLysee.Lines.Text);
end;

procedure TLspadForm.acHelpAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Release;
  end;
end;

procedure TLspadForm.acRunCheckExecute(Sender: TObject);
begin
  ClearError;
  FEngine.Clear;
  try
    if FFileName = '' then
      FEngine.MainFile := ExpandFileName('Untitled.ls') else
      FEngine.MainFile := FFileName;
    FEngine.CompileCode(smLysee.Lines.Text);
    if FEngine.Errno <> 0 then
    begin
      if FEngine.ErrorModule = 'main' then
      begin
        FErrorRow := FEngine.ErrorRow;
        smLysee.CaretY := FErrorRow;
        smLysee.CaretX := FEngine.ErrorCol;
        smLysee.Refresh;
        SetPanelText(1, Format(' %s (%d, %d) - %s', [FEngine.ErrorName,
          FEngine.ErrorRow, FEngine.ErrorCol, FEngine.ErrorMsg]));
      end
      else SetPanelText(1, ' ' + FEngine.Error);
    end;
  finally
    FEngine.Clear;
  end;
end;

procedure TLspadForm.acRunRunExecute(Sender: TObject);
begin
  ExecOpen(FProgram, '--pause', FFileName);
end;

procedure TLspadForm.btnCloseClick(Sender: TObject);
begin
  pnFindReplace.Visible := false;
end;

procedure TLspadForm.btnFindClick(Sender: TObject);
begin
  if smLysee.SearchReplace(edtFindText.Text, '', FOptions) < 1 then
    MsgErr('No matches found!');
  smLysee.SetFocus;
end;

procedure TLspadForm.btnReplaceClick(Sender: TObject);
begin
  if Sender = btnReplace then
  begin
    if smLysee.SelText <> '' then
      smLysee.SelText := edtReplaceText.Text;
    btnFindClick(nil);
  end
  else
  begin
    smLysee.SearchReplace(edtFindText.Text, edtReplaceText.Text,
      FOptions + [ssoReplaceAll]);
    smLysee.SetFocus;
  end;
end;

procedure TLspadForm.chkCaseSensitiveChange(Sender: TObject);
begin
  if chkCaseSensitive.Checked then
    FOptions := FOptions + [ssoMatchCase] else
    FOptions := FOptions - [ssoMatchCase];
end;

procedure TLspadForm.chkWholeWordChange(Sender: TObject);
begin
  if chkWholeWord.Checked then
    FOptions := FOptions + [ssoWholeWord] else
    FOptions := FOptions - [ssoWholeWord];
end;

procedure TLspadForm.edtFindTextChange(Sender: TObject);
var
  S: string;
begin
  S := edtFindText.Text;
  btnFind.Enabled := (S <> '');
  btnReplace.Enabled := (S <> '');
  btnReplaceAll.Enabled := (S <> '');
end;

procedure TLspadForm.edtFindTextKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#10, #13] then
    if edtFindText.Text <> '' then
      btnFindClick(nil);
end;

procedure TLspadForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  smLyseeStatusChange(nil, []);
end;

procedure TLspadForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  R: TSearchRec;
begin
  if FTempFile <> '' then
  begin
    FTempFile := ChangeFileExt(FTempFile, '.*');
    if SysUtils.FindFirst(FTempFile, faAnyFile, R) = 0 then
    try
      FTempFile := ExtractFilePath(FTempFile);
      repeat
        SysUtils.DeleteFile(FTempFile + R.Name);
      until SysUtils.FindNext(R) <> 0;
    finally
      SysUtils.FindClose(R);
    end;
  end;
  PromptSave;
end;

procedure TLspadForm.ResetCaption;
begin
  if FFileName <> '' then
  begin
    Caption := 'Lysee PAD - ' + FFileName;
    Application.Title := ExtractFileName(FFileName);
  end
  else Caption := 'Lysee PAD';
end;

function TLspadForm.PromptSave: boolean;
begin
  Result := not FModified;
  if not Result then
    if AnsYes('File has been modified, Save?') then
    begin
      acFileSaveExecute(nil);
      Result := not FModified;
    end
    else Result := true;
end;

procedure TLspadForm.SetPanelText(Index: integer; const AText: string);
begin
  StatusBar.Panels[Index].Text := AText;
end;

procedure TLspadForm.ResetSyntaxHilighter;
begin
  if FSynLysee = nil then
    FSynLysee := TLyseeSyn.Create(Self) else
    smLysee.Highlighter := nil;
  smLysee.Highlighter := FSynLysee;
  expHTML.Highlighter := FSynLysee;
  expHTML.ExportAsText := true;
end;

procedure TLspadForm.ClearError;
begin
  if FErrorRow <> 0 then
  begin
    FErrorRow := 0;
    SetPanelText(1, '');
    smLysee.Refresh;
  end;
end;

procedure TLspadForm.OpenNew(const fname: string);
begin
  ExecOpen(Application.ExeName, '', fname);
end;

procedure TLspadForm.ExecOpen(const ExeName, Options, FileName: string);

  function QS(const S: string): string;
  begin
    Result := Trim(S);
    if Pos(' ', Result) > 1 then
      {$IFDEF WINDOWS}
      Result := '"' + Result + '"';
      {$ELSE}
      Result := '''' + Result + '''';
      {$ENDIF}
  end;

var
  P: TProcess;
begin
  P := TProcess.Create(Self);
  try
    {$IFNDEF WINDOWS}
    P.Options := P.Options + [poNewConsole];
    {$ENDIF}
    P.CommandLine := Trim(QS(ExeName) + ' ' + Options + ' ' + QS(FileName));
    P.Execute;
  finally
    P.Free;
  end;
end;

function TLspadForm.SameFile(const F1, F2: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result := CompareText(F1, F2) = 0;
  {$ELSE}
  Result := CompareStr(F1, F2) = 0;
  {$ENDIF}
end;

initialization
  {$I lse_pad_fpc.lrs}

end.

