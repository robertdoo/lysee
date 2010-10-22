{==============================================================================}
{        UNIT: lysee_pad_fpc                                                   }
{ DESCRIPTION: main form of lysee_pad_fpc (FPC)                                }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2010/10/21                                                      }
{==============================================================================}
{ Copyright (c) 2008-2010, Li Yun Jie                                          }
{ All rights reserved.                                                         }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ Redistributions of source code must retain the above copyright notice, this  }
{ list of conditions and the following disclaimer.                             }
{                                                                              }
{ Redistributions in binary form must reproduce the above copyright notice,    }
{ this list of conditions and the following disclaimer in the documentation    }
{ and/or other materials provided with the distribution.                       }
{                                                                              }
{ Neither the name of Li Yun Jie nor the names of its contributors may         }
{ be used to endorse or promote products derived from this software without    }
{ specific prior written permission.                                           }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    }
{ IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   }
{ ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  }
{ ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       }
{ DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   }
{ SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   }
{ CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           }
{ LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    }
{ OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  }
{ DAMAGE.                                                                      }
{==============================================================================}
{ The Initial Developer of the Original Code is Li Yun Jie (CHINA).            }
{ Portions created by Li Yun Jie are Copyright (C) 2003-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_pad_fpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ComCtrls, SynEdit, SynMemo,
  SynHighlighterJava, SynExportHTML, lseu, lse_synedit;

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
    dlgFind: TFindDialog;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    dlgReplace: TReplaceDialog;
    popEdit: TPopupMenu;
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
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miRunClick(Sender: TObject);
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
    procedure ResetCaption;
    procedure SetPanelText(Index: integer; const AText: string);
    procedure ResetSyntaxHilighter;
    procedure ClearError;
    procedure OpenNew(const fname: string);
    procedure ExecOpen(const ExeName, Options, FileName: string);
    function PromptSave: boolean;
    function IsLspCode: boolean;
    function SameFile(const F1, F2: string): boolean;
  end;

var
  LspadForm: TLspadForm;

implementation

uses
  Process, Clipbrd, SynEditTypes, lse_msgbox, lse_about_fpc, lse_pad_open_fpc;

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
  SetLyseeKeywords(lse_keywords);
  FEngine := TLseEngine.Create(nil);

  ResetCaption;
  ResetSyntaxHilighter;
end;

procedure TLspadForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEngine);
  lse_cleanup;
end;

procedure TLspadForm.miRunClick(Sender: TObject);
begin
  acRunRun.Enabled := FProgramExist and not FModified and (FFileName <> '');
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
  dlgFind.CloseDialog;
  dlgReplace.Execute;
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
  if dlgFind.FindText = '' then
    acEditFindExecute(nil) else
    dlgFindFind(nil);
end;

procedure TLspadForm.acEditFindExecute(Sender: TObject);
begin
  dlgReplace.CloseDialog;
  dlgFind.Execute;
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
    FEngine.CompileCode(smLysee.Lines.Text, IsLspCode);
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

procedure TLspadForm.dlgFindFind(Sender: TObject);
var
  options: TSynSearchOptions;
begin
  options := [];

  if frWholeWord in dlgFind.Options then
    options := options + [ssoWholeWord];

  if frMatchCase in dlgFind.Options then
    options := options + [ssoMatchCase];

  if smLysee.SearchReplace(dlgFind.FindText, '', options) < 1 then
    MsgErr('No matches found!');

  dlgReplace.FindText := dlgFind.FindText;
end;

procedure TLspadForm.dlgReplaceFind(Sender: TObject);
var
  options: TSynSearchOptions;
begin
  options := [];

  if frWholeWord in dlgReplace.Options then
    options := options + [ssoWholeWord];

  if frMatchCase in dlgReplace.Options then
    options := options + [ssoMatchCase];

  if smLysee.SearchReplace(dlgReplace.FindText, '', options) < 1 then
    MsgErr('No matches found!');

  dlgFind.FindText := dlgReplace.FindText;
end;

procedure TLspadForm.dlgReplaceReplace(Sender: TObject);
var
  options: TSynSearchOptions;
begin
  if frReplaceAll in dlgReplace.Options then
  begin
    options := [ssoReplaceAll];

    if frWholeWord in dlgReplace.Options then
      options := options + [ssoWholeWord];

    if frMatchCase in dlgReplace.Options then
      options := options + [ssoMatchCase];

    smLysee.SearchReplace(dlgReplace.FindText, dlgReplace.ReplaceText, options);
    dlgFind.FindText := dlgReplace.FindText;
  end
  else
  begin
    if smLysee.SelText <> '' then
      smLysee.SelText := dlgReplace.ReplaceText;
    dlgReplaceFind(nil);
  end;
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
  FSynLysee.IsLsp := IsLspCode;
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

function TLspadForm.IsLspCode: boolean;
begin
  Result := (FFileName <> '') and lse_is_lsp_file(FFileName);
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

