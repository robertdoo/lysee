unit frmEfxMacro;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  lseu, lse_efx, ImgList, ComCtrls, ToolWin, ExtCtrls, StdCtrls,
  ActnList, SynEdit, SynMemo;

type
  TEfxMacroFrame = class(TFrame)
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    acSave: TAction;
    StatusBar1: TStatusBar;
    acFind: TAction;
    acReplace: TAction;
    acCheck: TAction;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    dlgFind: TFindDialog;
    dlgReplace: TReplaceDialog;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    acInsert: TAction;
    procedure acSaveExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure acCheckExecute(Sender: TObject);
    procedure acInsertExecute(Sender: TObject);
  private
    FScript: TEasyScript;
    FMemo: TSynMemo;
    FEntry: string;
    FErrorRow: integer;
    function GetForm: TEasyForm;
    procedure SetEntry(const Value: string);
    procedure MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure MemoChange(Sender: TObject);
    procedure MemoSpecialLine(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SetStatus(Index: integer; const Status: string);
  public
    constructor Create(AOwner: TComponent);override;
    procedure SetScriptEntry(Script: TEasyScript; const Entry: string);
    property Form: TEasyForm read GetForm;
    property Script: TEasyScript read FScript;
    property Entry: string read FEntry;
    property Memo: TSynMemo read FMemo;
  end;

implementation

uses
  SynEditTypes, lse_msgbox, frmInsert;

{$R *.dfm}

var
  find_text: string;

{ TEfxMacroFrame }

constructor TEfxMacroFrame.Create(AOwner: TComponent);
begin
  inherited;
  FMemo := TSynMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Options := [eoAutoIndent, eoDragDropEditing, eoGroupUndo,
    eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces];
  FMemo.Gutter.ShowLineNumbers := true;
  FMemo.Gutter.Font.Assign(FMemo.Font);
  FMemo.Gutter.Font.Size := 8;
  FMemo.Gutter.LeftOffset := 0;
  FMemo.Align := alClient;
  FMemo.Visible := true;
  FMemo.BorderStyle := bsNone;
  FMemo.Gutter.Visible := false;
  FMemo.OnStatusChange := MemoStatusChange;
  FMemo.OnChange := MemoChange;
end;

function TEfxMacroFrame.GetForm: TEasyForm;
begin
  Result := FScript.Form;
end;

procedure TEfxMacroFrame.acSaveExecute(Sender: TObject);
begin
  FScript.Write(FEntry, FMemo.Lines.Text);
  acSave.Enabled := false;
  FMemo.Modified := false;
end;

procedure TEfxMacroFrame.MemoStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  SetStatus(0, Format('%4d:%3d', [FMemo.CaretY, FMemo.CaretX]));
end;

procedure TEfxMacroFrame.SetEntry(const Value: string);
begin
  FEntry := Trim(Value);
  FMemo.Lines.Text := FScript.Read(FEntry, '');
  FMemo.Modified := false;
  acSave.Enabled := false;
end;

procedure TEfxMacroFrame.acFindExecute(Sender: TObject);
begin
  if dlgFind.FindText = '' then
    dlgFind.FindText := find_text;
  dlgFind.Execute;
  dlgReplace.CloseDialog;
end;

procedure TEfxMacroFrame.acReplaceExecute(Sender: TObject);
begin
  if dlgReplace.FindText = '' then
    dlgReplace.FindText := find_text;
  dlgReplace.Execute;
  dlgFind.CloseDialog;
end;

procedure TEfxMacroFrame.dlgFindFind(Sender: TObject);
var
  opts: TSynSearchOptions;
begin
  opts := [];
  if frWholeWord in dlgFind.Options then
    opts := opts + [ssoWholeWord];
  if frMatchCase in dlgFind.Options then
    opts := opts + [ssoMatchCase];
  if FMemo.SearchReplace(dlgFind.FindText, '', opts) < 1 then
    if Assigned(Sender) then
      MsgErr('没有找到匹配的内容！');
  find_text := dlgFind.FindText;
  dlgReplace.FindText := dlgFind.FindText;
  dlgReplace.Options := dlgFind.Options;
end;

procedure TEfxMacroFrame.dlgReplaceFind(Sender: TObject);
var
  opts: TSynSearchOptions;
begin
  opts := [];
  if frWholeWord in dlgReplace.Options then
    opts := opts + [ssoWholeWord];
  if frMatchCase in dlgReplace.Options then
    opts := opts + [ssoMatchCase];
  if FMemo.SearchReplace(dlgReplace.FindText, '', opts) < 1 then
    if Assigned(Sender) then
      MsgErr('没有找到匹配的内容！');
  find_text := dlgReplace.FindText;
  dlgFind.FindText := dlgReplace.FindText;
  dlgFind.Options := dlgReplace.Options;
end;

procedure TEfxMacroFrame.dlgReplaceReplace(Sender: TObject);
var
  opts: TSynSearchOptions;
begin
  if frReplaceAll in dlgReplace.Options then
  begin
    opts := [ssoReplaceAll];
    if frWholeWord in dlgFind.Options then
      opts := opts + [ssoWholeWord];
    if frMatchCase in dlgFind.Options then
      opts := opts + [ssoMatchCase];
    if FMemo.SearchReplace(dlgReplace.FindText,
      dlgReplace.ReplaceText, opts) < 1 then
        MsgErr('没有找到匹配的内容！');
  end
  else if FMemo.SelText <> '' then
    FMemo.SelText := dlgReplace.ReplaceText;
  dlgReplaceFind(nil);
end;

procedure TEfxMacroFrame.MemoChange(Sender: TObject);
begin
  acSave.Enabled := true;
  if Assigned(FMemo.OnSpecialLineColors) then
  begin
    FMemo.OnSpecialLineColors := nil;
    FMemo.Refresh;
  end;
end;

procedure TEfxMacroFrame.SetScriptEntry(Script: TEasyScript; const Entry: string);
begin
  FScript := Script;
  SetEntry(Entry);
end;

procedure TEfxMacroFrame.acCheckExecute(Sender: TObject);
var
  engine: TLseEngine;
begin
  engine := TLseEngine.Create(Self);
  try
    if not engine.CompileCode(FMemo.Lines.Text) then
    begin
      FErrorRow := engine.ErrorRow;;
      FMemo.CaretY := FErrorRow;
      FMemo.CaretX := engine.ErrorCol;
      FMemo.OnSpecialLineColors := MemoSpecialLine;
      FMemo.Refresh;
      FMemo.AlwaysShowCaret := true;
      FMemo.AlwaysShowCaret := false;
      SetStatus(2, Format('%s(%d, %d) %s', [engine.ErrorName,
        FErrorRow, engine.ErrorCol, engine.ErrorMsg]));
    end
    else SetStatus(2, '');
  finally
    engine.Free;
  end;
end;

procedure TEfxMacroFrame.SetStatus(Index: integer; const Status: string);
begin
  StatusBar1.Panels[Index].Text := Status;
end;

procedure TEfxMacroFrame.MemoSpecialLine(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if FErrorRow = Line then
  begin
    FG := clWhite;
    BG := clRed;
    Special := true;
  end;
end;

procedure TEfxMacroFrame.acInsertExecute(Sender: TObject);
var
  code: string;
begin
  if InsertCode(code) then
    FMemo.SelText := code;
end;

end.
