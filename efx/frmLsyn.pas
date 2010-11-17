unit frmLsyn;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ImgList, ExtCtrls, ActnList, lseu, SynEdit, SynMemo,
  SynEditExport, SynExportHTML, SynEditKeyCmds, StdCtrls;

type
  TLsynFrame = class(TFrame)
    Actions: TActionList;
    acExecute: TAction;
    acTerminate: TAction;
    acMultiline: TAction;
    acManual: TAction;
    acCmdPrev: TAction;
    acCmdNext: TAction;
    SecondTimer: TTimer;
    ImageList1: TImageList;
    MenuBar: TToolBar;
    btnManual: TToolButton;
    ToolButton1: TToolButton;
    btnMultiline: TToolButton;
    ToolButton2: TToolButton;
    btnExecute: TToolButton;
    btnTerminate: TToolButton;
    StatusBar: TStatusBar;
    ToolButton3: TToolButton;
    btnRestart: TToolButton;
    acRestart: TAction;
    procedure FrameResize(Sender: TObject);
    procedure acCmdPrevExecute(Sender: TObject);
    procedure acCmdNextExecute(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure acTerminateExecute(Sender: TObject);
    procedure acMultilineExecute(Sender: TObject);
    procedure acManualExecute(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
  private
    FEngine: TLseEngine;               {<--lysee engine}
    FArguments: string;                {<--arguments}
    FExeName: string;                  {<--application name}
    FRunning: boolean;                 {<--running}
    FResult: string;                   {<--execution result}
    FStartTime: TDateTime;             {<--begin execute time}
    FLastRead: string;                 {<--last line read from STDIN}
    FMemo: TSynMemo;                   {<--syntax highlighted editor}
    FStartXY: TPoint;                  {<--input command position}
    FSelStart: integer;                {<--input position}
    FMultiLine: boolean;               {<--allow multiline}
    FRunLines: TList;                  {<--output lines}
    FErrorLine: integer;               {<--error line index}
    FHistory: TStrings;                {<--command storage}
    FHistoryIndex: integer;            {<--command index}
    FOnQuit: TNotifyEvent;             {<--notify quit}
    procedure SetStatusText(Index: integer; const Text: string);
    procedure ShowPrmpt(const Text: string = '>>> ');
    procedure WriteText(const Text: string; EnsureLine: boolean);
    procedure Execute(const code, Args: string; Row: integer);
    procedure OnBeginExecute(Sender: TObject);
    procedure OnEndExecute(Sender: TObject);
    procedure OnReadStr(Sender: TObject; var S: String);
    procedure OnWriteStr(Sender: TObject; const Str: PChar; var Count: Integer);
    procedure OnSpecialLine(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure OnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure AddHistory(const cmd: string);
    procedure SetCommand(const text: string);
    procedure SetArguments(const Value: string);
    procedure MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Terminate;
    property Runnint: boolean read FRunning;
    property Arguments: string read FArguments write SetArguments;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property SynMemo: TSynMemo read FMemo;
  end;

implementation

uses
  DateUtils, ShellAPI, lse_msgbox;

{$R *.dfm}

type
  TLyseeMemo = class(TSynMemo)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

procedure TLyseeMemo.KeyDown;
begin
  with Parent as TLsynFrame do
    if (Key = 13) and (FRunning or not FMultiLine) then
    begin
      acExecuteExecute(nil);
      Key := 0;
    end
    else
    if Key = 8 then
      if FMemo.CaretY = FStartXY.Y then
        if FMemo.CaretX = FStartXY.X then
          Key := 0;
  inherited;
end;

{ TLyswinFrame }

procedure TLsynFrame.AddHistory(const cmd: string);
begin
  if FHistory.Count > 0 then
  begin
    FHistoryIndex := FHistory.Count - 1;
    if FHistory[FHistoryIndex] = cmd then Exit;
  end;
  FHistoryIndex := FHistory.Add(cmd);
end;

constructor TLsynFrame.Create(AOwner: TComponent);
begin
  inherited;
  FExeName := Application.ExeName;
  FArguments := FExeName;
  FErrorLine := -1;
  FRunLines := TList.Create;
  FHistory := TStringList.Create;
  FHistoryIndex := -1;
  FEngine := TLseEngine.Create(Self);
  FEngine.OnExecuting := OnBeginExecute;
  FEngine.OnExecuted := OnEndExecute;
  FEngine.OnReadln := OnReadStr;
  FEngine.OnWrite := OnWriteStr;
  FMemo := TLyseeMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Options := [eoAutoIndent, eoDragDropEditing, eoGroupUndo,
    eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces];
  FMemo.Gutter.ShowLineNumbers := true;
  FMemo.Gutter.Font.Assign(FMemo.Font);
  FMemo.Gutter.Font.Size := 8;
  FMemo.Gutter.LeftOffset := 0;
  FMemo.Align := alClient;
  FMemo.Visible := true;
  FMemo.OnSpecialLineColors := OnSpecialLine;
  FMemo.OnStatusChange := OnStatusChange;
  FMemo.MaxUndo := 0;
  FMemo.Lines.Add('');
  FMemo.BorderStyle := bsNone;
  FMemo.Gutter.Visible := false;
  FMemo.OnStatusChange := MemoStatusChange;
  FMemo.Lines.Add('');
  ShowPrmpt;
end;

destructor TLsynFrame.Destroy;
begin
  if Assigned(FEngine) then
  begin
    acTerminateExecute(nil);
    FreeAndNil(FHistory);
    FreeAndNil(FRunLines);
  end;
  inherited;
end;

procedure TLsynFrame.Execute(const code, Args: string; Row: integer);
begin
  FEngine.Arguments := FArguments;
  if Length(Trim(code)) > 0 then
  begin
    try
      if not FEngine.ExecuteCode(StringOfChar(#13, Row) + code, false) then Abort;
      WriteText(FResult, false);
    except
      FErrorLine := FEngine.ErrorRow;
      FMemo.Refresh;
      WriteText(FEngine.Error, false);
    end;
    ShowPrmpt;
    AddHistory(code);
  end
  else if Row > 0 then ShowPrmpt;
end;

procedure TLsynFrame.OnBeginExecute(Sender: TObject);
begin
  FStartTime := Now;
  acTerminate.Enabled := true;
  acTerminate.Visible := true;
  acExecute.Visible := false;
  SecondTimer.Enabled := true;
  FRunning := true;
end;

procedure TLsynFrame.OnEndExecute(Sender: TObject);
begin
  FRunning := false;
  FResult := FEngine.ResultText;
  acTerminate.Enabled := false;
  acTerminate.Visible := false;
  acExecute.Visible := true;
  SecondTimer.Enabled := false;
end;

procedure TLsynFrame.OnReadStr(Sender: TObject; var S: String);
begin
  if FMemo.Showing then FMemo.SetFocus;
  Tag := 1;
  while Tag <> 0 do
    Application.ProcessMessages;
  S := FLastRead;
end;

procedure TLsynFrame.OnSpecialLine(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if FErrorLine = Line then
  begin
    Special := true;
    BG := clRed;
    FG := clWhite;
  end
  else
  if FRunLines.IndexOf(pointer(Line)) >= 0 then
  begin
    Special := true;
    FG := clBlue;
  end;
end;

procedure TLsynFrame.OnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  FMemo.ReadOnly := (FMemo.SelStart < FSelStart) or
    (FMemo.SelEnd < FSelStart);
end;

procedure TLsynFrame.OnWriteStr(Sender: TObject; const Str: PChar; var Count: Integer);
var
  S: string;
begin
  if Count > 0 then
  begin
    SetString(S, Str, Count);
    WriteText(S, false);
  end;
end;

procedure TLsynFrame.SetCommand(const text: string);
var
  base, next: pchar;
  temp, line: string;
  index: integer;
begin
  temp := TrimRight(Copy(FMemo.Lines[FStartXY.Y - 1], 1, FStartXY.X - 1));
  if Length(temp) < FStartXY.X - 1 then
    temp := temp + StringOfChar(' ', FStartXY.X - Length(temp) - 1);
  temp := temp + text;
  for index := FMemo.Lines.Count - 1 downto FStartXY.Y - 1 do
    FMemo.Lines.Delete(index);
  base := pchar(temp);
  if (base = nil) or (base^ = #0) then Exit;
  next := base;
  while next^ <> #0 do
  begin
    if next^ in [#13, #10] then
    begin
      SetString(line, base, next - base);
      FMemo.Lines.Add(line);
      base := next + 1;
      if (next^ = #13) and (base^ = #10) then Inc(base);
      next := base;
    end
    else
    if next^ in LeadBytes then
    begin
      if (next - base) >= FMemo.RightEdge then
      begin
        SetString(line, base, next - base);
        FMemo.Lines.Add(line);
        base := next;
      end
      else
      begin
        Inc(next);
        if next^ in LeadBytes then Inc(next);
      end;
    end
    else
    if (next - base) >= FMemo.RightEdge then
    begin
      SetString(line, base, next - base);
      FMemo.Lines.Add(line);
      base := next;
    end
    else Inc(next);
  end;
  SetString(line, base, next - base);
  if line <> '' then
  begin
    FMemo.Lines.Add(line);
    FrameResize(nil);
  end;
  FMemo.CaretY := FStartXY.Y;
  FMemo.CaretX := FStartXY.X;
  with FMemo do
    if TopLine + LinesInWindow <= FStartXY.Y then
      TopLine := FStartXY.Y - LinesInWindow + 1;
end;

procedure TLsynFrame.SetStatusText(Index: integer; const Text: string);
begin
  StatusBar.Panels[Index].Text := Text;
end;

procedure TLsynFrame.ShowPrmpt(const Text: string);
begin
  WriteText(Text, true);
end;

procedure TLsynFrame.WriteText(const Text: string; EnsureLine: boolean);
var
  base, next: pchar;
  line: string;
  size: integer;

  procedure MergeLastLine;
  var
    index: integer;
  begin
    index := FMemo.Lines.Count - 1;
    FMemo.Lines[index] := FMemo.Lines[index] + line;
    if FRunning then
      FRunLines.Add(pointer(index + 1));
  end;

begin
  if EnsureLine then
    if FMemo.Lines[FMemo.Lines.Count - 1] <> '' then
      FMemo.Lines.Add('');
  size := Length(FMemo.Lines[FMemo.Lines.Count - 1]);
  base := pchar(text);
  if (base <> nil) and (base^ <> #0) then
  begin
    next := base;
    while next^ <> #0 do
    begin
      if next^ in [#13, #10] then
      begin
        SetString(line, base, next - base);
        MergeLastLine;
        FMemo.Lines.Add('');
        size := 0;
        base := next + 1;
        if (next^ = #13) and (base^ = #10) then Inc(base);
        next := base;
      end
      else
      if next^ in LeadBytes then
      begin
        if (next - base) + size >= FMemo.RightEdge then
        begin
          SetString(line, base, next - base);
          MergeLastLine;
          FMemo.Lines.Add('');
          size := 0;
          base := next;
        end
        else
        begin
          Inc(next);
          if next^ in LeadBytes then Inc(next);
        end;
      end
      else
      if (next - base) + size >= FMemo.RightEdge then
      begin
        SetString(line, base, next - base);
        MergeLastLine;
        FMemo.Lines.Add('');
        size := 0;
        base := next;
      end
      else Inc(next);
    end;
    SetString(line, base, next - base);
    if line <> '' then MergeLastLine;
  end;
  line := FMemo.Lines[FMemo.Lines.Count - 1];
  FStartXY.X := Length(line) + 1;
  FStartXY.Y := FMemo.Lines.Count;
  FMemo.CaretY := FStartXY.Y;
  FMemo.CaretX := FStartXY.X;
  FSelStart := FMemo.SelStart;
  FrameResize(nil);
end;

procedure TLsynFrame.FrameResize(Sender: TObject);
begin
  if FMemo.Lines.Count > FMemo.LinesInWindow then
    FMemo.ScrollBars := ssVertical else
    FMemo.ScrollBars := ssNone;
end;

procedure TLsynFrame.acCmdPrevExecute(Sender: TObject);
begin
  if FHistoryIndex >= 0 then
  begin
    SetCommand(FHistory[FHistoryIndex]);
    Dec(FHistoryIndex);
  end
  else SetCommand('');
end;

procedure TLsynFrame.acCmdNextExecute(Sender: TObject);
begin
  if FHistoryIndex < FHistory.Count - 1 then
  begin
    Inc(FHistoryIndex);
    SetCommand(FHistory[FHistoryIndex]);
  end
  else SetCommand('');
end;

procedure TLsynFrame.acExecuteExecute(Sender: TObject);
var
  L, R: integer;
  S: string;

  function GetInput: string;
  var
    A: integer;
  begin
    Result := TrimRight(Copy(FMemo.Lines[FStartXY.Y - 1], FStartXY.X, MaxInt));
    for A := FStartXY.Y to FMemo.Lines.Count - 1 do
      Result := Result + sLineBreak + TrimRight(FMemo.Lines[A]);
  end;

begin
  S := GetInput; //mEnter.Lines.Text;
  if not FEngine.Running then
  begin
    S := TrimRight(S);
    if S <> '' then
    begin
      R := FStartXY.Y - 1;
      WriteText('', true);
      if Trim(S) <> 'quit' then Execute(S, FArguments, R) else
      if Assigned(FOnQuit) then FOnQuit(Self);
    end;
  end
  else if Tag <> 0 then
  begin
    L := Length(S);
    if (L > 0) and (S[L] in [#10, #13]) then
    begin
      Dec(L);
      if (L > 0) and (S[L] = #13) and (S[L + 1] = #10) then Dec(L);
      SetLength(S, L);
    end;
    FLastRead := S;
    FMemo.Lines.Add('');
    Tag := 0;
  end;
end;

procedure TLsynFrame.acTerminateExecute(Sender: TObject);
begin
  Tag := 0; {<--cancel read}
  FEngine.Terminate;
end;

procedure TLsynFrame.acMultilineExecute(Sender: TObject);
begin
  FMultiLine := not FMultiLine;
  acMultiline.Checked := FMultiLine;
end;

procedure TLsynFrame.acManualExecute(Sender: TObject);
var
  F: string;
begin
  F := ExtractFilePath(Application.ExeName) + 'docs\documents\index.htm';
  ShellExecute(Handle, 'open', PChar(F), '', '', SW_NORMAL);
end;

procedure TLsynFrame.Terminate;
begin
  acTerminateExecute(nil);
end;

procedure TLsynFrame.SetArguments(const Value: string);
begin
  FArguments := Value;
end;

procedure TLsynFrame.acRestartExecute(Sender: TObject);
begin
  FMemo.Lines.BeginUpdate;
  try
    FMemo.Lines.Clear;
    FMemo.Lines.Add(Format('RESTARTED AT: %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss zzz', Now)]));
    FMemo.Lines.Add('');
    FMemo.Lines.Add('');
    ShowPrmpt;
    FEngine.Clear;
    FErrorLine := -1;
  finally
    FMemo.Lines.EndUpdate;
  end;
end;

procedure TLsynFrame.MemoStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  SetStatusText(0, Format('%4d:%3d', [FMemo.CaretY, FMemo.CaretX]));
end;

end.
