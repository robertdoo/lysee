unit frmStyle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, lse_efx, Spin, ImgList;

type
  TStyleForm = class(TForm)
    lsbFonts: TListBox;
    grpInfo: TGroupBox;
    btnOK: TButton;
    btnAdd: TButton;
    cbxFont: TComboBox;
    Label1: TLabel;
    edtSize: TSpinEdit;
    Label2: TLabel;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    cbxHorz: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    cbxVert: TComboBox;
    pnTextColorBox: TPanel;
    dlgColor: TColorDialog;
    pnBackColorBox: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnTextColor: TPanel;
    pnBackColor: TPanel;
    edtLineDistance: TSpinEdit;
    Label7: TLabel;
    pnView: TPanel;
    btnRename: TButton;
    btnCancel: TButton;
    btnDelete: TButton;
    btnDeleteUS: TButton;
    chkWordWrap: TCheckBox;
    cbxItemType: TComboBox;
    Label8: TLabel;
    ilsEdges: TImageList;
    Label9: TLabel;
    chkLeft: TCheckBox;
    chkRight: TCheckBox;
    chkTop: TCheckBox;
    chkBottom: TCheckBox;
    Panel3: TPanel;
    Panel4: TPanel;
    pnViewSample: TPanel;
    pbxSample: TPaintBox;
    procedure lsbFontsClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxFontChange(Sender: TObject);
    procedure edtSizeChange(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure pnTextColorBoxClick(Sender: TObject);
    procedure pnBackColorBoxClick(Sender: TObject);
    procedure cbxHorzChange(Sender: TObject);
    procedure cbxVertChange(Sender: TObject);
    procedure edtLineDistanceChange(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDeleteUSClick(Sender: TObject);
    procedure chkWordWrapClick(Sender: TObject);
    procedure cbxItemTypeChange(Sender: TObject);
    procedure chkLeftClick(Sender: TObject);
    procedure chkRightClick(Sender: TObject);
    procedure chkTopClick(Sender: TObject);
    procedure chkBottomClick(Sender: TObject);
    procedure pbxSamplePaint(Sender: TObject);
  private
    FStyleList: TEasyStyles;
    FStyle: TEasyStyle;
    FPutting: boolean;
    function Select(Font: TEasyStyle): boolean;
    function InputStyleName(var ID: string): boolean;
    procedure NotifyChange;
    procedure PutColor(Box, Value: TPanel; Color: TColor);
  end;

var
  StyleForm: TStyleForm;

function PickStyle(var AStyle: TEasyStyle; AStyleList: TEasyStyles): boolean;

implementation

uses lse_lml, lse_msgbox;

{$R *.dfm}

function PickStyle(var AStyle: TEasyStyle; AStyleList: TEasyStyles): boolean;
begin
  with TStyleForm.Create(Application) do
  try
    FStyleList := AStyleList;
    FStyleList.ListTo(lsbFonts.Items, nil);
    Select(AStyle);
    Result := (ShowModal = mrOK);
    if Result then
      AStyle := FStyle;
  finally
    Release;
  end;
end;

procedure TStyleForm.btnAddClick(Sender: TObject);
var
  ID: string;
  font: TEasyStyle;
begin
  ID := '';
  if InputStyleName(ID) then
  begin
    font := TEasyStyle.Create(ID);
    if FStyle <> nil then
      font.Assign(FStyle);
    FStyleList.Add(font);
    lsbFonts.Items.AddObject(font.Name, font);
    Select(font);
    NotifyChange;
  end;
end;

procedure TStyleForm.btnDeleteClick(Sender: TObject);
var
  index: integer;
begin
  if (FStyle <> nil) then
    if (Sender = nil) or AnsYesFmt('Delete style "%s"?', [FStyle.Name]) then
    begin
      index := lsbFonts.ItemIndex;
      FStyleList.Remove(FStyle);
      NotifyChange;
      lsbFonts.Items.Delete(index);
      if index = lsbFonts.Items.Count then
        Dec(index);
      lsbFonts.ItemIndex := index;
      lsbFontsClick(nil);
    end;
end;

procedure TStyleForm.btnDeleteUSClick(Sender: TObject);
var
  index: integer;
  style: TEasyStyle;
begin
  if AnsYes('Delete all styles, are you sure?') then
  begin
    lock_cursor;
    lsbFonts.Items.BeginUpdate;
    try
      style := FStyle;
      for index :=lsbFonts.Items.Count - 1 downto 0 do
      begin
        lsbFonts.ItemIndex := index;
        lsbFontsClick(nil);
        if FStyle <> nil then
          btnDeleteClick(nil);
      end;
      Select(style);
    finally
      lsbFonts.Items.EndUpdate;
      unlock_cursor;
    end;
  end;
end;

procedure TStyleForm.btnRenameClick(Sender: TObject);
var
  ID: string;
  index: integer;
begin
  if FStyle <> nil then
  begin
    ID := FStyle.Name;
    if AnsiSameText('us_', Copy(ID, 1, 3)) or
       AnsiSameText('ns_', Copy(ID, 1, 3)) then
         ID := Copy(ID, 4, Length(ID));
    if InputStyleName(ID) then
    begin
      FStyle.Name := ID;
      lsbFonts.Sorted := false;
      index := lsbFonts.ItemIndex;
      lsbFonts.Items[index] := FStyle.Name;
      lsbFonts.Sorted := true;
      NotifyChange;
    end;
  end;
end;

procedure TStyleForm.cbxFontChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.FontName := cbxFont.Text;
    NotifyChange;
  end;
end;

procedure TStyleForm.cbxHorzChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.HAlign := TEasyHorzAlign(cbxHorz.ItemIndex);
    NotifyChange;
  end;
end;

procedure TStyleForm.cbxItemTypeChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.CellType := TEasyStyleType(cbxItemType.ItemIndex);
    NotifyChange;
  end;
end;

procedure TStyleForm.cbxVertChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.VAlign := TEasyVertAlign(cbxVert.ItemIndex);
    NotifyChange;
  end;
end;

procedure TStyleForm.chkLeftClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    if chkLeft.Checked then
      FStyle.Edges := FStyle.Edges + [eeLeft] else
      FStyle.Edges := FStyle.Edges - [eeLeft];
    NotifyChange;
  end;
end;

procedure TStyleForm.chkRightClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    if chkRight.Checked then
      FStyle.Edges := FStyle.Edges + [eeRight] else
      FStyle.Edges := FStyle.Edges - [eeRight];
    NotifyChange;
  end;
end;

procedure TStyleForm.chkTopClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    if chkTop.Checked then
      FStyle.Edges := FStyle.Edges + [eeTop] else
      FStyle.Edges := FStyle.Edges - [eeTop];
    NotifyChange;
  end;
end;

procedure TStyleForm.chkBoldClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.Bold := chkBold.Checked;
    NotifyChange;
  end;
end;

procedure TStyleForm.chkBottomClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    if chkBottom.Checked then
      FStyle.Edges := FStyle.Edges + [eeBottom] else
      FStyle.Edges := FStyle.Edges - [eeBottom];
    NotifyChange;
  end;
end;

procedure TStyleForm.chkItalicClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.Italic := chkItalic.Checked;
    NotifyChange;
  end;
end;

procedure TStyleForm.chkUnderlineClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.Underline := chkUnderline.Checked;
    NotifyChange;
  end;
end;

procedure TStyleForm.chkWordWrapClick(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.WordWrap := chkWordWrap.Checked;
    NotifyChange;
  end;
end;

procedure TStyleForm.edtLineDistanceChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.LineDistance := edtLineDistance.Value;
    NotifyChange;
  end;
end;

procedure TStyleForm.edtSizeChange(Sender: TObject);
begin
  if not FPutting and (FStyle <> nil) then
  begin
    FStyle.FontSize := edtSize.Value;
    NotifyChange;
  end;
end;

procedure TStyleForm.FormCreate(Sender: TObject);
begin
  cbxFont.Items.Assign(Screen.Fonts);
end;

function TStyleForm.InputStyleName(var ID: string): boolean;
begin
  ID := Trim(InputBox('Style', 'New style name:', ID));
  Result := lml_is_tag_name(ID);
  if Result then
  begin
    ID := 'us_' + ID;
    Result := not FStyleList.Exists(ID);
    if not Result then
      MsgErr('Style name conflict!');
  end
  else MsgErr('Invalid style name!');
end;

procedure TStyleForm.lsbFontsClick(Sender: TObject);
var
  index: integer;
begin
  pbxSample.Visible := false;
  index := lsbFonts.ItemIndex;
  if index >= 0 then
    FStyle := TEasyStyle(lsbFonts.Items.Objects[index]) else
    FStyle := nil;
  btnOk.Enabled := (FStyle <> nil);
  grpInfo.Enabled := (FStyle <> nil);
  btnRename.Enabled := (FStyle <> nil);
  btnDelete.Enabled := (FStyle <> nil);
  if FStyle <> nil then
  begin
    FPutting := true;
    cbxFont.ItemIndex := cbxFont.Items.IndexOf(FStyle.FontName);
    edtSize.Value := FStyle.FontSize;
    chkBold.Checked := FStyle.Bold;
    chkItalic.Checked := FStyle.Italic;
    chkUnderline.Checked := FStyle.Italic;
    PutColor(pnTextColor, pnTextColorBox, FStyle.TextColor);
    PutColor(pnBackColor, pnBackColorBox, FStyle.Background);
    cbxHorz.ItemIndex := Ord(FStyle.HAlign);
    cbxVert.ItemIndex := Ord(FStyle.VAlign);
    edtLineDistance.Value := FStyle.LineDistance;
    chkWordWrap.Checked := FStyle.WordWrap;
    cbxItemType.ItemIndex := Ord(FStyle.CellType);
    chkLeft.Checked := eeLeft in FStyle.Edges;
    chkTop.Checked := eeTop in FStyle.Edges;
    chkRight.Checked := eeRight in FStyle.Edges;
    chkBottom.Checked := eeBottom in FStyle.Edges;
    FPutting := false;
    pbxSample.Visible := true;
  end;
end;

procedure TStyleForm.NotifyChange;
begin
  FStyleList.Change(FStyleList);
  pbxSample.Invalidate;
end;

procedure TStyleForm.pbxSamplePaint(Sender: TObject);
var
  V: TCanvas;
  R: TRect;
  L: TStrings;
begin
  if FStyle = nil then Exit;
  R := MakeRect(0, 0, pbxSample.Width, pbxSample.Height);
  V := pbxSample.Canvas;
  V.Brush.Style := bsSolid;
  V.Brush.Color := FStyle.Background;
  V.FillRect(R);
  V.Brush.Style := bsClear;

  R := ZoomRect(R, -1, -1);
  FStyle.Write(V.Font);
  L := TStringList.Create;
  try
    L.Add('Sample text.');
    L.Add('Virtue is its own reward.');
    Rectout(L.CommaText, V, R, FStyle.HAlign, FStyle.VAlign,
      FStyle.LineDistance, FStyle.WordWrap);
  finally
    L.Free;
  end;

  R := ZoomRect(R, 1, 1);
  V.Pen.Color := DefLineColor;
  DrawEdges(V, R, [eeLeft, eeTop, eeRight, eeBottom], 1, 1);
  
  if FStyle.Edges <> [] then
  begin
    V.Pen.Color := DefEdgeColor;
    DrawEdges(V, R, FStyle.Edges, 1, 1);
  end;
end;

procedure TStyleForm.pnBackColorBoxClick(Sender: TObject);
begin
  if FStyle <> nil then
  begin
    dlgColor.Color := FStyle.Background;
    if dlgColor.Execute then
      if dlgColor.Color <> FStyle.Background then
      begin
        FStyle.Background := dlgColor.Color;
        PutColor(pnBackColor, pnBackColorBox, FStyle.Background);
        NotifyChange;
      end;
  end;
end;

procedure TStyleForm.pnTextColorBoxClick(Sender: TObject);
begin
  if FStyle <> nil then
  begin
    dlgColor.Color := FStyle.TextColor;
    if dlgColor.Execute then
      if dlgColor.Color <> FStyle.TextColor then
      begin
        FStyle.TextColor := dlgColor.Color;
        PutColor(pnTextColor, pnTextColorBox, FStyle.TextColor);
        NotifyChange;
      end;
  end;
end;

procedure TStyleForm.PutColor(Box, Value: TPanel; Color: TColor);
begin
  Box.Color := Color;
  Value.Caption := Format('%.6x ', [Color]);
end;

function TStyleForm.Select(Font: TEasyStyle): boolean;
begin
  lsbFonts.ItemIndex := lsbFonts.Items.IndexOfObject(Font);
  lsbFontsClick(nil);
  Result := (FStyle <> nil);
end;

end.
