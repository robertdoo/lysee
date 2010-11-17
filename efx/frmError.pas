unit frmError;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, ImgList, lse_efx,
  ExtCtrls, StdCtrls;

type
  TErrorForm = class(TForm)
    imsIndex: TImageList;
    Panel1: TPanel;
    btnCopy: TButton;
    btnSave: TButton;
    lvError: TListView;
    dlgSave: TSaveDialog;
    procedure lvErrorSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvErrorCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormHide(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FView: TEasyView;
    FUpdateCount: integer;
    FOnShowList: TNotifyEvent;
    FOnHideList: TNotifyEvent;
    function GetErrorCount: integer;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure Sort;
    procedure ErrorAt(Sender: TObject; Page: TEfxPage; const Cell, Msg: string);
    procedure SaveToList(list: TStrings);
    property View: TEasyView read FView write FView;
    property ErrorCount: integer read GetErrorCount;
    property OnShowList: TNotifyEvent read FOnShowList write FOnShowList;
    property OnHideList: TNotifyEvent read FOnHideList write FOnHideList;
  end;

var
  ErrorForm: TErrorForm;

function ErrorList: TErrorForm;

procedure ShowErrorList;
procedure HideErrorList;

implementation

uses
  lse_msgbox, Clipbrd;

{$R *.dfm}

function ErrorList: TErrorForm;
begin
  if ErrorForm = nil then
    ErrorForm := TErrorForm.Create(Application);
  Result := ErrorForm;
end;

procedure ShowErrorList;
begin
  ErrorList.Show;
end;

procedure HideErrorList;
begin
  ErrorList.Close;
end;

procedure TErrorForm.lvErrorSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  P: TPoint;
begin
  if csDestroying in ComponentState then Exit;
  Item.ImageIndex := integer(Selected);
  if Selected then
  begin
    if DecodeRangeID(Item.SubItems[0], P) then
      FView.SelectRange(P, P);
    Caption := Format('Error [%5d/%-5d]', [Item.Index + 1, ErrorCount]);
  end;
end;

procedure TErrorForm.lvErrorCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  P1, P2: TPoint;
begin
  Compare := StrToInt(Item1.Caption) - StrToInt(Item2.Caption);
  if Compare = 0 then
  begin
    Compare := AnsiCompareText(Item1.SubItems[1], Item2.SubItems[1]);
    if Compare = 0 then
    begin
      DecodeRangeID(Item1.SubItems[0], P1);
      DecodeRangeID(Item2.SubItems[0], P2);
      Compare := P1.Y - P2.Y;
      if Compare = 0 then
        Compare := P1.X - P2.X;
    end;
  end;
end;

procedure TErrorForm.BeginUpdate;
begin
  lse_msgbox.lock_cursor;
  lvError.Items.BeginUpdate;
  Inc(FUpdateCount);
end;

procedure TErrorForm.Clear;
begin
  lvError.Clear;
end;

procedure TErrorForm.EndUpdate;
var
  item: TListItem;
  index: integer;
begin
  lvError.Items.EndUpdate;
  Inc(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    Sort;
    if ErrorCount > 0 then Show else Close;
  end;
  item := lvError.Selected;
  if item <> nil then
    index := item.ImageIndex + 1 else
    index := 0;
  Caption := Format('错误列表 [%5d/%-5d]', [Index, ErrorCount]);
  lse_msgbox.unlock_cursor;
end;

procedure TErrorForm.ErrorAt(Sender: TObject; Page: TEfxPage;
  const Cell, Msg: string);
var
  item: TListItem;
begin
  item := lvError.Items.Add;
  item.Caption := IntToStr(Page.PageIndex + 1);
  item.SubItems.Add(Cell);
  item.SubItems.Add(Msg);
  item.ImageIndex := 0;
  Show;
end;

procedure TErrorForm.Sort;
begin
  lse_msgbox.lock_cursor;
  try
    lvError.SortType := stBoth;
    lvError.SortType := stNone;
  finally
    lse_msgbox.unlock_cursor;
  end;
end;

procedure TErrorForm.FormCreate(Sender: TObject);
begin
  Top := Height - ClientHeight - 3;
  Left := Screen.Width - Width - 0;
end;

function TErrorForm.GetErrorCount: integer;
begin
  Result := lvError.Items.Count;
end;

procedure TErrorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FOnHideList) then
    FOnHideList(Self);
end;

procedure TErrorForm.FormShow(Sender: TObject);
begin
  if Assigned(FOnShowList) then
    FOnShowList(Self);
end;

procedure TErrorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FOnHideList) then
    FOnHideList(Self);
end;

procedure TErrorForm.FormHide(Sender: TObject);
begin
  if Assigned(FOnHideList) then
    FOnHideList(Self);
end;

procedure TErrorForm.SaveToList(list: TStrings);
const
  F = '%-12s  %-12s  %s';
var
  A: integer;
  N: TListItem;
begin
  list.Clear;
  list.Add(Format(F, ['页码', '位置(列行)', '错误说明']));
  list.Add(Format(F, ['--------', '------------', '--------------------------------']));
  for A := 0 to lvError.Items.Count - 1 do
  begin
    N := lvError.Items[A];
    list.Add(Format(F, [N.Caption, N.SubItems[0], N.SubItems[1]]));
  end;
end;

procedure TErrorForm.btnCopyClick(Sender: TObject);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    SaveToList(L);
    Clipboard.AsText := L.Text;
  finally
    L.Free;
  end;
end;

procedure TErrorForm.btnSaveClick(Sender: TObject);
var
  L: TStrings;
begin
  if not dlgSave.Execute then Exit;
  L := TStringList.Create;
  try
    SaveToList(L);
    L.SaveToFile(dlgSave.FileName);
  finally
    L.Free;
  end;
end;

end.
