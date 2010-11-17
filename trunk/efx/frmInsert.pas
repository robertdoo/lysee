unit frmInsert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ToolWin, ExtCtrls;

type
  TInsertForm = class(TForm)
    lvAppx: TListView;
    eFind: TEdit;
    btnNext: TButton;
    btnOK: TButton;
    Label1: TLabel;
    btnCancel: TButton;
    procedure lvAppxColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvAppxCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
    procedure eFindChange(Sender: TObject);
    procedure lvAppxSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvAppxDblClick(Sender: TObject);
  private
    procedure SetupAppxInserts;
  end;

var
  InsertForm: TInsertForm;

function InsertCode(var code: string): boolean;

implementation

uses lseu, lse_lml, lse_msgbox;

{$R *.dfm}

function InsertCode(var code: string): boolean;
begin
  if InsertForm = nil then
    InsertForm := TInsertForm.Create(Application);
  Result := (InsertForm.ShowModal = mrOK);
  if Result then
    code := InsertForm.lvAppx.Selected.SubItems[1];
end;

{ TInsertForm }

procedure TInsertForm.SetupAppxInserts;
var
  index, next: integer;
  node: TListItem;
  engine: TLseEngine;
  clss, func: TLMLNode;
  builtins: TLMLDocument;

  procedure assemble(const APrototype: string);
  var
    inpos: integer;
    prototype: string;
  begin
    prototype := Trim(APrototype);
    inpos := System.Pos(' ', prototype);
    node.SubItems.Add(Trim(Copy(prototype, 1, inpos - 1)));
    node.SubItems.Add(Trim(Copy(prototype, inpos + 1, Length(prototype))));
  end;

begin
  builtins := TLMLDocument.Create('');
  try
    engine := TLseEngine.Create(Self);
    try
      engine.ExecuteCode('return sys::builtins().content();');
      builtins.LoadFromPPCString(engine.ResultText);
      for index := 0 to builtins.Root.Count - 1 do
      begin
        clss := builtins.Root.Item[index];
        for next := 0 to clss.Count - 1 do
        begin
          func := clss.Item[next];
          node := lvAppx.Items.Add;
          node.Caption := clss.Text;
          assemble(func.Text);
          node.SubItems.Add(func.attr.Read('description', ''));
          node.ImageIndex := 50;
        end;
      end;
      lvAppx.Column[0].Tag := 1;
      lvAppx.Column[2].Tag := 1;
      lvAppxColumnClick(lvAppx, lvAppx.Column[2]);
      lvAppxColumnClick(lvAppx, lvAppx.Column[0]);
      Caption := Format('插入代码  -  [%d]', [lvAppx.Items.Count]);
    finally
      engine.Free;
    end;
  finally
    builtins.Free;
  end;
end;

procedure TInsertForm.lvAppxColumnClick(Sender: TObject; Column: TListColumn);
var
  V: TListView;
  M: TListItem;
begin
  lse_msgbox.lock_cursor;
  try
    V := Sender as TListView;
    V.Tag := Column.Index;
    Column.Tag := 1 - Column.Tag;
    V.SortType := stBoth;
    V.SortType := stNone;
    M := V.Selected;
    if Assigned(M) then M.MakeVisible(true);
  finally
    lse_msgbox.unlock_cursor;
  end;
end;

procedure TInsertForm.lvAppxCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  V: TListView;
  S1, S2: string;
begin
  V := Sender as TListView;
  if V.Tag = 0 then
  begin
    S1 := Item1.Caption;
    S2 := Item2.Caption;
  end
  else
  begin
    S1 := Item1.SubItems[V.Tag - 1];
    S2 := Item2.SubItems[V.Tag - 1];
  end;
  if V.Columns[V.Tag].Tag = 0 then
    Compare := CompareText(S1, S2) else
    Compare := CompareText(S2, S1);
end;

procedure TInsertForm.FormCreate(Sender: TObject);
begin
  SetupAppxInserts;
end;

procedure TInsertForm.eFindChange(Sender: TObject);
var
  node: TListItem;
  index, next: integer;
  text: string;

  function Match: boolean;
  var
    X: integer;
  begin
    Result := Pos(text, LowerCase(node.Caption)) > 0;
    if Result then Exit;
    for X := 0 to node.SubItems.Count - 1 do
    begin
      Result := Pos(text, LowerCase(node.SubItems[X])) > 0;
      if Result then Exit;
    end;
  end;

begin
  text := LowerCase(Trim(eFind.Text));
  btnNext.Enabled := (text <> '');
  
  if not btnNext.Enabled then Exit;

  node := lvAppx.Selected;
  if node <> nil then
  begin
    index := node.Index;
    if Sender = btnNext then
      Inc(index);
  end
  else index := 0;

  for next := index to lvAppx.Items.Count - 1 do
  begin
    node := lvAppx.Items[next];
    if Match then
    begin
      lvAppx.Selected := node;
      node.MakeVisible(false);
      Exit;
    end;
  end;

  for next := 0 to index - 1 do
  begin
    node := lvAppx.Items[next];
    if Match then
    begin
      lvAppx.Selected := node;
      node.MakeVisible(false);
      Exit;
    end;
  end;
end;

procedure TInsertForm.lvAppxSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnOk.Enabled := Selected;
  if Selected then
    Caption := Format('插入代码  -  [%4d/%-4d]',
      [Item.Index + 1, lvAppx.Items.Count]);
end;

procedure TInsertForm.lvAppxDblClick(Sender: TObject);
begin
  if btnOK.Enabled then ModalResult := mrOK;
end;

end.
