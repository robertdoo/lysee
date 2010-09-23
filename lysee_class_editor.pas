{==============================================================================}
{        UNIT: lysee_class_editor                                              }
{ DESCRIPTION: lysee class editor for lazarus                                  }
{     CREATED: 2010/09/21                                                      }
{    MODIFIED: 2010/09/23                                                      }
{==============================================================================}
{ Copyright (c) 2010, Li Yun Jie                                               }
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
unit lysee_class_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  ComponentEditors, PropEdits, LCLIntf, LCLType,
  Graphics, lysee_laz;

type
  TLyseeClassComponentEditor = class;
  
  { TLyseeClassEditor }

  TLyseeClassEditor = class(TForm)
    lstMethod: TListBox;
    pnlToolbar: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnRefresh: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure lstMethodDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LyseeClassEditorClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LyseeClassEditorKeyPress(Sender: TObject; var Key: char);
    procedure lstMethodClick(Sender: TObject);
    procedure lstMethodDblClick(Sender: TObject);
  private
    FLyseeClass: TLyseeClass;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TLyseeClassComponentEditor;
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
    procedure OnDeletePersistent(var APersistent: TPersistent);
    procedure SelectOnly(AComponent: TComponent);
    function GetSelectedMethod: TLyseeMethod;
    function GetMethod(Index: integer): TLyseeMethod;
    procedure EnableButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLyseeClass(ALyseeClass: TLyseeClass);
    procedure SetDesigner(ADesigner: TComponentEditorDesigner);
    procedure SetComponentEditor(AComponentEditor: TLyseeClassComponentEditor);
    procedure FillClassMethods;
  end;

  { TLyseeClassComponentEditor }

  TLyseeClassComponentEditor = class(TComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
    FEditor: TLyseeClassEditor;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner);override;
    destructor Destroy; override;
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

const
  MarginX = 2;
  MarginY = 2;

{ TLyseeClassEditor }

procedure TLyseeClassEditor.OnComponentRenamed(AComponent: TComponent);
var
  index: integer;
begin
  index := lstMethod.Items.IndexOfObject(AComponent);
  if index >= 0 then
    lstMethod.Items[index] := AComponent.Name;
  EnableButtons;
end;

procedure TLyseeClassEditor.OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
var
  method: TLyseeMethod;
begin
  if Assigned(OnSetSelection) and (OnSetSelection.Count > 0) and
    (OnSetSelection.Items[0] is TLyseeMethod) and
    (TLyseeMethod(OnSetSelection.Items[0]).LyseeClass = FLyseeClass) then
  begin
    method := TLyseeMethod(OnSetSelection.Items[0]);
    if method <> GetSelectedMethod then
      lstMethod.ItemIndex := lstMethod.Items.IndexOf(method.Name);
  end
  else lstMethod.ItemIndex := -1;
  EnableButtons;
end;

procedure TLyseeClassEditor.OnDeletePersistent(var APersistent: TPersistent);
var
  index: integer;
begin
  if APersistent = FLyseeClass then
  begin
    FLyseeClass := nil;
    lstMethod.Items.Clear;
  end
  else
  begin
    index := lstMethod.Items.IndexOfObject(APersistent);
    if index >= 0 then
      lstMethod.Items.Delete(index);
  end;
  EnableButtons;
end;

function TLyseeClassEditor.GetSelectedMethod: TLyseeMethod;
var
  index: integer;
begin
  index := lstMethod.ItemIndex;
  if index >= 0 then
    Result := GetMethod(index) else
    Result := nil;
end;

function TLyseeClassEditor.GetMethod(Index: integer): TLyseeMethod;
begin
  Result := TLyseeMethod(lstMethod.Items.Objects[index]);
  if (FLyseeClass = nil) or not FLyseeClass.Exists(Result) then
  begin
    Result := nil;
    lstMethod.Items.Delete(index);
    EnableButtons;
  end;
end;

procedure TLyseeClassEditor.EnableButtons;
begin
  btnAdd.Enabled := (FLyseeClass <> nil);
  btnDelete.Enabled := (FLyseeClass <> nil) and (lstMethod.ItemIndex >= 0);
  btnRefresh.Enabled := (FLyseeClass <> nil);
end;

procedure TLyseeClassEditor.SelectOnly(AComponent: TComponent);
begin
  if AComponent <> nil then
    FDesigner.SelectOnlyThisComponent(AComponent);
end;

procedure TLyseeClassEditor.btnAddClick(Sender: TObject);
var
  method: TLyseeMethod;
begin
  method := TLyseeMethod.Create(FLyseeClass.Owner);
  method.Name := FDesigner.CreateUniqueComponentName(method.ClassName);
  method.LyseeClass := FLyseeClass;
  FDesigner.PropertyEditorHook.PersistentAdded(method, false);
  FDesigner.Modified;
  lstMethod.ItemIndex := lstMethod.Items.AddObject(method.Name, method);
  SelectOnly(method);
end;

procedure TLyseeClassEditor.btnRefreshClick(Sender: TObject);
begin
  FillClassMethods;
end;

procedure TLyseeClassEditor.lstMethodDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  method: TLyseeMethod;
begin
  lstMethod.Canvas.FillRect(ARect);
  method := GetMethod(Index);
  if method <> nil then
    lstMethod.Canvas.TextOut(ARect.Left + MarginX, ARect.Top + MarginY,
      Format('%-20s %s', [method.Name, method.Prototype]));
end;

procedure TLyseeClassEditor.LyseeClassEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FComponentEditor.FEditor := nil;
  CloseAction := caFree;
end;

procedure TLyseeClassEditor.LyseeClassEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Close;
end;

procedure TLyseeClassEditor.btnDeleteClick(Sender: TObject);
var
  index: Integer;
  method: TLyseeMethod;
begin
  method := GetSelectedMethod;
  if method <> nil then
  begin
    index := lstMethod.ItemIndex;
    try
      lstMethod.Items.Delete(index);
      FDesigner.PropertyEditorHook.DeletePersistent(TPersistent(method));
    except
      on E: Exception do begin
        MessageDlg('Error deleting method',
          'Error while deleting emthod:' + sLineBreak +
          E.Message, mtError, [mbOk], 0);
      end;
    end;
    if lstMethod.Items.Count > 0 then
    begin
      if index >= lstMethod.Items.Count then
        lstMethod.ItemIndex := lstMethod.Items.Count -1 else
        lstMethod.ItemIndex := index;
      SelectOnly(GetSelectedMethod);
    end
    else SelectOnly(FLyseeClass);
    EnableButtons;
  end
  else FillClassMethods;
end;

procedure TLyseeClassEditor.lstMethodClick(Sender: TObject);
begin
  SelectOnly(GetSelectedMethod);
  EnableButtons;
end;

procedure TLyseeClassEditor.lstMethodDblClick(Sender: TObject);
var
  method: TLyseeMethod;
begin
  if lstMethod.GetIndexAtY(lstMethod.ScreenToClient(Mouse.CursorPos).Y) >= 0 then
  begin
    method := GetSelectedMethod;
    if method <> nil then
      CreateComponentEvent(method, 'OnExecute');
  end;
end;

constructor TLyseeClassEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
    GlobalDesignHook.AddHandlerDeletePersistent(@OnDeletePersistent);
  end;
  FLyseeClass := nil;
  EnableButtons;
  lstMethod.ItemHeight := lstMethod.Canvas.TextHeight('H') + MarginY * 2;
end;

destructor TLyseeClassEditor.Destroy;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TLyseeClassEditor.SetLyseeClass(ALyseeClass: TLyseeClass);
begin
  FLyseeClass := ALyseeClass;
  FillClassMethods;
end;

procedure TLyseeClassEditor.SetDesigner(ADesigner: TComponentEditorDesigner);
begin
  FDesigner := ADesigner;
end;

procedure TLyseeClassEditor.SetComponentEditor(AComponentEditor: TLyseeClassComponentEditor);
begin
  FComponentEditor := AComponentEditor;
end;

procedure TLyseeClassEditor.FillClassMethods;
var
  curr: string;
  index: Integer;
  method: TLyseeMethod;
begin
  index := lstMethod.ItemIndex;
  if index >= 0 then
    curr := lstMethod.Items[index] else
    curr := '';

  lstMethod.Items.BeginUpdate;
  try
    lstMethod.Clear;
    if FLyseeClass <> nil then
      for index := 0 to FLyseeClass.MethodCount - 1 do
      begin
        method := FLyseeClass.Methods[index];
        lstMethod.Items.AddObject(method.Name, method);
      end;
  finally
    lstMethod.Items.EndUpdate;
  end;

  lstMethod.Sorted := true;
  lstMethod.Sorted := false;

  index := lstMethod.Items.IndexOf(curr);
  if index >= 0 then
    lstMethod.ItemIndex := index else
  if lstMethod.ItemIndex < 0 then
    SelectOnly(FLyseeClass);

  EnableButtons;
end;

{ TLyseeClassComponentEditor }

constructor TLyseeClassComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
  FEditor := nil;
end;

destructor TLyseeClassComponentEditor.Destroy;
begin
  FreeThenNil(FEditor);
  inherited Destroy;
end;

procedure TLyseeClassComponentEditor.Edit;
var
  LyseeClass: TLyseeClass;
begin
  LyseeClass := GetComponent as TLyseeClass;
  if LyseeClass = nil then
    raise Exception.Create('TLyseeClassComponentEditor.Edit LyseeClass = nil');
  if FEditor = nil then
    FEditor := TLyseeClassEditor.Create(Application) else
  if FEditor.lstMethod.Items.Count > 0 then
    FEditor.lstMethod.ItemIndex := -1;
  FEditor.SetComponentEditor(Self);
  FEditor.SetDesigner(FDesigner);
  FEditor.SetLyseeClass(LyseeClass);
  FEditor.ShowOnTop;
end;

function TLyseeClassComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TLyseeClassComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit class ...';
end;

procedure TLyseeClassComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;


initialization
  {$I lysee_class_editor.lrs}
  RegisterComponentEditor(TLyseeClass, TLyseeClassComponentEditor);

end.

