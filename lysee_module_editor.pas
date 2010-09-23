{==============================================================================}
{        UNIT: lysee_module_editor                                             }
{ DESCRIPTION: lysee module editor for lazarus                                 }
{     CREATED: 2010/09/23                                                      }
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
unit lysee_module_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  ComponentEditors, PropEdits, LCLIntf, LCLType,
  Graphics, lysee_laz;

type
  TLyseeModuleComponentEditor = class;
  
  { TLyseeModuleEditor }

  TLyseeModuleEditor = class(TForm)
    lstFunc: TListBox;
    pnlToolbar: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnRefresh: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure lstFuncDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LyseeClassEditorClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LyseeClassEditorKeyPress(Sender: TObject; var Key: char);
    procedure lstFuncClick(Sender: TObject);
    procedure lstFuncDblClick(Sender: TObject);
  private
    FLyseeModule: TLyseeModule;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TLyseeModuleComponentEditor;
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
    procedure OnDeletePersistent(var APersistent: TPersistent);
    procedure SelectOnly(AComponent: TComponent);
    function GetSelectedFunc: TLyseeFunc;
    function GetFunc(Index: integer): TLyseeFunc;
    procedure EnableButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLyseeClass(ALyseeClass: TLyseeModule);
    procedure SetDesigner(ADesigner: TComponentEditorDesigner);
    procedure SetComponentEditor(AComponentEditor: TLyseeModuleComponentEditor);
    procedure FillClassMethods;
  end;

  { TLyseeModuleComponentEditor }

  TLyseeModuleComponentEditor = class(TComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
    FEditor: TLyseeModuleEditor;
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

{ TLyseeModuleEditor }

procedure TLyseeModuleEditor.OnComponentRenamed(AComponent: TComponent);
var
  index: integer;
begin
  index := lstFunc.Items.IndexOfObject(AComponent);
  if index >= 0 then
    lstFunc.Items[index] := AComponent.Name;
  EnableButtons;
end;

procedure TLyseeModuleEditor.OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
var
  func: TLyseeFunc;
begin
  if Assigned(OnSetSelection) and (OnSetSelection.Count > 0) and
    (OnSetSelection.Items[0] is TLyseeFunc) and
    (TLyseeFunc(OnSetSelection.Items[0]).Module = FLyseeModule) then
  begin
    func := TLyseeFunc(OnSetSelection.Items[0]);
    if func <> GetSelectedFunc then
      lstFunc.ItemIndex := lstFunc.Items.IndexOf(func.Name);
  end
  else lstFunc.ItemIndex := -1;
  EnableButtons;
end;

procedure TLyseeModuleEditor.OnDeletePersistent(var APersistent: TPersistent);
var
  index: integer;
begin
  if APersistent = FLyseeModule then
  begin
    FLyseeModule := nil;
    lstFunc.Items.Clear;
  end
  else
  begin
    index := lstFunc.Items.IndexOfObject(APersistent);
    if index >= 0 then
      lstFunc.Items.Delete(index);
  end;
  EnableButtons;
end;

function TLyseeModuleEditor.GetSelectedFunc: TLyseeFunc;
var
  index: integer;
begin
  index := lstFunc.ItemIndex;
  if index >= 0 then
    Result := GetFunc(index) else
    Result := nil;
end;

function TLyseeModuleEditor.GetFunc(Index: integer): TLyseeFunc;
begin
  Result := TLyseeFunc(lstFunc.Items.Objects[index]);
  if (FLyseeModule = nil) or not FLyseeModule.FuncExists(Result) then
  begin
    Result := nil;
    lstFunc.Items.Delete(index);
    EnableButtons;
  end;
end;

procedure TLyseeModuleEditor.EnableButtons;
begin
  btnAdd.Enabled := (FLyseeModule <> nil);
  btnDelete.Enabled := (FLyseeModule <> nil) and (lstFunc.ItemIndex >= 0);
  btnRefresh.Enabled := (FLyseeModule <> nil);
end;

procedure TLyseeModuleEditor.SelectOnly(AComponent: TComponent);
begin
  if AComponent <> nil then
    FDesigner.SelectOnlyThisComponent(AComponent);
end;

procedure TLyseeModuleEditor.btnAddClick(Sender: TObject);
var
  func: TLyseeFunc;
begin
  func := TLyseeFunc.Create(FLyseeModule.Owner);
  func.Name := FDesigner.CreateUniqueComponentName(func.ClassName);
  func.Module := FLyseeModule;
  FDesigner.PropertyEditorHook.PersistentAdded(func, false);
  FDesigner.Modified;
  lstFunc.ItemIndex := lstFunc.Items.AddObject(func.Name, func);
  SelectOnly(func);
end;

procedure TLyseeModuleEditor.btnRefreshClick(Sender: TObject);
begin
  FillClassMethods;
end;

procedure TLyseeModuleEditor.lstFuncDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  func: TLyseeFunc;
begin
  lstFunc.Canvas.FillRect(ARect);
  func := GetFunc(Index);
  if func <> nil then
    lstFunc.Canvas.TextOut(ARect.Left + MarginX, ARect.Top + MarginY,
      Format('%-20s %s', [func.Name, func.Prototype]));
end;

procedure TLyseeModuleEditor.LyseeClassEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FComponentEditor.FEditor := nil;
  CloseAction := caFree;
end;

procedure TLyseeModuleEditor.LyseeClassEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Close;
end;

procedure TLyseeModuleEditor.btnDeleteClick(Sender: TObject);
var
  index: Integer;
  func: TLyseeFunc;
begin
  func := GetSelectedFunc;
  if func <> nil then
  begin
    index := lstFunc.ItemIndex;
    try
      lstFunc.Items.Delete(index);
      FDesigner.PropertyEditorHook.DeletePersistent(TPersistent(func));
    except
      on E: Exception do begin
        MessageDlg('Error deleting func',
          'Error while deleting emthod:' + sLineBreak +
          E.Message, mtError, [mbOk], 0);
      end;
    end;
    if lstFunc.Items.Count > 0 then
    begin
      if index >= lstFunc.Items.Count then
        lstFunc.ItemIndex := lstFunc.Items.Count -1 else
        lstFunc.ItemIndex := index;
      SelectOnly(GetSelectedFunc);
    end
    else SelectOnly(FLyseeModule);
    EnableButtons;
  end
  else FillClassMethods;
end;

procedure TLyseeModuleEditor.lstFuncClick(Sender: TObject);
begin
  SelectOnly(GetSelectedFunc);
  EnableButtons;
end;

procedure TLyseeModuleEditor.lstFuncDblClick(Sender: TObject);
var
  func: TLyseeFunc;
begin
  if lstFunc.GetIndexAtY(lstFunc.ScreenToClient(Mouse.CursorPos).Y) >= 0 then
  begin
    func := GetSelectedFunc;
    if func <> nil then
      CreateComponentEvent(func, 'OnExecute');
  end;
end;

constructor TLyseeModuleEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
    GlobalDesignHook.AddHandlerDeletePersistent(@OnDeletePersistent);
  end;
  FLyseeModule := nil;
  EnableButtons;
  lstFunc.ItemHeight := lstFunc.Canvas.TextHeight('H') + MarginY * 2;
end;

destructor TLyseeModuleEditor.Destroy;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TLyseeModuleEditor.SetLyseeClass(ALyseeClass: TLyseeModule);
begin
  FLyseeModule := ALyseeClass;
  FillClassMethods;
end;

procedure TLyseeModuleEditor.SetDesigner(ADesigner: TComponentEditorDesigner);
begin
  FDesigner := ADesigner;
end;

procedure TLyseeModuleEditor.SetComponentEditor(AComponentEditor: TLyseeModuleComponentEditor);
begin
  FComponentEditor := AComponentEditor;
end;

procedure TLyseeModuleEditor.FillClassMethods;
var
  curr: string;
  index: Integer;
  func: TLyseeFunc;
begin
  index := lstFunc.ItemIndex;
  if index >= 0 then
    curr := lstFunc.Items[index] else
    curr := '';

  lstFunc.Items.BeginUpdate;
  try
    lstFunc.Clear;
    if FLyseeModule <> nil then
      for index := 0 to FLyseeModule.FuncCount - 1 do
      begin
        func := FLyseeModule.Funcs[index];
        lstFunc.Items.AddObject(func.Name, func);
      end;
  finally
    lstFunc.Items.EndUpdate;
  end;

  lstFunc.Sorted := true;
  lstFunc.Sorted := false;

  index := lstFunc.Items.IndexOf(curr);
  if index >= 0 then
    lstFunc.ItemIndex := index else
  if lstFunc.ItemIndex < 0 then
    SelectOnly(FLyseeModule);

  EnableButtons;
end;

{ TLyseeModuleComponentEditor }

constructor TLyseeModuleComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
  FEditor := nil;
end;

destructor TLyseeModuleComponentEditor.Destroy;
begin
  FreeThenNil(FEditor);
  inherited Destroy;
end;

procedure TLyseeModuleComponentEditor.Edit;
var
  LyseeModule: TLyseeModule;
begin
  LyseeModule := GetComponent as TLyseeModule;
  if LyseeModule = nil then
    raise Exception.Create('TLyseeModuleComponentEditor.Edit LyseeModule = nil');
  if FEditor = nil then
    FEditor := TLyseeModuleEditor.Create(Application) else
  if FEditor.lstFunc.Items.Count > 0 then
    FEditor.lstFunc.ItemIndex := -1;
  FEditor.SetComponentEditor(Self);
  FEditor.SetDesigner(FDesigner);
  FEditor.SetLyseeClass(LyseeModule);
  FEditor.ShowOnTop;
end;

function TLyseeModuleComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TLyseeModuleComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit module ...';
end;

procedure TLyseeModuleComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;


initialization
  {$I lysee_module_editor.lrs}
  RegisterComponentEditor(TLyseeModule, TLyseeModuleComponentEditor);

end.

