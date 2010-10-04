{==============================================================================}
{        UNIT: lse_module_editor                                               }
{ DESCRIPTION: lysee module editor for lazarus                                 }
{     CREATED: 2010/09/23                                                      }
{    MODIFIED: 2010/10/02                                                      }
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
unit lse_module_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Dialogs,
  ExtCtrls, Buttons, ComponentEditors, PropEdits, LCLIntf,
  LCLType, Graphics, ComCtrls, ButtonPanel, lse_components;

type
  TLyseeModuleComponentEditor = class;
  
  { TLyseeModuleEditor }

  TLyseeModuleEditor = class(TForm)
    ImageList1: TImageList;
    pnForm: TPanel;
    barStatus: TStatusBar;
    ToolBar1: TToolBar;
    btnAddClass: TToolButton;
    btnAddProc: TToolButton;
    btnRemove: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    trModule: TTreeView;
    procedure btnAddClassClick(Sender: TObject);
    procedure btnAddProcClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LyseeClassEditorClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure trModuleChange(Sender: TObject; Node: TTreeNode);
    procedure trModuleDblClick(Sender: TObject);
  private
    FLyseeModule: TLyseeModule;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TLyseeModuleComponentEditor;
    FRemoving: integer;
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure SelectOnly(AComponent: TComponent);
    procedure SetButtons;
    function MakeLabel(AComponent: TComponent): string;
    function SelectedNode: TTreeNode;
    function SelectedComponent: TLyseeComponent;
    function FindNode(APersistent: TPersistent): TTreeNode;
    function SetupClassNode(AClass: TLyseeClass): TTreeNode;
    procedure SetLyseeModule(ALyseeModule: TLyseeModule);
    procedure SetDesigner(ADesigner: TComponentEditorDesigner);
    procedure SetComponentEditor(AComponentEditor: TLyseeModuleComponentEditor);
    procedure SetupTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

{ TLyseeModuleEditor }

procedure TLyseeModuleEditor.OnComponentRenamed(AComponent: TComponent);
var
  node: TTreeNode;
begin
  if AComponent is TLyseeComponent then
  begin
    node := FindNode(AComponent);
    if node <> nil then
      node.Text := MakeLabel(AComponent);
  end;
end;

procedure TLyseeModuleEditor.OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
begin
  if (OnSetSelection <> nil) and (OnSetSelection.Count > 0) then
  begin
    trModule.Selected := FindNode(OnSetSelection.Items[0]);
    SetButtons;
  end;
end;

procedure TLyseeModuleEditor.OnPersistentDeleting(APersistent: TPersistent);
var
  node: TTreeNode;
begin
  if (FRemoving = 0) and (APersistent is TLyseeComponent) then
  begin
    node := FindNode(APersistent);
    if node <> nil then
    begin
      Inc(FRemoving);
      try
        node.DeleteChildren;
        node.Delete;
      finally
        Dec(FRemoving);
      end;
    end;
  end;
end;

procedure TLyseeModuleEditor.SetButtons;
var
  curr: TLyseeComponent;
begin
  curr := SelectedComponent;
  if curr = nil then
  begin
    barStatus.Panels[0].Text := 'none';
    barStatus.Panels[1].Text := '';
  end
  else
  begin
    if curr.ComponentType = lsoFunction then
      barStatus.Panels[0].Text := 'function' else
      barStatus.Panels[0].Text := LowerCase(Copy(curr.ClassName, 7, 100));
    case curr.ComponentType of
      lsoModule  : barStatus.Panels[1].Text := TLyseeModule(curr).Name;
      lsoFunction: barStatus.Panels[1].Text := TLyseeFunc(curr).Prototype;
      lsoClass   : barStatus.Panels[1].Text := TLyseeClass(curr).Name;
      lsoMethod  : barStatus.Panels[1].Text := TLyseeMethod(curr).Prototype;
      else         barStatus.Panels[1].Text := '';
    end;
    if curr.ComponentType in [lsoModule, lsoFunction] then
      btnAddProc.Caption := '&Function' else
      btnAddProc.Caption := '&Method';
  end;
  btnAddClass.Enabled := true;
  btnAddProc.Enabled := (curr <> nil);
  btnRemove.Enabled := (curr <> nil) and not (curr is TLyseeModule);
end;

function TLyseeModuleEditor.MakeLabel(AComponent: TComponent): string;
begin
  Result := AComponent.Name;
end;

function TLyseeModuleEditor.SelectedNode: TTreeNode;
begin
  Result := trModule.Selected;
end;

function TLyseeModuleEditor.SelectedComponent: TLyseeComponent;
var
  node: TTreeNode;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    Result := TLyseeComponent(node.Data);
    if (Result = FLyseeModule) or FLyseeModule.Exists(Result) then Exit;
    SetupTree;
  end;
  Result := nil;
end;

function TLyseeModuleEditor.FindNode(APersistent: TPersistent): TTreeNode;
var
  index: integer;
begin
  for index := 0 to trModule.Items.Count - 1 do
  begin
    Result := trModule.Items[index];
    if TPersistent(Result.Data) = APersistent then Exit;
  end;
  Result := nil;
end;

function TLyseeModuleEditor.SetupClassNode(AClass: TLyseeClass): TTreeNode;
begin
  Result := trModule.Items.AddObject(nil, MakeLabel(AClass), AClass);
  Result.ImageIndex := 1;
  Result.SelectedIndex := 1;
end;

procedure TLyseeModuleEditor.SelectOnly(AComponent: TComponent);
begin
  if AComponent <> nil then
    FDesigner.SelectOnlyThisComponent(AComponent);
end;

procedure TLyseeModuleEditor.btnAddProcClick(Sender: TObject);
var
  curr: TLyseeComponent;
  node: TTreeNode;
  func: TLyseeFunc;
  clss: TLyseeClass;
  meth: TLyseeMethod;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    if node.Level > 0 then node := node.Parent;
    curr := TLyseeComponent(node.Data);
    trModule.OnChange := nil;
    try
      if curr = FLyseeModule then
      begin
        func := TLyseeFunc.Create(FLyseeModule.Owner);
        func.Name := FDesigner.CreateUniqueComponentName(func.ClassName);
        func.Module := FLyseeModule;
        FDesigner.PropertyEditorHook.PersistentAdded(func, true);
        FDesigner.Modified;
        node := trModule.Items.AddChildObject(node, MakeLabel(func), func);
        node.ImageIndex := 2;
        node.SelectedIndex := 2;
        trModule.Selected := node;
      end
      else
      if curr.ComponentType = lsoClass then
      begin
        clss := TLyseeClass(curr);
        meth := TLyseeMethod.Create(FLyseeModule.Owner);
        meth.Name := FDesigner.CreateUniqueComponentName(meth.ClassName);
        meth.LyseeClass := clss;
        FDesigner.PropertyEditorHook.PersistentAdded(meth, true);
        FDesigner.Modified;
        node := trModule.Items.AddChildObject(node, MakeLabel(meth), meth);
        node.ImageIndex := 2;
        node.SelectedIndex := 2;
        trModule.Selected := node;
      end;
    finally
      trModule.OnChange := @trModuleChange;
    end;
    SetButtons;
  end;
end;

procedure TLyseeModuleEditor.btnAddClassClick(Sender: TObject);
var
  clss: TLyseeClass;
begin
  clss := TLyseeClass.Create(FLyseeModule.Owner);
  clss.Name := FDesigner.CreateUniqueComponentName(clss.ClassName);
  clss.Module := FLyseeModule;
  FDesigner.PropertyEditorHook.PersistentAdded(clss, true);
  FDesigner.Modified;
  trModule.OnChange := nil;
  try
    trModule.Selected := SetupClassNode(clss);
  finally
    trModule.OnChange := @trModuleChange;
  end;
end;

procedure TLyseeModuleEditor.btnRemoveClick(Sender: TObject);
var
  curr: TLyseeComponent;
  node: TTreeNode;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    curr := TLyseeComponent(node.Data);
    if curr <> FLyseeModule then
    begin
      Inc(FRemoving);
      trModule.OnChange := nil;
      try
        try
          node.DeleteChildren;
          node.Delete;
          FDesigner.PropertyEditorHook.DeletePersistent(curr);
        except
          on E: Exception do begin
            MessageDlg('Error deleting lysee component',
              'Error while deleting lysee component:' + sLineBreak +
              E.Message, mtError, [mbOk], 0);
          end;
        end;
      finally
        trModule.OnChange := @trModuleChange;
        Dec(FRemoving);
      end;
      SetButtons;
    end;
  end;
end;

procedure TLyseeModuleEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Self.Close;
end;

procedure TLyseeModuleEditor.LyseeClassEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FComponentEditor.FEditor := nil;
end;

procedure TLyseeModuleEditor.trModuleChange(Sender: TObject; Node: TTreeNode);
begin
  SelectOnly(SelectedComponent);
  SetButtons;
end;

procedure TLyseeModuleEditor.trModuleDblClick(Sender: TObject);
var
  P: TPoint;
  H: THitTests;
  curr: TLyseeComponent;
begin
  P := trModule.ScreenToClient(Mouse.CursorPos);
  H := trModule.GetHitTestInfoAt(P.x, P.y);
  if (H * [htOnItem, htOnIcon, htOnIndent, htOnLabel]) <> [] then
  begin
    curr := SelectedComponent;
    if curr.ComponentType in [lsoFunction, lsoMethod] then
      CreateComponentEvent(curr, 'OnExecute');
  end;
end;

constructor TLyseeModuleEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
    GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  end;
  FLyseeModule := nil;
  SetButtons;
end;

destructor TLyseeModuleEditor.Destroy;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TLyseeModuleEditor.SetLyseeModule(ALyseeModule: TLyseeModule);
begin
  FLyseeModule := ALyseeModule;
  SetupTree;
  SetButtons;
  trModule.Selected := FindNode(FLyseeModule);
end;

procedure TLyseeModuleEditor.SetDesigner(ADesigner: TComponentEditorDesigner);
begin
  FDesigner := ADesigner;
end;

procedure TLyseeModuleEditor.SetComponentEditor(AComponentEditor: TLyseeModuleComponentEditor);
begin
  FComponentEditor := AComponentEditor;
end;

procedure TLyseeModuleEditor.SetupTree;
var
  root, node: TTreeNode;
  func: TLyseeFunc;
  clss: TLyseeClass;
  meth: TLyseeMethod;
  X, I: integer;
begin
  trModule.OnChange := nil;
  trModule.Items.BeginUpdate;
  try
    trModule.Items.Clear;
    root := trModule.Items.AddObject(nil, MakeLabel(FLyseeModule), FLyseeModule);
    root.ImageIndex := 0;
    root.SelectedIndex := 0;

    for X := 0 to FLyseeModule.FuncCount - 1 do
    begin
      func := FLyseeModule.Funcs[X];
      node := trModule.Items.AddChildObject(root, MakeLabel(func), func);
      node.ImageIndex := 2;
      node.SelectedIndex := 2;
    end;

    for I := 0 to FLyseeModule.ClassCount - 1 do
    begin
      clss := FLyseeModule.Classes[I];
      root := SetupClassNode(clss);
      for X := 0 to clss.MethodCount - 1 do
      begin
        meth := clss.Methods[X];
        node := trModule.Items.AddChildObject(root, MakeLabel(meth), meth);
        node.ImageIndex := 2;
        node.SelectedIndex := 2;
      end;
    end;
  finally
    trModule.Items.EndUpdate;
    trModule.OnChange := @trModuleChange;
  end;
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
  if FEditor.trModule.Items.Count > 0 then
    FEditor.trModule.Selected := nil;
  FEditor.SetComponentEditor(Self);
  FEditor.SetDesigner(FDesigner);
  FEditor.SetLyseeModule(LyseeModule);
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
  {$I lse_module_editor.lrs}
  RegisterComponentEditor(TLyseeModule, TLyseeModuleComponentEditor);

end.

