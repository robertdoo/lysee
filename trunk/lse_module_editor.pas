{==============================================================================}
{        UNIT: lse_module_editor                                               }
{ DESCRIPTION: lysee module editor for lazarus                                 }
{     CREATED: 2010/09/23                                                      }
{    MODIFIED: 2010/10/17                                                      }
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
  LCLType, Graphics, ComCtrls, ButtonPanel, lseu;

type
  TLseModuleComponentEditor = class;
  
  { TLseModuleEditor }

  TLseModuleEditor = class(TForm)
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
    FModule: TLseModule;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TLseModuleComponentEditor;
    FRemoving: integer;
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure SelectOnly(AComponent: TComponent);
    procedure SetButtons;
    function MakeLabel(AComponent: TComponent): string;
    function SelectedNode: TTreeNode;
    function SelectedComponent: TLseComponent;
    function FindNode(APersistent: TPersistent): TTreeNode;
    function SetupClassNode(AClass: TLseClass): TTreeNode;
    procedure SetLyseeModule(ALyseeModule: TLseModule);
    procedure SetDesigner(ADesigner: TComponentEditorDesigner);
    procedure SetComponentEditor(AComponentEditor: TLseModuleComponentEditor);
    procedure SetupTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TLseModuleComponentEditor }

  TLseModuleComponentEditor = class(TComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
    FEditor: TLseModuleEditor;
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

{ TLseModuleEditor }

procedure TLseModuleEditor.OnComponentRenamed(AComponent: TComponent);
var
  node: TTreeNode;
begin
  if AComponent is TLseComponent then
  begin
    node := FindNode(AComponent);
    if node <> nil then
      node.Text := MakeLabel(AComponent);
  end;
end;

procedure TLseModuleEditor.OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
begin
  if (OnSetSelection <> nil) and (OnSetSelection.Count > 0) then
  begin
    trModule.Selected := FindNode(OnSetSelection.Items[0]);
    SetButtons;
  end;
end;

procedure TLseModuleEditor.OnPersistentDeleting(APersistent: TPersistent);
var
  node: TTreeNode;
begin
  if (FRemoving = 0) and (APersistent is TLseComponent) then
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

procedure TLseModuleEditor.SetButtons;
var
  curr: TLseComponent;
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
      barStatus.Panels[0].Text := LowerCase(Copy(curr.ClassName, 5, 100));
    case curr.ComponentType of
      lsoModule  : barStatus.Panels[1].Text := TLseModule(curr).Name;
      lsoFunction: barStatus.Panels[1].Text := TLseFunc(curr).Prototype;
      lsoClass   : barStatus.Panels[1].Text := TLseClass(curr).Name;
      lsoMethod  : barStatus.Panels[1].Text := TLseMethod(curr).Prototype;
      else         barStatus.Panels[1].Text := '';
    end;
    if curr.ComponentType in [lsoModule, lsoFunction] then
      btnAddProc.Caption := '&Function' else
      btnAddProc.Caption := '&Method';
  end;
  btnAddClass.Enabled := true;
  btnAddProc.Enabled := (curr <> nil);
  btnRemove.Enabled := (curr <> nil) and not (curr is TLseModule);
end;

function TLseModuleEditor.MakeLabel(AComponent: TComponent): string;
begin
  Result := AComponent.Name;
end;

function TLseModuleEditor.SelectedNode: TTreeNode;
begin
  Result := trModule.Selected;
end;

function TLseModuleEditor.SelectedComponent: TLseComponent;
var
  node: TTreeNode;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    Result := TLseComponent(node.Data);
    if (Result = FModule) or FModule.Exists(Result) then Exit;
    SetupTree;
  end;
  Result := nil;
end;

function TLseModuleEditor.FindNode(APersistent: TPersistent): TTreeNode;
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

function TLseModuleEditor.SetupClassNode(AClass: TLseClass): TTreeNode;
begin
  Result := trModule.Items.AddObject(nil, MakeLabel(AClass), AClass);
  Result.ImageIndex := 1;
  Result.SelectedIndex := 1;
end;

procedure TLseModuleEditor.SelectOnly(AComponent: TComponent);
begin
  if AComponent <> nil then
    FDesigner.SelectOnlyThisComponent(AComponent);
end;

procedure TLseModuleEditor.btnAddProcClick(Sender: TObject);
var
  curr: TLseComponent;
  node: TTreeNode;
  func: TLseFunc;
  clss: TLseClass;
  meth: TLseMethod;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    if node.Level > 0 then node := node.Parent;
    curr := TLseComponent(node.Data);
    trModule.OnChange := nil;
    try
      if curr = FModule then
      begin
        func := TLseFunc.Create(FModule.Owner);
        func.Name := FDesigner.CreateUniqueComponentName(func.ClassName);
        func.Module := FModule;
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
        clss := TLseClass(curr);
        meth := TLseMethod.Create(FModule.Owner);
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

procedure TLseModuleEditor.btnAddClassClick(Sender: TObject);
var
  clss: TLseClass;
begin
  clss := TLseClass.Create(FModule.Owner);
  clss.Name := FDesigner.CreateUniqueComponentName(clss.ClassName);
  clss.Module := FModule;
  FDesigner.PropertyEditorHook.PersistentAdded(clss, true);
  FDesigner.Modified;
  trModule.OnChange := nil;
  try
    trModule.Selected := SetupClassNode(clss);
  finally
    trModule.OnChange := @trModuleChange;
  end;
end;

procedure TLseModuleEditor.btnRemoveClick(Sender: TObject);
var
  curr: TLseComponent;
  node: TTreeNode;
begin
  node := SelectedNode;
  if node <> nil then
  begin
    curr := TLseComponent(node.Data);
    if curr <> FModule then
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

procedure TLseModuleEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Self.Close;
end;

procedure TLseModuleEditor.LyseeClassEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FComponentEditor.FEditor := nil;
end;

procedure TLseModuleEditor.trModuleChange(Sender: TObject; Node: TTreeNode);
begin
  SelectOnly(SelectedComponent);
  SetButtons;
end;

procedure TLseModuleEditor.trModuleDblClick(Sender: TObject);
var
  P: TPoint;
  H: THitTests;
  curr: TLseComponent;
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

constructor TLseModuleEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
    GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  end;
  FModule := nil;
  SetButtons;
end;

destructor TLseModuleEditor.Destroy;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TLseModuleEditor.SetLyseeModule(ALyseeModule: TLseModule);
begin
  FModule := ALyseeModule;
  SetupTree;
  SetButtons;
  trModule.Selected := FindNode(FModule);
end;

procedure TLseModuleEditor.SetDesigner(ADesigner: TComponentEditorDesigner);
begin
  FDesigner := ADesigner;
end;

procedure TLseModuleEditor.SetComponentEditor(AComponentEditor: TLseModuleComponentEditor);
begin
  FComponentEditor := AComponentEditor;
end;

procedure TLseModuleEditor.SetupTree;
var
  root, node: TTreeNode;
  func: TLseFunc;
  clss: TLseClass;
  meth: TLseMethod;
  X, I: integer;
begin
  trModule.OnChange := nil;
  trModule.Items.BeginUpdate;
  try
    trModule.Items.Clear;
    root := trModule.Items.AddObject(nil, MakeLabel(FModule), FModule);
    root.ImageIndex := 0;
    root.SelectedIndex := 0;

    for X := 0 to FModule.FuncCount - 1 do
    begin
      func := FModule.Funcs[X];
      node := trModule.Items.AddChildObject(root, MakeLabel(func), func);
      node.ImageIndex := 2;
      node.SelectedIndex := 2;
    end;

    for I := 0 to FModule.ClassCount - 1 do
    begin
      clss := FModule.Classes[I];
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

{ TLseModuleComponentEditor }

constructor TLseModuleComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
  FEditor := nil;
end;

destructor TLseModuleComponentEditor.Destroy;
begin
  FreeThenNil(FEditor);
  inherited Destroy;
end;

procedure TLseModuleComponentEditor.Edit;
var
  Module: TLseModule;
begin
  Module := GetComponent as TLseModule;
  if Module = nil then
    raise Exception.Create('TLseModuleComponentEditor.Edit Module = nil');
  if FEditor = nil then
    FEditor := TLseModuleEditor.Create(Application) else
  if FEditor.trModule.Items.Count > 0 then
    FEditor.trModule.Selected := nil;
  FEditor.SetComponentEditor(Self);
  FEditor.SetDesigner(FDesigner);
  FEditor.SetLyseeModule(Module);
  FEditor.ShowOnTop;
end;

function TLseModuleComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TLseModuleComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit module ...';
end;

procedure TLseModuleComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;


initialization
  {$I lse_module_editor.lrs}
  RegisterComponentEditor(TLseModule, TLseModuleComponentEditor);

end.

