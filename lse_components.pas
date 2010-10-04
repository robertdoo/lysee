{==============================================================================}
{        UNIT: lse_components                                                  }
{ DESCRIPTION: lysee components for lazarus and delphi                         }
{     CREATED: 2010/09/20                                                      }
{    MODIFIED: 2010/10/03                                                      }
{==============================================================================}
{ Copyright (c) 2010, Li Yun Jie                                               }
{ All rights reserved.                                                         }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ Redistributions of source code must retain the above copyright notice, obj   }
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
unit lse_components;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, lseu, lse_api, lse_funcs, lse_kernel,
  lse_export, lse_symbol, LResources;

type

  TLyseeObject    = class; {forward}
  TLyseeMethod    = class;
  TLyseeClass     = class;
  TLyseeFunc      = class;
  TLyseeModule    = class;

  { TLyseeComponent }

  TLyseeComponentType = (lsoNone, lsoEngine, lsoModule, lsoFunction,
                         lsoClass, lsoMethod);

  TLyseeComponent = class(TComponent)
  private
    FComponentType: TLyseeComponentType;
  public
    function IsDestroying: boolean;
    function IsDesignLoading: boolean;
    property ComponentType: TLyseeComponentType read FComponentType;
  end;

  { TLyseeEngine }

  TLyseeEngine = class(TLyseeComponent)
  private
    FEngine: TLseEngine;
    FFileName: string;
    FIsLSP: boolean;
    FCodes: TStrings;
    FOnBeginExecute: TNotifyEvent;
    FOnCloseStderr: TNotifyEvent;
    FOnCloseStdin: TNotifyEvent;
    FOnCloseStdout: TNotifyEvent;
    FOnEndExecute: TNotifyEvent;
    FOnFlushStderr: TNotifyEvent;
    FOnFlushStdout: TNotifyEvent;
    FOnRead: TLseRead;
    FOnReadln: TLseReadln;
    FOnWrite: TLseWrite;
    FOnEofStdin: TLseEof;
    procedure EventBeginExecute(Sender: TObject);
    procedure EventCloseStderr(Sender: TObject);
    procedure EventCloseStdin(Sender: TObject);
    procedure EventCloseStdout(Sender: TObject);
    procedure EventEndExecute(Sender: TObject);
    procedure EventEofStdin(Sender: TObject; var Eof: boolean);
    procedure EventFlushStderr(Sender: TObject);
    procedure EventFlushStdout(Sender: TObject);
    procedure EventRead(Sender: TObject; const Buf: pchar; var Count: integer);
    procedure EventReadln(Sender: TObject; var S: string);
    procedure EventWrite(Sender: TObject; const Buf: pchar; var Count: integer);
    function GetArguments: TStrings;
    function GetExited: boolean;
    function GetKernelEngine: KLiEngine;
    function GetReady: boolean;
    function GetRunning: boolean;
    function GetSearchPath: string;
    function GetTerminated: boolean;
    function GetTerminating: boolean;
    procedure SetArguments(const AValue: TStrings);
    procedure SetCodes(const AValue: TStrings);
    procedure SetOnBeginExecute(const AValue: TNotifyEvent);
    procedure SetOnCloseStderr(const AValue: TNotifyEvent);
    procedure SetOnCloseStdin(const AValue: TNotifyEvent);
    procedure SetOnCloseStdout(const AValue: TNotifyEvent);
    procedure SetOnEndExecute(const AValue: TNotifyEvent);
    procedure SetOnEofStdin(const AValue: TLseEof);
    procedure SetOnFlushStderr(const AValue: TNotifyEvent);
    procedure SetOnFlushStdout(const AValue: TNotifyEvent);
    procedure SetOnRead(const AValue: TLseRead);
    procedure SetOnReadln(const AValue: TLseReadln);
    procedure SetOnWrite(const AValue: TLseWrite);
    procedure SetSearchPath(const AValue: string);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function Execute: boolean;
    procedure Terminate;
    property Engine: TLseEngine read FEngine;
    property KernelEngine: KLiEngine read GetKernelEngine;
    property Ready: boolean read GetReady;
    property Running: boolean read GetRunning;
    property Terminating: boolean read GetTerminating;
    property Terminated: boolean read GetTerminated;
    property Exited: boolean read GetExited;
  published
    property Arguments: TStrings read GetArguments write SetArguments;
    property Codes: TStrings read FCodes write SetCodes;
    property FileName: string read FFileName write FFileName;
    property SearchPath: string read GetSearchPath write SetSearchPath;
    property IsLSP: boolean read FIsLSP write FIsLSP;
    property OnBeginExecute: TNotifyEvent read FOnBeginExecute write SetOnBeginExecute;
    property OnEndExecute: TNotifyEvent read FOnEndExecute write SetOnEndExecute;
    property OnReadln: TLseReadln read FOnReadln write SetOnReadln;
    property OnRead: TLseRead read FOnRead write SetOnRead;
    property OnWrite: TLseWrite read FOnWrite write SetOnWrite;
    property OnEofStdin: TLseEof read FOnEofStdin write SetOnEofStdin;
    property OnCloseStdin: TNotifyEvent read FOnCloseStdin write SetOnCloseStdin;
    property OnCloseStdout: TNotifyEvent read FOnCloseStdout write SetOnCloseStdout;
    property OnFlushStdout: TNotifyEvent read FOnFlushStdout write SetOnFlushStdout;
    property OnCloseStderr: TNotifyEvent read FOnCloseStderr write SetOnCloseStderr;
    property OnFlushStderr: TNotifyEvent read FOnFlushStderr write SetOnFlushStderr;
  end;

  { TLyseeModule }

  TLyseeModule = class(TLyseeComponent)
  private
    FKernelModule: KLiModule;
    FFuncList: TList;
    FClassList: TList;
    FPrevModule: TLyseeModule;
    FNextModule: TLyseeModule;
    function GetClass(Index: integer): TLyseeClass;
    function GetClassCount: integer;
    function GetFunc(Index: integer): TLyseeFunc;
    function GetFuncCount: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure SetupModule;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    function AddFunc(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
    function Exists(ALyseeComponent: TLyseeComponent): boolean;
    function FuncExists(AFunc: TLyseeFunc): boolean;
    function ClassExists(AClass: TLyseeClass): boolean;
    function MethodExists(AMethod: TLyseeMethod): boolean;
    property KernelModule: KLiModule read FKernelModule;
    property ClassCount: integer read GetClassCount;
    property Classes[Index: integer]: TLyseeClass read GetClass;
    property FuncCount: integer read GetFuncCount;
    property Funcs[Index: integer]: TLyseeFunc read GetFunc;
  end;

  { TLyseeFunc }

  TEventFunc = procedure(Invoker: KLiInvoke) of object;

  TLyseeFunc = class(TLyseeComponent)
  private
    FKernelFunc: KLiFunc;
    FModule: TLyseeModule;
    FPrototype: string;
    FOnExecute: TEventFunc;
    procedure SetModule(const AValue: TLyseeModule);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    procedure Execute(Param: PLseParam);
    property KernelFunc: KLiFunc read FKernelFunc;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property Module: TLyseeModule read FModule write SetModule;
    property OnExecute: TEventFunc read FOnExecute write FOnExecute;
  end;

  { TLyseeClass }

  TCreateObject = procedure(Sender: TObject; var Lobj: TLyseeObject) of object;
  TDestroyObject = procedure(Sender: TObject; Lobj: TLyseeObject) of object;

  TLyseeClass = class(TLyseeComponent)
  private
    FKernelClass: KLiClass;
    FModule: TLyseeModule;
    FMethodList: TList;
    FOnCreateObject: TCreateObject;
    function GetMethod(Index: integer): TLyseeMethod;
    function GetMethodCount: integer;
    procedure SetModule(const AValue: TLyseeModule);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
    procedure SetupClass;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    function AddMethod(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
    function FindMethod(Method: KLiFunc): TLyseeMethod;
    function Exists(Method: TLyseeMethod): boolean;
    property KernelClass: KLiClass read FKernelClass;
    property MethodCount: integer read GetMethodCount;
    property Methods[Index: integer]: TLyseeMethod read GetMethod;
  published
    property Module: TLyseeModule read FModule write SetModule;
    property OnCreateObject: TCreateObject read FOnCreateObject write FOnCreateObject;
  end;

  { TLyseeMethod }

  TEventMethod = procedure(Lobj: TLyseeObject; Invoker: KLiInvoke) of object;

  TLyseeMethod = class(TLyseeComponent)
  private
    FKernelMethod: KLiFunc;
    FClass: TLyseeClass;
    FPrototype: string;
    FOnExecute: TEventMethod;
    procedure SetClass(const AValue: TLyseeClass);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    procedure Execute(Param: PLseParam);
    function KernelClass: KLiClass;
    property KernelMethod: KLiFunc read FKernelMethod;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property LyseeClass: TLyseeClass read FClass write SetClass;
    property OnExecute: TEventMethod read FOnExecute write FOnExecute;
  end;

  { TLyseeObject }

  TLyseeObject = class(TLseObject)
  private
    FClass: TLyseeClass;
  public
    constructor Create(AClass: TLyseeClass);
    property LyseeClass: TLyseeClass read FClass;
  end;

var
  HeadModule: TLyseeModule = nil;

procedure Register;
procedure SetupLyseeModules;
procedure StartLysee;overload;
procedure StartLysee(const ExeName: string);overload;
procedure StartLysee(const ExeName, Kernel: string);overload;
procedure CloseLysee;

implementation

procedure Register;
begin
  RegisterComponents('Lysee', [TLyseeEngine, TLyseeModule]);
  RegisterNoIcon([TLyseeFunc, TLyseeClass, TLyseeMethod]);
end;

procedure SetupLyseeModules;
var
  module: TLyseeModule;
begin
  module := HeadModule;
  while module <> nil do
  begin
    module.Setup;
    module := module.FNextModule;
  end;
end;

procedure StartLysee;
begin
  StartLysee(ParamStr(0));
end;

procedure StartLysee(const ExeName: string);
begin
  StartLysee(ExeName, ExeName);
end;

procedure StartLysee(const ExeName, Kernel: string);
var
  X: integer;
begin
  if lse_startup then
  begin
    if ExeName <> '' then
      lse_set_program_file(ExeName) else
      lse_set_program_file(ParamStr(0));

    if Kernel <> '' then
      lse_set_kernel_file(Kernel) else
    if ExeName <> '' then
      lse_set_kernel_file(ExeName) else
      lse_set_kernel_file(ParamStr(0));

    SetupLyseeModules;
  end;
end;

procedure CloseLysee;
begin
  lse_cleanup;
end;

procedure RemoveFromList(List: TList; Data: pointer);
begin
  if List <> nil then
    List.Remove(Data);
end;

procedure ExecModuleFunc(const Param: PLseParam);cdecl;
var
  func: TLyseeFunc;
begin
  func := TLyseeFunc(KLiFunc(Param^.func).Component);
  if func <> nil then
    func.Execute(Param);
end;

procedure ExecClassMethod(const Param: PLseParam);cdecl;
var
  curr: KLiFunc;
begin
  curr := KLiFunc(Param^.func);
  TLyseeMethod(curr.Component).Execute(Param);
end;

procedure ExecClassConstructor(const Param: PLseParam);cdecl;
var
  curr: KLiFunc;
  func: TLyseeMethod;
  clss: TLyseeClass;
  nobj: TLyseeObject;
begin
  curr := KLiFunc(Param^.func);
  func := TLyseeMethod(curr.Component);
  nobj := nil;
  clss := func.FClass;
  if Assigned(clss.FOnCreateObject) then
    clss.FOnCreateObject(clss, nobj);
  if nobj = nil then
    nobj := TLyseeObject.Create(clss);
  nobj.IncRefcount;
  try
    __SetObject(Param^.param[0], clss.KernelClass, nobj);
    func.Execute(Param);
  finally
    __SetObject(Param^.result, clss.KernelClass, nobj);
    nobj.DecRefcount;
  end;
end;

{ TLyseeComponent }

function TLyseeComponent.IsDestroying: boolean;
begin
  Result := (csDestroying in ComponentState);
end;

function TLyseeComponent.IsDesignLoading: boolean;
begin
  Result := (csDesigning in ComponentState) or
            (csLoading in ComponentState);
end;

{ TLyseeEngine }

function TLyseeEngine.GetArguments: TStrings;
begin
  Result := GetKernelEngine.Arguments;
end;

function TLyseeEngine.GetExited: boolean;
begin
  Result := FEngine.Exited;
end;

function TLyseeEngine.GetKernelEngine: KLiEngine;
begin
  Result := KLiEngine(FEngine.EngineRec^.kernel_engine);
end;

function TLyseeEngine.GetReady: boolean;
begin
  Result := FEngine.Ready;
end;

function TLyseeEngine.GetRunning: boolean;
begin
  Result := FEngine.Running;
end;

function TLyseeEngine.GetSearchPath: string;
begin
  Result := GetKernelEngine.MainSearchPath;
end;

function TLyseeEngine.GetTerminated: boolean;
begin
  Result := FEngine.Terminated;
end;

function TLyseeEngine.GetTerminating: boolean;
begin
  Result := FEngine.Terminating;
end;

procedure TLyseeEngine.EventBeginExecute(Sender: TObject);
begin
  FOnBeginExecute(Self);
end;

procedure TLyseeEngine.EventCloseStderr(Sender: TObject);
begin
  FOnCloseStderr(Self);
end;

procedure TLyseeEngine.EventCloseStdin(Sender: TObject);
begin
  FOnCloseStdin(Self);
end;

procedure TLyseeEngine.EventCloseStdout(Sender: TObject);
begin
  FOnCloseStdout(Self);
end;

procedure TLyseeEngine.EventEndExecute(Sender: TObject);
begin
  FOnEndExecute(Self);
end;

procedure TLyseeEngine.EventEofStdin(Sender: TObject; var Eof: boolean);
begin
  FOnEofStdin(Self, Eof);
end;

procedure TLyseeEngine.EventFlushStderr(Sender: TObject);
begin
  FOnFlushStderr(Self);
end;

procedure TLyseeEngine.EventFlushStdout(Sender: TObject);
begin
  FOnFlushStdout(Self);
end;

procedure TLyseeEngine.EventRead(Sender: TObject; const Buf: pchar; var Count: integer);
begin
  FOnRead(Self, Buf, Count);
end;

procedure TLyseeEngine.EventReadln(Sender: TObject; var S: string);
begin
  FOnReadln(Self, S);
end;

procedure TLyseeEngine.EventWrite(Sender: TObject; const Buf: pchar;
  var Count: integer);
begin
  FOnWrite(Self, Buf, Count);
end;

procedure TLyseeEngine.SetArguments(const AValue: TStrings);
begin
  if AValue <> nil then
    GetKernelEngine.Arguments.Assign(AValue) else
    GetKernelEngine.Arguments.Clear;
end;

procedure TLyseeEngine.SetCodes(const AValue: TStrings);
begin
  if AValue <> nil then
    FCodes.Assign(AValue) else
    FCodes.Clear;
end;

procedure TLyseeEngine.SetOnBeginExecute(const AValue: TNotifyEvent);
begin
  FOnBeginExecute := AValue;
  if Assigned(FOnBeginExecute) then
    FEngine.OnBeginExecute := @EventBeginExecute else
    FEngine.OnBeginExecute := nil;
end;

procedure TLyseeEngine.SetOnCloseStderr(const AValue: TNotifyEvent);
begin
  FOnCloseStderr := AValue;
  if Assigned(FOnCloseStderr) then
    FEngine.OnCloseStderr := @EventCloseStderr else
    FEngine.OnCloseStderr := nil;
end;

procedure TLyseeEngine.SetOnCloseStdin(const AValue: TNotifyEvent);
begin
  FOnCloseStdin := AValue;
  if Assigned(FOnCloseStdin) then
    FEngine.OnCloseStdin := @EventCloseStdin else
    FEngine.OnCloseStdin := nil;
end;

procedure TLyseeEngine.SetOnCloseStdout(const AValue: TNotifyEvent);
begin
  FOnCloseStdout := AValue;
  if Assigned(FOnCloseStdout) then
    FEngine.OnCloseStdout := @EventCloseStdout else
    FEngine.OnCloseStdout := nil;
end;

procedure TLyseeEngine.SetOnEndExecute(const AValue: TNotifyEvent);
begin
  FOnEndExecute := AValue;
  if Assigned(FOnEndExecute) then
    FEngine.OnEndExecute := @EventEndExecute else
    FEngine.OnEndExecute := nil;
end;

procedure TLyseeEngine.SetOnEofStdin(const AValue: TLseEof);
begin
  FOnEofStdin := AValue;
  if Assigned(FOnEofStdin) then
    FEngine.OnEofStdin := @EventEofStdin else
    FEngine.OnEofStdin := nil;
end;

procedure TLyseeEngine.SetOnFlushStderr(const AValue: TNotifyEvent);
begin
  FOnFlushStderr := AValue;
  if Assigned(FOnFlushStderr) then
    FEngine.OnFlushStderr := @EventFlushStderr else
    FEngine.OnFlushStderr := nil;
end;

procedure TLyseeEngine.SetOnFlushStdout(const AValue: TNotifyEvent);
begin
  FOnFlushStdout := AValue;
  if Assigned(FOnFlushStdout) then
    FEngine.OnFlushStdout := @EventFlushStdout else
    FEngine.OnFlushStdout := nil;
end;

procedure TLyseeEngine.SetOnRead(const AValue: TLseRead);
begin
  FOnRead := AValue;
  if Assigned(FOnRead) then
    FEngine.OnRead := @EventRead else
    FEngine.OnRead := nil;
end;

procedure TLyseeEngine.SetOnReadln(const AValue: TLseReadln);
begin
  FOnReadln := AValue;
  if Assigned(FOnReadln) then
    FEngine.OnReadln := @EventReadln else
    FEngine.OnReadln := nil;
end;

procedure TLyseeEngine.SetOnWrite(const AValue: TLseWrite);
begin
  FOnWrite := AValue;
  if Assigned(FOnWrite) then
    FEngine.OnWrite := @EventWrite else
    FEngine.OnWrite := nil;
end;

procedure TLyseeEngine.SetSearchPath(const AValue: string);
begin
  GetKernelEngine.MainSearchPath := Trim(AValue);
end;

constructor TLyseeEngine.Create(AOwner: TComponent);
begin
  inherited;
  FComponentType := lsoEngine;
  lse_startup;
  FEngine := TLseEngine.Create;
  FCodes := TStringList.Create;
end;

destructor TLyseeEngine.Destroy;
begin
  FreeAndNil(FCodes);
  FreeAndNil(FEngine);
  inherited Destroy;
end;

function TLyseeEngine.Execute: boolean;
begin
  if FFileName = '' then
    Result := FEngine.ExecuteCode(FCodes.Text, FIsLSP) else
    Result := FEngine.ExecuteFile(FFileName, FIsLSP);
end;

procedure TLyseeEngine.Terminate;
begin
  FEngine.Terminate;
end;

{ TLyseeModule }

function TLyseeModule.AddFunc(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
var
  R: RLseFuncRec;
begin
  if FKernelModule <> nil then
  begin
    R.fr_prot := pchar(Prototype);
    R.fr_addr := Proc;
    R.fr_desc := nil;
    Result := FKernelModule.ModuleClass.SetupMethod(@R);
  end
  else Result := nil;
end;

function TLyseeModule.Exists(ALyseeComponent: TLyseeComponent): boolean;
begin
  Result := false;
  if ALyseeComponent <> nil then
    if ALyseeComponent is TLyseeFunc then
      Result := FuncExists(ALyseeComponent as TLyseeFunc) else
    if ALyseeComponent is TLyseeClass then
      Result := ClassExists(ALyseeComponent as TLyseeClass) else
    if ALyseeComponent is TLyseeMethod then
      Result := MethodExists(ALyseeComponent as TLyseeMethod);
end;

function TLyseeModule.FuncExists(AFunc: TLyseeFunc): boolean;
begin
  Result := FFuncList.IndexOf(AFunc) >= 0;
end;

function TLyseeModule.ClassExists(AClass: TLyseeClass): boolean;
begin
  Result := FClassList.IndexOf(AClass) >= 0;
end;

function TLyseeModule.MethodExists(AMethod: TLyseeMethod): boolean;
var
  index: integer;
begin
  for index := 0 to GetClassCount - 1 do
    if GetClass(index).Exists(AMethod) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

procedure TLyseeModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent is TLyseeComponent then
    begin
      RemoveFromList(FFuncList, AComponent);
      RemoveFromList(FClassList, AComponent);
    end;
end;

function TLyseeModule.GetClassCount: integer;
begin
  if FClassList <> nil then
    Result := FClassList.Count else
    Result := 0;
end;

function TLyseeModule.GetFunc(Index: integer): TLyseeFunc;
begin
  Result := TLyseeFunc(FFuncList[Index]);
end;

function TLyseeModule.GetFuncCount: integer;
begin
  if FFuncList <> nil then
    Result := FFuncList.Count else
    Result := 0;
end;

function TLyseeModule.GetClass(Index: integer): TLyseeClass;
begin
  Result := TLyseeClass(FClassList[Index]);
end;

constructor TLyseeModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoModule;
  FFuncList := TList.Create;
  FClassList := TList.Create;
  FPrevModule := nil;
  FNextModule := HeadModule;
  if HeadModule <> nil then
    HeadModule.FPrevModule := Self;
  HeadModule := Self;
end;

destructor TLyseeModule.Destroy;
begin
  if FNextModule <> nil then
    FNextModule.FPrevModule := FPrevModule;
  if FPrevModule <> nil then
    FPrevModule.FNextModule := FNextModule else
    HeadModule := FNextModule;
  FreeAndNil(FFuncList);
  FreeAndNil(FClassList);
  inherited Destroy;
end;

procedure TLyseeModule.SetupModule;
var
  M: RLseModuleRec;
begin
  if not IsDesignLoading then
    if (FKernelModule = nil) and (Name <> '') then
    begin
      FillChar(M, sizeof(RLseModuleRec), 0);
      M.iw_version := LSE_VERSION;
      M.iw_desc := nil;
      FKernelModule := __SetupModule(Name, @M);
    end;
end;

procedure TLyseeModule.Setup;
var
  X: integer;
begin
  SetupModule;
  if FKernelModule <> nil then
  begin
    for X := 0 to GetClassCount - 1 do
      GetClass(X).SetupClass;

    for X := 0 to GetClassCount - 1 do
      GetClass(X).Setup;

    for X := 0 to GetFuncCount - 1 do
      GetFunc(X).Setup;
  end;
end;

{ TLyseeFunc }

procedure TLyseeFunc.SetModule(const AValue: TLyseeModule);
begin
  if (FKernelFunc = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FFuncList.Add(Self);
    end;
  end;
end;

procedure TLyseeFunc.SetPrototype(const AValue: string);
begin
  if FKernelFunc = nil then
    FPrototype := Trim(AValue);
end;

procedure TLyseeFunc.Leave;
begin
  if FModule <> nil then
  begin
    FModule.Notification(Self, opRemove);
    FModule := nil;
  end;
end;

procedure TLyseeFunc.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FModule then
      Leave;
end;

constructor TLyseeFunc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoFunction;
end;

destructor TLyseeFunc.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLyseeFunc.Setup;
begin
  if not IsDesignLoading then
    if (FKernelFunc = nil) and (FPrototype <> '') and (FModule <> nil) then
    begin
      FKernelFunc := FModule.AddFunc(FPrototype, @ExecModuleFunc);
      if FKernelFunc <> nil then
        FKernelFunc.Component := Self;
    end;
end;

procedure TLyseeFunc.Execute(Param: PLseParam);
var
  ivk: KLiInvoke;
  prm: PLseParam;
begin
  if Assigned(FOnExecute) then
  begin
    ivk := __AsEngine(Param).Invoker;
    prm := ivk.Param;
    try
      ivk.Param := Param;
      FOnExecute(ivk);
    finally
      ivk.Param := prm;
    end;
  end;
end;

{ TLyseeClass }

function TLyseeClass.AddMethod(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
var
  R: RLseFuncRec;
begin
  if FKernelClass <> nil then
  begin
    R.fr_prot := pchar(Prototype);
    R.fr_addr := Proc;
    R.fr_desc := nil;
    Result := FKernelClass.SetupMethod(@R);
  end
  else Result := nil;
end;

function TLyseeClass.GetMethodCount: integer;
begin
  if FMethodList <> nil then
    Result := FMethodList.Count else
    Result := 0;
end;

function TLyseeClass.GetMethod(Index: integer): TLyseeMethod;
begin
  Result := TLyseeMethod(FMethodList[Index]);
end;

procedure TLyseeClass.SetModule(const AValue: TLyseeModule);
begin
  if (FKernelClass = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FClassList.Add(Self);
    end;
  end;
end;

procedure TLyseeClass.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FModule then Leave else
    if AComponent is TLyseeModule then
      RemoveFromList(FMethodList, AComponent);
end;

procedure TLyseeClass.Leave;
begin
  if FModule <> nil then
  begin
    FModule.Notification(Self, opRemove);
    FModule := nil;
  end;
end;

constructor TLyseeClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoClass;
  FMethodList := TList.Create;
end;

destructor TLyseeClass.Destroy;
begin
  Leave;
  FreeAndNil(FMethodList);
  inherited Destroy;
end;

procedure TLyseeClass.SetupClass;
var
  R: RLseClassRec;
begin
  if not IsDesignLoading then
    if (FKernelClass = nil) and (Name <> '') then
      if (FModule <> nil) and (FModule.FKernelModule <> nil) then
      begin
        FillChar(R, sizeof(RLseClassRec), 0);
        R.vtype := LSV_OBJECT;
        R.name := pchar(Name);
        R.desc := nil;
        R.incRefcount := @lse_incRefcount;
        R.decRefcount := @lse_decRefcount;
        __SetupClasses(@R, 1, FModule.FKernelModule);
        FKernelClass := KLiClass(R.lysee_class);
      end;
end;

procedure TLyseeClass.Setup;
var
  X: integer;
begin
  SetupClass;
  if FKernelClass <> nil then
    for X := 0 to GetMethodCount - 1 do
      GetMethod(X).Setup;
end;

function TLyseeClass.FindMethod(Method: KLiFunc): TLyseeMethod;
var
  X: integer;
begin
  for X := 0 to GetMethodCount - 1 do
  begin
    Result := GetMethod(X);
    if Result.KernelMethod = Method then Exit;
  end;
  Result := nil;
end;

function TLyseeClass.Exists(Method: TLyseeMethod): boolean;
begin
  Result := FMethodList.IndexOf(Method) >= 0;
end;

{ TLyseeMethod }

procedure TLyseeMethod.SetClass(const AValue: TLyseeClass);
begin
  if (FKernelMethod = nil) and (FClass <> AValue) then
  begin
    Leave;
    FClass := AValue;
    if FClass <> nil then
    begin
      FreeNotification(FClass);
      FClass.FMethodList.Add(Self);
    end;
  end;
end;

procedure TLyseeMethod.SetPrototype(const AValue: string);
begin
  if FKernelMethod = nil then
    FPrototype := Trim(AValue);
end;

procedure TLyseeMethod.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FClass then
      Leave;
end;

procedure TLyseeMethod.Leave;
begin
  if FClass <> nil then
  begin
    FClass.Notification(Self, opRemove);
    FClass := nil;
  end;
end;

constructor TLyseeMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentType := lsoMethod;
end;

destructor TLyseeMethod.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLyseeMethod.Setup;
begin
  if not IsDesignLoading then
    if (FKernelMethod = nil) and (FPrototype <> '') and (FClass <> nil) then
    begin
      FKernelMethod := FClass.AddMethod(FPrototype, @ExecClassMethod);
      if FKernelMethod <> nil then
      begin
        FKernelMethod.Component := Self;
        if FKernelMethod.IsConstructor then
          FKernelMethod.Proc := @ExecClassConstructor;
      end;
    end;
end;

procedure TLyseeMethod.Execute(Param: PLseParam);
var
  ivk: KLiInvoke;
  prm: PLseParam;
begin
  if Assigned(FOnExecute) then
  begin
    ivk := __AsEngine(Param).Invoker;
    prm := ivk.Param;
    try
      ivk.Param := Param;
      FOnExecute(TLyseeObject(Param^.param[0]^.VObject), ivk);
    finally
      ivk.Param := prm;
    end;
  end;
end;

function TLyseeMethod.KernelClass: KLiClass;
begin
  if FKernelMethod <> nil then
    Result := FKernelMethod.OwnerClass else
  if FClass <> nil then
    Result := FClass.KernelClass else
    Result := nil;
end;

{ TLyseeObject }

constructor TLyseeObject.Create(AClass: TLyseeClass);
begin
  inherited Create;
  FClass := AClass;
end;

initialization
  {$IFDEF FPC}
  {$I lse_components.lrs}
  {$ENDIF}

end.
