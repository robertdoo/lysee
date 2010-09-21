unit lysee_laz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu, lse_api, lse_funcs, lse_kernel,
  lse_export, lse_symbol, LResources;

type

  TLyseeObject = class; {forward}
  TLyseeMethod = class;
  TLyseeClass  = class;
  TLyseeFunc   = class;
  TLyseeModule = class;

  { TLyseeEngine }

  TLyseeEngine = class(TComponent)
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

  TLyseeModule = class(TComponent)
  private
    FDescription: string;
    FModuleName: string;
    FModule: KLiModule;
    FFuncs: TList;
    FClasses: TList;
    function GetClass(Index: integer): TLyseeClass;
    function GetClassCount: integer;
    function GetFunc(Index: integer): TLyseeFunc;
    function GetFuncCount: integer;
    procedure SetDescription(const AValue: string);
    procedure SetModuleName(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure SetupModule;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    function AddFunc(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
    property KernelModule: KLiModule read FModule;
    property ClassCount: integer read GetClassCount;
    property Classes[Index: integer]: TLyseeClass read GetClass;
    property FuncCount: integer read GetFuncCount;
    property Funces[Index: integer]: TLyseeFunc read GetFunc;
  published
    property ModuleName: string read FModuleName write SetModuleName;
    property Description: string read FDescription write SetDescription;
  end;

  { TLyseeFunc }

  TEventExecute = procedure(Sender: TObject; Param: PLseParam) of object;
  TEventInvoke = procedure(Sender: TObject; Invoker: TLseInvoke) of object;

  TLyseeFunc = class(TComponent)
  private
    FDescription: string;
    FFunc: KLiFunc;
    FModule: TLyseeModule;
    FOnExecute: TEventExecute;
    FOnInvoke: TEventInvoke;
    FPrototype: string;
    function GetKernelClass: KLiClass;
    procedure SetDescription(const AValue: string);
    procedure SetModule(const AValue: TLyseeModule);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Loaded;override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    procedure Execute(Param: PLseParam);
    property KernelClass: KLiClass read GetKernelClass;
    property KernelFunc: KLiFunc read FFunc;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property Description: string read FDescription write SetDescription;
    property Module: TLyseeModule read FModule write SetModule;
    property OnInvoke: TEventInvoke read FOnInvoke write FOnInvoke;
    property OnExecute: TEventExecute read FOnExecute write FOnExecute;
  end;

  { TLyseeClass }

  TCreateObject = procedure(Sender: TObject; var Obj: TLyseeObject) of object;
  TDestroyObject = procedure(Sender: TObject; Obj: TLyseeObject) of object;

  TLyseeClass = class(TComponent)
  private
    FClass: KLiClass;
    FClassName: string;
    FDescription: string;
    FMethods: TList;
    FModule: TLyseeModule;
    FOnCreateObject: TCreateObject;
    FOnDestroyObject: TDestroyObject;
    function GetMethod(Index: integer): TLyseeMethod;
    function GetMethodCount: integer;
    procedure SetClassName(const AValue: string);
    procedure SetDescription(const AValue: string);
    procedure SetModule(const AValue: TLyseeModule);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Loaded;override;
    procedure Leave;
    procedure SetupClass;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    function AddMethod(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
    function FindMethod(Method: KLiFunc): TLyseeMethod;
    property KernelClass: KLiClass read FClass;
    property MethodCount: integer read GetMethodCount;
    property Methods[Index: integer]: TLyseeMethod read GetMethod;
  published
    property ClassName: string read FClassName write SetClassName;
    property Description: string read FDescription write SetDescription;
    property Module: TLyseeModule read FModule write SetModule;
    property OnCreateObject: TCreateObject read FOnCreateObject write FOnCreateObject;
    property OnDestroyObject: TDestroyObject read FOnDestroyObject write FOnDestroyObject;
  end;

  { TLyseeMethod }

  TLyseeMethod = class(TComponent)
  private
    FDescription: string;
    FMethod: KLiFunc;
    FClass: TLyseeClass;
    FOnExecute: TEventExecute;
    FOnInvoke: TEventInvoke;
    FPrototype: string;
    function GetKernelClass: KLiClass;
    procedure SetClass(const AValue: TLyseeClass);
    procedure SetDescription(const AValue: string);
    procedure SetPrototype(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Loaded;override;
    procedure Leave;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Setup;
    procedure Execute(Param: PLseParam);
    property KernelClass: KLiClass read GetKernelClass;
    property KernelMethod: KLiFunc read FMethod;
  published
    property Prototype: string read FPrototype write SetPrototype;
    property Description: string read FDescription write SetDescription;
    property LyseeClass: TLyseeClass read FClass write SetClass;
    property OnInvoke: TEventInvoke read FOnInvoke write FOnInvoke;
    property OnExecute: TEventExecute read FOnExecute write FOnExecute;
  end;

  { TLyseeObject }

  TLyseeObject = class(TLseObject)
  private
    FClass: TLyseeClass;
    FData: pointer;
    procedure SetData(const AData: pointer);
  protected
    procedure DataChanging(const NewData: pointer; var AllowChange: boolean);virtual;
    procedure DataChanged;virtual;
  public
    constructor Create(AClass: TLyseeClass);
    destructor Destroy;override;
    property LyseeClass: TLyseeClass read FClass;
    property Data: pointer read FData write SetData;
  end;

procedure Register;

procedure InitLysee(const Modules: array of TLyseeModule);overload;
procedure InitLysee(const ExeName: string;
                    const Modules: array of TLyseeModule);overload;
procedure InitLysee(const ExeName, Kernel: string;
                    const Modules: array of TLyseeModule);overload;
procedure ExitLysee;

implementation

procedure Register;
begin
  RegisterComponents('Lysee', [TLyseeEngine, TLyseeModule,
    TLyseeFunc, TLyseeClass, TLyseeMethod]);
end;

procedure InitLysee(const Modules: array of TLyseeModule);
begin
  InitLysee(ParamStr(0), Modules);
end;

procedure InitLysee(const ExeName: string;
                    const Modules: array of TLyseeModule);
begin
  InitLysee(ExeName, ExeName, Modules);
end;

procedure InitLysee(const ExeName, Kernel: string;
                    const Modules: array of TLyseeModule);
var
  X: integer;
begin
  if ExeName <> '' then
    lse_set_program_file(ExeName) else
    lse_set_program_file(ParamStr(0));

  if Kernel <> '' then
    lse_set_kernel_file(Kernel) else
  if ExeName <> '' then
    lse_set_kernel_file(ExeName) else
    lse_set_kernel_file(ParamStr(0));

  for X := 0 to Length(Modules) - 1 do
    Modules[X].Setup;
end;

procedure ExitLysee;
begin
  lse_cleanup;
end;

procedure ModuleFuncProc(const Param: PLseParam);cdecl;
var
  func: TLyseeFunc;
begin
  func := TLyseeFunc(KLiFunc(Param^.func).Component);
  if func <> nil then
    func.Execute(Param);
end;

procedure ClassMethodProc(const Param: PLseParam);cdecl;
var
  curr: KLiFunc;
  func: TLyseeMethod;
  clss: TLyseeClass;
  nobj: TLyseeObject;
begin
  curr := KLiFunc(Param^.func);
  func := TLyseeMethod(curr.Component);
  if func <> nil then
    if curr.IsConstructor then
    begin
      nobj := nil;
      clss := func.FClass;
      if Assigned(clss.FOnCreateObject) then
        clss.FOnCreateObject(clss, nobj);
      if nobj = nil then
        nobj := TLyseeObject.Create(clss);
      nobj.IncRefcount;
      try
        __SetObject(Param^.param[0], clss.FClass, nobj);
        func.Execute(Param);
      finally
        __SetObject(Param^.result, clss.FClass, nobj);
        nobj.DecRefcount;
      end;
    end
    else func.Execute(Param);
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
  if FModule <> nil then
  begin
    R.fr_prot := pchar(Prototype);
    R.fr_addr := Proc;
    R.fr_desc := nil;
    Result := FModule.ModuleClass.SetupMethod(@R);
  end
  else Result := nil;
end;

procedure TLyseeModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if FFuncs <> nil then
      FFuncs.Remove(AComponent);
    if FClasses <> nil then
      FClasses.Remove(AComponent);
  end;
end;

procedure TLyseeModule.SetDescription(const AValue: string);
begin
  FDescription := Trim(AValue);
  if FModule <> nil then
    FModule.Description := FDescription;
end;

function TLyseeModule.GetClassCount: integer;
begin
  if FClasses <> nil then
    Result := FClasses.Count else
    Result := 0;
end;

function TLyseeModule.GetFunc(Index: integer): TLyseeFunc;
begin
  Result := TLyseeFunc(FFuncs[Index]);
end;

function TLyseeModule.GetFuncCount: integer;
begin
  if FFuncs <> nil then
    Result := FFuncs.Count else
    Result := 0;
end;

function TLyseeModule.GetClass(Index: integer): TLyseeClass;
begin
  Result := TLyseeClass(FClasses[Index]);
end;

procedure TLyseeModule.SetModuleName(const AValue: string);
var
  S: string;
begin
  if FModule = nil then
  begin
    S := Trim(AValue);
    if not IsLyseeID(S) then
      lse_error('invalid module name: ' + S);
    FModuleName := S;
  end;
end;

constructor TLyseeModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFuncs := TList.Create;
  FClasses := TList.Create;
end;

destructor TLyseeModule.Destroy;
var
  X: integer;
  F: TLyseeFunc;
  C: TLyseeClass;
begin
  for X := 0 to FFuncs.Count - 1 do
  begin
    F := TLyseeFunc(FFuncs[X]);
    F.Leave;
  end;
  FreeAndNil(FFuncs);

  for X := 0 to FClasses.Count - 1 do
  begin
    C := TLyseeClass(FClasses[X]);
    C.Leave;
  end;
  FreeAndNil(FClasses);

  inherited Destroy;
end;

procedure TLyseeModule.SetupModule;
var
  M: RLseModuleRec;
  X: integer;
  F: TLyseeFunc;
  C: TLyseeClass;
begin
  if not (csDesigning in ComponentState) then
    if not (csLoading in ComponentState) then
      if (FModule = nil) and (FModuleName <> '') then
      begin
        FillChar(M, sizeof(RLseModuleRec), 0);
        M.iw_version := LSE_VERSION;
        M.iw_desc := pchar(FDescription);
        FModule := __SetupModule(FModuleName, @M);
      end;
end;

procedure TLyseeModule.Setup;
var
  X: integer;
begin
  SetupModule;
  if FModule <> nil then
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
  if (FFunc = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FFuncs.Add(Self);
      Setup;
    end;
  end;
end;

procedure TLyseeFunc.SetPrototype(const AValue: string);
begin
  if FFunc = nil then
    FPrototype := Trim(AValue);
end;

procedure TLyseeFunc.SetDescription(const AValue: string);
begin
  FDescription := Trim(AValue);
  if FFunc <> nil then
    FFunc.Description := FDescription;
end;

function TLyseeFunc.GetKernelClass: KLiClass;
begin
  if FFunc <> nil then
    Result := FFunc.OwnerClass else
    Result := nil;
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

procedure TLyseeFunc.Loaded;
begin
  inherited Loaded;
  Setup;
end;

constructor TLyseeFunc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFunc := nil;
  FModule := nil;
end;

destructor TLyseeFunc.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLyseeFunc.Setup;
var
  R: RLseFuncRec;
begin
  if not (csDesigning in ComponentState) then
    if not (csLoading in ComponentState) then
      if (FFunc = nil) and (FPrototype <> '') then
        if (FModule <> nil) and (FModule.FModule <> nil) then
        begin
          R.fr_prot := pchar(FPrototype);
          R.fr_addr := @ModuleFuncProc;
          R.fr_desc := pchar(FDescription);
          FFunc := FModule.FModule.ModuleClass.SetupMethod(@R);
          if FFunc <> nil then
            FFunc.Component := Self;
        end;
end;

procedure TLyseeFunc.Execute(Param: PLseParam);
var
  call: TLseInvoke;
begin
  if Assigned(FOnInvoke) then
  begin
    call := TLseInvoke.Create(Param);
    try
      FOnInvoke(Self, call);
    finally
      call.Free;
    end;
  end
  else
  if Assigned(FOnExecute) then
    FOnExecute(Self, Param);
end;

{ TLyseeClass }

function TLyseeClass.AddMethod(const Prototype: string; Proc: TLseFuncCall): KLiFunc;
var
  R: RLseFuncRec;
begin
  if FClass <> nil then
  begin
    R.fr_prot := pchar(Prototype);
    R.fr_addr := Proc;
    R.fr_desc := nil;
    Result := FClass.SetupMethod(@R);
  end
  else Result := nil;
end;

procedure TLyseeClass.SetDescription(const AValue: string);
begin
  FDescription := Trim(AValue);
  if FClass <> nil then
    FClass.ClassRec^.desc := pchar(FDescription);
end;

procedure TLyseeClass.SetClassName(const AValue: string);
var
  S: string;
begin
  if FClass = nil then
  begin
    S := Trim(AValue);
    if not IsLyseeID(S) then
      lse_error('invalid class name: ' + S);
    FClassName := S;
  end;
end;

function TLyseeClass.GetMethodCount: integer;
begin
  if FMethods <> nil then
    Result := FMethods.Count else
    Result := 0;
end;

function TLyseeClass.GetMethod(Index: integer): TLyseeMethod;
begin
  Result := TLyseeMethod(FMethods[Index]);
end;

procedure TLyseeClass.SetModule(const AValue: TLyseeModule);
begin
  if (FClass = nil) and (FModule <> AValue) then
  begin
    Leave;
    FModule := AValue;
    if FModule <> nil then
    begin
      FreeNotification(FModule);
      FModule.FClasses.Add(Self);
      Setup;
    end;
  end;
end;

procedure TLyseeClass.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FModule then Leave else
    if FMethods <> nil then
      FMethods.Remove(AComponent);
end;

procedure TLyseeClass.Loaded;
begin
  inherited Loaded;
  Setup;
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
  FMethods := TList.Create;
end;

destructor TLyseeClass.Destroy;
var
  X: integer;
  M: TLyseeMethod;
begin
  Leave;
  for X := GetMethodCount - 1 downto 0 do
    GetMethod(X).Leave;
  FreeAndNil(FMethods);
  inherited Destroy;
end;

procedure TLyseeClass.SetupClass;
var
  R: RLseClassRec;
begin
  if not (csDesigning in ComponentState) then
    if not (csLoading in ComponentState) then
      if (FClass = nil) and (FClassName <> '') then
        if (FModule <> nil) and (FModule.FModule <> nil) then
        begin
          FillChar(R, sizeof(RLseClassRec), 0);
          R.vtype := LSV_OBJECT;
          R.name := pchar(FClassName);
          R.desc := pchar(FDescription);
          R.incRefcount := @lse_incRefcount;
          R.decRefcount := @lse_decRefcount;
          __SetupClasses(@R, 1, FModule.FModule);
          FClass := KLiClass(R.lysee_class);
        end;
end;

procedure TLyseeClass.Setup;
var
  X: integer;
begin
  SetupClass;
  if FClass <> nil then
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
    if Result.FMethod = Method then Exit;
  end;
  Result := nil;
end;

{ TLyseeMethod }

procedure TLyseeMethod.SetDescription(const AValue: string);
begin
  FDescription := Trim(AValue);
  if FMethod <> nil then
    FMethod.Description := FDescription;
end;

procedure TLyseeMethod.SetClass(const AValue: TLyseeClass);
begin
  if (FMethod = nil) and (FClass <> AValue) then
  begin
    Leave;
    FClass := AValue;
    if FClass <> nil then
    begin
      FreeNotification(FClass);
      FClass.FMethods.Add(Self);
      Setup;
    end;
  end;
end;

function TLyseeMethod.GetKernelClass: KLiClass;
begin
  if FMethod <> nil then
    Result := FMethod.OwnerClass else
    Result := nil;
end;

procedure TLyseeMethod.SetPrototype(const AValue: string);
begin
  if FMethod = nil then
    FPrototype := Trim(AValue);
end;

procedure TLyseeMethod.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FClass then
      Leave;
end;

procedure TLyseeMethod.Loaded;
begin
  inherited Loaded;
  Setup;
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
  FMethod := nil;
  FClass := nil;
end;

destructor TLyseeMethod.Destroy;
begin
  Leave;
  inherited Destroy;
end;

procedure TLyseeMethod.Setup;
var
  R: RLseFuncRec;
begin
  if not (csDesigning in ComponentState) then
    if not (csLoading in ComponentState) then
      if (FMethod = nil) and (FPrototype <> '') then
        if (FClass <> nil) and (FClass.FClass <> nil) then
        begin
          R.fr_prot := pchar(FPrototype);
          R.fr_addr := @ClassMethodProc;
          R.fr_desc := pchar(FDescription);
          FMethod := FClass.FClass.SetupMethod(@R);
          if FMethod <> nil then
            FMethod.Component := Self;
        end;
end;

procedure TLyseeMethod.Execute(Param: PLseParam);
var
  call: TLseInvoke;
begin
  if Assigned(FOnInvoke) then
  begin
    call := TLseInvoke.Create(Param);
    try
      FOnInvoke(Self, call);
    finally
      call.Free;
    end;
  end
  else
  if Assigned(FOnExecute) then
    FOnExecute(Self, Param);
end;

{ TLyseeObject }

procedure TLyseeObject.SetData(const AData: pointer);
var
  AllowChange: boolean;
begin
  if FData <> AData then
  begin
    AllowChange := true;
    DataChanging(AData, AllowChange);
    if AllowChange then
    begin
      FData := AData;
      DataChanged;
    end;
  end;
end;

procedure TLyseeObject.DataChanging(const NewData: pointer; var AllowChange: boolean);
begin

end;

procedure TLyseeObject.DataChanged;
begin

end;

constructor TLyseeObject.Create(AClass: TLyseeClass);
begin
  inherited Create;
  FClass := AClass;
end;

destructor TLyseeObject.Destroy;
begin
  if FClass <> nil then
    if Assigned(FClass.FOnDestroyObject) then
      FClass.FOnDestroyObject(FClass, Self);
  inherited Destroy;
end;

initialization
  {$I lysee_laz.lrs}

end.
