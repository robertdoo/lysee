{==============================================================================}
{        UNIT: lse_spawn                                                       }
{ DESCRIPTION: create and control outside processes                            }
{     CREATED: 2005/01/15                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2005-2010, Li Yun Jie                                          }
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
{ Portions created by Li Yun Jie are Copyright (C) 2005-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_spawn;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu,
  {$IFDEF WINDOWS}
  Windows
  {$ELSE}
  Process
  {$ENDIF};

const
  SPACE_CH = [#$09, #$0A, #$0C, #$0D, #$20];
  MAX_OPTS = 256; {<-- max options in command line }
  MAX_CMDS = 32;  {<-- max commands seperated by pipe '|' }


type
  TOptList = array[0..MAX_OPTS - 1] of pchar;
  POptList = ^TOptList;

  TCmdList = array[0..MAX_CMDS - 1] of pchar;
  PCmdList = ^TCmdList;

  {$IFDEF WINDOWS}
  RChildIOTable = packed record
    childStdIn     : THandle;
    childStdOut    : THandle;
    childStdErr    : THandle;
    dwFlags        : dword;
    wShowWindow    : word;
  end;
  PChildIOTable = ^RChildIOTable;
  {$ENDIF}

  TSpawnWait = (spwNowait, spwStatus, spwOutput);

  TSpawn = class
  private
    FMemoryList: TList;        {<--allocated memory list}
    FOpt: pchar;
    FOptList: TOptList;
    FOptCount: integer;
    FCmd: pchar;
    FCmdName: TCmdList;
    FCmdLine: TCmdList;
    FCmdCount: integer;
    FCmdIndex: integer;
    FWait: TSpawnWait;
    FShowWindow: boolean;
    FStatus: integer;
    FInputFile: pchar;
    FOutputFile: pchar;
    FOutput: TMemoryStream; {<--spawn result stream}
    FAppend: boolean;
    FErrno: integer;
    FErrorMsg: string;
    FWorkDir: string;
    {$IFDEF WINDOWS}
    FOutputHandle: THandle; {<--handle of FOutputFile}
    FReadPipe: THandle;
    FWritePipe: THandle;
    FPipeList: TList;
    FSA: SECURITY_ATTRIBUTES;
    procedure SysError;
    procedure ClosePipeList(CloseReadPipe: boolean);
    function GetChildIO(ptbl: PChildIOTable; spawnlast: boolean): boolean;
    function Getenv(name: pchar): pchar;
    function QualifiedPath(cmdname: pchar): pchar;
    function WriteToOutput(buf: pchar; count: cardinal): boolean;
    {$ENDIF}
    function SetupCommandLine(cmdline: pchar): boolean;
    function Unquote(S: pchar; Trim: boolean): pchar;
    function MemGet(size: integer): pointer;
    procedure ClearMemList;
    procedure Error(const msg: string; errno: integer = -1);
    function Spawnvp(spawnlast: boolean): integer;
  public
    constructor Create;
    destructor Destroy;override;
    function Spawn(cmdline: pchar): integer;
    function GetOutput(ClearOutput: boolean): string;
    property Wait: TSpawnWait read FWait write FWait;
    property ShowWindow: boolean read FShowWindow write FShowWindow;
    property Status: integer read FStatus write FStatus;
    property Errno: integer read FErrno write FErrno;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
    property WorkDir: string read FWorkDir write FWorkDir;
  end;

{-----------------------------------------------------------------------
( F_NAME: __spawn_XX
(
( F_DESC: execute commandline with input(file/stream) and output(file/stream)
(
( F_ARGS: const CmdLine: string - command line
(         const InitDir: string - working directory
(         InputXXXX - input file or input stream
(         OutputXXXX - output file or output stream
(
( F_TYPE: integer - exit status
(
( EXCEPT:
(----------------------------------------------------------------------}
{$IFNDEF WINDOWS}
function __spawn_ss(const CmdLine, InitDir: string; Input, Output: TStream): integer;
function __spawn_fs(const CmdLine, InitDir, InputFile: string; Output: TStream): integer;
function __spawn_sf(const CmdLine, InitDir: string; Input: TStream; const OutputFile: string): integer;
function __spawn_ff(const CmdLine, InitDir, InputFile, OutputFile: string): integer;
{$ENDIF}

{-----------------------------------------------------------------------
( F_NAME: spawn_shouts
(
( F_DESC: execute commandline and get its output
(
( F_ARGS: const CmdLine: string - command line
(         const InitDir: string - working directory
(         var ExitCode: integer - exit code
(
( F_TYPE: string - command line output
(
( EXCEPT:
(----------------------------------------------------------------------}
function spawn_shouts(const CmdLine, InitDir: string;
  var ExitCode: integer): string;cdecl;

{-----------------------------------------------------------------------
( F_NAME: spawn_shexec
(
( F_DESC: execute commandline
(
( F_ARGS: const CmdLine: string - command line
(         const InitDir: string - working directory
(         WaitFor: boolean - wait for
(         var ExitCode: integer - exit code
(
( F_TYPE: 
(
( EXCEPT:
(----------------------------------------------------------------------}
function spawn_shexec(const CmdLine, InitDir: string; WaitFor: boolean;
  var ExitCode: integer): boolean;cdecl;

implementation

var
  osver_ready: boolean;
  {$IFDEF WINDOWS}
  osver: OSVERSIONINFO;
  {$ELSE}
  { osver: LINUX | UNIX | MACOS | ...}
  {$ENDIF}

procedure prepare_osver_info;
begin
  if not osver_ready then
  begin
    osver_ready := true;
    {$IFDEF WINDOWS}
    osver.dwOSVersionInfoSize := sizeof(osver);
    GetVersionEx(osver);
    {$ELSE}
    { GET osver OF LINUX, UNIX, MACOS ...}
    {$ENDIF}
  end;
end;

function is_win95: boolean;
begin
  prepare_osver_info;
  {$IFDEF WINDOWS}
  Result := (osver.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function is_NT_32: boolean;
begin
  prepare_osver_info;
  {$IFDEF WINDOWS}
  Result := (osver.dwPlatformId = VER_PLATFORM_WIN32_NT);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function has_space(S: pchar): boolean;
begin
  if S <> nil then
    while (S^ <> #0) and not (S^ in SPACE_CH) do
      Inc(S);
  Result := (S <> nil) and (S^ <> #0);
end;

function spawn_shouts(const CmdLine, InitDir: string;
  var ExitCode: integer): string;cdecl;
var
  spawn: TSpawn;
begin
  spawn := TSpawn.Create;
  try
    spawn.Wait := spwOutput;
    spawn.ShowWindow := false;
    spawn.WorkDir := ExpandFileName(InitDir);
    if spawn.Spawn(pchar(CmdLine)) >= 0 then
    begin
      Result := spawn.GetOutput(true);
      ExitCode := spawn.Status;
    end
    else
    begin
      ExitCode := -1;
      Result := '';
    end;
  finally
    spawn.Free;
  end;
end;

function spawn_shexec(const CmdLine, InitDir: string; WaitFor: boolean;
  var ExitCode: integer): boolean;cdecl;
var
  spawn: TSpawn;
begin
  spawn := TSpawn.Create;
  try
    if WaitFor then
      spawn.Wait := spwStatus else
      spawn.Wait := spwNowait;
    spawn.ShowWindow := true;
    spawn.WorkDir := ExpandFileName(InitDir);
    Result := spawn.Spawn(pchar(CmdLine)) >= 0;
    if Result then
    begin
      if WaitFor then
        ExitCode := spawn.Status else
        ExitCode := 0;
    end
    else ExitCode := -1;
  finally
    spawn.Free;
  end;
end;

{$IFNDEF WINDOWS}
function __spawn_ss(const CmdLine, InitDir: string; Input, Output: TStream): integer;
const
  PROPTS = [poUsePipes, poStderrToOutPut, poNoConsole];
var
  buffer: array[0..1023] of char;
  count: integer;
  process: TProcess;
  done: boolean;
  temp: string;
begin
  try
    process := TProcess.Create(nil);
    try
      // 1. start process
      process.CommandLine := CmdLine;
      process.CurrentDirectory := InitDir;
      process.Options := process.Options + PROPTS;
      process.Execute;

      // 2. send input into stdin
      while process.Running and (Input <> nil) do
      begin
        count := input.Read(buffer, sizeof(buffer));
        if count > 0 then
          process.Input.Write(buffer, count) else
          Input := nil;
      end;
      process.CloseInput;

      // 3. receive output from stdout
      if Output = nil then process.CloseOutput;
      while process.Running do
        if Output <> nil then
        begin
          count := process.Output.Read(buffer, sizeof(buffer));
          if count > 0 then
            Output.WriteBuffer(buffer, count) else
            Sleep(10);
        end
        else Sleep(10);

      // 4. read remaining output
      if Output <> nil then
      begin
        count := process.Output.Read(buffer, sizeof(buffer));
        while count > 0 do
        begin
          Output.WriteBuffer(buffer, count);
          count := process.Output.Read(buffer, sizeof(buffer));
        end;
      end;

      // 5. save exit status
      Result := process.ExitStatus;
    finally
      process.Free;
    end;
  except
    Result := -1;
  end;
end;

function __spawn_fs(const CmdLine, InitDir, InputFile: string; Output: TStream): integer;
var
  input: TFileStream;
begin
  try
    if InputFile <> '' then
    begin
      input := TFileStream.Create(lse_veryPD(InputFile), fmShareDenyWrite);
      try
        Result := __spawn_ss(CmdLine, InitDir, input, Output);
      finally
        input.Free;
      end;
    end
    else Result := __spawn_ss(CmdLine, InitDir, nil, Output);
  except
    Result := -1;
  end;
end;

function __spawn_sf(const CmdLine, InitDir: string; Input: TStream;
  const OutputFile: string): integer;
var
  output: TFileStream;
begin
  try
    if OutputFile <> '' then
    begin
      output := TFileStream.Create(lse_veryPD(OutputFile), fmCreate);
      try
        Result := __spawn_ss(CmdLine, InitDir, Input, output);
      finally
        output.Free;
      end;
    end
    else Result := __spawn_ss(CmdLine, InitDir, Input, nil);
  except
    Result := -1;
  end;
end;

function __spawn_ff(const CmdLine, InitDir, InputFile, OutputFile: string): integer;
var
  input, output: TFileStream;
begin
  try
    if InputFile = '' then
    begin
      Result := __spawn_sf(CmdLine, InitDir, nil, OutputFile);
      Exit;
    end;

    if OutputFile = '' then
    begin
      Result := __spawn_fs(CmdLine, InitDir, InputFile, nil);
      Exit;
    end;

    input := TFileStream.Create(lse_veryPD(InputFile), fmShareDenyWrite);
    try
      output := TFileStream.Create(lse_veryPD(OutputFile), fmCreate);
      try
        Result := __spawn_ss(CmdLine, InitDir, input, output);
      finally
        output.Free;
      end;
    finally
      input.Free;
    end;
  except
    Result := -1;
  end;
end;
{$ENDIF}

{ TSpawn }

procedure TSpawn.ClearMemList;
var
  index: integer;
begin
  for index := 0 to FMemoryList.Count - 1 do
    lse_mem_free(FMemoryList[index], 0);
  FMemoryList.Clear;
end;

{$IFDEF WINDOWS}
procedure TSpawn.ClosePipeList(CloseReadPipe: boolean);
var
  index: integer;
  handle: THandle;
begin
  for index := FPipeList.Count - 1 downto 0 do
  begin
    handle := THandle(FPipeList[index]);
    if CloseReadPipe or (handle <> FReadPipe) then
    begin
      CloseHandle(handle);
      FPipeList.Delete(index);
    end;
  end;
  if CloseReadPipe then
    FReadPipe := INVALID_HANDLE_VALUE;
  FWritePipe := INVALID_HANDLE_VALUE;
end;
{$ENDIF}

constructor TSpawn.Create;
begin
  FMemoryList := TList.Create;
  {$IFDEF WINDOWS}
  FWritePipe := INVALID_HANDLE_VALUE;
  FReadPipe := INVALID_HANDLE_VALUE;
  FPipeList := TList.Create;
  FillChar(FSA, sizeof(FSA), 0);
  FSA.nLength := sizeof(FSA);
  FSA.bInheritHandle := true;
  FOutputHandle := INVALID_HANDLE_VALUE;
  {$ENDIF}
end;

destructor TSpawn.Destroy;
begin
  ClearMemList;
  FMemoryList.Free;
  FreeAndNil(FOutput);
  {$IFDEF WINDOWS}
  FPipeList.Free;
  {$ENDIF}
  inherited;
end;

procedure TSpawn.Error(const msg: string; errno: integer);
begin
  FErrorMsg := msg;
  FErrno := errno;
end;

{$IFDEF WINDOWS}
function TSpawn.GetChildIO(ptbl: PChildIOTable; spawnlast: boolean): boolean;
var
  smode, cmode: THandle;
begin
  Result := false;

  if FCmdIndex = 0 then
  begin
    if FInputFile <> nil then
    begin
      smode := FILE_SHARE_READ;
      ptbl^.childStdIn := CreateFile(FInputFile, GENERIC_READ, smode,
        @FSA, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if ptbl^.childStdIn = INVALID_HANDLE_VALUE then Exit;
      FPipeList.Add(pointer(ptbl^.childStdIn));
    end
    else ptbl^.childStdIn := GetStdHandle(STD_INPUT_HANDLE);
  end
  else ptbl^.childStdIn := FReadPipe;

  if not spawnlast or (Wait = spwOutput) or (FOutputFile <> nil) then
  begin
    if not CreatePipe(FReadPipe, FWritePipe, @FSA, 0) then Exit;
    ptbl^.childStdOut := FWritePipe;
    ptbl^.childStdErr := FWritePipe;
    FPipeList.Add(pointer(FWritePipe));
    FPipeList.Add(pointer(FReadPipe));
    if spawnlast and (FOutputFile <> nil) then
    begin
      smode := FILE_SHARE_READ or FILE_SHARE_WRITE;
      if is_NT_32 then
        smode := smode or FILE_SHARE_DELETE;
      if FAppend then
        cmode := OPEN_ALWAYS else
        cmode := CREATE_ALWAYS;
      FOutputHandle := CreateFile(FOutputFile, GENERIC_WRITE, smode,
        @FSA, cmode, FILE_ATTRIBUTE_NORMAL, 0);
      if FOutputHandle = INVALID_HANDLE_VALUE then Exit;
      if FAppend then
        SetFilePointer(FOutputHandle, 0, nil, FILE_END);
    end;
  end
  else
  if FOutputFile = nil then
  begin
    ptbl^.childStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
    ptbl^.childStdErr := GetStdHandle(STD_ERROR_HANDLE);
  end;

  ptbl^.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  if FShowWindow then
    ptbl^.wShowWindow := SW_NORMAL else
    ptbl^.wShowWindow := SW_HIDE;

  Result := true;
end;

function TSpawn.Getenv(name: pchar): pchar;
var
  envlen: integer;
  env: string;
begin
  env := SysUtils.GetEnvironmentVariable(name);
  envlen := Length(env);
  if envlen > 0 then
  begin
    Result := MemGet(envlen + 1);
    StrPLCopy(Result, env, envlen);
  end
  else Result := nil;
end;
{$ENDIF}

function TSpawn.GetOutput(ClearOutput: boolean): string;
var
  mem: pchar;
begin
  if (FOutput <> nil) and (FOutput.Size > 0) then
  begin
    mem := FOutput.Memory;
    SetString(Result, mem, FOutput.Size);
    if ClearOutput then
      FOutput.Clear;
  end
  else Result := '';
end;

function TSpawn.MemGet(size: integer): pointer;
begin
  if size > 0 then
  begin
    Result := lse_mem_alloc_zero(size);
    FMemoryList.Add(Result);
  end
  else Result := nil;
end;

{$IFDEF WINDOWS}
function TSpawn.QualifiedPath(cmdname: pchar): pchar;
var
  path, fname, fnext, cname, slash: pchar;
  cmdlen: integer;

  function GetNextPath: boolean;
  begin
    while path^ = ';' do Inc(path);
    fnext := fname;
    while (path^ <> #0) and (path^ <> ';') do
    begin
      if path^ = '"' then // foo;"baz;etc";bar
      begin
        Inc(path); // skip initial '"'
        while (path^ <> #0) and (path^ <> '"') do
        begin
          fnext^ := path^;
          Inc(fnext);
          Inc(path);
        end;
        if path^ <> #0 then
          Inc(path); // skip trailing '"'
      end
	    else
      begin
        fnext^ := path^;
        Inc(fnext);
        Inc(path);
      end;
    end;
    Result := fnext > fname;
    if Result and not ((fnext - 1)^ in ['/', '\']) then
    begin
      fnext^ := '\';
      Inc(fnext);
    end;
  end;

begin
  Result := nil;

  slash := nil;
  cname := cmdname;
  while cname^ <> #0 do
  begin
    if cname^ in ['/', '\', ':'] then
      slash := cname;
    Inc(cname);
  end;
  cmdlen := cname - cmdname;

  if slash <> nil then
  begin
    cname := StrRScan(slash, '.');
    if cname <> nil then Exit;
  end
  else cname := StrRScan(cmdname, '.');

  fname := MemGet(MAX_PATH + cmdlen + 6);
  fnext := fname;
  path := Getenv('PATH');
  repeat
    StrCopy(fnext, cmdname);
    Inc(fnext, cmdlen);
    if cname = nil then
      StrCopy(fnext, '.exe');
    if FileExists(fname) then
    begin
      Result := fname;
      Exit;
    end;
  until (slash <> nil) or not GetNextPath;
end;
{$ENDIF}

function TSpawn.SetupCommandLine(cmdline: pchar): boolean;
var
  next, cmd: pchar;
  index, pipes, count: integer;

  function seek_next: pchar;
  begin
    while cmd^ in SPACE_CH do Inc(cmd);
    Result := cmd;
  end;

  function read_quote: boolean;
  var
    quote: char;
  begin
    quote := cmd^;
    repeat
      next^ := cmd^;
      Inc(next);
      Inc(cmd);
    until (cmd^ = #0) or (cmd^ = quote);
    Result := (cmd^ = quote);
    if Result then
    begin
      next^ := quote;
      Inc(next);
      next^ := #0;
      Inc(next);
      Inc(cmd);
    end;
  end;

  function read_option: boolean;
  const
    echars = SPACE_CH + [#0, '|', '<', '>'];
    rchars = [' '..#255] - echars - ['"', ''''];
  begin
    Result := not (cmd^ in echars);
    if Result then
    begin
      while cmd^ in rchars do
      begin
        next^ := cmd^;
        Inc(next);
        Inc(cmd);
      end;
      next^ := #0;
      Inc(next);
    end;
  end;

  function read_stdin: boolean;
  begin
    Result := (pipes = 0) and (FInputFile = nil);
    if not Result then Exit;

    Inc(cmd);
    if seek_next^ in ['''', '"'] then
      Result := read_quote else
      Result := read_option;
    if not Result then Exit;

    Dec(FOptCount);
    FInputFile := Unquote(FOptList[FOptCount], true);
    Result := (FInputFile <> nil);
    if not Result then Exit;

    FOptList[FOptCount] := nil;
    Dec(count);
  end;

  function read_stdout: boolean;
  begin
    Result := (FOutputFile = nil);
    if not Result then Exit;

    Inc(cmd);
    FAppend := (cmd^ = '>');
    if FAppend then
      Inc(cmd);
    if seek_next^ in ['''', '"'] then
      Result := read_quote else
      Result := read_option;
    if not Result then Exit;

    Dec(FOptCount);
    FOutputFile := Unquote(FOptList[FOptCount], true);
    Result := (FOutputFile <> nil);
    if not Result then Exit;

    FOptList[FOptCount] := nil;
    Dec(count);
  end;

  function seek_head: boolean;
  begin
    Result := not (seek_next^ in ['|', '<', '>', '*', '?', #0]);
  end;

begin
  // 1. initialize flag variables

  FillChar(FCmdName, sizeof(FCmdName), 0);
  FillChar(FCmdLine, sizeof(FCmdLine), 0);
  FCmdCount := 0;
  FCmdIndex := 0;
  FCmd := nil;

  FillChar(FOptList, sizeof(FOptList), 0);
  FOptCount := 0;
  FOpt := nil;
  FInputFile := nil;
  FOutputFile := nil;
  FAppend := false;

  Result := false;

  // 2. parsing words of the command line

  cmd := cmdline;
  if (cmd = nil) or not seek_head then Exit;
  pipes := 0;
  count := 0;
  FOpt := MemGet(strlen(cmd) * 2);
  next := FOpt;
  repeat
    FOptList[FOptCount] := next;
    Inc(FOptCount);
    case cmd^ of
      '<': if not read_stdin then Exit;  {<-- stdin file name }
      '>': if not read_stdout then Exit; {<-- stdout file & mode }
     '''',
      '"': if not read_quote  then Exit; {<-- '...' or "..." }
      else if not read_option then Exit; {<-- options }
    end;
    Inc(count);
    if seek_next^ = '|' then
    begin
      if (count < 1) or (FOutputFile <> nil) then Exit;
      Inc(FOptCount);
      Inc(cmd);
      Inc(pipes);
      if not seek_head then Exit;
      count := 0;
    end;
  until cmd^ = #0;

  // 3. setup command lines

  FCmd := MemGet(strlen(cmdline) * 2);
  next := FCmd;
  index := 0;
  count := 0;
  repeat
    FCmdName[count] := Unquote(FOptList[index], true);
    if FCmdName[count] = nil then Exit;
    FCmdLine[count] := next;
    StrCopy(next, FOptList[index]);
    Inc(next, strlen(next));
    Inc(index);
    while FOptList[index] <> nil do
    begin
      next^ := ' ';
      Inc(next);
      StrCopy(next, FOptList[index]);
      Inc(next, strlen(next));
      Inc(index);
    end;
    Inc(next);
    Inc(count);
    Inc(index);
  until FOptList[index] = nil;
  FCmdCount := count;

  Result := FCmdCount > 0;
end;

function TSpawn.Spawn(cmdline: pchar): integer;
begin
  Status := -1;
  Result := -1;

  if not SetupCommandLine(cmdline) then
  begin
    Error('Invalid shell command line');
    Exit;
  end;

  {$IFDEF WINDOWS}
  if FOutput <> nil then FOutput.Clear else
  if (FWait = spwOutput) and (FOutputFile = nil) then
    FOutput := TMemoryStream.Create;
  {$ENDIF}

  if FWorkDir = '' then
    FWorkDir := GetCurrentDir;
    
  while FCmdIndex < FCmdCount do
  begin
    Status := Spawnvp(FCmdIndex = FCmdCount - 1);
    if Status < 0 then Break;
    Inc(FCmdIndex);
  end;
  
  {$IFDEF WINDOWS}
  CloseHandle(FOutputHandle);
  FOutputHandle := INVALID_HANDLE_VALUE;
  {$ENDIF}
  ClearMemList;

  Result := Status;
  if FWait <> spwOutput then
    FreeAndNil(FOutput);
end;

function TSpawn.Spawnvp(spawnlast: boolean): integer;
{$IFDEF WINDOWS}
var
  cline, cname, next: pchar;
  tbl: RChildIOTable;
  supin: STARTUPINFO;
  pinfo: PROCESS_INFORMATION;
  sp_create, sp_status: dword;
  inbuf: array[0..4095] of char;
  bytes: cardinal;

  function Execute(xname, xline: pchar): boolean;
  begin
    Result := CreateProcess(xname, xline, nil, nil, true, sp_create,
      nil, pchar(FWorkDir), supin, pinfo);
  end;

begin
  Result := -1;

  FillChar(tbl, sizeof(tbl), 0);
  if not GetChildIO(@tbl, spawnlast) then
  begin
    SysError;
    Exit;
  end;

  FillChar(supin, sizeof(supin), 0);
  supin.cb := sizeof(supin);
  supin.hStdInput := tbl.childStdIn;
  supin.hStdOutput := tbl.childStdOut;
  supin.hStdError := tbl.childStdErr;
  supin.dwFlags := tbl.dwFlags;
  supin.wShowWindow := tbl.wShowWindow;

  if not FShowWindow then sp_create := DETACHED_PROCESS else
  if (supin.hStdInput = INVALID_HANDLE_VALUE) and
     (supin.hStdOutput = INVALID_HANDLE_VALUE) and
     (supin.hStdError = INVALID_HANDLE_VALUE) then
    sp_create := CREATE_NEW_CONSOLE else
    sp_create := 0;

  if not spawnlast or (Wait = spwNowait) then
    sp_create := sp_create or windows.CREATE_NEW_PROCESS_GROUP;

  cname := FCmdName[FCmdIndex];
  cline := FCmdLine[FCmdIndex];

  if not Execute(cname, cline) then
  begin
    cname := QualifiedPath(cname);
    if cname <> nil then
    begin
      cline := MemGet(strlen(cline) + strlen(cname) + 5);
      next := cline;
      if has_space(cname) then
      begin
        next^ := '"';
        Inc(next);
        StrCopy(next, cname);
        Inc(next, strlen(cname));
        next^ := '"';
        Inc(next);
      end
      else
      begin
        StrCopy(next, cname);
        Inc(next, strlen(cname));
      end;
      StrCopy(next, FCmdLine[FCmdIndex] + strlen(FCmdName[FCmdIndex]));
      if not Execute(cname, cline) then
        cname := nil;
    end;
    if cname = nil then
    begin
      cline := MemGet(strlen(FCmdLine[FCmdIndex]) + 16);
      next := cline;
      if is_NT_32 then
        StrCopy(next, 'cmd.exe /x/d/c ') else
        strCopy(next, 'command.com /c ');
      Inc(next, strlen(next));
      StrCopy(next, FCmdLine[FCmdIndex]);
      if not Execute(nil, cline) then
      begin
        SysError;
        Exit;
      end;
    end;
  end;

  ClosePipeList(false);

  if not spawnlast or ((FOutputFile = nil) and (Wait = spwNowait)) then
  begin
    // asynchronous spawn -- store handle, return PID
    Result := integer(pinfo.dwProcessId);
    if is_win95 and (Result < 0) then
      Result := - Result;
  end
  else { wait for the last process}
  begin
    if (FOutputFile <> nil) or (Wait = spwOutput) then
    begin
      while ReadFile(FReadPipe, inbuf, sizeof(inbuf), bytes, nil) do
        WriteToOutput(inbuf, bytes);
      ClosePipeList(true);
    end
    else WaitForSingleObject(pinfo.hProcess, INFINITE);
	  // FIXME: if msgwait returned due to message perhaps forward the
	  // "signal" to the process
    GetExitCodeProcess(pinfo.hProcess, sp_status);
    Result := sp_status;
    CloseHandle(pinfo.hProcess);
  end;

  CloseHandle(pinfo.hThread);
end;
{$ELSE}
var
  input: TMemoryStream;
  process: TProcess;
  cline, cname: string;
begin
  cname := FCmdName[FCmdIndex];
  cline := FCmdLine[FCmdIndex];

  if FCmdIndex = 0 then
  begin
    FreeAndNil(FOutput);
    FOutput := TMemoryStream.Create;
    Result := __spawn_fs(cline, FWorkDir, FInputFile, FOutput);
  end
  else
  begin
    input := FOutput;
    try
      FOutput := TMemoryStream.Create;
      Result := __spawn_ss(cline, FWorkDir, input, FOutput);
    finally
      input.Free;
    end;
  end;

  FOutput.Position := 0;
  if spawnlast then
    if FOutputFile <> '' then
      FOutput.SaveToFile(FOutputFile);
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TSpawn.SysError;
begin
  FErrno := GetLastError;
  FErrorMsg := SysUtils.SysErrorMessage(FErrno);
end;
{$ENDIF}

function TSpawn.Unquote(S: pchar; Trim: boolean): pchar;
var
  L: integer;
begin
  Result := MemGet(strlen(S) + 1);
  L := 0;
  while S^ <> #0 do
  begin
    if not (S^ in ['"', '''']) then
    begin
      Result[L] := S^;
      Inc(L);
    end;
    Inc(S);
  end;
  if Trim then
  begin
    Dec(L);
    while (L >= 0) and (Result[L] in SPACE_CH) do
    begin
      Result[L] := #0;
      Dec(L);
    end;
    while Result^ in SPACE_CH do
      Inc(Result);
  end;
  if Result^ = #0 then
    Result := nil;
end;

{$IFDEF WINDOWS}
function TSpawn.WriteToOutput(buf: pchar; count: cardinal): boolean;
var
  bytes: cardinal;
begin
  if FOutputFile <> nil then
  begin
    while count > 0 do
    begin
      {$IFDEF WINDOWS}
      Result := WriteFile(FOutputHandle, buf^, count, bytes, nil);
      {$ELSE}
      Result := FileWrite(FOutputHandle, buf^, bytes) = bytes;
      {$ENDIF}
      if not Result then Exit;
      Dec(count, bytes);
      Inc(buf, bytes);
    end;
  end
  else FOutput.Write(buf^, count);
  Result := true;
end;
{$ENDIF}

end.
