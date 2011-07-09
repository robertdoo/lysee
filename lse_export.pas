{==============================================================================}
{        UNIT: lse_export                                                      }
{ DESCRIPTION: binary interface between lseu and lse_kernel                    }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2004/04/12                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Copyright (c) 2004-2010, Li Yun Jie                                          }
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
{ Portions created by Li Yun Jie are Copyright (C) 2004-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_export;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu, lse_kernel, lse_funcs;

function  qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
procedure qe_engine_destroy(const Engine: pointer);cdecl;
function  qe_engine_compile(const Engine: pointer; const code: pchar): integer;cdecl;
function  qe_engine_compile_file(const Engine: pointer; const fname: pchar): integer;cdecl;
function  qe_engine_execute(const Engine: pointer; const code: pchar): integer;cdecl;
function  qe_engine_execute_file(const Engine: pointer; const fname: pchar): integer;cdecl;
procedure qe_engine_terminate(const Engine: pointer);cdecl;
procedure qe_engine_clear(const Engine: pointer);cdecl;
function  qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
function  qe_engine_errno(const Engine: pointer): integer;cdecl;
function  qe_engine_error_row(const Engine: pointer): integer;cdecl;
function  qe_engine_error_col(const Engine: pointer): integer;cdecl;
function  qe_engine_error_name(const Engine: pointer): pchar;cdecl;
function  qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
function  qe_engine_error_module(const Engine: pointer): pchar;cdecl;
function  qe_engine_error_file(const Engine: pointer): pchar;cdecl;
function  qe_engine_result_type(const Engine: pointer): pchar;cdecl;
function  qe_engine_result_text(const Engine: pointer): pchar;cdecl;
function  qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
function  qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
function  qe_engine_ready(const Engine: pointer): integer;cdecl;
function  qe_engine_running(const Engine: pointer): integer;cdecl;
function  qe_engine_terminated(const Engine: pointer): integer;cdecl;
function  qe_engine_exited(const Engine: pointer): integer;cdecl;
procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
function  qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
function  qe_engine_readln(const Engine: pointer): PLseString;cdecl;
procedure qe_begin_cgi(const Engine: pointer);cdecl;
procedure qe_end_cgi(const Engine: pointer);cdecl;
function  qe_register_module(const Name: pchar; const initrec: PLseModule): pointer;cdecl;
function  qe_register_type(const CR: PLseType): PLseType;cdecl;
function  qe_typerec(const KernelType: pointer): PLseType;cdecl;
function  qe_register_func(const FR: PLseFunc): pointer;cdecl;
procedure qe_casto_string(const V: PLseValue);cdecl;
function  qe_param_engine(const Param: PLseParam): PLseEngine;cdecl;
function  qe_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
function  qe_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
function  qe_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
function  qe_simple_test(const Script: pchar): integer;cdecl;
function  qe_startup: integer;cdecl;
procedure qe_cleanup;cdecl;
function  qe_keywords: pchar;cdecl;
function  qe_get_kernel_file: pchar;cdecl;
procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
function  qe_get_program_file: pchar;cdecl;
procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
procedure qe_load_config(const ConfigFile: pchar);cdecl;
function  qe_production: pchar;cdecl;
function  qe_version: pchar;cdecl;
function  qe_copyright: pchar;cdecl;
function  qe_tmpath: pchar;cdecl;
function  qe_query(const ID: pchar): pointer;cdecl;

var
  qe_entries: RLseEntry = (
    cik_types             : @sys_type_list;
    { engine }
    cik_create            : {$IFDEF FPC}@{$ENDIF}qe_engine_create;
    cik_destroy           : {$IFDEF FPC}@{$ENDIF}qe_engine_destroy;
    cik_compile           : {$IFDEF FPC}@{$ENDIF}qe_engine_compile;
    cik_fcompile          : {$IFDEF FPC}@{$ENDIF}qe_engine_compile_file;
    cik_execute           : {$IFDEF FPC}@{$ENDIF}qe_engine_execute;
    cik_fexecute          : {$IFDEF FPC}@{$ENDIF}qe_engine_execute_file;
    cik_terminate         : {$IFDEF FPC}@{$ENDIF}qe_engine_terminate;
    cik_clear             : {$IFDEF FPC}@{$ENDIF}qe_engine_clear;
    cik_get_args          : {$IFDEF FPC}@{$ENDIF}qe_engine_get_args;
    cik_set_args          : {$IFDEF FPC}@{$ENDIF}qe_engine_set_args;
    cik_errno             : {$IFDEF FPC}@{$ENDIF}qe_engine_errno;
    cik_error_row         : {$IFDEF FPC}@{$ENDIF}qe_engine_error_row;
    cik_error_col         : {$IFDEF FPC}@{$ENDIF}qe_engine_error_col;
    cik_error_name        : {$IFDEF FPC}@{$ENDIF}qe_engine_error_name;
    cik_error_msg         : {$IFDEF FPC}@{$ENDIF}qe_engine_error_msg;
    cik_error_module      : {$IFDEF FPC}@{$ENDIF}qe_engine_error_module;
    cik_error_file        : {$IFDEF FPC}@{$ENDIF}qe_engine_error_file;
    cik_result_type       : {$IFDEF FPC}@{$ENDIF}qe_engine_result_type;
    cik_result_text       : {$IFDEF FPC}@{$ENDIF}qe_engine_result_text;
    cik_get_search_path   : {$IFDEF FPC}@{$ENDIF}qe_engine_get_search_path;
    cik_set_search_path   : {$IFDEF FPC}@{$ENDIF}qe_engine_set_search_path;
    cik_get_main_file     : {$IFDEF FPC}@{$ENDIF}qe_engine_get_main_file;
    cik_set_main_file     : {$IFDEF FPC}@{$ENDIF}qe_engine_set_main_file;
    cik_ready             : {$IFDEF FPC}@{$ENDIF}qe_engine_ready;
    cik_running           : {$IFDEF FPC}@{$ENDIF}qe_engine_running;
    cik_terminated        : {$IFDEF FPC}@{$ENDIF}qe_engine_terminated;
    cik_exited            : {$IFDEF FPC}@{$ENDIF}qe_engine_exited;
    cik_write             : {$IFDEF FPC}@{$ENDIF}qe_engine_write;
    cik_read              : {$IFDEF FPC}@{$ENDIF}qe_engine_read;
    cik_readln            : {$IFDEF FPC}@{$ENDIF}qe_engine_readln;
    cik_begin_cgi         : {$IFDEF FPC}@{$ENDIF}qe_begin_cgi;
    cik_end_cgi           : {$IFDEF FPC}@{$ENDIF}qe_end_cgi;
    cik_register_module   : {$IFDEF FPC}@{$ENDIF}qe_register_module;
    cik_register_type     : {$IFDEF FPC}@{$ENDIF}qe_register_type;
    cik_typerec           : {$IFDEF FPC}@{$ENDIF}qe_typerec;
    cik_register_func     : {$IFDEF FPC}@{$ENDIF}qe_register_func;
    cik_casto_string      : {$IFDEF FPC}@{$ENDIF}qe_casto_string;
    { param }
    cik_param_engine      : {$IFDEF FPC}@{$ENDIF}qe_param_engine;
    cik_param_format      : {$IFDEF FPC}@{$ENDIF}qe_param_format;
    cik_param_error       : {$IFDEF FPC}@{$ENDIF}qe_param_error;
    cik_param_push        : {$IFDEF FPC}@{$ENDIF}qe_param_push;
    cik_param_goon        : {$IFDEF FPC}@{$ENDIF}qe_param_goon;
    { others }
    cik_production        : {$IFDEF FPC}@{$ENDIF}qe_production;
    cik_version           : {$IFDEF FPC}@{$ENDIF}qe_version;
    cik_copyright         : {$IFDEF FPC}@{$ENDIF}qe_copyright;
    cik_tmpath            : {$IFDEF FPC}@{$ENDIF}qe_tmpath;
    cik_query             : {$IFDEF FPC}@{$ENDIF}qe_query;
    cik_simple_test       : {$IFDEF FPC}@{$ENDIF}qe_simple_test;
    cik_startup           : {$IFDEF FPC}@{$ENDIF}qe_startup;
    cik_cleanup           : {$IFDEF FPC}@{$ENDIF}qe_cleanup;
    cik_keywords          : {$IFDEF FPC}@{$ENDIF}qe_keywords;
    cik_get_kernel_file   : {$IFDEF FPC}@{$ENDIF}qe_get_kernel_file;
    cik_set_kernel_file   : {$IFDEF FPC}@{$ENDIF}qe_set_kernel_file;
    cik_get_program_file  : {$IFDEF FPC}@{$ENDIF}qe_get_program_file;
    cik_set_program_file  : {$IFDEF FPC}@{$ENDIF}qe_set_program_file;
    cik_load_config       : {$IFDEF FPC}@{$ENDIF}qe_load_config
  );

implementation

uses
  Math, DateUtils, lse_symbol, lse_api, lse_cgi;

function qe_param_engine(const Param: PLseParam): PLseEngine;cdecl;
begin
  try
    if Param <> nil then
      Result := __AsEngine(Param).EngineRec else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure qe_engine_destroy(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Free;
  except
    { do nothing }
  end;
end;

function qe_engine_compile(const Engine: pointer; const code: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileCode(code));
  except
    Result := 0;
  end;
end;

function qe_engine_compile_file(const Engine: pointer; const fname: pchar): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileFile(fname));
  except
    Result := 0;
  end;
end;

procedure qe_begin_cgi(const Engine: pointer);cdecl;
var
  eng: KLiEngine;
  CGI: KLiCGI;
begin
  try
    eng := KLiEngine(Engine);
    if eng.CGI = nil then
    begin
      CGI := KLiCGI.Create(eng);
      if CGI.HasError then CGI.Free;
    end;
  except
    { do nothing }
  end;
end;

procedure qe_end_cgi(const Engine: pointer);cdecl;
begin
  try
    if KLiEngine(Engine).CGI <> nil then
      KLiEngine(Engine).CGI.Free;
  except
    { do nothing }
  end;
end;

function qe_engine_execute(const Engine: pointer; const code: pchar): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    E := KLiEngine(Engine);
    E.PrepareCompile;
    {if IsLsp <> 0 then
    begin
      qe_begin_cgi(Engine);
      try
        if E.CGI <> nil then
          Result := Ord(E.TryExecuteCode(code, true)) else
          Result := 0;
      finally
        qe_end_cgi(Engine);
      end;
    end
    else}
    Result := Ord(E.TryExecuteCode(code));
  except
    Result := 0;
  end;
end;

function qe_engine_execute_file(const Engine: pointer; const fname: pchar): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    E := KLiEngine(Engine);
    E.PrepareCompile;
    {if IsLsp <> 0 then
    begin
      qe_begin_cgi(Engine);
      try
        if E.CGI <> nil then
          Result := Ord(E.TryExecuteFile(fname, true)) else
          Result := 0;
      finally
        qe_end_cgi(Engine);
      end;
    end
    else}
    Result := Ord(E.TryExecuteFile(fname));
  except
    Result := 0;
  end;
end;

procedure qe_engine_terminate(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Terminate;
  except
    { do nothing }
  end;
end;

procedure qe_engine_clear(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Clear;
  except
    { do nothing }
  end;
end;

function qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_strec_alloc(KLiEngine(Engine).Arguments.Text);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Arguments.Text := Args;
  except
    { do nothing }
  end;
end;

function qe_engine_errno(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.errno;
  except
    Result := -1;
  end;
end;

function qe_engine_error_row(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Row;
  except
    Result := -1;
  end;
end;

function qe_engine_error_col(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Col;
  except
    Result := -1;
  end;
end;

function qe_engine_error_name(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Name);
  except
    Result := nil;
  end;
end;

function qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Msg);
  except
    Result := nil;
  end;
end;

function qe_engine_error_module(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.Module);
  except
    Result := nil;
  end;
end;

function qe_engine_error_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).Error.ModuleFile);
  except
    Result := nil;
  end;
end;

function qe_engine_result_type(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultType);
  except
    Result := nil;
  end;
end;

function qe_engine_result_text(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultText);
  except
    Result := nil;
  end;
end;

function qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainSearchPath);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainSearchPath := Trim(Path);
  except
    { do nothing }
  end;
end;

function qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainFile);
  except
    Result := nil;
  end;
end;

procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainFile := fname;
  except
    { do nothing }
  end;
end;

function qe_engine_ready(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Ready);
  except
    Result := 0;
  end;
end;

function qe_engine_running(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Running);
  except
    Result := 0;
  end;
end;

function qe_engine_terminated(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Terminated);
  except
    Result := 0;
  end;
end;

function qe_engine_exited(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Exited);
  except
    Result := 0;
  end;
end;

procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
begin
  try
    lse_stream_write(KLiEngine(Engine).Output, Text, Count);
  except
    { do nothing }
  end;
end;

function qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
begin
  try
    Result := lse_stream_read(KLiEngine(Engine).Input, Buf, Count);
  except
    Result := 0;
  end;
end;

function qe_engine_readln(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_stream_readln(KLiEngine(Engine).Input);
  except
    Result := nil;
  end;
end;

function qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
begin
  try
    Result := KLiEngine.Create(EngineRec);
  except
    Result := nil;
  end;
end;

function qe_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
begin
  try
    if Param <> nil then
      Result := lse_strec_alloc(__AsRunner(Param).FormatFor(Fmt, nil)) else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    __SetError(PLseParam(Param), ID, Errno, Msg);
  except
    { do nothing }
  end;
end;

function qe_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := __AsRunner(Param);
    Result := rnnr.Stack.Count;
    rnnr.Stack.Add(Value);
  except
    Result := -1;
  end;
end;

function qe_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := __AsRunner(Param);
    Result := Ord(rnnr.Goon(KLiFunc(Func), Params, ResValue));
  except
    Result := 0;
  end;
end;

function qe_register_module(const Name: pchar; const initrec: PLseModule): pointer;cdecl;
begin
  try
    Result := __SetupModule(Name, initrec);
  except
    Result := nil;
  end;
end;

function qe_register_type(const CR: PLseType): PLseType;cdecl;
begin
  try
    Result := nil;
    if CR <> nil then
    begin
      CR^.cr_class := sys_module.SetupType(CR);
      if CR^.cr_class <> nil then
        Result := KLiType(CR^.cr_class).TypeRec;
    end;
  except
    Result := nil;
  end;
end;

function qe_typerec(const KernelType: pointer): PLseType;cdecl;
begin
  try
    if KernelType <> nil then
      Result := KLiType(KernelType).TypeRec else
      Result := nil;
  except
    Result := nil;
  end;
end;

function  qe_register_func(const FR: PLseFunc): pointer;cdecl;
begin
  try
    Result := sys_module.SetupFunc(FR);
  except
    Result := nil;
  end;
end;

procedure qe_casto_string(const V: PLseValue);cdecl;
begin
  __SetTypeValue(nil, V, KT_STRING);
end;

function qe_simple_test(const Script: pchar): integer;cdecl;
begin
  try
    Result := lse_symbol.SimpleTest(Script);
  except
    Result := SCT_ERROR;
  end;
end;

function qe_startup: integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := 1;
  except
    Result := 0;
  end;
end;

procedure qe_cleanup;cdecl;
begin
  try
    { do nothing }
  except
    { do nothing }
  end;
end;

function qe_keywords: pchar;cdecl;
begin
  try
    ReservedWords;
    Result := pchar(reserved_words);
  except
    Result := nil;
  end;
end;

function qe_get_kernel_file: pchar;cdecl;
begin
  try
    Result := pchar(sys_kernel);
  except
    Result := nil;
  end;
end;

procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
begin
  try
    sys_kernel := lse_expand_fname(KernelFile);
    __LoadConfig('');
  except
    { do nothing }
  end;
end;

function qe_get_program_file: pchar;cdecl;
begin
  try
    Result := pchar(sys_program);
  except
    Result := nil;
  end;
end;

procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
begin
  try
    sys_program := lse_expand_fname(ProgramFile);
  except
    { do nothing }
  end;
end;

procedure qe_load_config(const ConfigFile: pchar);cdecl;
begin
  try
    __LoadConfig(lse_expand_fname(ConfigFile));
  except
    { do nothing }
  end;
end;

function qe_production: pchar;cdecl;
begin
  try
    Result := LSE_ID;
  except
    Result := LSE_ID;
  end;
end;

function qe_version: pchar;cdecl;
begin
  try
    Result := pchar(sys_version);
  except
    Result := pchar(sys_version);
  end;
end;

function qe_copyright: pchar;cdecl;
begin
  try
    Result := LSE_COPYRIGHT;
  except
    Result := LSE_COPYRIGHT;
  end;
end;

function qe_tmpath: pchar;cdecl;
begin
  try
    Result := pchar(sys_tmpath);
  except
    Result := pchar(sys_tmpath);
  end;
end;

function qe_query(const ID: pchar): pointer;cdecl;
var
  name: string;
begin
  try
    Result := nil;
    name := LowerCase(Trim(ID));
    if name = 'qe_entries'  then
      Result := @qe_entries;
  except
    Result := nil;
  end;
end;

initialization
begin
  lse_entries := @qe_entries;
end;

end.
