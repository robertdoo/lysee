{==============================================================================}
{        UNIT: lse_export                                                      }
{ DESCRIPTION: binary interface between lseu and lse_kernel                    }
{     CREATED: 2004/04/12                                                      }
{    MODIFIED: 2010/10/04                                                      }
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
function  qe_module_count: integer;cdecl;
function  qe_get_module(Index: integer): pointer;cdecl;
function  qe_setup_module(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
function  qe_find_module(const Name: pchar): pointer;cdecl;
function  qe_module_class(const Module: pointer): PLseClassRec;cdecl;
function  qe_setup_class(const Module: pointer; const CR: PLseClassRec): PLseClassRec;cdecl;
function  qe_find_class(const Name: pchar): PLseClassRec;cdecl;
function  qe_class_module(const ClassRec: PLseClassRec): pointer;cdecl;
function  qe_setup_method(const ClassRec: PLseClassRec; const FR: PLseFuncRec): pointer;cdecl;
function  qe_find_method(const ClassRec: PLseClassRec; const Name: pchar): pointer;cdecl;
function  qe_method_name(const Method: pointer): pchar;cdecl;
function  qe_param_engine(const Param: PLseParam): PLseEngine;cdecl;
function  qe_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
function  qe_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
function  qe_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
procedure qe_value_set_object(const Data, obj, obj_class: pointer);cdecl;
procedure qe_value_set_stream(const Data: pointer; Value: PLseStream);cdecl;
function  qe_dbv_provide(const Vendor: pchar): PLseDB;cdecl;
function  qe_dbv_register(const dbv: PLseDBVendor): integer;cdecl;
function  qe_database_encode_TUPSP(const Target, User, Password, Source, Params: pchar): PLseString;cdecl;
procedure qe_database_decode_TUPSP(const ConnectionStr: pchar; const TUPSP: PLseTUPSP);cdecl;
function  qe_simple_test(const Script: pchar): integer;cdecl;
function  qe_startup: integer;cdecl;
procedure qe_cleanup;cdecl;
function  qe_reserved_words: pchar;cdecl;
function  qe_get_kernel_file: pchar;cdecl;
procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
function  qe_get_program_file: pchar;cdecl;
procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
procedure qe_load_config(const ConfigFile: pchar);cdecl;
function  qe_production: pchar;cdecl;
function  qe_version: pchar;cdecl;
function  qe_copyright: pchar;cdecl;
function  qe_tmpath: pchar;cdecl;
function  qe_expand_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
function  qe_complete_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
procedure qe_log(const Msg: pchar; Count: integer);cdecl;
function  QueryEntry(const ID: pchar): pointer;cdecl;

var
  qe_entries: RLseEntryRec = (
    cik_classes         : @sys_class_list;
    cik_create_engine   : {$IFDEF FPC}@{$ENDIF}qe_engine_create;
    cik_module_count    : {$IFDEF FPC}@{$ENDIF}qe_module_count;
    cik_get_module      : {$IFDEF FPC}@{$ENDIF}qe_get_module; 
    cik_setup_module    : {$IFDEF FPC}@{$ENDIF}qe_setup_module;
    cik_find_module     : {$IFDEF FPC}@{$ENDIF}qe_find_module;
    cik_module_class    : {$IFDEF FPC}@{$ENDIF}qe_module_class;
    cik_setup_class     : {$IFDEF FPC}@{$ENDIF}qe_setup_class;
    cik_find_class      : {$IFDEF FPC}@{$ENDIF}qe_find_class;
    cik_class_module    : {$IFDEF FPC}@{$ENDIF}qe_class_module;
    cik_setup_method    : {$IFDEF FPC}@{$ENDIF}qe_setup_method;
    cik_find_method     : {$IFDEF FPC}@{$ENDIF}qe_find_method;
    cik_method_name     : {$IFDEF FPC}@{$ENDIF}qe_method_name; 
    cik_get_engine      : {$IFDEF FPC}@{$ENDIF}qe_param_engine;
    cik_format          : {$IFDEF FPC}@{$ENDIF}qe_param_format;
    cik_set_error       : {$IFDEF FPC}@{$ENDIF}qe_param_error;
    cik_push            : {$IFDEF FPC}@{$ENDIF}qe_push;
    cik_goon            : {$IFDEF FPC}@{$ENDIF}qe_goon;
    cik_set_object      : {$IFDEF FPC}@{$ENDIF}qe_value_set_object;
    cik_set_stream      : {$IFDEF FPC}@{$ENDIF}qe_value_set_stream;
    { database }
    cik_dbv_provide     : {$IFDEF FPC}@{$ENDIF}qe_dbv_provide;
    cik_dbv_register    : {$IFDEF FPC}@{$ENDIF}qe_dbv_register;
    cik_encode_TUPSP    : {$IFDEF FPC}@{$ENDIF}qe_database_encode_TUPSP;
    cik_decode_TUPSP    : {$IFDEF FPC}@{$ENDIF}qe_database_decode_TUPSP;
    { others }
    cik_production      : {$IFDEF FPC}@{$ENDIF}qe_production;
    cik_version         : {$IFDEF FPC}@{$ENDIF}qe_version;
    cik_copyright       : {$IFDEF FPC}@{$ENDIF}qe_copyright;
    cik_tmpath          : {$IFDEF FPC}@{$ENDIF}qe_tmpath;
    cik_query           : {$IFDEF FPC}@{$ENDIF}QueryEntry;
    cik_malloc          : {$IFDEF FPC}@{$ENDIF}lse_mem_alloc;
    cik_free            : {$IFDEF FPC}@{$ENDIF}lse_mem_free;
    cik_simple_test     : {$IFDEF FPC}@{$ENDIF}qe_simple_test;
    cik_startup         : {$IFDEF FPC}@{$ENDIF}qe_startup;
    cik_cleanup         : {$IFDEF FPC}@{$ENDIF}qe_cleanup;
    cik_reserved_words  : {$IFDEF FPC}@{$ENDIF}qe_reserved_words;
    cik_get_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_get_kernel_file;
    cik_set_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_set_kernel_file;
    cik_get_program_file: {$IFDEF FPC}@{$ENDIF}qe_get_program_file;
    cik_set_program_file: {$IFDEF FPC}@{$ENDIF}qe_set_program_file;
    cik_load_config     : {$IFDEF FPC}@{$ENDIF}qe_load_config;
    cik_expand_fname    : {$IFDEF FPC}@{$ENDIF}qe_expand_fname;
    cik_complete_fname  : {$IFDEF FPC}@{$ENDIF}qe_complete_fname;
    cik_log             : {$IFDEF FPC}@{$ENDIF}qe_log;
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
    __log('qe_param_engine()', lse_exception_str);
  end;
end;

procedure qe_engine_destroy(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Free;
  except
    __log('qe_engine_destroy()', lse_exception_str);
  end;
end;

function qe_engine_compile(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileCode(code, IsLsp <> 0));
  except
    Result := 0;
    __log('qe_engine_compile()', lse_exception_str);
  end;
end;

function qe_engine_compile_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
begin
  try
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileFile(fname, IsLsp <> 0));
  except
    Result := 0;
    __log('qe_engine_compile_file()', lse_exception_str);
  end;
end;

procedure qe_begin_cgi(const Engine: pointer);cdecl;
var
  eng: KLiEngine;
  CGI: KLiCGI;
begin
  try
    __log('qe_begin_cgi()');
    eng := KLiEngine(Engine);
    if eng.CGI = nil then
    begin
      CGI := KLiCGI.Create(eng);
      if CGI.HasError then CGI.Free;
    end;
  except
    __log('qe_begin_cgi()', lse_exception_str);
  end;
end;

procedure qe_end_cgi(const Engine: pointer);cdecl;
begin
  try
    if KLiEngine(Engine).CGI <> nil then
      KLiEngine(Engine).CGI.Free;
    __log('qe_end_cgi()');
  except
    __log('qe_end_cgi()', lse_exception_str);
  end;
end;

function qe_engine_execute(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    __log('qe_engine_execute()');
    E := KLiEngine(Engine);
    E.PrepareCompile;
    if IsLsp <> 0 then
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
    else Result := Ord(E.TryExecuteCode(code, false));
  except
    Result := 0;
    __log('qe_engine_execute()', lse_exception_str);
  end;
end;

function qe_engine_execute_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    __log('qe_engine_execute_file()');
    E := KLiEngine(Engine);
    E.PrepareCompile;
    if IsLsp <> 0 then
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
    else Result := Ord(E.TryExecuteFile(fname, false));
  except
    Result := 0;
    __log('qe_engine_execute_file()', lse_exception_str);
  end;
end;

procedure qe_engine_terminate(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Terminate;
  except
    __log('qe_engine_terminate()', lse_exception_str);
  end;
end;

procedure qe_engine_clear(const Engine: pointer);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Clear;
  except
    __log('qe_engine_clear()', lse_exception_str);
  end;
end;

function qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_strec_alloc(KLiEngine(Engine).Arguments.Text);
  except
    Result := nil;
    __log('qe_engine_get_args()', lse_exception_str);
  end;
end;

procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
begin
  try
    if Engine <> nil then
      KLiEngine(Engine).Arguments.Text := Args;
  except
    __log('qe_engine_set_args()', lse_exception_str);
  end;
end;

function qe_engine_errno(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.errno;
  except
    Result := -1;
    __log('qe_engine_errno()', lse_exception_str);
  end;
end;

function qe_engine_error_row(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Row;
  except
    Result := -1;
    __log('qe_engine_error_row()', lse_exception_str);
  end;
end;

function qe_engine_error_col(const Engine: pointer): integer;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.Col;
  except
    Result := -1;
    __log('qe_engine_error_col()', lse_exception_str);
  end;
end;

function qe_engine_error_name(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.ErrorRec^.error;
  except
    Result := nil;
    __log('qe_engine_error_name()', lse_exception_str);
  end;
end;

function qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.ErrorRec^.errmsg;
  except
    Result := nil;
    __log('qe_engine_error_msg()', lse_exception_str);
  end;
end;

function qe_engine_error_module(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.ErrorRec^.module;
  except
    Result := nil;
    __log('qe_engine_error_module()', lse_exception_str);
  end;
end;

function qe_engine_error_ifile(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.ErrorRec^.ifname;
  except
    Result := nil;
    __log('qe_engine_error_ifile()', lse_exception_str);
  end;
end;

function qe_engine_result_type(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultType);
  except
    Result := nil;
    __log('qe_engine_result_type()', lse_exception_str);
  end;
end;

function qe_engine_result_text(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).ExitResultText);
  except
    Result := nil;
    __log('qe_engine_result_text()', lse_exception_str);
  end;
end;

function qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainSearchPath);
  except
    Result := nil;
    __log('qe_engine_get_search_path()', lse_exception_str);
  end;
end;

procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainSearchPath := Trim(Path);
  except
    __log('qe_engine_set_search_path()', lse_exception_str);
  end;
end;

function qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := pchar(KLiEngine(Engine).MainFile);
  except
    Result := nil;
    __log('qe_engine_get_main_file()', lse_exception_str);
  end;
end;

procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
begin
  try
    KLiEngine(Engine).MainFile := fname;
  except
    __log('qe_engine_set_main_file()', lse_exception_str);
  end;
end;

function qe_engine_ready(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Ready);
  except
    Result := 0;
    __log('qe_engine_ready()', lse_exception_str);
  end;
end;

function qe_engine_running(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Running);
  except
    Result := 0;
    __log('qe_engine_running()', lse_exception_str);
  end;
end;

function qe_engine_terminated(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Terminated);
  except
    Result := 0;
    __log('qe_engine_terminated()', lse_exception_str);
  end;
end;

function qe_engine_exited(const Engine: pointer): integer;cdecl;
begin
  try
    Result := Ord(KLiEngine(Engine).Exited);
  except
    Result := 0;
    __log('qe_engine_exited()', lse_exception_str);
  end;
end;

procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
begin
  try
    lse_stream_write(KLiEngine(Engine).StdoutStream, Text, Count);
  except
    __log('qe_engine_write()', lse_exception_str);
  end;
end;

function qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
begin
  try
    Result := lse_stream_read(KLiEngine(Engine).StdinStream, Buf, Count);
  except
    Result := 0;
    __log('qe_engine_read()', lse_exception_str);
  end;
end;

function qe_engine_readln(const Engine: pointer): PLseString;cdecl;
begin
  try
    Result := lse_stream_readln(KLiEngine(Engine).StdinStream);
  except
    Result := nil;
    __log('qe_engine_readln()', lse_exception_str);
  end;
end;

function qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
begin
  try
    EngineRec^.krnl_destroy := @qe_engine_destroy;
    EngineRec^.krnl_compile := @qe_engine_compile;
    EngineRec^.krnl_compile_file := @qe_engine_compile_file;
    EngineRec^.krnl_execute := @qe_engine_execute;
    EngineRec^.krnl_execute_file := @qe_engine_execute_file;
    EngineRec^.krnl_terminate := @qe_engine_terminate;
    EngineRec^.krnl_clear := @qe_engine_clear;
    EngineRec^.krnl_get_args := @qe_engine_get_args;
    EngineRec^.krnl_set_args := @qe_engine_set_args;
    EngineRec^.krnl_result_type := @qe_engine_result_type;
    EngineRec^.krnl_result_text := @qe_engine_result_text;
    EngineRec^.krnl_errno := @qe_engine_errno;
    EngineRec^.krnl_error_row := @qe_engine_error_row;
    EngineRec^.krnl_error_col := @qe_engine_error_col;
    EngineRec^.krnl_error_name := @qe_engine_error_name;
    EngineRec^.krnl_error_msg := @qe_engine_error_msg;
    EngineRec^.krnl_error_module := @qe_engine_error_module;
    EngineRec^.krnl_error_ifile := @qe_engine_error_ifile;
    EngineRec^.krnl_get_search_path := @qe_engine_get_search_path;
    EngineRec^.krnl_set_search_path := @qe_engine_set_search_path;
    EngineRec^.krnl_get_main_file := @qe_engine_get_main_file;
    EngineRec^.krnl_set_main_file := @qe_engine_set_main_file;
    EngineRec^.krnl_ready := @qe_engine_ready;
    EngineRec^.krnl_running := @qe_engine_running;
    EngineRec^.krnl_terminated := @qe_engine_terminated;
    EngineRec^.krnl_exited := @qe_engine_exited;
    EngineRec^.krnl_write := @qe_engine_write;
    EngineRec^.krnl_read := @qe_engine_read;
    EngineRec^.krnl_readln := @qe_engine_readln;
    EngineRec^.krnl_begin_cgi := @qe_begin_cgi;
    EngineRec^.krnl_end_cgi := @qe_end_cgi;
    Result := KLiEngine.Create(EngineRec);
  except
    Result := nil;
    __log('qe_engine_create()', lse_exception_str);
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
    __log('qe_param_format()', lse_exception_str);
  end;
end;

procedure qe_value_set_Object(const Data, obj, obj_class: pointer);cdecl;
begin
  try
    __SetObject(Data, KLiClass(obj_class), obj);
  except
    __log('qe_value_set_Object()', lse_exception_str);
  end;
end;

procedure qe_value_set_stream(const Data: pointer; Value: PLseStream);cdecl;
begin
  try
    __SetStream(Data, Value);
  except
    __log('qe_value_set_stream()', lse_exception_str);
  end;
end;

procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    __SetError(PLseParam(Param), ID, Errno, Msg);
  except
    __log('qe_param_error()', lse_exception_str);
  end;
end;

function qe_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := __AsRunner(Param);
    Result := rnnr.Stack.Count;
    rnnr.Stack.Push(Value);
  except
    __log('qe_push()', lse_exception_str);
    Result := -1;
  end;
end;

function qe_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := __AsRunner(Param);
    Result := Ord(rnnr.Goon(KLiFunc(Func), Params, ResValue));
  except
    __log('qe_goon()', lse_exception_str);
    Result := 0;
  end;
end;

function qe_module_count: integer;cdecl;
begin
  try
    if sys_libraries <> nil then
      Result := sys_libraries.Count else
      Result := 0;
  except
    Result := 0;
    __log('qe_module_count()', lse_exception_str);
  end;
end;

function qe_get_module(Index: integer): pointer;cdecl;
begin
  try
    if sys_libraries <> nil then
      Result := pointer(sys_libraries.Objects[Index]) else
      Result := nil;
  except
    Result := nil;
    __log('qe_get_module()', lse_exception_str);
  end;
end;

function qe_setup_module(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
begin
  try
    Result := __SetupModule(Name, initrec);
  except
    Result := nil;
    __log('qe_setup_module()', lse_exception_str);
  end;
end;

function qe_find_module(const Name: pchar): pointer;cdecl;
var
  m_name: string;
begin
  try
    m_name := Trim(Name);
    if m_name <> '' then
      Result := __findNamed(sys_libraries, m_name) else
      Result := nil;
  except
    Result := nil;
    __log('qe_find_module()', lse_exception_str);
  end;
end;

function qe_module_class(const Module: pointer): PLseClassRec;cdecl;
begin
  try
    if Module <> nil then
      Result := KLiModule(Module).ModuleClass.ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_module_class()', lse_exception_str);
  end;
end;

function qe_setup_class(const Module: pointer; const CR: PLseClassRec): PLseClassRec;cdecl;
begin
  try
    Result := nil;
    if (Module <> nil) and (CR <> nil) then
    begin
      __SetupClasses(@CR^, 1, KLiModule(Module));
      if CR^.lysee_class <> nil then
        Result := KLiClass(CR^.lysee_class).ClassRec;
    end;
  except
    Result := nil;
    __log('qe_setup_class()', lse_exception_str);
  end;
end;

function qe_find_class(const Name: pchar): PLseClassRec;cdecl;
var
  m_name, c_name: string;
  module: KLiModule;
  clss: KLiClass;
begin
  try
    Result := nil;
    m_name := '';
    c_name := __decodeClassName(Name, m_name);
    if m_name <> '' then
      module := KLiModule(__findNamed(sys_libraries, m_name)) else
      module := sys_module;
    if module <> nil then
    begin
      clss := module.FindClass(c_name);
      if clss <> nil then
        Result := clss.ClassRec;
    end;
  except
    Result := nil;
    __log('qe_find_class()', lse_exception_str);
  end;
end;

function qe_class_module(const ClassRec: PLseClassRec): pointer;cdecl;
begin
  try
    if ClassRec <> nil then
      Result := KLiClass(ClassRec^.lysee_class).Module else
      Result := nil;
  except
    Result := nil;
    __log('qe_class_module()', lse_exception_str);
  end;
end;

function qe_setup_method(const ClassRec: PLseClassRec; const FR: PLseFuncRec): pointer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := nil;
    if ClassRec <> nil then
    begin
      clss := KLiClass(ClassRec^.lysee_class); 
      Result := clss.SetupMethod(FR);
    end;
  except
    Result := nil;
    __log('qe_setup_method()', lse_exception_str);
  end;
end;

function qe_find_method(const ClassRec: PLseClassRec; const Name: pchar): pointer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := nil;
    if ClassRec <> nil then
    begin
      clss := KLiClass(ClassRec^.lysee_class);
      if clss.IsModuleClass then
        Result := clss.Module.FindFunc(Trim(Name)) else
        Result := clss.FindMethod(cmMethod, Trim(Name));
    end;
  except
    Result := nil;
    __log('qe_find_method()', lse_exception_str);
  end;
end;

function qe_method_name(const Method: pointer): pchar;cdecl;
begin
  try
    if Method <> nil then
      Result := pchar(KLiFunc(Method).Name) else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_name()', lse_exception_str);
  end;
end;

function qe_simple_test(const Script: pchar): integer;cdecl;
begin
  try
    Result := lse_symbol.SimpleTest(Script);
  except
    Result := SCT_ERROR;
    __log('qe_simple_test()', lse_exception_str);
  end;
end;

function qe_dbv_provide(const Vendor: pchar): PLseDB;cdecl;
begin
  try
    Result := dbv_provide(Vendor);
  except
    Result := nil;
    __log('qe_database_new()', lse_exception_str);
  end;
end;

function qe_dbv_register(const dbv: PLseDBVendor): integer;cdecl;
begin
  try
    Result := Ord(dbv_register(dbv));
  except
    Result := 0;
    __log('qe_database_register_vendor()', lse_exception_str);
  end;
end;

function qe_database_encode_TUPSP(const Target, User, Password, Source, Params: pchar): PLseString;cdecl;
var
  line: string;
begin
  try
    line := lse_encode_TUPSP(Target, User, Password, Source, Params);
    Result := lse_strec_alloc(line);
  except
    Result := nil;
    __log('qe_database_encode_TUPSP()', lse_exception_str);
  end;
end;

procedure qe_database_decode_TUPSP(const ConnectionStr: pchar; const TUPSP: PLseTUPSP);cdecl;
var
  target, user, password, source, params: string;
begin
  try
    lse_decode_TUPSP(ConnectionStr, target, user, password, source, params);
    TUPSP^.target := lse_strec_alloc(target);
    TUPSP^.user := lse_strec_alloc(user);
    TUPSP^.password := lse_strec_alloc(password);
    TUPSP^.source := lse_strec_alloc(source);
    TUPSP^.params := lse_strec_alloc(params);
  except
    __log('qe_database_decode_TUPSP()', lse_exception_str);
  end;
end;

function qe_startup: integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := 1;
  except
    Result := 0;
    __log('qe_startup()', lse_exception_str);
  end;
end;

procedure qe_cleanup;cdecl;
begin
  try
    { do nothing }
  except
    __log('qe_cleanup()', lse_exception_str);
  end;
end;

function qe_reserved_words: pchar;cdecl;
begin
  try
    ReservedWords;
    Result := pchar(reserved_words);
  except
    Result := nil;
    __log('qe_reserved_words()', lse_exception_str);
  end;
end;

function qe_get_kernel_file: pchar;cdecl;
begin
  try
    Result := pchar(sys_kernel);
  except
    Result := nil;
    __log('qe_get_kernel_file()', lse_exception_str);
  end;
end;

procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
begin
  try
    sys_kernel := lse_expand_fname(KernelFile);
    __LoadConfig('');
  except
    __log('qe_set_kernel_file()', lse_exception_str);
  end;
end;

function qe_get_program_file: pchar;cdecl;
begin
  try
    Result := pchar(sys_program);
  except
    Result := nil;
    __log('qe_get_program_file()', lse_exception_str);
  end;
end;

procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
begin
  try
    sys_program := lse_expand_fname(ProgramFile);
  except
    __log('qe_set_program_file()', lse_exception_str);
  end;
end;

procedure qe_load_config(const ConfigFile: pchar);cdecl;
begin
  try
    __LoadConfig(lse_expand_fname(ConfigFile));
  except
    __log('qe_set_program_file()', lse_exception_str);
  end;
end;

function qe_production: pchar;cdecl;
begin
  try
    Result := LSE_ID;
  except
    Result := LSE_ID;
    __log('qe_production()', lse_exception_str);
  end;
end;

function qe_version: pchar;cdecl;
begin
  try
    Result := pchar(sys_version);
  except
    Result := pchar(sys_version);
    __log('qe_version()', lse_exception_str);
  end;
end;

function qe_copyright: pchar;cdecl;
begin
  try
    Result := LSE_COPYRIGHT;
  except
    Result := LSE_COPYRIGHT;
    __log('qe_copyright()', lse_exception_str);
  end;
end;

function qe_tmpath: pchar;cdecl;
begin
  try
    Result := pchar(sys_tmpath);
  except
    Result := pchar(sys_tmpath);
    __log('qe_tmpath()', lse_exception_str);
  end;
end;

function qe_expand_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
var
  S: string;
begin
  try
    Result := 0;
    if (buf <> nil) and (buf_size > 0) then
    begin
      S := lse_expand_fname(fname);
      Result := Max(0, Min(Length(S), buf_size - 1));
      if Result > 0 then
        StrPLCopy(buf, S, Result) else
        buf[0] := #0;
    end;
  except
    Result := 0;
    __log('qe_expand_fname()', lse_exception_str);
  end;
end;

function qe_complete_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
var
  S: string;
begin
  try
    Result := 0;
    if (buf <> nil) and (buf_size > 0) then
    begin
      S := lse_complete_fname(fname);
      Result := Max(0, Min(Length(S), buf_size - 1));
      if Result > 0 then
        StrPLCopy(buf, S, Result) else
        buf[0] := #0;
    end;
  except
    Result := 0;
    __log('qe_complete_fname()', lse_exception_str);
  end;
end;

procedure qe_log(const Msg: pchar; Count: integer);cdecl;
begin
  try
    __log(Msg, Count);
  except
    { do nothing }
  end;
end;

function QueryEntry(const ID: pchar): pointer;cdecl;
var
  name: string;
begin
  try
    Result := nil;
    name := LowerCase(Trim(ID));
    if name = 'qe_entries'  then Result := @qe_entries;
  except
    Result := nil;
    __log('QueryEntry()', lse_exception_str);
  end;
end;

initialization
begin
  lse_entries := @qe_entries;
end;

end.
