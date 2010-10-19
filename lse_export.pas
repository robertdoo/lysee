{==============================================================================}
{        UNIT: lse_export                                                      }
{ DESCRIPTION: binary interface between lseu and lse_kernel                    }
{     CREATED: 2004/04/12                                                      }
{    MODIFIED: 2010/10/18                                                      }
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
function  qe_engine_compile(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
function  qe_engine_compile_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
function  qe_engine_execute(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
function  qe_engine_execute_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
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
function  qe_module_count: integer;cdecl;
function  qe_module(Index: integer): pointer;cdecl;
function  qe_module_setup(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
function  qe_module_find(const Name: pchar): pointer;cdecl;
function  qe_module_class(const Module: pointer): PLseClassRec;cdecl;
function  qe_class_count(const Module: pointer): integer;cdecl;
function  qe_class(const Module: pointer; Index: integer): PLseClassRec;cdecl;
function  qe_class_setup(const Module: pointer; const CR: PLseClassRec): PLseClassRec;cdecl;
function  qe_class_find(const Name: pchar): PLseClassRec;cdecl;
function  qe_class_module(const CR: PLseClassRec): pointer;cdecl;
function  qe_class_rec(const KernelClass: pointer): PLseClassRec;cdecl;
function  qe_method_count(const CR: PLseClassRec): integer;cdecl;
function  qe_method_get(const CR: PLseClassRec; Index: integer): pointer;cdecl;
function  qe_method_setup(const CR: PLseClassRec; const FR: PLseFuncRec): pointer;cdecl;
function  qe_find_method(const CR: PLseClassRec; const Name: pchar): pointer;cdecl;
function  qe_method_name(const Method: pointer): pchar;cdecl;
function  qe_method_type(const Method: pointer): PLseClassRec;cdecl;
function  qe_method_class(const Method: pointer): PLseClassRec;cdecl;
function  qe_method_param_count(const Method: pointer): integer;cdecl;
function  qe_method_param_name(const Method: pointer; Index: integer): pchar;cdecl;
function  qe_method_param_type(const Method: pointer; Index: integer): PLseClassRec;cdecl;
function  qe_method_bind(const Method: pointer; Data: pointer): pointer;cdecl;
function  qe_method_get_bind(const Method: pointer): pointer;cdecl;
function  qe_method_is_creator(const Method: pointer): integer;cdecl;
function  qe_method_get_proc(const Method: pointer): pointer;cdecl;
function  qe_method_set_proc(const Method, NewProc: pointer): pointer;cdecl;
function  qe_param_engine(const Param: PLseParam): PLseEngine;cdecl;
function  qe_param_format(const Param: PLseParam; const Fmt: pchar): PLseString;cdecl;
procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
function  qe_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
function  qe_param_goon(const Param: PLseParam; Func: pointer; Params: integer; const ResValue: PLseValue): integer;cdecl;
function  qe_dbv_provide(const Vendor: pchar): PLseDB;cdecl;
function  qe_dbv_register(const dbv: PLseDBVendor): integer;cdecl;
function  qe_database_encode_TUPSP(const Target, User, Password, Source, Params: pchar): PLseString;cdecl;
procedure qe_database_decode_TUPSP(const ConnectionStr: pchar; const TUPSP: PLseTUPSP);cdecl;
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
procedure qe_log(const Msg: pchar; Count: integer);cdecl;
function  qe_query(const ID: pchar): pointer;cdecl;

var
  qe_entries: RLseEntryRec = (
    cik_classes           : @sys_class_list;
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
    { module }
    cik_module_count      : {$IFDEF FPC}@{$ENDIF}qe_module_count;
    cik_module            : {$IFDEF FPC}@{$ENDIF}qe_module; 
    cik_module_setup      : {$IFDEF FPC}@{$ENDIF}qe_module_setup;
    cik_module_find       : {$IFDEF FPC}@{$ENDIF}qe_module_find;
    cik_module_class      : {$IFDEF FPC}@{$ENDIF}qe_module_class;
    { class }
    cik_class_count       : {$IFDEF FPC}@{$ENDIF}qe_class_count;
    cik_class             : {$IFDEF FPC}@{$ENDIF}qe_class; 
    cik_class_setup       : {$IFDEF FPC}@{$ENDIF}qe_class_setup;
    cik_class_find        : {$IFDEF FPC}@{$ENDIF}qe_class_find;
    cik_class_module      : {$IFDEF FPC}@{$ENDIF}qe_class_module;
    cik_class_rec         : {$IFDEF FPC}@{$ENDIF}qe_class_rec;
    { method }
    cik_method_count      : {$IFDEF FPC}@{$ENDIF}qe_method_count;
    cik_method            : {$IFDEF FPC}@{$ENDIF}qe_method_get;
    cik_method_setup      : {$IFDEF FPC}@{$ENDIF}qe_method_setup;
    cik_method_find       : {$IFDEF FPC}@{$ENDIF}qe_find_method;
    cik_method_name       : {$IFDEF FPC}@{$ENDIF}qe_method_name;
    cik_method_type       : {$IFDEF FPC}@{$ENDIF}qe_method_type;
    cik_method_class      : {$IFDEF FPC}@{$ENDIF}qe_method_class;
    cik_method_param_count: {$IFDEF FPC}@{$ENDIF}qe_method_param_count;
    cik_method_param_name : {$IFDEF FPC}@{$ENDIF}qe_method_param_name;
    cik_method_param_type : {$IFDEF FPC}@{$ENDIF}qe_method_param_type;
    cik_method_bind       : {$IFDEF FPC}@{$ENDIF}qe_method_bind;
    cik_method_get_bind   : {$IFDEF FPC}@{$ENDIF}qe_method_get_bind;
    cik_method_is_creator : {$IFDEF FPC}@{$ENDIF}qe_method_is_creator;
    cik_method_get_proc   : {$IFDEF FPC}@{$ENDIF}qe_method_get_proc;
    cik_method_set_proc   : {$IFDEF FPC}@{$ENDIF}qe_method_set_proc; 
    { param }
    cik_param_engine      : {$IFDEF FPC}@{$ENDIF}qe_param_engine;
    cik_param_format      : {$IFDEF FPC}@{$ENDIF}qe_param_format;
    cik_param_error       : {$IFDEF FPC}@{$ENDIF}qe_param_error;
    cik_param_push        : {$IFDEF FPC}@{$ENDIF}qe_param_push;
    cik_param_goon        : {$IFDEF FPC}@{$ENDIF}qe_param_goon;
    { database }
    cik_dbv_provide       : {$IFDEF FPC}@{$ENDIF}qe_dbv_provide;
    cik_dbv_register      : {$IFDEF FPC}@{$ENDIF}qe_dbv_register;
    cik_encode_TUPSP      : {$IFDEF FPC}@{$ENDIF}qe_database_encode_TUPSP;
    cik_decode_TUPSP      : {$IFDEF FPC}@{$ENDIF}qe_database_decode_TUPSP;
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
    cik_load_config       : {$IFDEF FPC}@{$ENDIF}qe_load_config;
    cik_log               : {$IFDEF FPC}@{$ENDIF}qe_log;
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

function qe_engine_error_file(const Engine: pointer): pchar;cdecl;
begin
  try
    Result := KLiEngine(Engine).Error.ErrorRec^.ifname;
  except
    Result := nil;
    __log('qe_engine_error_file()', lse_exception_str);
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

procedure qe_param_error(const Param: PLseParam; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    __SetError(PLseParam(Param), ID, Errno, Msg);
  except
    __log('qe_param_error()', lse_exception_str);
  end;
end;

function qe_param_push(const Param: PLseParam; const Value: PLseValue): integer;cdecl;
var
  rnnr: KLiRunner;
begin
  try
    rnnr := __AsRunner(Param);
    Result := rnnr.Stack.Count;
    rnnr.Stack.Push(Value);
  except
    __log('qe_param_push()', lse_exception_str);
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
    __log('qe_param_goon()', lse_exception_str);
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

function qe_module(Index: integer): pointer;cdecl;
begin
  try
    if sys_libraries <> nil then
      Result := pointer(sys_libraries.Objects[Index]) else
      Result := nil;
  except
    Result := nil;
    __log('qe_module()', lse_exception_str);
  end;
end;

function qe_module_setup(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
begin
  try
    Result := __SetupModule(Name, initrec);
  except
    Result := nil;
    __log('qe_module_setup()', lse_exception_str);
  end;
end;

function qe_module_find(const Name: pchar): pointer;cdecl;
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
    __log('qe_module_find()', lse_exception_str);
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

function qe_class_count(const Module: pointer): integer;cdecl;
begin
  try
    if Module <> nil then
      Result := KLiModule(Module).ClassCount else
      Result := 0;
  except
    Result := 0;
    __log('qe_class_count()', lse_exception_str);
  end;
end;

function qe_class(const Module: pointer; Index: integer): PLseClassRec;cdecl;
begin
  try
    if Module <> nil then
      Result := KLiModule(Module).GetClass(Index).ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_class()', lse_exception_str);
  end;
end;

function qe_class_setup(const Module: pointer; const CR: PLseClassRec): PLseClassRec;cdecl;
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
    __log('qe_class_setup()', lse_exception_str);
  end;
end;

function qe_class_find(const Name: pchar): PLseClassRec;cdecl;
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
    __log('qe_class_find()', lse_exception_str);
  end;
end;

function qe_class_module(const CR: PLseClassRec): pointer;cdecl;
begin
  try
    if CR <> nil then
      Result := KLiClass(CR^.lysee_class).Module else
      Result := nil;
  except
    Result := nil;
    __log('qe_class_module()', lse_exception_str);
  end;
end;

function qe_class_rec(const KernelClass: pointer): PLseClassRec;cdecl;
begin
  try
    if KernelClass <> nil then
      Result := KLiClass(KernelClass).ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_class_rec()', lse_exception_str);
  end;
end;

function qe_method_count(const CR: PLseClassRec): integer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := 0;
    if CR <> nil then
    begin
      clss := KLiClass(CR^.lysee_class);
      if clss.IsModuleClass then
        Result := clss.Module.FuncCount else
        Result := clss.MethodList.Count; 
    end;
  except
    Result := 0;
    __log('qe_method_count()', lse_exception_str);
  end;
end;

function qe_method_get(const CR: PLseClassRec; Index: integer): pointer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := nil;
    if CR <> nil then
    begin
      clss := KLiClass(CR^.lysee_class);
      if clss.IsModuleClass then
        Result := clss.Module.GetFunc(Index) else
        Result := clss.MethodList.Objects[Index]; 
    end;
  except
    Result := nil;
    __log('qe_method_get()', lse_exception_str);
  end;
end;

function qe_method_setup(const CR: PLseClassRec; const FR: PLseFuncRec): pointer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := nil;
    if CR <> nil then
    begin
      clss := KLiClass(CR^.lysee_class); 
      Result := clss.SetupMethod(FR);
    end;
  except
    Result := nil;
    __log('qe_method_setup()', lse_exception_str);
  end;
end;

function qe_find_method(const CR: PLseClassRec; const Name: pchar): pointer;cdecl;
var
  clss: KLiClass;
begin
  try
    Result := nil;
    if CR <> nil then
    begin
      clss := KLiClass(CR^.lysee_class);
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

function qe_method_type(const Method: pointer): PLseClassRec;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).ResultType.ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_type()', lse_exception_str);
  end;
end;

function qe_method_class(const Method: pointer): PLseClassRec;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).OwnerClass.ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_class()', lse_exception_str);
  end;
end;

function qe_method_param_count(const Method: pointer): integer;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).ParamCount else
      Result := 0;
  except
    Result := 0;
    __log('qe_method_param_count()', lse_exception_str);
  end;
end;

function qe_method_param_name(const Method: pointer; Index: integer): pchar;cdecl;
begin
  try
    if Method <> nil then
      Result := pchar(KLiFunc(Method).Params[Index].Name) else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_param_name()', lse_exception_str);
  end;
end;

function qe_method_param_type(const Method: pointer; Index: integer): PLseClassRec;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).Params[Index].ValueType.ClassRec else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_param_type()', lse_exception_str);
  end;
end;

function qe_method_bind(const Method: pointer; Data: pointer): pointer;cdecl;
var
  func: KLiFunc;
begin
  try
    if Method <> nil then
    begin
      func := KLiFunc(Method);
      Result := func.BindData;
      func.BindData := Data;
    end
    else Result := nil;
  except
    Result := nil;
    __log('qe_method_bind()', lse_exception_str);
  end;
end;

function qe_method_get_bind(const Method: pointer): pointer;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).BindData else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_get_bind()', lse_exception_str);
  end;
end;

function qe_method_is_creator(const Method: pointer): integer;cdecl;
begin
  try
    if Method <> nil then
      Result := Ord(KLiFunc(Method).IsConstructor) else
      Result := 0;
  except
    Result := 0;
    __log('qe_method_is_creator()', lse_exception_str);
  end;
end;

function qe_method_get_proc(const Method: pointer): pointer;cdecl;
begin
  try
    if Method <> nil then
      Result := KLiFunc(Method).Proc else
      Result := nil;
  except
    Result := nil;
    __log('qe_method_get_proc()', lse_exception_str);
  end;
end;

function qe_method_set_proc(const Method, NewProc: pointer): pointer;cdecl;
var
  func: KLiFunc;
begin
  try
    func := KLiFunc(Method);
    if (func <> nil) and not func.IsScript then
    begin
      Result := func.Proc;
      func.Proc := NewProc;
    end
    else Result := nil;
  except
    Result := nil;
    __log('qe_method_set_proc()', lse_exception_str);
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

function qe_keywords: pchar;cdecl;
begin
  try
    ReservedWords;
    Result := pchar(reserved_words);
  except
    Result := nil;
    __log('qe_keywords()', lse_exception_str);
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

procedure qe_log(const Msg: pchar; Count: integer);cdecl;
begin
  try
    __log(Msg, Count);
  except
    { do nothing }
  end;
end;

function qe_query(const ID: pchar): pointer;cdecl;
var
  name: string;
begin
  try
    Result := nil;
    name := LowerCase(Trim(ID));
    if name = 'qe_entries'  then Result := @qe_entries;
  except
    Result := nil;
    __log('qe_query()', lse_exception_str);
  end;
end;

initialization
begin
  lse_entries := @qe_entries;
end;

end.
