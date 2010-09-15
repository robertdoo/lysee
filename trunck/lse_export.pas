{==============================================================================}
{        UNIT: lse_export                                                      }
{ DESCRIPTION: binary interface between lseu and lse_kernel                    }
{     CREATED: 2004/04/12                                                      }
{    MODIFIED: 2010/08/31                                                      }
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

{ engine }

function qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
procedure qe_engine_destroy(const Engine: pointer);cdecl;
function qe_engine_compile(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
function qe_engine_compile_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
function qe_engine_execute(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
function qe_engine_execute_file(const Engine: pointer; const fname: pchar; IsLsp: integer): integer;cdecl;
procedure qe_engine_terminate(const Engine: pointer);cdecl;
procedure qe_engine_clear(const Engine: pointer);cdecl;
function qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
function qe_engine_errno(const Engine: pointer): integer;cdecl;
function qe_engine_error_row(const Engine: pointer): integer;cdecl;
function qe_engine_error_col(const Engine: pointer): integer;cdecl;
function qe_engine_error_name(const Engine: pointer): pchar;cdecl;
function qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
function qe_engine_error_module(const Engine: pointer): pchar;cdecl;
function qe_engine_error_ifile(const Engine: pointer): pchar;cdecl;
function qe_engine_result_type(const Engine: pointer): pchar;cdecl;
function qe_engine_result_text(const Engine: pointer): pchar;cdecl;
function qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
function qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
function qe_engine_ready(const Engine: pointer): integer;cdecl;
function qe_engine_running(const Engine: pointer): integer;cdecl;
function qe_engine_terminated(const Engine: pointer): integer;cdecl;
function qe_engine_exited(const Engine: pointer): integer;cdecl;
procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
function qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
function qe_engine_readln(const Engine: pointer): PLseString;cdecl;
procedure qe_begin_cgi(const Engine: pointer);cdecl;
procedure qe_end_cgi(const Engine: pointer);cdecl;
function qe_module_setup(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
function qe_find_class(const Name: pchar): PLseClassRec;cdecl;
function qe_param_engine(const Param: PLseParam): pointer;cdecl;
function qe_param_format(const Param: pointer; const Fmt: pchar): PLseString;cdecl;
procedure qe_param_error(const Param: pointer; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
procedure qe_value_set_object(const Data, obj, obj_class: pointer);cdecl;
procedure qe_value_set_stream(const Data: pointer; Value: PLseStream);cdecl;
function qe_dbv_provide(const Vendor: pchar): PLseDB;cdecl;
function qe_dbv_register(const dbv: PLseDBVendor): integer;cdecl;
function qe_database_encode_TUPSP(const Target, User, Password, Source, Params: pchar): PLseString;cdecl;
procedure qe_database_decode_TUPSP(const ConnectionStr: pchar; const TUPSP: PLseTUPSP);cdecl;

{ strlist }

function qe_strlist_class: PLseClassRec;cdecl;
function qe_strlist_create: pointer;cdecl;
procedure qe_strlist_free(strlist: pointer);cdecl;
function qe_strlist_count(strlist: pointer): integer;cdecl;
function qe_strlist_add(strlist: pointer; S: pchar): integer;cdecl;
procedure qe_strlist_insert(strlist: pointer; Index: integer; S: pchar);cdecl;
function qe_strlist_get(strlist: pointer; Index: integer): PLseString;cdecl;
procedure qe_strlist_set(strlist: pointer; Index: integer; S: pchar);cdecl;
procedure qe_strlist_delete(strlist: pointer; Index: integer);cdecl;
procedure qe_strlist_clear(strlist: pointer);cdecl;
function qe_strlist_get_text(strlist: pointer; Index: integer): PLseString;cdecl;
procedure qe_strlist_set_text(strlist: pointer; S: pchar);cdecl;

{ misc }

function qe_simple_test(const Script: pchar): integer;cdecl;
procedure qe_cleanup;cdecl;
function qe_reserved_words: pchar;cdecl;
function qe_get_kernel_file: pchar;cdecl;
procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
function qe_get_program_file: pchar;cdecl;
procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
function qe_production: pchar;cdecl;
function qe_version: pchar;cdecl;
function qe_copyright: pchar;cdecl;
function qe_tmpath: pchar;cdecl;
function qe_expand_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
function qe_complete_fname(const fname, buf: pchar; buf_size: integer):integer;cdecl;
procedure qe_log(const Msg: pchar; Count: integer);cdecl;

function QueryEntry(const ID: pchar): pointer;cdecl;

var
  qe_entries: RLseEntryRec = (
    cik_classes         : @sys_class_list;
    { engine }
    cik_create_engine   : {$IFDEF FPC}@{$ENDIF}qe_engine_create;
    cik_destroy_engine  : {$IFDEF FPC}@{$ENDIF}qe_engine_destroy;
    cik_compile         : {$IFDEF FPC}@{$ENDIF}qe_engine_compile;
    cik_compile_file    : {$IFDEF FPC}@{$ENDIF}qe_engine_compile_file;
    cik_execute         : {$IFDEF FPC}@{$ENDIF}qe_engine_execute;
    cik_execute_file    : {$IFDEF FPC}@{$ENDIF}qe_engine_execute_file;
    cik_terminate       : {$IFDEF FPC}@{$ENDIF}qe_engine_terminate;
    cik_clear           : {$IFDEF FPC}@{$ENDIF}qe_engine_clear;
    cik_get_args        : {$IFDEF FPC}@{$ENDIF}qe_engine_get_args;
    cik_set_args        : {$IFDEF FPC}@{$ENDIF}qe_engine_set_args;
    cik_errno           : {$IFDEF FPC}@{$ENDIF}qe_engine_errno;
    cik_error_row       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_row;
    cik_error_col       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_col;
    cik_error_name      : {$IFDEF FPC}@{$ENDIF}qe_engine_error_name;
    cik_error_msg       : {$IFDEF FPC}@{$ENDIF}qe_engine_error_msg;
    cik_error_module    : {$IFDEF FPC}@{$ENDIF}qe_engine_error_module;
    cik_error_ifile     : {$IFDEF FPC}@{$ENDIF}qe_engine_error_ifile;
    cik_result_type     : {$IFDEF FPC}@{$ENDIF}qe_engine_result_type;
    cik_result_text     : {$IFDEF FPC}@{$ENDIF}qe_engine_result_text;
    cik_get_search_path : {$IFDEF FPC}@{$ENDIF}qe_engine_get_search_path;
    cik_set_search_path : {$IFDEF FPC}@{$ENDIF}qe_engine_set_search_path;
    cik_get_main_file   : {$IFDEF FPC}@{$ENDIF}qe_engine_get_main_file;
    cik_set_main_file   : {$IFDEF FPC}@{$ENDIF}qe_engine_set_main_file;
    cik_ready           : {$IFDEF FPC}@{$ENDIF}qe_engine_ready;
    cik_running         : {$IFDEF FPC}@{$ENDIF}qe_engine_running;
    cik_terminated      : {$IFDEF FPC}@{$ENDIF}qe_engine_terminated;
    cik_exited          : {$IFDEF FPC}@{$ENDIF}qe_engine_exited;
    cik_write           : {$IFDEF FPC}@{$ENDIF}qe_engine_write;
    cik_read            : {$IFDEF FPC}@{$ENDIF}qe_engine_read;
    cik_readln          : {$IFDEF FPC}@{$ENDIF}qe_engine_readln;
    cik_begin_cgi       : {$IFDEF FPC}@{$ENDIF}qe_begin_cgi;
    cik_end_cgi         : {$IFDEF FPC}@{$ENDIF}qe_end_cgi;
    cik_setup_module    : {$IFDEF FPC}@{$ENDIF}qe_module_setup;
    cik_find_class      : {$IFDEF FPC}@{$ENDIF}qe_find_class;
    cik_get_engine      : {$IFDEF FPC}@{$ENDIF}qe_param_engine;
    cik_format          : {$IFDEF FPC}@{$ENDIF}qe_param_format;
    cik_set_error       : {$IFDEF FPC}@{$ENDIF}qe_param_error;
    cik_set_object      : {$IFDEF FPC}@{$ENDIF}qe_value_set_object;
    cik_set_stream      : {$IFDEF FPC}@{$ENDIF}qe_value_set_stream;
    { database }
    cik_dbv_provide     : {$IFDEF FPC}@{$ENDIF}qe_dbv_provide;
    cik_dbv_register    : {$IFDEF FPC}@{$ENDIF}qe_dbv_register;
    cik_encode_TUPSP    : {$IFDEF FPC}@{$ENDIF}qe_database_encode_TUPSP;
    cik_decode_TUPSP    : {$IFDEF FPC}@{$ENDIF}qe_database_decode_TUPSP;
    { strlist }
    cik_strlist_class   : {$IFDEF FPC}@{$ENDIF}qe_strlist_class;
    cik_strlist_create  : {$IFDEF FPC}@{$ENDIF}qe_strlist_create;
    cik_strlist_free    : {$IFDEF FPC}@{$ENDIF}qe_strlist_free;
    cik_strlist_count   : {$IFDEF FPC}@{$ENDIF}qe_strlist_count;
    cik_strlist_add     : {$IFDEF FPC}@{$ENDIF}qe_strlist_add;
    cik_strlist_insert  : {$IFDEF FPC}@{$ENDIF}qe_strlist_insert;
    cik_strlist_get     : {$IFDEF FPC}@{$ENDIF}qe_strlist_get;
    cik_strlist_set     : {$IFDEF FPC}@{$ENDIF}qe_strlist_set;
    cik_strlist_delete  : {$IFDEF FPC}@{$ENDIF}qe_strlist_delete;
    cik_strlist_clear   : {$IFDEF FPC}@{$ENDIF}qe_strlist_clear;
    cik_strlist_get_text: {$IFDEF FPC}@{$ENDIF}qe_strlist_get_text;
    cik_strlist_set_text: {$IFDEF FPC}@{$ENDIF}qe_strlist_set_text;
    { others }
    cik_production      : {$IFDEF FPC}@{$ENDIF}qe_production;
    cik_version         : {$IFDEF FPC}@{$ENDIF}qe_version;
    cik_copyright       : {$IFDEF FPC}@{$ENDIF}qe_copyright;
    cik_tmpath          : {$IFDEF FPC}@{$ENDIF}qe_tmpath;
    cik_query           : {$IFDEF FPC}@{$ENDIF}QueryEntry;
    cik_malloc          : {$IFDEF FPC}@{$ENDIF}lse_mem_alloc;
    cik_free            : {$IFDEF FPC}@{$ENDIF}lse_mem_free;
    cik_simple_test     : {$IFDEF FPC}@{$ENDIF}qe_simple_test;
    cik_cleanup         : {$IFDEF FPC}@{$ENDIF}qe_cleanup;
    cik_reserved_words  : {$IFDEF FPC}@{$ENDIF}qe_reserved_words;
    cik_get_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_get_kernel_file;
    cik_set_kernel_file : {$IFDEF FPC}@{$ENDIF}qe_set_kernel_file;
    cik_get_program_file: {$IFDEF FPC}@{$ENDIF}qe_get_program_file;
    cik_set_program_file: {$IFDEF FPC}@{$ENDIF}qe_set_program_file;
    cik_expand_fname    : {$IFDEF FPC}@{$ENDIF}qe_expand_fname;
    cik_complete_fname  : {$IFDEF FPC}@{$ENDIF}qe_complete_fname;
    cik_log             : {$IFDEF FPC}@{$ENDIF}qe_log;
  );

implementation

uses
  Math, DateUtils, lse_symbol, lse_api, lse_cgi;

function qe_find_class(const Name: pchar): PLseClassRec;cdecl;
var
  m_name, c_name: string;
  module: KLiModule;
  clss: KLiClass;
begin
  try
    InitLyseeKernel;
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

function qe_param_engine(const Param: PLseParam): pointer;cdecl;
begin
  try
    InitLyseeKernel;
    if Param <> nil then
      Result := __asEngine(Param) else
      Result := nil;
  except
    Result := nil;
    __log('qe_param_engine()', lse_exception_str);
  end;
end;

function qe_engine_create(const EngineRec: PLseEngine): pointer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine.Create(EngineRec);
  except
    Result := nil;
    __log('qe_engine_create()', lse_exception_str);
  end;
end;

procedure qe_engine_destroy(const Engine: pointer);cdecl;
begin
  try
    InitLyseeKernel;
    if Engine <> nil then
      KLiEngine(Engine).Free;
  except
    __log('qe_engine_destroy()', lse_exception_str);
  end;
end;

function qe_engine_compile(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
begin
  try
    InitLyseeKernel;
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
    InitLyseeKernel;
    KLiEngine(Engine).PrepareCompile;
    Result := Ord(KLiEngine(Engine).TryCompileFile(fname, IsLsp <> 0));
  except
    Result := 0;
    __log('qe_engine_compile_file()', lse_exception_str);
  end;
end;

function qe_engine_execute(const Engine: pointer; const code: pchar; IsLsp: integer): integer;cdecl;
var
  E: KLiEngine;
begin
  try
    InitLyseeKernel;
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
    InitLyseeKernel;
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
    InitLyseeKernel;
    if Engine <> nil then
      KLiEngine(Engine).Terminate;
  except
    __log('qe_engine_terminate()', lse_exception_str);
  end;
end;

procedure qe_engine_clear(const Engine: pointer);cdecl;
begin
  try
    InitLyseeKernel;
    if Engine <> nil then
      KLiEngine(Engine).Clear;
  except
    __log('qe_engine_clear()', lse_exception_str);
  end;
end;

function qe_engine_get_args(const Engine: pointer): PLseString;cdecl;
begin
  try
    InitLyseeKernel;
    Result := lse_strec_alloc(KLiEngine(Engine).Arguments.Text);
  except
    Result := nil;
    __log('qe_engine_get_args()', lse_exception_str);
  end;
end;

procedure qe_engine_set_args(const Engine: pointer; const Args: pchar);cdecl;
begin
  try
    InitLyseeKernel;
    if Engine <> nil then
      KLiEngine(Engine).Arguments.Text := Args;
  except
    __log('qe_engine_set_args()', lse_exception_str);
  end;
end;

function qe_engine_errno(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.errno;
  except
    Result := -1;
    __log('qe_engine_errno()', lse_exception_str);
  end;
end;

function qe_engine_error_row(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.Row;
  except
    Result := -1;
    __log('qe_engine_error_row()', lse_exception_str);
  end;
end;

function qe_engine_error_col(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.Col;
  except
    Result := -1;
    __log('qe_engine_error_col()', lse_exception_str);
  end;
end;

function qe_engine_error_name(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.ErrorRec^.error;
  except
    Result := nil;
    __log('qe_engine_error_name()', lse_exception_str);
  end;
end;

function qe_engine_error_msg(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.ErrorRec^.errmsg;
  except
    Result := nil;
    __log('qe_engine_error_msg()', lse_exception_str);
  end;
end;

function qe_engine_error_module(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.ErrorRec^.module;
  except
    Result := nil;
    __log('qe_engine_error_module()', lse_exception_str);
  end;
end;

function qe_engine_error_ifile(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := KLiEngine(Engine).Error.ErrorRec^.ifname;
  except
    Result := nil;
    __log('qe_engine_error_ifile()', lse_exception_str);
  end;
end;

function qe_engine_result_type(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(KLiEngine(Engine).ExitResultType);
  except
    Result := nil;
    __log('qe_engine_result_type()', lse_exception_str);
  end;
end;

function qe_engine_result_text(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(KLiEngine(Engine).ExitResultText);
  except
    Result := nil;
    __log('qe_engine_result_text()', lse_exception_str);
  end;
end;

function qe_engine_get_search_path(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(KLiEngine(Engine).MainSearchPath);
  except
    Result := nil;
    __log('qe_engine_get_search_path()', lse_exception_str);
  end;
end;

procedure qe_engine_set_search_path(const Engine: pointer; const Path: pchar);cdecl;
begin
  try
    InitLyseeKernel;
    KLiEngine(Engine).MainSearchPath := Trim(Path);
  except
    __log('qe_engine_set_search_path()', lse_exception_str);
  end;
end;

function qe_engine_get_main_file(const Engine: pointer): pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(KLiEngine(Engine).MainFile);
  except
    Result := nil;
    __log('qe_engine_get_main_file()', lse_exception_str);
  end;
end;

procedure qe_engine_set_main_file(const Engine: pointer; const fname: pchar);cdecl;
begin
  try
    InitLyseeKernel;
    KLiEngine(Engine).MainFile := fname;
  except
    __log('qe_engine_set_main_file()', lse_exception_str);
  end;
end;

function qe_engine_ready(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := Ord(KLiEngine(Engine).Ready);
  except
    Result := 0;
    __log('qe_engine_ready()', lse_exception_str);
  end;
end;

function qe_engine_running(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := Ord(KLiEngine(Engine).Running);
  except
    Result := 0;
    __log('qe_engine_running()', lse_exception_str);
  end;
end;

function qe_engine_terminated(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := Ord(KLiEngine(Engine).Terminated);
  except
    Result := 0;
    __log('qe_engine_terminated()', lse_exception_str);
  end;
end;

function qe_engine_exited(const Engine: pointer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := Ord(KLiEngine(Engine).Exited);
  except
    Result := 0;
    __log('qe_engine_exited()', lse_exception_str);
  end;
end;

procedure qe_engine_write(const Engine: pointer; const Text: pchar; Count: integer);cdecl;
begin
  try
    InitLyseeKernel;
    lse_stream_write(KLiEngine(Engine).StdoutStream, Text, Count);
  except
    __log('qe_engine_write()', lse_exception_str);
  end;
end;

function qe_engine_read(const Engine: pointer; const Buf: pchar; Count: integer): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := lse_stream_read(KLiEngine(Engine).StdinStream, Buf, Count);
  except
    Result := 0;
    __log('qe_engine_read()', lse_exception_str);
  end;
end;

function qe_engine_readln(const Engine: pointer): PLseString;cdecl;
begin
  try
    InitLyseeKernel;
    Result := lse_stream_readln(KLiEngine(Engine).StdinStream);
  except
    Result := nil;
    __log('qe_engine_readln()', lse_exception_str);
  end;
end;

procedure qe_begin_cgi(const Engine: pointer);cdecl;
var
  eng: KLiEngine;
  CGI: KLiCGI;
begin
  try
    InitLyseeKernel;
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
    InitLyseeKernel;
    if KLiEngine(Engine).CGI <> nil then
      KLiEngine(Engine).CGI.Free;
    __log('qe_end_cgi()');
  except
    __log('qe_end_cgi()', lse_exception_str);
  end;
end;

function qe_param_format(const Param: pointer; const Fmt: pchar): PLseString;cdecl;
begin
  try
    InitLyseeKernel;
    if Param <> nil then
      Result := lse_strec_alloc(__asRunner(PLseParam(Param)).FormatFor(Fmt, nil)) else
      Result := nil;
  except
    Result := nil;
    __log('qe_param_format()', lse_exception_str);
  end;
end;

procedure qe_value_set_Object(const Data, obj, obj_class: pointer);cdecl;
begin
  try
    InitLyseeKernel;
    __setObject(Data, KLiClass(obj_class), obj);
  except
    __log('qe_value_set_Object()', lse_exception_str);
  end;
end;

procedure qe_value_set_stream(const Data: pointer; Value: PLseStream);cdecl;
begin
  try
    InitLyseeKernel;
    __setStream(Data, Value);
  except
    __log('qe_value_set_stream()', lse_exception_str);
  end;
end;

procedure qe_param_error(const Param: pointer; const ID: pchar; Errno: integer; const Msg: pchar);cdecl;
begin
  try
    InitLyseeKernel;
    __setError(PLseParam(Param), ID, Errno, Msg);
  except
    __log('qe_param_error()', lse_exception_str);
  end;
end;

function qe_module_setup(const Name: pchar; const initrec: PLseModuleRec): pointer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := __setupModule(Name, initrec);
  except
    Result := nil;
    __log('qe_module_setup()', lse_exception_str);
  end;
end;

function qe_simple_test(const Script: pchar): integer;cdecl;
begin
  try
    InitLyseeKernel;
    Result := lse_symbol.SimpleTest(Script);
  except
    Result := SCT_ERROR;
    __log('qe_simple_test()', lse_exception_str);
  end;
end;

function qe_dbv_provide(const Vendor: pchar): PLseDB;cdecl;
begin
  try
    InitLyseeKernel;
    Result := dbv_provide(Vendor);
  except
    Result := nil;
    __log('qe_database_new()', lse_exception_str);
  end;
end;

function qe_dbv_register(const dbv: PLseDBVendor): integer;cdecl;
begin
  try
    InitLyseeKernel;
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
    InitLyseeKernel;
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
    InitLyseeKernel;
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

function qe_strlist_class: PLseClassRec;cdecl;
begin
  Result := KR_STRLIST;
end;

function qe_strlist_create: pointer;cdecl;
begin
  Result := pointer(KLiStrlist.Create);
end;

procedure qe_strlist_free(strlist: pointer);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    list.Free;
end;

function qe_strlist_count(strlist: pointer): integer;cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    Result := list.Count else
    Result := 0;
end;

function qe_strlist_add(strlist: pointer; S: pchar): integer;cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    Result := list.Add(S) else
    Result := -1;
end;

procedure qe_strlist_insert(strlist: pointer; Index: integer; S: pchar);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    if Index >= list.Count then
      list.Add(S) else
    if Index >= 0 then
      list.Insert(Index, S);
end;

function qe_strlist_get(strlist: pointer; Index: integer): PLseString;cdecl;
var
  list: KLiStrlist;
begin
  Result := nil;
  list := KLiStrlist(strlist);
  if list <> nil then
    if (Index >= 0) and (Index < list.Count) then
      Result := lse_strec_alloc(list[Index]);
end;

procedure qe_strlist_set(strlist: pointer; Index: integer; S: pchar);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    if (Index >= 0) and (Index < list.Count) then
      list[Index] := S;
end;

procedure qe_strlist_delete(strlist: pointer; Index: integer);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    if (Index >= 0) and (Index < list.Count) then
      list.Delete(Index);
end;

procedure qe_strlist_clear(strlist: pointer);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    list.Clear;
end;

function qe_strlist_get_text(strlist: pointer; Index: integer): PLseString;cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    Result := lse_strec_alloc(list.Text) else
    Result := nil;
end;

procedure qe_strlist_set_text(strlist: pointer; S: pchar);cdecl;
var
  list: KLiStrlist;
begin
  list := KLiStrlist(strlist);
  if list <> nil then
    list.Text := S;
end;

procedure qe_cleanup;cdecl;
begin
  try
//  InitLyseeKernel;
  except
    __log('qe_cleanup()', lse_exception_str);
  end;
end;

function qe_reserved_words: pchar;cdecl;
begin
  try
    InitLyseeKernel;
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
    InitLyseeKernel;
    Result := pchar(sys_kernel);
  except
    Result := nil;
    __log('qe_get_kernel_file()', lse_exception_str);
  end;
end;

procedure qe_set_kernel_file(const KernelFile: pchar);cdecl;
var
  fname: string;
begin
  try
    InitLyseeKernel;
    fname := Trim(KernelFile);
    if FileExists(fname) then
    begin
      fname := ExpandFileName(fname);
      if not __sameFileName(sys_kernel, fname) then
      begin
        sys_kernel := fname;
        __loadConfig;
      end;
    end;
  except
    __log('qe_set_kernel_file()', lse_exception_str);
  end;
end;

function qe_get_program_file: pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(sys_program);
  except
    Result := nil;
    __log('qe_get_program_file()', lse_exception_str);
  end;
end;

procedure qe_set_program_file(const ProgramFile: pchar);cdecl;
var
  fname: string;
begin
  try
    InitLyseeKernel;
    fname := Trim(ProgramFile);
    if fname <> '' then
      if FileExists(fname) then
        sys_program := ExpandFileName(fname) else
        sys_program := fname;
  except
    __log('qe_set_program_file()', lse_exception_str);
  end;
end;

function qe_production: pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := LSE_ID;
  except
    Result := LSE_ID;
    __log('qe_production()', lse_exception_str);
  end;
end;

function qe_version: pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := pchar(sys_version);
  except
    Result := pchar(sys_version);
    __log('qe_version()', lse_exception_str);
  end;
end;

function qe_copyright: pchar;cdecl;
begin
  try
    InitLyseeKernel;
    Result := LSE_COPYRIGHT;
  except
    Result := LSE_COPYRIGHT;
    __log('qe_copyright()', lse_exception_str);
  end;
end;

function qe_tmpath: pchar;cdecl;
begin
  try
    InitLyseeKernel;
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
