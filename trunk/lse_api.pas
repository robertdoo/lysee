{==============================================================================}
{        UNIT: lse_api                                                         }
{ DESCRIPTION: APIs builtin in lysee kernel                                    }
{     CREATED: 2003/02/26                                                      }
{    MODIFIED: 2010/10/12                                                      }
{==============================================================================}
{ Copyright (c) 2003-2010, Li Yun Jie                                          }
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
unit lse_api;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, inifiles, lseu, lse_kernel, lse_funcs
  {$IFDEF WINDOWS},Windows{$ENDIF};

{ sys }

procedure pp_system_dir(const Param: PLseParam);cdecl;
procedure pp_system_cd(const Param: PLseParam);cdecl;
procedure pp_system_mkdir(const Param: PLseParam);cdecl;
procedure pp_system_rmdir(const Param: PLseParam);cdecl;
procedure pp_system_cp(const Param: PLseParam);cdecl;
procedure pp_system_rm(const Param: PLseParam);cdecl;
procedure pp_system_mv(const Param: PLseParam);cdecl;
procedure pp_system_isdir(const Param: PLseParam);cdecl;
procedure pp_system_isfile(const Param: PLseParam);cdecl;
procedure pp_system_print(const Param: PLseParam);cdecl;
procedure pp_system_printf(const Param: PLseParam);cdecl;
procedure pp_system_println(const Param: PLseParam);cdecl;
procedure pp_system_readln(const Param: PLseParam);cdecl;
procedure pp_system_modules(const Param: PLseParam);cdecl;
procedure pp_system_exit(const Param: PLseParam);cdecl;
procedure pp_system_random(const Param: PLseParam);cdecl;
procedure pp_system_sleep(const Param: PLseParam);cdecl;
procedure pp_system_getenv(const Param: PLseParam);cdecl;
procedure pp_system_dumpc(const Param: PLseParam);cdecl;
procedure pp_system_length(const Param: PLseParam);cdecl;
procedure pp_system_getiv(const Param: PLseParam);cdecl;
procedure pp_system_setiv(const Param: PLseParam);cdecl;
procedure pp_system_getpv(const Param: PLseParam);cdecl;
procedure pp_system_setpv(const Param: PLseParam);cdecl;
procedure pp_system_genid(const Param: PLseParam);cdecl;
procedure pp_system_load(const Param: PLseParam);cdecl;
procedure pp_system_parse(const Param: PLseParam);cdecl;
procedure pp_system_eval(const Param: PLseParam);cdecl;
procedure pp_system_format(const Param: PLseParam);cdecl;
procedure pp_system_now(const Param: PLseParam);cdecl;
procedure pp_system_max(const Param: PLseParam);cdecl;
procedure pp_system_min(const Param: PLseParam);cdecl;
procedure pp_system_leap(const Param: PLseParam);cdecl;
procedure pp_system_which(const Param: PLseParam);cdecl;
procedure pp_system_current_dbvs(const Param: PLseParam);cdecl;
procedure pp_system_exportApi(const Param: PLseParam);cdecl;
procedure pp_system_curry(const Param: PLseParam);cdecl;
procedure pp_system_curryone(const Param: PLseParam);cdecl;
procedure pp_system_gc(const Param: PLseParam);cdecl;
procedure pp_system_apply(const Param: PLseParam);cdecl;
procedure pp_system_isValidDate(const Param: PLseParam);cdecl;
procedure pp_system_encodeDateTime(const Param: PLseParam);cdecl;
procedure pp_system_tmpfname(const Param: PLseParam);cdecl;
procedure pp_system_encodeGMT(const Param: PLseParam);cdecl;
procedure pp_system_decodeGMT(const Param: PLseParam);cdecl;
procedure pp_system_encodeUTF8(const Param: PLseParam);cdecl;
procedure pp_system_decodeUTF8(const Param: PLseParam);cdecl;
procedure pp_system_encodeS(const Param: PLseParam);cdecl;
procedure pp_system_decodeS(const Param: PLseParam);cdecl;
procedure pp_system_openfs(const Param: PLseParam);cdecl;
procedure pp_system_memory(const Param: PLseParam);cdecl;
procedure pp_system_incPD(const Param: PLseParam);cdecl;
procedure pp_system_excPD(const Param: PLseParam);cdecl;
procedure pp_system_veryPD(const Param: PLseParam);cdecl;
procedure pp_system_veryUD(const Param: PLseParam);cdecl;
procedure pp_system_system(const Param: PLseParam);cdecl;
procedure pp_system_shexec(const Param: PLseParam);cdecl;
procedure pp_system_msecs(const Param: PLseParam);cdecl;
procedure pp_system_current_module(const Param: PLseParam);cdecl;
procedure pp_system_current_func(const Param: PLseParam);cdecl;
procedure pp_system_current_error(const Param: PLseParam);cdecl;
procedure pp_system_current_args(const Param: PLseParam);cdecl;
procedure pp_system_current_prmc(const Param: PLseParam);cdecl;
procedure pp_system_current_prms(const Param: PLseParam);cdecl;
procedure pp_system_current_xcode(const Param: PLseParam);cdecl;
procedure pp_system_current_line(const Param: PLseParam);cdecl;
procedure pp_system_current_envs(const Param: PLseParam);cdecl;
procedure pp_system_current_file(const Param: PLseParam);cdecl;
procedure pp_system_current_ifile(const Param: PLseParam);cdecl;
procedure pp_system_current_pd(const Param: PLseParam);cdecl;
procedure pp_system_eol(const Param: PLseParam);cdecl;
procedure pp_system_each(const Param: PLseParam);cdecl;
procedure pp_system_map(const Param: PLseParam);cdecl;
procedure pp_system_reduce(const Param: PLseParam);cdecl;
procedure pp_system_filter(const Param: PLseParam);cdecl;
procedure pp_system_sum(const Param: PLseParam);cdecl;
procedure pp_system_maxint(const Param: PLseParam);cdecl;
procedure pp_system_minint(const Param: PLseParam);cdecl;
procedure pp_system_gget(const Param: PLseParam);cdecl;
procedure pp_system_gput(const Param: PLseParam);cdecl;
procedure pp_system_get_stdin(const Param: PLseParam);cdecl;
procedure pp_system_set_stdin(const Param: PLseParam);cdecl;
procedure pp_system_get_stdout(const Param: PLseParam);cdecl;
procedure pp_system_set_stdout(const Param: PLseParam);cdecl;
procedure pp_system_get_stderr(const Param: PLseParam);cdecl;
procedure pp_system_set_stderr(const Param: PLseParam);cdecl;
procedure pp_system_log(const Param: PLseParam);cdecl;
procedure pp_system_abs(const Param: PLseParam);cdecl;
procedure pp_system_find(const Param: PLseParam);cdecl;
procedure pp_system_gsub(const Param: PLseParam);cdecl;
procedure pp_system_split(const Param: PLseParam);cdecl;
procedure pp_system_getcs(const Param: PLseParam);cdecl;
procedure pp_system_throw(const Param: PLseParam);cdecl;
procedure pp_system_writeTo(const Param: PLseParam);cdecl;

const
  sys_func_count = 94;
  sys_func_array: array[0..sys_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string dir()';
     fr_addr:@pp_system_dir;
     fr_desc:'get current directory';
    ),
    (fr_prot:'bool cd(string newDirectory)';
     fr_addr:@pp_system_cd;
     fr_desc:'change current directory';
    ),
    (fr_prot:'bool mkdir(string dir)';
     fr_addr:@pp_system_mkdir;
     fr_desc:'create new directory';
    ),
    (fr_prot:'bool rmdir(string dir)';
     fr_addr:@pp_system_rmdir;
     fr_desc:'remove specified directory';
    ),
    (fr_prot:'bool cp(string source, string desti)';
     fr_addr:@pp_system_cp;
     fr_desc:'copy source file to desti file';
    ),
    (fr_prot:'bool rm(string fileName)';
     fr_addr:@pp_system_rm;
     fr_desc:'remove specified file';
    ),
    (fr_prot:'bool mv(string source, string desti)';
     fr_addr:@pp_system_mv;
     fr_desc:'rename source file to desti file';
    ),
    (fr_prot:'bool isdir(string directory)';
     fr_addr:@pp_system_isdir;
     fr_desc:'test if directory exists';
    ),
    (fr_prot:'bool isfile(string fileName)';
     fr_addr:@pp_system_isfile;
     fr_desc:'test if file exists';
    ),
    (fr_prot:'varlist modules()';
     fr_addr:@pp_system_modules;
     fr_desc:'return imported module list';
    ),
    (fr_prot:'void print(string text)';
     fr_addr:@pp_system_print;
     fr_desc:'print text into STDOUT';
    ),
    (fr_prot:'void println(string text)';
     fr_addr:@pp_system_println;
     fr_desc:'print text and a line break into STDOUT';
    ),
    (fr_prot:'void printf(string fileName)';
     fr_addr:@pp_system_printf;
     fr_desc:'print file content into STDOUT';
    ),
    (fr_prot:'string readln()';
     fr_addr:@pp_system_readln;
     fr_desc:'read a line from STDIN';
    ),
    (fr_prot:'void exportAPI(string fileName)';
     fr_addr:@pp_system_exportApi;
     fr_desc:'export builtin APIs into HTML file';
    ),
    (fr_prot:'void exit(int status)';
     fr_addr:@pp_system_exit;
     fr_desc:'exit with status code';
    ),
    (fr_prot:'int random(int low, int high)';
     fr_addr:@pp_system_random;
     fr_desc:'generate random number';
    ),
    (fr_prot:'void sleep(int milliSeconds)';
     fr_addr:@pp_system_sleep;
     fr_desc:'sleep a number of milliseconds';
    ),
    (fr_prot:'string getenv(string name)';
     fr_addr:@pp_system_getenv;
     fr_desc:'get environment value';
    ),
    (fr_prot:'string dumpc(variant any)';
     fr_addr:@pp_system_dumpc;
     fr_desc:'dump object p-codes';
    ),
    (fr_prot:'int length(variant any)';
     fr_addr:@pp_system_length;
     fr_desc:'get string length or item count';
    ),
    (fr_prot:'variant getiv(variant obj, int index)';
     fr_addr:@pp_system_getiv;
     fr_desc:'get item value by index';
    ),
    (fr_prot:'void setiv(variant obj, int index, variant value)';
     fr_addr:@pp_system_setiv;
     fr_desc:'set item value by index';
    ),
    (fr_prot:'variant getpv(variant obj, string name)';
     fr_addr:@pp_system_getpv;
     fr_desc:'get property by name';
    ),
    (fr_prot:'void setpv(variant obj, string name, variant value)';
     fr_addr:@pp_system_setpv;
     fr_desc:'set property by name';
    ),
    (fr_prot:'string genid()';
     fr_addr:@pp_system_genid;
     fr_desc:'generate a global unique string';
    ),
    (fr_prot:'module load(string fileName)';
     fr_addr:@pp_system_load;
     fr_desc:'load module';
    ),
    (fr_prot:'function parse(string script, bool isLSP)';
     fr_addr:@pp_system_parse;
     fr_desc:'parse and compile script';
    ),
    (fr_prot:'variant eval(string script, bool isLSP)';
     fr_addr:@pp_system_eval;
     fr_desc:'evaluate a block of lysee script';
    ),
    (fr_prot:'string format(string fmt, varlist args)';
     fr_addr:@pp_system_format;
     fr_desc:'format string';
    ),
    (fr_prot:'time now()';
     fr_addr:@pp_system_now;
     fr_desc:'get current time';
    ),
    (fr_prot:'variant max(variant v1, variant v2)';
     fr_addr:@pp_system_max;
     fr_desc:'get max value';
    ),
    (fr_prot:'variant min(variant v1, variant v2)';
     fr_addr:@pp_system_min;
     fr_desc:'get min value';
    ),
    (fr_prot:'bool leap(int year)';
     fr_addr:@pp_system_leap;
     fr_desc:'test if is a leap year';
    ),
    (fr_prot:'variant which(string name)';
     fr_addr:@pp_system_which;
     fr_desc:'find variable, function, module or anything else by name';
    ),
    (fr_prot:'function curry(function func, varlist paramList)';
     fr_addr:@pp_system_curry;
     fr_desc:'curry function with parametre list';
    ),
    (fr_prot:'function curryOne(function func, variant value)';
     fr_addr:@pp_system_curryone;
     fr_desc:'curry function with one parametre';
    ),
    (fr_prot:'int gc()';
     fr_addr:@pp_system_gc;
     fr_desc:'execute garbage collection immediately';
    ),
    (fr_prot:'variant apply(function func, varlist params)';
     fr_addr:@pp_system_apply;
     fr_desc:'call function with supplied parametres';
    ),
    (fr_prot:'bool isValidDate(int year, int month, int day)';
     fr_addr:@pp_system_isValidDate;
     fr_desc:'test if is valid date';
    ),
    (fr_prot:'time encodeDateTime(int year, int month, int day, int hour, int minute, int second, int milliSecond)';
     fr_addr:@pp_system_encodeDateTime;
     fr_desc:'encode date time';
    ),
    (fr_prot:'string tempFileName(string fileExt)';
     fr_addr:@pp_system_tmpfname;
     fr_desc:'generate temp file name at temp path';
    ),
    (fr_prot:'string encodeGMT(time date)';
     fr_addr:@pp_system_encodeGMT;
     fr_desc:'encode date to GMT string';
    ),
    (fr_prot:'time decodeGMT(string GMT)';
     fr_addr:@pp_system_decodeGMT;
     fr_desc:'decode GMT string to date';
    ),
    (fr_prot:'string encodeUTF8(string ANSI)';
     fr_addr:@pp_system_encodeUTF8;
     fr_desc:'encode ANSI string to UTF8 format';
    ),
    (fr_prot:'string decodeUTF8(string UTF8)';
     fr_addr:@pp_system_decodeUTF8;
     fr_desc:'decode UTF8 string to ANSI format';
    ),
    (fr_prot:'string encodeS(string S, bool includeLocalBytes)';
     fr_addr:@pp_system_encodeS;
     fr_desc:'encode string value';
    ),
    (fr_prot:'string decodeS(string S)';
     fr_addr:@pp_system_decodeS;
     fr_desc:'decode string value';
    ),
    (fr_prot:'stream openfs(string fileName, string mode)';
     fr_addr:@pp_system_openfs;
     fr_desc:'open file stream by specified mode: CRWE (default R)';
    ),
    (fr_prot:'stream memory(int size)';
     fr_addr:@pp_system_memory;
     fr_desc:'create memory stream';
    ),
    (fr_prot:'string incPD(string dir)';
     fr_addr:@pp_system_incPD;
     fr_desc:'include trailing path delimiter';
    ),
    (fr_prot:'string excPD(string path)';
     fr_addr:@pp_system_excPD;
     fr_desc:'exclude trailing path delimiter';
    ),
    (fr_prot:'string veryPD(string path)';
     fr_addr:@pp_system_veryPD;
     fr_desc:'correct path delimiter';
    ),
    (fr_prot:'string veryUD(string RUL)';
     fr_addr:@pp_system_veryUD;
     fr_desc:'correct URL delimiter';
    ),
    (fr_prot:'string system(string commandline, string dir)';
     fr_addr:@pp_system_system;
     fr_desc:'execute command line and get its output';
    ),
    (fr_prot:'bool shexec(string commandline, string dir, bool wait)';
     fr_addr:@pp_system_shexec;
     fr_desc:'execute command line';
    ),
    (fr_prot:'int msecs(function func, varlist params)';
     fr_addr:@pp_system_msecs;
     fr_desc:'count milliseconds used to call a function';
    ),
    (fr_prot:'strlist __dbvs__()';
     fr_addr:@pp_system_current_dbvs;
     fr_desc:'database vendor list';
    ),
    (fr_prot:'module __module__()';
     fr_addr:@pp_system_current_module;
     fr_desc:'get current module';
    ),
    (fr_prot:'function __func__()';
     fr_addr:@pp_system_current_func;
     fr_desc:'get current function';
    ),
    (fr_prot:'error __error__()';
     fr_addr:@pp_system_current_error;
     fr_desc:'get current error';
    ),
    (fr_prot:'strlist __args__()';
     fr_addr:@pp_system_current_args;
     fr_desc:'get argument list';
    ),
    (fr_prot:'int __prmc__()';
     fr_addr:@pp_system_current_prmc;
     fr_desc:'get actual parametre count';
    ),
    (fr_prot:'varlist __prms__()';
     fr_addr:@pp_system_current_prms;
     fr_desc:'get parametre value list';
    ),
    (fr_prot:'int __xcode__()';
     fr_addr:@pp_system_current_xcode;
     fr_desc:'get last shell exit code';
    ),
    (fr_prot:'int __line__()';
     fr_addr:@pp_system_current_line;
     fr_desc:'get current line number';
    ),
    (fr_prot:'strlist __envs__()';
     fr_addr:@pp_system_current_envs;
     fr_desc:'get environment value list';
    ),
    (fr_prot:'string __file__()';
     fr_addr:@pp_system_current_file;
     fr_desc:'get current file';
    ),
    (fr_prot:'string __ifile__()';
     fr_addr:@pp_system_current_ifile;
     fr_desc:'get current included file';
    ),
    (fr_prot:'string __pd__()';
     fr_addr:@pp_system_current_pd;
     fr_desc:'get system path delimiter';
    ),
    (fr_prot:'string eol()';
     fr_addr:@pp_system_eol;
     fr_desc:'get eol(line break)';
    ),
    (fr_prot:'void each(vargen any, function proc)';
     fr_addr:@pp_system_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'varlist map(vargen any, function proc)';
     fr_addr:@pp_system_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'variant reduce(vargen any, variant initValue, function proc)';
     fr_addr:@pp_system_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'varlist filter(vargen any, function proc)';
     fr_addr:@pp_system_filter;
     fr_desc:'filter item: |item| ... end';
    ),
    (fr_prot:'variant sum(vargen any, function proc, variant initValue)';
     fr_addr:@pp_system_sum;
     fr_desc:'sum all: |result, item| ... end';
    ),
    (fr_prot:'int __maxint__()';
     fr_addr:@pp_system_maxint;
     fr_desc:'get max integer value';
    ),
    (fr_prot:'int __minint__()';
     fr_addr:@pp_system_minint;
     fr_desc:'get min integer value';
    ),
    (fr_prot:'variant gget(string name)';
     fr_addr:@pp_system_gget;
     fr_desc:'get global value';
    ),
    (fr_prot:'void gput(string name, variant value)';
     fr_addr:@pp_system_gput;
     fr_desc:'set global value';
    ),
    (fr_prot:'stream get_cin()';
     fr_addr:@pp_system_get_stdin;
     fr_desc:'get stdin stream';
    ),
    (fr_prot:'void set_cin(stream stdin)';
     fr_addr:@pp_system_set_stdin;
     fr_desc:'set stdin stream';
    ),
    (fr_prot:'stream cout';
     fr_addr:@pp_system_get_stdout;
     fr_desc:'get stdout stream';
    ),
    (fr_prot:'void set_cout(stream stdout)';
     fr_addr:@pp_system_set_stdout;
     fr_desc:'set stdout stream';
    ),
    (fr_prot:'stream get_cerr()';
     fr_addr:@pp_system_get_stderr;
     fr_desc:'get stderr stream';
    ),
    (fr_prot:'void set_cerr(stream stderr)';
     fr_addr:@pp_system_set_stderr;
     fr_desc:'set stderr stream';
    ),
    (fr_prot:'void log(string message)';
     fr_addr:@pp_system_log;
     fr_desc:'save message to log file';
    ),
    (fr_prot:'variant abs(variant value)';
     fr_addr:@pp_system_abs;
     fr_desc:'get absolute value';
    ),
    (fr_prot:'varlist find(string S, string patten, bool findAll)';
     fr_addr:@pp_system_find;
     fr_desc:'find patten matches';
    ),
    (fr_prot:'string gsub(string S, string patten, string newStr, int count)';
     fr_addr:@pp_system_gsub;
     fr_desc:'replace patten with new string';
    ),
    (fr_prot:'strlist split(string S, string patten)';
     fr_addr:@pp_system_split;
     fr_desc:'split string to strlist';
    ),
    (fr_prot:'varlist getcs(int index)';
     fr_addr:@pp_system_getcs;
     fr_desc:'get call stack item';
    ),
    (fr_prot:'void throw(string exceptionMsg, string exceptionID)';
     fr_addr:@pp_system_throw;
     fr_desc:'throw exception';
    ),
    (fr_prot:'int writeTo(stream stream, variant someThing)';
     fr_addr:@pp_system_writeTo;
     fr_desc:'write something to stream';
    )
  );

  sys_module_funcs: RLseFuncListRec = (
    count: sys_func_count;
    entry:@sys_func_array;
  );

{ error }

procedure pp_error_text(const Param: PLseParam);cdecl;
procedure pp_error_module(const Param: PLseParam);cdecl;
procedure pp_error_name(const Param: PLseParam);cdecl;
procedure pp_error_message(const Param: PLseParam);cdecl;
procedure pp_error_row(const Param: PLseParam);cdecl;
procedure pp_error_col(const Param: PLseParam);cdecl;
procedure pp_error_errno(const Param: PLseParam);cdecl;

const
  KTN_ERROR = 'error';
  KTD_ERROR = 'error';

  error_func_count = 7;
  error_func_array: array[0..error_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string get_text()';
     fr_addr:@pp_error_text;
     fr_desc:'error text';
    ),
    (fr_prot:'string get_module()';
     fr_addr:@pp_error_module;
     fr_desc:'error module';
    ),
    (fr_prot:'string get_name()';
     fr_addr:@pp_error_name;
     fr_desc:'error name';
    ),
    (fr_prot:'string get_message()';
     fr_addr:@pp_error_message;
     fr_desc:'error message';
    ),
    (fr_prot:'int get_row()';
     fr_addr:@pp_error_row;
     fr_desc:'error row';
    ),
    (fr_prot:'int get_col()';
     fr_addr:@pp_error_col;
     fr_desc:'error column';
    ),
    (fr_prot:'int get_errno()';
     fr_addr:@pp_error_errno;
     fr_desc:'error code';
    )
  );

{ float }

  KTN_FLOAT = 'float';
  KTD_FLOAT = 'float data type';

{ function }

procedure pp_func_name(const Param: PLseParam);cdecl;
procedure pp_func_type(const Param: PLseParam);cdecl;
procedure pp_func_prototype(const Param: PLseParam);cdecl;
procedure pp_func_module(const Param: PLseParam);cdecl;
procedure pp_func_parent(const Param: PLseParam);cdecl;
procedure pp_func_params(const Param: PLseParam);cdecl;
procedure pp_func_locals(const Param: PLseParam);cdecl;

const
  KTN_FUNC = 'function';
  KTD_FUNC = 'function';

  func_func_count = 7;
  func_func_array: array[0..func_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string get_name()';
     fr_addr:@pp_func_name;
     fr_desc:'function name';
    ),
    (fr_prot:'string get_type()';
     fr_addr:@pp_func_type;
     fr_desc:'get function result type';
    ),
    (fr_prot:'string get_prototype()';
     fr_addr:@pp_func_prototype;
     fr_desc:'function prototype';
    ),
    (fr_prot:'module get_module()';
     fr_addr:@pp_func_module;
     fr_desc:'function module';
    ),
    (fr_prot:'type get_parent()';
     fr_addr:@pp_func_parent;
     fr_desc:'get parent class';
    ),
    (fr_prot:'varlist get_params()';
     fr_addr:@pp_func_params;
     fr_desc:'get parametre list';
    ),
    (fr_prot:'varlist get_locals()';
     fr_addr:@pp_func_locals;
     fr_desc:'get local varible list';
    )
  );

{ hashed }

procedure pp_hashed_create(const Param: PLseParam);cdecl;
procedure pp_hashed_length(const Param: PLseParam);cdecl;
procedure pp_hashed_exchange(const Param: PLseParam);cdecl;
procedure pp_hashed_getpv(const Param: PLseParam);cdecl;
procedure pp_hashed_setpv(const Param: PLseParam);cdecl;
procedure pp_hashed_gget(const Param: PLseParam);cdecl;
procedure pp_hashed_gput(const Param: PLseParam);cdecl;
procedure pp_hashed_read(const Param: PLseParam);cdecl;
procedure pp_hashed_remove(const Param: PLseParam);cdecl;
procedure pp_hashed_clear(const Param: PLseParam);cdecl;
procedure pp_hashed_isset(const Param: PLseParam);cdecl;
procedure pp_hashed_keys(const Param: PLseParam);cdecl;
procedure pp_hashed_listKV(const Param: PLseParam);cdecl;
procedure pp_hashed_values(const Param: PLseParam);cdecl;
procedure pp_hashed_curry(const Param: PLseParam);cdecl;

const
  KTN_HASHED = 'hashed';
  KTD_HASHED = 'hashed key-value list';

  hashed_func_count = 19;
  hashed_func_array: array[0..hashed_func_count - 1] of RLseFuncRec = (
    (fr_prot:'hashed hashed(int buckets)';
     fr_addr:@pp_hashed_create;
     fr_desc:'create a hashed key-value list';
    ),
    (fr_prot:'variant getpv(string key)';
     fr_addr:@pp_hashed_getpv;
     fr_desc:'get value by key name';
    ),
    (fr_prot:'void setpv(string key, variant value)';
     fr_addr:@pp_hashed_setpv;
     fr_desc:'set value by key name';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_hashed_length;
     fr_desc:'get item count';
    ),
    (fr_prot:'variant getiv(int index)';
     fr_addr:@pp_hashed_getpv;
     fr_desc:'get value by index';
    ),
    (fr_prot:'void setiv(int index, variant value)';
     fr_addr:@pp_hashed_setpv;
     fr_desc:'set value by index';
    ),
    (fr_prot:'void exchange(int index1, int index2)';
     fr_addr:@pp_hashed_exchange;
     fr_desc:'exchange two item valuea';
    ),
    (fr_prot:'variant gget(string key)';
     fr_addr:@pp_hashed_gget;
     fr_desc:'get value by chain';
    ),
    (fr_prot:'void gput(string key, variant value)';
     fr_addr:@pp_hashed_gput;
     fr_desc:'set value by chain';
    ),
    (fr_prot:'variant read(string key, variant defaultValue)';
     fr_addr:@pp_hashed_read;
     fr_desc:'read key value';
    ),
    (fr_prot:'void write(string key, variant value)';
     fr_addr:@pp_hashed_setpv;
     fr_desc:'set key value';
    ),
    (fr_prot:'strlist get_keys()';
     fr_addr:@pp_hashed_keys;
     fr_desc:'get hashed key list';
    ),
    (fr_prot:'varlist listKV()';
     fr_addr:@pp_hashed_listKV;
     fr_desc:'list keys and values';
    ),
    (fr_prot:'varlist get_values()';
     fr_addr:@pp_hashed_values;
     fr_desc:'get hashed value list';
    ),
    (fr_prot:'void remove(string key)';
     fr_addr:@pp_hashed_remove;
     fr_desc:'remove specifed key value';
    ),
    (fr_prot:'void delete(int index)';
     fr_addr:@pp_hashed_remove;
     fr_desc:'delete by index';
    ),
    (fr_prot:'void clear()';
     fr_addr:@pp_hashed_clear;
     fr_desc:'clear list item';
    ),
    (fr_prot:'bool isset(string key)';
     fr_addr:@pp_hashed_isset;
     fr_desc:'check if the key exists';
    ),
    (fr_prot:'function curry(string name, function func)';
     fr_addr:@pp_hashed_curry;
     fr_desc:'curry self and save the result function';
    )
  );

{ int }

procedure pp_int_hex(const Param: PLseParam);cdecl;
procedure pp_int_bitlist(const Param: PLseParam);cdecl;
procedure pp_int_upto(const Param: PLseParam);cdecl;
procedure pp_int_downto(const Param: PLseParam);cdecl;

const
  KTN_INT = 'int';
  KTD_INT = 'integer value';

  int_func_count = 4;
  int_func_array: array[0..int_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string hex(int size)';
     fr_addr:@pp_int_hex;
     fr_desc:'convert to hex string';
    ),
    (fr_prot:'string bitlist(int size)';
     fr_addr:@pp_int_bitlist;
     fr_desc:'convert to bit list string';
    ),
    (fr_prot:'vargen upto(int value, int step)';
     fr_addr:@pp_int_upto;
     fr_desc:'create a upto range';
    ),
    (fr_prot:'vargen downto(int value, int step)';
     fr_addr:@pp_int_downto;
     fr_desc:'create a downto range';
    )
  );

{ module }

procedure pp_module_name(const Param: PLseParam);cdecl;
procedure pp_module_file(const Param: PLseParam);cdecl;
procedure pp_module_modules(const Param: PLseParam);cdecl;
procedure pp_module_funcs(const Param: PLseParam);cdecl;
procedure pp_module_types(const Param: PLseParam);cdecl;
procedure pp_module_version(const Param: PLseParam);cdecl;
procedure pp_module_getpv(const Param: PLseParam);cdecl;
procedure pp_module_main(const Param: PLseParam);cdecl;
procedure pp_module_imports(const Param: PLseParam);cdecl;

const
  KTN_MODULE = 'module';
  KTD_MODULE = 'module';

  module_func_count = 9;
  module_func_array: array[0..module_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string get_name()';
     fr_addr:@pp_module_name;
     fr_desc:'module name';
    ),
    (fr_prot:'string get_file()';
     fr_addr:@pp_module_file;
     fr_desc:'module file';
    ),
    (fr_prot:'varlist get_modules()';
     fr_addr:@pp_module_modules;
     fr_desc:'imported module list';
    ),
    (fr_prot:'varlist get_funcs()';
     fr_addr:@pp_module_funcs;
     fr_desc:'global function list';
    ),
    (fr_prot:'varlist get_classes()';
     fr_addr:@pp_module_types;
     fr_desc:'get type list';
    ),
    (fr_prot:'string get_version()';
     fr_addr:@pp_module_version;
     fr_desc:'type version';
    ),
    (fr_prot:'variant getpv(string name)';
     fr_addr:@pp_module_getpv;
     fr_desc:'find class, function or const value by name';
    ),
    (fr_prot:'function get_main()';
     fr_addr:@pp_module_main;
     fr_desc:'module main function';
    ),
    (fr_prot:'module imports(string moduleName)';
     fr_addr:@pp_module_imports;
     fr_desc:'import module by name';
    )
  );

{ database }

procedure pp_database_create(const Param: PLseParam);cdecl;
procedure pp_database_execSQL(const Param: PLseParam);cdecl;
procedure pp_database_openSQL(const Param: PLseParam);cdecl;
procedure pp_database_tables(const Param: PLseParam);cdecl;
procedure pp_database_procedures(const Param: PLseParam);cdecl;
procedure pp_database_connecTo(const Param: PLseParam);cdecl;
procedure pp_database_disconnect(const Param: PLseParam);cdecl;
procedure pp_database_reconnect(const Param: PLseParam);cdecl;
procedure pp_database_transact(const Param: PLseParam);cdecl;
procedure pp_database_inTransaction(const Param: PLseParam);cdecl;
procedure pp_database_commit(const Param: PLseParam);cdecl;
procedure pp_database_commitRetaining(const Param: PLseParam);cdecl;
procedure pp_database_commitAndTransact(const Param: PLseParam);cdecl;
procedure pp_database_rollback(const Param: PLseParam);cdecl;
procedure pp_database_rollbackRetaining(const Param: PLseParam);cdecl;
procedure pp_database_rollbackAndTransact(const Param: PLseParam);cdecl;
procedure pp_database_connected(const Param: PLseParam);cdecl;
procedure pp_database_escape(const Param: PLseParam);cdecl;

const
  KTN_DB = 'database';
  KTD_DB = 'database';

  db_execute_count = 18;
  db_execute_array: array[0..db_execute_count - 1] of RLseFuncRec = (
    (fr_prot:'database database(string vendor)';
     fr_addr:@pp_database_create;
     fr_desc:'create database object';
    ),
    (fr_prot:'int execSQL(string SQL)';
     fr_addr:@pp_database_execSQL;
     fr_desc:'execute SQL command';
    ),
    (fr_prot:'dataset openSQL(string SQL)';
     fr_addr:@pp_database_openSQL;
     fr_desc:'query dataset';
    ),
    (fr_prot:'strlist get_tables()';
     fr_addr:@pp_database_tables;
     fr_desc:'get table name list';
    ),
    (fr_prot:'strlist get_procedures()';
     fr_addr:@pp_database_procedures;
     fr_desc:'get stored procedure name list';
    ),
    (fr_prot:'void connecTo(string target, string user, string password, string source, string params)';
     fr_addr:@pp_database_connecTo;
     fr_desc:'connect to target database: Access, Mssql, ODBC';
    ),
    (fr_prot:'void disconnect()';
     fr_addr:@pp_database_disconnect;
     fr_desc:'close database connection';
    ),
    (fr_prot:'void reconnect()';
     fr_addr:@pp_database_reconnect;
     fr_desc:'reopen database connection';
    ),
    (fr_prot:'bool get_connected()';
     fr_addr:@pp_database_connected;
     fr_desc:'test if database connection is active';
    ),
    (fr_prot:'void transact()';
     fr_addr:@pp_database_transact;
     fr_desc:'begin transaction';
    ),
    (fr_prot:'bool get_inTransaction()';
     fr_addr:@pp_database_inTransaction;
     fr_desc:'test if is in transaction';
    ),
    (fr_prot:'void commit()';
     fr_addr:@pp_database_commit;
     fr_desc:'commit transaction';
    ),
    (fr_prot:'void commitRetaining()';
     fr_addr:@pp_database_commitRetaining;
     fr_desc:'commit retaining transactions';
    ),
    (fr_prot:'void commitAndTransact()';
     fr_addr:@pp_database_commitAndTransact;
     fr_desc:'commit retaining and start a new transaction';
    ),
    (fr_prot:'void rollback()';
     fr_addr:@pp_database_rollback;
     fr_desc:'rollback transaction';
    ),
    (fr_prot:'void rollbackRetaining()';
     fr_addr:@pp_database_rollbackRetaining;
     fr_desc:'rollback retaining transactions';
    ),
    (fr_prot:'void rollbackAndTransact()';
     fr_addr:@pp_database_rollbackAndTransact;
     fr_desc:'rollback retaining and start a new transaction';
    ),
    (fr_prot:'string escape(string stringValue)';
     fr_addr:@pp_database_escape;
     fr_desc:'escape string value';
    )
  );

{ dataset }

procedure pp_dataset_close(const Param: PLseParam);cdecl;
procedure pp_dataset_open(const Param: PLseParam);cdecl;
procedure pp_dataset_first(const Param: PLseParam);cdecl;
procedure pp_dataset_last(const Param: PLseParam);cdecl;
procedure pp_dataset_prior(const Param: PLseParam);cdecl;
procedure pp_dataset_next(const Param: PLseParam);cdecl;
procedure pp_dataset_eof(const Param: PLseParam);cdecl;
procedure pp_dataset_bof(const Param: PLseParam);cdecl;
procedure pp_dataset_count(const Param: PLseParam);cdecl;
procedure pp_dataset_length(const Param: PLseParam);cdecl;
procedure pp_dataset_getiv(const Param: PLseParam);cdecl;
procedure pp_dataset_getpv(const Param: PLseParam);cdecl;
procedure pp_dataset_indexOf(const Param: PLseParam);cdecl;
procedure pp_dataset_field_name(const Param: PLseParam);cdecl;
procedure pp_dataset_field_type(const Param: PLseParam);cdecl;
procedure pp_dataset_string(const Param: PLseParam);cdecl;
procedure pp_dataset_int(const Param: PLseParam);cdecl;
procedure pp_dataset_bool(const Param: PLseParam);cdecl;
procedure pp_dataset_float(const Param: PLseParam);cdecl;
procedure pp_dataset_money(const Param: PLseParam);cdecl;

const
  KTN_DS = 'dataset';
  KTD_DS = 'dataset';

  ds_execute_count = 20;
  ds_execute_array: array[0..ds_execute_count - 1] of RLseFuncRec = (
    (fr_prot:'int get_length()';
     fr_addr:@pp_dataset_length;
     fr_desc:'get field count';
    ),
    (fr_prot:'variant getiv(int index)';
     fr_addr:@pp_dataset_getiv;
     fr_desc:'get field value by index';
    ),
    (fr_prot:'variant getpv(string fieldName)';
     fr_addr:@pp_dataset_getpv;
     fr_desc:'get field value by name';
    ),
    (fr_prot:'int indexOf(string fieldName)';
     fr_addr:@pp_dataset_indexOf;
     fr_desc:'get field index by name';
    ),
    (fr_prot:'void close()';
     fr_addr:@pp_dataset_close;
     fr_desc:'close current dataset';
    ),
    (fr_prot:'dataset open(string SQL)';
     fr_addr:@pp_dataset_open;
     fr_desc:'close current dataset and start a new query';
    ),
    (fr_prot:'int get_count()';
     fr_addr:@pp_dataset_count;
     fr_desc:'get record count';
    ),
    (fr_prot:'void first()';
     fr_addr:@pp_dataset_first;
     fr_desc:'seek to first record';
    ),
    (fr_prot:'void last()';
     fr_addr:@pp_dataset_last;
     fr_desc:'seek to last record';
    ),
    (fr_prot:'void prior()';
     fr_addr:@pp_dataset_prior;
     fr_desc:'seek to prior record';
    ),
    (fr_prot:'void next()';
     fr_addr:@pp_dataset_next;
     fr_desc:'seek to next record';
    ),
    (fr_prot:'bool get_eof()';
     fr_addr:@pp_dataset_eof;
     fr_desc:'test if is at the end of the dataset';
    ),
    (fr_prot:'bool get_bof()';
     fr_addr:@pp_dataset_bof;
     fr_desc:'test if is at the head of the dataset';
    ),
    (fr_prot:'string name(int index)';
     fr_addr:@pp_dataset_field_name;
     fr_desc:'get field name by index';
    ),
    (fr_prot:'type type(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_field_type;
     fr_desc:'get field type by name or index';
    ),
    (fr_prot:'string string(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_string;
     fr_desc:'get string field value by name or index';
    ),
    (fr_prot:'int int(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_int;
     fr_desc:'get integer field value by name or index';
    ),
    (fr_prot:'bool bool(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_bool;
     fr_desc:'get boolean field value by name or index';
    ),
    (fr_prot:'float float(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_float;
     fr_desc:'get float field value by name or index';
    ),
    (fr_prot:'money money(variant fieldNameOrIndex)';
     fr_addr:@pp_dataset_money;
     fr_desc:'get money field value by name or index';
    )
  );

{ vargen }

procedure pp_vargen_create(const Param: PLseParam);cdecl;
procedure pp_vargen_eof(const Param: PLseParam);cdecl;
procedure pp_vargen_next(const Param: PLseParam);cdecl;
procedure pp_vargen_rewind(const Param: PLseParam);cdecl;
procedure pp_vargen_send(const Param: PLseParam);cdecl;

const
  KTN_VARGEN = 'vargen';
  KTD_VARGEN = 'variant generator';

  vargen_func_count = 9;
  vargen_func_array: array[0..vargen_func_count - 1] of RLseFuncRec = (
    (fr_prot:'vargen vargen(variant any)';
     fr_addr:@pp_vargen_create;
     fr_desc:'create variant generator';
    ),
    (fr_prot:'bool get_eof()';
     fr_addr:@pp_vargen_eof;
     fr_desc:'test if finished';
    ),
    (fr_prot:'variant next()';
     fr_addr:@pp_vargen_next;
     fr_desc:'generate next value';
    ),
    (fr_prot:'bool rewind()';
     fr_addr:@pp_vargen_rewind;
     fr_desc:'restart from first element';
    ),
    (fr_prot:'bool send(variable varb)';
     fr_addr:@pp_vargen_send;
     fr_desc:'try sending value into a variable';
    ),
    (fr_prot:'void each(function proc)';
     fr_addr:@pp_system_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'varlist map(function proc)';
     fr_addr:@pp_system_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'variant reduce(variant initValue, function proc)';
     fr_addr:@pp_system_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'varlist filter(function proc)';
     fr_addr:@pp_system_filter;
     fr_desc:'filter item: |item| ... end';
    )
  );

{ stream }

procedure pp_stream_close(const Param: PLseParam);cdecl;
procedure pp_stream_eof(const Param: PLseParam);cdecl;
procedure pp_stream_get_position(const Param: PLseParam);cdecl;
procedure pp_stream_set_position(const Param: PLseParam);cdecl;
procedure pp_stream_get_length(const Param: PLseParam);cdecl;
procedure pp_stream_set_length(const Param: PLseParam);cdecl;
procedure pp_stream_read(const Param: PLseParam);cdecl;
procedure pp_stream_readln(const Param: PLseParam);cdecl;
procedure pp_stream_write(const Param: PLseParam);cdecl;
procedure pp_stream_writeln(const Param: PLseParam);cdecl;
procedure pp_stream_writeTo(const Param: PLseParam);cdecl;
procedure pp_stream_flush(const Param: PLseParam);cdecl;
procedure pp_stream_lines(const Param: PLseParam);cdecl;

const
  KTN_STREAM = 'stream';
  KTD_STREAM = 'stream';

  stream_func_count = 13;
  stream_func_array: array[0..stream_func_count - 1] of RLseFuncRec = (
    (fr_prot:'void close()';
     fr_addr:@pp_stream_close;
     fr_desc:'close stream';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_stream_get_length;
     fr_desc:'get stream size';
    ),
    (fr_prot:'void set_length(int length)';
     fr_addr:@pp_stream_set_length;
     fr_desc:'set stream size';
    ),
    (fr_prot:'bool get_eof()';
     fr_addr:@pp_stream_eof;
     fr_desc:'test if is at end of the stream';
    ),
    (fr_prot:'string get_position()';
     fr_addr:@pp_stream_get_position;
     fr_desc:'get current position';
    ),
    (fr_prot:'void set_position(int newPosition)';
     fr_addr:@pp_stream_set_position;
     fr_desc:'set current position';
    ),
    (fr_prot:'string read(int count)';
     fr_addr:@pp_stream_read;
     fr_desc:'read value';
    ),
    (fr_prot:'string readln()';
     fr_addr:@pp_stream_readln;
     fr_desc:'read a line';
    ),
    (fr_prot:'int write(string text)';
     fr_addr:@pp_stream_write;
     fr_desc:'write into stream';
    ),
    (fr_prot:'int writeln(string text)';
     fr_addr:@pp_stream_writeln;
     fr_desc:'write text and a line break into stream';
    ),
    (fr_prot:'int writeTo(stream stream, int count)';
     fr_addr:@pp_stream_writeTo;
     fr_desc:'write part data into another stream';
    ),
    (fr_prot:'void flush()';
     fr_addr:@pp_stream_flush;
     fr_desc:'flush stream';
    ),
    (fr_prot:'vargen get_lines()';
     fr_addr:@pp_stream_lines;
     fr_desc:'wrap stram as a line generator';
    )
  );

{ char }

procedure pp_char_ord(const Param: PLseParam);cdecl;
procedure pp_char_inMBCS(const Param: PLseParam);cdecl;

const
  char_func_count = 2;
  char_func_array: array[0..char_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int ord()';
     fr_addr:@pp_char_ord;
     fr_desc:'get ordinal value of the character';
    ),
    (fr_prot:'bool inMBCS()';
     fr_addr:@pp_char_inMBCS;
     fr_desc:'test if it is in multibyte character sets';
    )
  );
  
{ string }

procedure pp_string_length(const Param: PLseParam);cdecl;
procedure pp_string_getiv(const Param: PLseParam);cdecl;
procedure pp_string_setAt(const Param: PLseParam);cdecl;
procedure pp_string_name(const Param: PLseParam);cdecl;
procedure pp_string_value(const Param: PLseParam);cdecl;
procedure pp_string_lower(const Param: PLseParam);cdecl;
procedure pp_string_upper(const Param: PLseParam);cdecl;
procedure pp_string_compare(const Param: PLseParam);cdecl;
procedure pp_string_replace(const Param: PLseParam);cdecl;
procedure pp_string_pos(const Param: PLseParam);cdecl;
procedure pp_string_lastPos(const Param: PLseParam);cdecl;
procedure pp_string_left(const Param: PLseParam);cdecl;
procedure pp_string_right(const Param: PLseParam);cdecl;
procedure pp_string_trim(const Param: PLseParam);cdecl;
procedure pp_string_trimLeft(const Param: PLseParam);cdecl;
procedure pp_string_trimRight(const Param: PLseParam);cdecl;
procedure pp_string_trimAll(const Param: PLseParam);cdecl;
procedure pp_string_copy(const Param: PLseParam);cdecl;
procedure pp_string_delete(const Param: PLseParam);cdecl;
procedure pp_string_insert(const Param: PLseParam);cdecl;
procedure pp_string_isAlpha(const Param: PLseParam);cdecl;
procedure pp_string_isAlnum(const Param: PLseParam);cdecl;
procedure pp_string_isCntrl(const Param: PLseParam);cdecl;
procedure pp_string_isDigit(const Param: PLseParam);cdecl;
procedure pp_string_isSpace(const Param: PLseParam);cdecl;
procedure pp_string_isHex(const Param: PLseParam);cdecl;
procedure pp_string_extractName(const Param: PLseParam);cdecl;
procedure pp_string_extractValue(const Param: PLseParam);cdecl;
procedure pp_string_saveToFile(const Param: PLseParam);cdecl;
procedure pp_string_fileText(const Param: PLseParam);cdecl;
procedure pp_string_lformat(const Param: PLseParam);cdecl;
procedure pp_string_rformat(const Param: PLseParam);cdecl;
procedure pp_string_center(const Param: PLseParam);cdecl;
procedure pp_string_randomOrder(const Param: PLseParam);cdecl;
procedure pp_string_html(const Param: PLseParam);cdecl;
procedure pp_string_reverse(const Param: PLseParam);cdecl;
procedure pp_string_isLower(const Param: PLseParam);cdecl;
procedure pp_string_isUpper(const Param: PLseParam);cdecl;
procedure pp_string_translate(const Param: PLseParam);cdecl;
procedure pp_string_filePath(const Param: PLseParam);cdecl;
procedure pp_string_fullFileName(const Param: PLseParam);cdecl;
procedure pp_string_fileName(const Param: PLseParam);cdecl;
procedure pp_string_fileExt(const Param: PLseParam);cdecl;
procedure pp_string_changeExt(const Param: PLseParam);cdecl;
procedure pp_string_md5sum(const Param: PLseParam);cdecl;
procedure pp_string_hexToInt(const Param: PLseParam);cdecl;
procedure pp_string_hash(const Param: PLseParam);cdecl;

const
  KTN_STRING = 'string';
  KTD_STRING = 'string';

  string_func_count = 47;
  string_func_array: array[0..string_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int get_length()';
     fr_addr:@pp_string_length;
     fr_desc:'get string length';
    ),
    (fr_prot:'char getiv(int index)';
     fr_addr:@pp_string_getiv;
     fr_desc:'get char by index';
    ),
    (fr_prot:'string setAt(int index, char value)';
     fr_addr:@pp_string_setAt;
     fr_desc:'set char by index';
    ),
    (fr_prot:'string get_name()';
     fr_addr:@pp_string_name;
     fr_desc:'extract name';
    ),
    (fr_prot:'string get_value()';
     fr_addr:@pp_string_value;
     fr_desc:'extract value';
    ),
    (fr_prot:'int compare(string S2, bool ignoreCase)';
     fr_addr:@pp_string_compare;
     fr_desc:'compare two string';
     ),
    (fr_prot:'string replace(string patten, string newStr, bool ignoreCase, bool replaceFirstOnly)';
     fr_addr:@pp_string_replace;
     fr_desc:'replace patten to new string';
    ),
    (fr_prot:'int pos(string SubStr, bool IgnoreCase)';
     fr_addr:@pp_string_pos;
     fr_desc:'get first sub-string position';
    ),
    (fr_prot:'int lastPos(string SubStr, bool IgnoreCase)';
     fr_addr:@pp_string_lastPos;
     fr_desc:'get last sub-string position';
    ),
    (fr_prot:'string left(int count)';
     fr_addr:@pp_string_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'string right(int count)';
     fr_addr:@pp_string_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'string trim()';
     fr_addr:@pp_string_trim;
     fr_desc:'trim left and right';
    ),
    (fr_prot:'string trimLeft()';
     fr_addr:@pp_string_trimLeft;
     fr_desc:'trim left';
    ),
    (fr_prot:'string trimRight()';
     fr_addr:@pp_string_trimRight;
     fr_desc:'trim right';
    ),
    (fr_prot:'string trimAll()';
     fr_addr:@pp_string_trimAll;
     fr_desc:'trim all spaces';
    ),
    (fr_prot:'string copy(int index, int count)';
     fr_addr:@pp_string_copy;
     fr_desc:'copy sub-string';
    ),
    (fr_prot:'string delete(int index, int count)';
     fr_addr:@pp_string_delete;
     fr_desc:'delete by range';
    ),
    (fr_prot:'string insert(string substr, int index)';
     fr_addr:@pp_string_insert;
     fr_desc:'insert sub-string';
    ),
    (fr_prot:'string extractName(string separator)';
     fr_addr:@pp_string_extractName;
     fr_desc:'extract name with specified separator';
    ),
    (fr_prot:'string extractValue(string separator)';
     fr_addr:@pp_string_extractValue;
     fr_desc:'extract value with specified separator';
    ),
    (fr_prot:'string lformat(int width, char filler)';
     fr_addr:@pp_string_lformat;
     fr_desc:'format to left';
    ),
    (fr_prot:'string rformat(int width, char filler)';
     fr_addr:@pp_string_rformat;
     fr_desc:'format to right';
    ),
    (fr_prot:'string center(int width, char filler)';
     fr_addr:@pp_string_center;
     fr_desc:'format to center';
    ),
    (fr_prot:'string html(bool translateMBC)';
     fr_addr:@pp_string_html;
     fr_desc:'encode to HTML code';
    ),
    (fr_prot:'string random()';
     fr_addr:@pp_string_randomOrder;
     fr_desc:'randomize string charactors';
    ),
    (fr_prot:'string lower()';
     fr_addr:@pp_string_lower;
     fr_desc:'convert to lower case string';
    ),
    (fr_prot:'string upper()';
     fr_addr:@pp_string_upper;
     fr_desc:'convert to upper case string';
    ),
    (fr_prot:'bool isAlpha()';
     fr_addr:@pp_string_isAlpha;
     fr_desc:'test if the string contains only alpha charactors';
    ),
    (fr_prot:'bool isAlnum()';
     fr_addr:@pp_string_isAlnum;
     fr_desc:'test if the string contains only alpha and digit charactors';
    ),
    (fr_prot:'bool isCntrl()';
     fr_addr:@pp_string_isCntrl;
     fr_desc:'test if the string contains only control charactors';
    ),
    (fr_prot:'bool isSpace()';
     fr_addr:@pp_string_isSpace;
     fr_desc:'test if the string contains only space charactors';
    ),
    (fr_prot:'bool isDigit()';
     fr_addr:@pp_string_isDigit;
     fr_desc:'test if the string contains only digit charactors';
    ),
    (fr_prot:'bool isHex()';
     fr_addr:@pp_string_isHex;
     fr_desc:'test if the string contains only HEX charactors';
    ),
    (fr_prot:'string fileText()';
     fr_addr:@pp_string_fileText;
     fr_desc:'get file text';
    ),
    (fr_prot:'void saveToFile(string fileName)';
     fr_addr:@pp_string_saveToFile;
     fr_desc:'save string to file';
    ),
    (fr_prot:'string fullFileName()';
     fr_addr:@pp_string_fullFileName;
     fr_desc:'expand to full file name';
    ),
    (fr_prot:'string filePath()';
     fr_addr:@pp_string_filePath;
     fr_desc:'extract file path';
    ),
    (fr_prot:'string fileName()';
     fr_addr:@pp_string_fileName;
     fr_desc:'extract file name';
    ),
    (fr_prot:'string fileExt()';
     fr_addr:@pp_string_fileExt;
     fr_desc:'extract file extension';
    ),
    (fr_prot:'string changeExt(string newFileExt)';
     fr_addr:@pp_string_changeExt;
     fr_desc:'change file extension';
    ),
    (fr_prot:'string md5sum(bool sumFile)';
     fr_addr:@pp_string_md5sum;
     fr_desc:'calculate MD5 sum';
    ),
    (fr_prot:'int hexToInt(int defaultValue)';
     fr_addr:@pp_string_hexToInt;
     fr_desc:'convert HEX string to integer value';
    ),
    (fr_prot:'string reverse()';
     fr_addr:@pp_string_reverse;
     fr_desc:'reverse string charactors';
    ),
    (fr_prot:'bool isLower()';
     fr_addr:@pp_string_isLower;
     fr_desc:'test if is lower case string';
    ),
    (fr_prot:'bool isUpper()';
     fr_addr:@pp_string_isUpper;
     fr_desc:'test if is upper case string';
    ),
    (fr_prot:'string translate(string OrdCharList, string newCharList)';
     fr_addr:@pp_string_translate;
     fr_desc:'translate original char to new char';
    ),
    (fr_prot:'int hash()';
     fr_addr:@pp_string_hash;
     fr_desc:'calculate hash value';
    )
  );

{ strlist }

procedure pp_strlist_create(const Param: PLseParam);cdecl;
procedure pp_strlist_length(const Param: PLseParam);cdecl;
procedure pp_strlist_getiv(const Param: PLseParam);cdecl;
procedure pp_strlist_setiv(const Param: PLseParam);cdecl;
procedure pp_strlist_read(const Param: PLseParam);cdecl;
procedure pp_strlist_getpv(const Param: PLseParam);cdecl;
procedure pp_strlist_setpv(const Param: PLseParam);cdecl;
procedure pp_strlist_getText(const Param: PLseParam);cdecl;
procedure pp_strlist_setText(const Param: PLseParam);cdecl;
procedure pp_strlist_getCommaText(const Param: PLseParam);cdecl;
procedure pp_strlist_setCommaText(const Param: PLseParam);cdecl;
procedure pp_strlist_add(const Param: PLseParam);cdecl;
procedure pp_strlist_addFrom(const Param: PLseParam);cdecl;
procedure pp_strlist_fill(const Param: PLseParam);cdecl;
procedure pp_strlist_insert(const Param: PLseParam);cdecl;
procedure pp_strlist_move(const Param: PLseParam);cdecl;
procedure pp_strlist_exchange(const Param: PLseParam);cdecl;
procedure pp_strlist_delete(const Param: PLseParam);cdecl;
procedure pp_strlist_clear(const Param: PLseParam);cdecl;
procedure pp_strlist_loadFromFile(const Param: PLseParam);cdecl;
procedure pp_strlist_saveToFile(const Param: PLseParam);cdecl;
procedure pp_strlist_indexOf(const Param: PLseParam);cdecl;
procedure pp_strlist_indexOfName(const Param: PLseParam);cdecl;
procedure pp_strlist_setName(const Param: PLseParam);cdecl;
procedure pp_strlist_setValue(const Param: PLseParam);cdecl;
procedure pp_strlist_names(const Param: PLseParam);cdecl;
procedure pp_strlist_copy(const Param: PLseParam);cdecl;
procedure pp_strlist_sort(const Param: PLseParam);cdecl;
procedure pp_strlist_getSorted(const Param: PLseParam);cdecl;
procedure pp_strlist_setSorted(const Param: PLseParam);cdecl;
procedure pp_strlist_reverse(const Param: PLseParam);cdecl;
procedure pp_strlist_unique(const Param: PLseParam);cdecl;
procedure pp_strlist_getCaseSensitive(const Param: PLseParam);cdecl;
procedure pp_strlist_setCaseSensitive(const Param: PLseParam);cdecl;
procedure pp_strlist_filter(const Param: PLseParam);cdecl;
procedure pp_strlist_min(const Param: PLseParam);cdecl;
procedure pp_strlist_max(const Param: PLseParam);cdecl;
procedure pp_strlist_first(const Param: PLseParam);cdecl;
procedure pp_strlist_last(const Param: PLseParam);cdecl;
procedure pp_strlist_shift(const Param: PLseParam);cdecl;
procedure pp_strlist_pop(const Param: PLseParam);cdecl;

const
  KTN_STRLIST = 'strlist';
  KTD_STRLIST = 'string list';

  strlist_func_count = 42;
  strlist_func_array: array[0..strlist_func_count - 1] of RLseFuncRec = (
    (fr_prot:'strlist strlist(string source)';
     fr_addr:@pp_strlist_create;
     fr_desc:'create string list';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_strlist_length;
     fr_desc:'get strlist length';
    ),
    (fr_prot:'string getiv(int index)';
     fr_addr:@pp_strlist_getiv;
     fr_desc:'get string item by index';
    ),
    (fr_prot:'void setiv(int index, string value)';
     fr_addr:@pp_strlist_setiv;
     fr_desc:'set string item by index';
    ),
    (fr_prot:'string getpv(string name)';
     fr_addr:@pp_strlist_getpv;
     fr_desc:'read value by name';
    ),
    (fr_prot:'void setpv(string name, string value)';
     fr_addr:@pp_strlist_setpv;
     fr_desc:'write value by name';
    ),
    (fr_prot:'string read(string name, string defaultValue)';
     fr_addr:@pp_strlist_read;
     fr_desc:'read value by name';
    ),
    (fr_prot:'void write(string name, string value)';
     fr_addr:@pp_strlist_setpv;
     fr_desc:'write value by name';
    ),
    (fr_prot:'strlist get_names()';
     fr_addr:@pp_strlist_names;
     fr_desc:'get name list';
    ),
    (fr_prot:'string get_text()';
     fr_addr:@pp_strlist_getText;
     fr_desc:'convert to text';
    ),
    (fr_prot:'void set_text(string newText)';
     fr_addr:@pp_strlist_setText;
     fr_desc:'clear and reset its text';
    ),
    (fr_prot:'string get_commaText()';
     fr_addr:@pp_strlist_getCommaText;
     fr_desc:'convert to comma text';
    ),
    (fr_prot:'void set_commaText(string newCommaText)';
     fr_addr:@pp_strlist_setCommaText;
     fr_desc:'clear and set comma text';
    ),
    (fr_prot:'bool get_sorted()';
     fr_addr:@pp_strlist_getSorted;
     fr_desc:'is sorted automatically?';
    ),
    (fr_prot:'void set_sorted(bool value)';
     fr_addr:@pp_strlist_setSorted;
     fr_desc:'set sort mode';
    ),
    (fr_prot:'bool get_caseSensitive()';
     fr_addr:@pp_strlist_getCaseSensitive;
     fr_desc:'is case sensitive?';
    ),
    (fr_prot:'void set_caseSensitive(bool value)';
     fr_addr:@pp_strlist_setCaseSensitive;
     fr_desc:'set case sensitive';
    ),
    (fr_prot:'int add(string S, bool unique)';
     fr_addr:@pp_strlist_add;
     fr_desc:'add item string';
    ),
    (fr_prot:'void addFrom(strlist strings)';
     fr_addr:@pp_strlist_addFrom;
     fr_desc:'add strings from';
    ),
    (fr_prot:'void fill(vargen source, bool clearBeforeFill)';
     fr_addr:@pp_strlist_fill;
     fr_desc:'add generated string';
    ),
    (fr_prot:'void insert(int index, string S)';
     fr_addr:@pp_strlist_insert;
     fr_desc:'insert item string';
    ),
    (fr_prot:'void move(int curIndex, int newIndex)';
     fr_addr:@pp_strlist_move;
     fr_desc:'move item string to another position';
    ),
    (fr_prot:'void exchange(int index1, int index2)';
     fr_addr:@pp_strlist_exchange;
     fr_desc:'exchange two item strings';
    ),
    (fr_prot:'void delete(int index)';
     fr_addr:@pp_strlist_delete;
     fr_desc:'delete by index';
    ),
    (fr_prot:'void clear()';
     fr_addr:@pp_strlist_clear;
     fr_desc:'clear string list';
    ),
    (fr_prot:'void loadFromFile(string fileName)';
     fr_addr:@pp_strlist_loadFromFile;
     fr_desc:'load from file';
    ),
    (fr_prot:'void saveToFile(string fileName)';
     fr_addr:@pp_strlist_saveToFile;
     fr_desc:'save to file';
    ),
    (fr_prot:'int indexOf(string S)';
     fr_addr:@pp_strlist_indexOf;
     fr_desc:'get item string index';
    ),
    (fr_prot:'int indexOfName(string name)';
     fr_addr:@pp_strlist_indexOfName;
     fr_desc:'get item name index';
    ),
    (fr_prot:'void setName(int index, string name)';
     fr_addr:@pp_strlist_setName;
     fr_desc:'set item name by index';
    ),
    (fr_prot:'void setValue(int index, string value)';
     fr_addr:@pp_strlist_setValue;
     fr_desc:'set item value by index';
    ),
    (fr_prot:'strlist copy(int index, int count)';
     fr_addr:@pp_strlist_copy;
     fr_desc:'copy sub string list';
    ),
    (fr_prot:'void sort()';
     fr_addr:@pp_strlist_sort;
     fr_desc:'sort string list';
    ),
    (fr_prot:'void reverse()';
     fr_addr:@pp_strlist_reverse;
     fr_desc:'reverse item string';
    ),
    (fr_prot:'void unique()';
     fr_addr:@pp_strlist_unique;
     fr_desc:'delete multiple strings';
    ),
    (fr_prot:'strlist filter(function filterFunc)';
     fr_addr:@pp_strlist_filter;
     fr_desc:'filter string list';
    ),
    (fr_prot:'string get_min()';
     fr_addr:@pp_strlist_min;
     fr_desc:'get min string item';
    ),
    (fr_prot:'string get_max()';
     fr_addr:@pp_strlist_max;
     fr_desc:'get max string item';
    ),
    (fr_prot:'string get_first()';
     fr_addr:@pp_strlist_first;
     fr_desc:'get first string item';
    ),
    (fr_prot:'string get_last()';
     fr_addr:@pp_strlist_last;
     fr_desc:'get last string item';
    ),
    (fr_prot:'string shift()';
     fr_addr:@pp_strlist_shift;
     fr_desc:'get and delete first string';
    ),
    (fr_prot:'string pop()';
     fr_addr:@pp_strlist_pop;
     fr_desc:'get and delete last string';
    )
  );

{ time }

procedure pp_time_yearOf(const Param: PLseParam);cdecl;
procedure pp_time_yearsBetween(const Param: PLseParam);cdecl;
procedure pp_time_incYear(const Param: PLseParam);cdecl;
procedure pp_time_monthOf(const Param: PLseParam);cdecl;
procedure pp_time_monthsBetween(const Param: PLseParam);cdecl;
procedure pp_time_incMonth(const Param: PLseParam);cdecl;
procedure pp_time_dayOf(const Param: PLseParam);cdecl;
procedure pp_time_dayOfWeek(const Param: PLseParam);cdecl;
procedure pp_time_dayOfYear(const Param: PLseParam);cdecl;
procedure pp_time_daysBetween(const Param: PLseParam);cdecl;
procedure pp_time_incDay(const Param: PLseParam);cdecl;
procedure pp_time_format(const Param: PLseParam);cdecl;
procedure pp_time_weekOf(const Param: PLseParam);cdecl;
procedure pp_time_weekOfMonth(const Param: PLseParam);cdecl;
procedure pp_time_weeksBetween(const Param: PLseParam);cdecl;
procedure pp_time_hourOf(const Param: PLseParam);cdecl;
procedure pp_time_hoursBetween(const Param: PLseParam);cdecl;
procedure pp_time_minuteOf(const Param: PLseParam);cdecl;
procedure pp_time_minutesBetween(const Param: PLseParam);cdecl;
procedure pp_time_secondOf(const Param: PLseParam);cdecl;
procedure pp_time_secondsBetween(const Param: PLseParam);cdecl;
procedure pp_time_milliSecondOf(const Param: PLseParam);cdecl;
procedure pp_time_milliSecondsBetween(const Param: PLseParam);cdecl;

const
  KTN_TIME = 'time';
  KTD_TIME = 'time';

  time_func_count = 23;
  time_func_array: array[0..time_func_count - 1] of RLseFuncRec = (
    (fr_prot:'int year()';
     fr_addr:@pp_time_yearOf;
     fr_desc:'get year';
    ),
    (fr_prot:'int yearsBetween(time then)';
     fr_addr:@pp_time_yearsBetween;
     fr_desc:'get years between two time';
    ),
    (fr_prot:'time incYear(int years)';
     fr_addr:@pp_time_incYear;
     fr_desc:'increase years';
    ),
    (fr_prot:'int month()';
     fr_addr:@pp_time_monthOf;
     fr_desc:'get month';
    ),
    (fr_prot:'int monthsBetween(time then)';
     fr_addr:@pp_time_monthsBetween;
     fr_desc:'get months between two time';
    ),
    (fr_prot:'time incMonth(int months)';
     fr_addr:@pp_time_incMonth;
     fr_desc:'increase months';
    ),
    (fr_prot:'int day()';
     fr_addr:@pp_time_dayOf;
     fr_desc:'get day';
    ),
    (fr_prot:'int weekDay()';
     fr_addr:@pp_time_dayOfWeek;
     fr_desc:'get week day';
    ),
    (fr_prot:'int dayOfYear()';
     fr_addr:@pp_time_dayOfYear;
     fr_desc:'get day of the year';
    ),
    (fr_prot:'int daysBetween(time then)';
     fr_addr:@pp_time_daysBetween;
     fr_desc:'get days between two time';
    ),
    (fr_prot:'time incDay(int days)';
     fr_addr:@pp_time_incDay;
     fr_desc:'increase days';
    ),
    (fr_prot:'int week()';
     fr_addr:@pp_time_weekOf;
     fr_desc:'get week';
    ),
    (fr_prot:'int weekOfMonth()';
     fr_addr:@pp_time_weekOfMonth;
     fr_desc:'get week of the month';
    ),
    (fr_prot:'int weeksBetween(time then)';
     fr_addr:@pp_time_weeksBetween;
     fr_desc:'get weeks between two time';
    ),
    (fr_prot:'int hour()';
     fr_addr:@pp_time_hourOf;
     fr_desc:'get hour';
    ),
    (fr_prot:'int hoursBetween(time then)';
     fr_addr:@pp_time_hoursBetween;
     fr_desc:'get hours between two time';
    ),
    (fr_prot:'int minute()';
     fr_addr:@pp_time_minuteOf;
     fr_desc:'get minute';
    ),
    (fr_prot:'int minutesBetween(time then)';
     fr_addr:@pp_time_minutesBetween;
     fr_desc:'get minutes between two time';
    ),
    (fr_prot:'int second()';
     fr_addr:@pp_time_secondOf;
     fr_desc:'get second';
    ),
    (fr_prot:'int secondsBetween(time then)';
     fr_addr:@pp_time_secondsBetween;
     fr_desc:'get seconds between two time';
    ),
    (fr_prot:'int millisecond()';
     fr_addr:@pp_time_milliSecondOf;
     fr_desc:'get millisecond';
    ),
    (fr_prot:'int millisecondsBetween(time then)';
     fr_addr:@pp_time_milliSecondsBetween;
     fr_desc:'get milliseconds between two time';
    ),
    (fr_prot:'string format(string fmt)';
     fr_addr:@pp_time_format;
     fr_desc:'format date time';
    )
  );

{ type }

procedure pp_type_name(const Param: PLseParam);cdecl;
procedure pp_type_description(const Param: PLseParam);cdecl;
procedure pp_type_simple(const Param: PLseParam);cdecl;
procedure pp_type_builtin(const Param: PLseParam);cdecl;
procedure pp_type_info(const Param: PLseParam);cdecl;
procedure pp_type_module(const Param: PLseParam);cdecl;
procedure pp_type_methods(const Param: PLseParam);cdecl;
procedure pp_type_getpv(const Param: PLseParam);cdecl;
procedure pp_type_isUDC(const Param: PLseParam);cdecl;
procedure pp_type_addr(const Param: PLseParam);cdecl;

const
  KTN_TYPE = 'type';
  KTD_TYPE = 'type';

  type_func_count = 10;
  type_func_array: array[0..type_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string get_name()';
     fr_addr:@pp_type_name;
     fr_desc:'type name';
    ),
    (fr_prot:'module get_module()';
     fr_addr:@pp_type_module;
     fr_desc:'owner module';
    ),
    (fr_prot:'varlist get_methods()';
     fr_addr:@pp_type_methods;
     fr_desc:'get method list';
    ),
    (fr_prot:'string get_description()';
     fr_addr:@pp_type_description;
     fr_desc:'type description';
    ),
    (fr_prot:'bool get_simple()';
     fr_addr:@pp_type_simple;
     fr_desc:'is simple type?';
    ),
    (fr_prot:'bool get_builtin()';
     fr_addr:@pp_type_builtin;
     fr_desc:'is builtin type?';
    ),
    (fr_prot:'string get_info()';
     fr_addr:@pp_type_info;
     fr_desc:'type information';
    ),
    (fr_prot:'function getpv(string methodName)';
     fr_addr:@pp_type_getpv;
     fr_desc:'get method function by name';
    ),
    (fr_prot:'bool get_isUDC()';
     fr_addr:@pp_type_isUDC;
     fr_desc:'is user defined class?';
    ),
    (fr_prot:'string get_addr()';
     fr_addr:@pp_type_addr;
     fr_desc:'type address';
    )
  );

{ variant }

  KTN_VARIANT = 'variant';
  KTD_VARIANT = 'variant data type';

{ variable }

procedure pp_varb_name(const Param: PLseParam);cdecl;
procedure pp_varb_type(const Param: PLseParam);cdecl;
procedure pp_varb_func(const Param: PLseParam);cdecl;

const
  KTN_VARIABLE   = 'variable';
  KTD_VARIABLE   = 'variable';

  varb_func_count = 3;
  varb_func_array: array[0..varb_func_count - 1] of RLseFuncRec = (
    (fr_prot:'string get_name()';
     fr_addr:@pp_varb_name;
     fr_desc:'variable name';
    ),
    (fr_prot:'type get_type()';
     fr_addr:@pp_varb_type;
     fr_desc:'variable type';
    ),
    (fr_prot:'function get_func()';
     fr_addr:@pp_varb_func;
     fr_desc:'owner function';
    )
  );

{ varlist }

procedure pp_varlist_create(const Param: PLseParam);cdecl;
procedure pp_varlist_getpv(const Param: PLseParam);cdecl;
procedure pp_varlist_setpv(const Param: PLseParam);cdecl;
procedure pp_varlist_isset(const Param: PLseParam);cdecl;
procedure pp_varlist_read(const Param: PLseParam);cdecl;
procedure pp_varlist_remove(const Param: PLseParam);cdecl;
procedure pp_varlist_keys(const Param: PLseParam);cdecl;
procedure pp_varlist_get_name(const Param: PLseParam);cdecl;
procedure pp_varlist_get_length(const Param: PLseParam);cdecl;
procedure pp_varlist_set_length(const Param: PLseParam);cdecl;
procedure pp_varlist_getiv(const Param: PLseParam);cdecl;
procedure pp_varlist_setiv(const Param: PLseParam);cdecl;
procedure pp_varlist_exchange(const Param: PLseParam);cdecl;
procedure pp_varlist_move(const Param: PLseParam);cdecl;
procedure pp_varlist_add(const Param: PLseParam);cdecl;
procedure pp_varlist_addFrom(const Param: PLseParam);cdecl;
procedure pp_varlist_fill(const Param: PLseParam);cdecl;
procedure pp_varlist_insert(const Param: PLseParam);cdecl;
procedure pp_varlist_delete(const Param: PLseParam);cdecl;
procedure pp_varlist_clear(const Param: PLseParam);cdecl;
procedure pp_varlist_copy(const Param: PLseParam);cdecl;
procedure pp_varlist_left(const Param: PLseParam);cdecl;
procedure pp_varlist_right(const Param: PLseParam);cdecl;
procedure pp_varlist_filter(const Param: PLseParam);cdecl;
procedure pp_varlist_min(const Param: PLseParam);cdecl;
procedure pp_varlist_max(const Param: PLseParam);cdecl;
procedure pp_varlist_first(const Param: PLseParam);cdecl;
procedure pp_varlist_last(const Param: PLseParam);cdecl;
procedure pp_varlist_shift(const Param: PLseParam);cdecl;
procedure pp_varlist_pop(const Param: PLseParam);cdecl;

const
  KTN_VARLIST = 'varlist';
  KTD_VARLIST = 'variant list';

  varlist_func_count = 31;
  varlist_func_array: array[0..varlist_func_count - 1] of RLseFuncRec = (
    (fr_prot:'varlist varlist(int count)';
     fr_addr:@pp_varlist_create;
     fr_desc:'create variant list';
    ),
    (fr_prot:'variant getpv(string name)';
     fr_addr:@pp_varlist_getpv;
     fr_desc:'get value by name';
    ),
    (fr_prot:'void setpv(string name, variant value)';
     fr_addr:@pp_varlist_setpv;
     fr_desc:'set value by name';
    ),
    (fr_prot:'bool isset(string name)';
     fr_addr:@pp_varlist_isset;
     fr_desc:'check if value exists';
    ),
    (fr_prot:'variant read(string name)';
     fr_addr:@pp_varlist_read;
     fr_desc:'read value by name';
    ),
    (fr_prot:'bool remove(string name)';
     fr_addr:@pp_varlist_remove;
     fr_desc:'remove value by name';
    ),
    (fr_prot:'strlist keys(bool sorted)';
     fr_addr:@pp_varlist_keys;
     fr_desc:'get key name list';
    ),
    (fr_prot:'string getName(int index)';
     fr_addr:@pp_varlist_get_name;
     fr_desc:'get name by index';
    ),
    (fr_prot:'void write(string name, variant value)';
     fr_addr:@pp_varlist_setpv;
     fr_desc:'write value by name';
    ),
    (fr_prot:'int get_length()';
     fr_addr:@pp_varlist_get_length;
     fr_desc:'get list size';
    ),
    (fr_prot:'void set_length(int count)';
     fr_addr:@pp_varlist_set_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'variant getiv(int index)';
     fr_addr:@pp_varlist_getiv;
     fr_desc:'get value by index';
    ),
    (fr_prot:'void setiv(int index, variant value)';
     fr_addr:@pp_varlist_setiv;
     fr_desc:'set value by index';
    ),
    (fr_prot:'void exchange(int X1, int X2)';
     fr_addr:@pp_varlist_exchange;
     fr_desc:'exchange by index';
    ),
    (fr_prot:'void move(int curIndex, int newIndex)';
     fr_addr:@pp_varlist_move;
     fr_desc:'move variant to new position';
    ),
    (fr_prot:'int add(variant value)';
     fr_addr:@pp_varlist_add;
     fr_desc:'add variant';
    ),
    (fr_prot:'int addFrom(varlist variants)';
     fr_addr:@pp_varlist_addFrom;
     fr_desc:'add variants from';
    ),
    (fr_prot:'void fill(vargen source, bool clearBeforeFill)';
     fr_addr:@pp_varlist_fill;
     fr_desc:'fill generated value';
    ),
    (fr_prot:'void insert(int index, variant value)';
     fr_addr:@pp_varlist_insert;
     fr_desc:'insert variant at specified position';
    ),
    (fr_prot:'void delete(int index)';
     fr_addr:@pp_varlist_delete;
     fr_desc:'delete variant by index';
    ),
    (fr_prot:'void clear()';
     fr_addr:@pp_varlist_clear;
     fr_desc:'clear variant list';
    ),
    (fr_prot:'varlist copy(int index, int count)';
     fr_addr:@pp_varlist_copy;
     fr_desc:'copy to another variant list';
    ),
    (fr_prot:'varlist left(int count)';
     fr_addr:@pp_varlist_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'varlist right(int count)';
     fr_addr:@pp_varlist_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'varlist filter(function filterFunc)';
     fr_addr:@pp_varlist_filter;
     fr_desc:'filter variant list';
    ),
    (fr_prot:'variant get_min()';
     fr_addr:@pp_varlist_min;
     fr_desc:'get min variant item';
    ),
    (fr_prot:'variant get_max()';
     fr_addr:@pp_varlist_max;
     fr_desc:'get max variant item';
    ),
    (fr_prot:'variant get_first()';
     fr_addr:@pp_varlist_first;
     fr_desc:'get first variant item';
    ),
    (fr_prot:'variant get_last()';
     fr_addr:@pp_varlist_last;
     fr_desc:'get last variant item';
    ),
    (fr_prot:'variant shift()';
     fr_addr:@pp_varlist_shift;
     fr_desc:'get and delete first item';
    ),
    (fr_prot:'variant pop()';
     fr_addr:@pp_varlist_pop;
     fr_desc:'get and delete last item';
    )
  );

{ void }

  KTN_VOID = 'void';
  KTD_VOID = 'void';

{ OTOS: object to string }

function otos_error(inst: pointer): PLseString;cdecl;
function otos_function(inst: pointer): PLseString;cdecl;
function otos_module(inst: pointer): PLseString;cdecl;
function otos_dataset(inst: pointer): PLseString;cdecl;
function otos_strlist(inst: pointer): PLseString;cdecl;
function otos_type(inst: pointer): PLseString;cdecl;
function otos_variable(inst: pointer): PLseString;cdecl;
function otos_varlist(inst: pointer): PLseString;cdecl;

{ STOO: string to object}

function stoo_module(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_strlist(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_type(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_variable(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_varlist(S: PLseString; KernelEngine: pointer): pointer;cdecl;

{ WTOS: write to stream }

function wtos_strlist(inst: pointer; stream: PLseStream): integer;cdecl;
function wtos_stream(inst: pointer; stream: PLseStream): integer;cdecl;
                             
{ CVGR: create RLseVargen record}

function cvgr_strlist(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_varlist(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_anylist(obj: pointer; crec: PLseClassRec; eng: KLiEngine): PLseVargen;cdecl;
function cvgr_func(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_stream(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_stream_lines(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_string(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_dataset(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_upto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_downto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_combine(List: KLiVarList; Count: integer): PLseVargen;

{ ADDI: add item }

function addi_strlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function addi_varlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function addi_stream(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function addi_function(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;

{ sqldb }

var
  db_vendor_list: array of RLseDBVendor;

function dbv_index(const Vendor: string): integer;
function dbv_register(dbv: PLseDBVendor): boolean;
function dbv_provide(const Vendor: string): PLseDB;

{ sys-curry }

function curry_func(func: KLiFunc; params: KLiVarList; module: KLiModule): KLiFunc;
function curry_one(func: KLiFunc; Value: PLseValue; module: KLiModule): KLiFunc;

implementation

uses
  Math, StrUtils, DateUtils, lse_symbol, lse_spawn, lse_patten;

{ OTOS: object to string }

function otos_error(inst: pointer): PLseString;cdecl;
begin
  if inst <> nil then
    Result := lse_strec_alloc(KLiError(inst).ErrorText) else
    Result := nil;
end;

function otos_function(inst: pointer): PLseString;cdecl;
var
  func: KLiFunc;
begin
  func := KLiFunc(inst);
  if func <> nil then
    Result := lse_strec_alloc(KLiFunc(inst).Prototype(false)) else
    Result := nil;
end;

function otos_module(inst: pointer): PLseString;cdecl;
begin
  if inst <> nil then
    Result := lse_strec_alloc(KLiModule(inst).Name) else
    Result := nil;
end;

function otos_dataset(inst: pointer): PLseString;cdecl;
var
  this: PLseDS;
  bm: pointer;
  ss: TStringStream;
  index, count: integer;
  sr: PLseString;
begin
  Result := nil;
  this := PLseDS(inst);
  if this <> nil then
  begin
    count := lse_ds_field_count(this);
    ss := TStringStream.Create('');
    try
      bm := this^.ds_getBMK(this);
      lse_ds_check(this);
      try
        // 1. list field names
        for index := 0 to count - 1 do
        begin
          if index > 0 then
            lse_stream_write(ss, ',');
          sr := lse_ds_fname(this, index, false);
          lse_strec_inclife(sr);
          lse_stream_write(ss, sr);
          lse_strec_declife(sr);
        end;
        lse_stream_writeln(ss);

        // 2. seek to first record
        lse_ds_first(this);

        // 3. show all records
        while not lse_ds_eof(this) do
        begin
          for index := 0 to count - 1 do
          begin
            if index > 0 then
              lse_stream_write(ss, ',');
            sr := lse_ds_getfs(this, index);
            lse_strec_inclife(sr);
            lse_stream_write(ss, sr);
            lse_strec_declife(sr);
          end;
          lse_stream_writeln(ss);
          lse_ds_next(this);
        end;
      finally
        this^.ds_gotoBMK(this, bm);
        lse_ds_check(this);
        this^.ds_freeBMK(this, bm);
        lse_ds_check(this);
      end;
      Result := lse_strec_alloc(ss.DataString);
    finally
      ss.Free;
    end;
  end;
end;

function otos_strlist(inst: pointer): PLseString;cdecl;
begin
  if inst <> nil then
    Result := lse_strec_alloc(KLiStrlist(inst).Text) else
    Result := nil;
end;

function otos_type(inst: pointer): PLseString;cdecl;
var
  clss: KLiClass;
begin
  clss := KLiClass(inst);
  if clss <> nil then
    Result := lse_strec_alloc(clss.FullName) else
    Result := nil;
end;

function otos_variable(inst: pointer): PLseString;cdecl;
var
  varb: KLiVarb;
begin
  if inst <> nil then
  begin
    varb := KLiVarb(inst);
    Result := lse_strec_alloc(varb.Prototype(false));
  end
  else Result := nil;
end;

function otos_varlist(inst: pointer): PLseString;cdecl;
begin
  if inst <> nil then
    Result := lse_strec_alloc(KLiVarList(inst).AsString) else
    Result := nil;
end;

{ STOO: string to object}

function stoo_module(S: PLseString; KernelEngine: pointer): pointer;cdecl;
begin
  Result := KLiEngine(KernelEngine).MainRunner.CurrentFunc.Module.
            FindModule(lse_strec_string(S), false);
end;

function stoo_strlist(S: PLseString; KernelEngine: pointer): pointer;cdecl;
begin
  Result := KLiStrlist.Create(lse_strec_string(S));
end;

function stoo_type(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  o_name, m_name, T: string;
  module: KLiModule;
begin
  module := KLiEngine(KernelEngine).MainRunner.CurrentFunc.Module;
  T := lse_strec_string(S);
  o_name := __decodeClassName(T, m_name);
  if (m_name <> '') or (System.Pos('::', T) > 0) then
  begin
    if m_name <> '' then
      module := module.FindModule(m_name, false); 
    Result := module.FindClass(o_name);
  end
  else Result := module.FindClassBy(o_name, '');
end;

function stoo_variable(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  func: KLiFunc;
  T: string;
begin
  func := KLiEngine(KernelEngine).MainRunner.CurrentFunc;
  T := lse_strec_string(S);
  Result := func.Params.Find(T);
  if (Result = nil) and (func.Codes.Locals <> nil) then
    Result := func.Codes.Locals.Find(T);
end;

function stoo_varlist(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  list: KLiVarList;
  strs: TStrings;
  index: integer;
begin
  list := __NewVarlist(KLiEngine(KernelEngine));
  strs := TStringList.Create;
  try
    strs.Text := lse_strec_string(S);
    for index := 0 to strs.Count - 1 do
      list.PushString(strs[index]);
  finally
    strs.Free;
  end;
  Result := list;
end;

{ WTOS: write to stream }

function wtos_strlist(inst: pointer; stream: PLseStream): integer;cdecl;
begin
  if (inst <> nil) and (stream <> nil) then
    Result := lse_stream_write(stream, KLiStrlist(inst).Text) else
    Result := 0;
end;

function wtos_stream(inst: pointer; stream: PLseStream): integer;cdecl;
begin
  if (inst <> nil) and (stream <> nil) and (inst <> stream) then
    Result := lse_stream_fill(stream, PLseStream(inst)) else
    Result := 0;
end;

{ CVGR: create RLseVargen record}

type
  RLiVG_strlist = packed record
    vgrec: RLseVargen;
    vgref: integer;
    list : KLiStrlist;
    index: integer;
  end;
  PLiVG_strlist = ^RLiVG_strlist;
  
function cvgr_strlist_rewind(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_strlist;
begin
  cvgr := PLiVG_strlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    index := 0;
    Result := Ord(index < list.Count);
  end;
end;

function cvgr_strlist_hasNext(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_strlist;
begin
  cvgr := PLiVG_strlist(vrec^.vg_data); 
  with cvgr^ do
    Result := Ord(index < list.Count);
end;

function cvgr_strlist_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  cvgr: PLiVG_strlist;
begin
  cvgr := PLiVG_strlist(vrec^.vg_data);
  with cvgr^ do
    if index < list.Count then
    begin
      lse_set_string(Value, list[index]);
      Result := 1;
      Inc(index);
    end
    else Result := 0;
end;

function cvgr_strlist_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_strlist;
begin
  cvgr := PLiVG_strlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.IncRefcount;
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_strlist));
  end;
end;

function cvgr_strlist_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_strlist;
begin
  cvgr := PLiVG_strlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.DecRefcount;
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_strlist));
  end;
end;

function cvgr_strlist_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := Ord(__contains(PLiVG_strlist(vrec^.vg_data)^.list, Value));
end;

function cvgr_strlist(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  list: KLiStrlist;
  cvgr: PLiVG_strlist;
begin
  list := KLiStrlist(obj);
  if list <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_strlist));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_strlist_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_strlist_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_strlist_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_strlist_addref;
    cvgr^.vgrec.vg_release := @cvgr_strlist_release;
    cvgr^.vgrec.vg_contains := @cvgr_strlist_contains;
    cvgr^.vgref := 0;
    cvgr^.list := list;
    cvgr^.index := 0;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_varlist = packed record
    vgrec: RLseVargen;
    vgref: integer;
    list : KLiVarList;
    index: integer;
  end;
  PLiVG_varlist = ^RLiVG_varlist;
  
function cvgr_varlist_rewind(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    index := 0;
    Result := Ord(index < list.Count);
  end;
end;

function cvgr_varlist_hasNext(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
    Result := Ord(index < list.Count);
end;

function cvgr_varlist_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data);
  with cvgr^ do
    if index < list.Count then
    begin
      lse_set_value(Value, list[index]);
      Result := 1;
      Inc(index);
    end
    else Result := 0;
end;

function cvgr_varlist_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.IncRefcount;
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_varlist));
  end;
end;

function cvgr_varlist_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_varlist;
begin
  cvgr := PLiVG_varlist(vrec^.vg_data); 
  with cvgr^ do
  begin
    list.DecRefcount;
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_varlist));
  end;
end;

function cvgr_varlist_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := Ord(__contains(PLiVG_varlist(vrec^.vg_data)^.list, Value, false));
end;

function cvgr_varlist(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  list: KLiVarList;
  cvgr: PLiVG_varlist;
begin
  list := KLiVarList(obj);
  if list <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_varlist));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_varlist_rewind;
    cvgr^.vgrec.vg_has_next := @cvgr_varlist_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_varlist_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_varlist_addref;
    cvgr^.vgrec.vg_release := @cvgr_varlist_release;
    cvgr^.vgrec.vg_contains := @cvgr_varlist_contains;
    cvgr^.vgref := 0;
    cvgr^.list := list;
    cvgr^.index := 0;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_anylist = packed record
    vgrec: RLseVargen;
    vgref: integer;
    list: pointer;
    clss: KLiClass;
    index: int64;
    engine: KLiEngine;
    count: KLiFunc;
    getiv: KLiFunc;
  end;
  PLiVG_anylist = ^RLiVG_anylist;

function cvgr_anylist_count(anylist: PLiVG_anylist): integer;
var
  runner: KLiRunner;
  data: RLseValue;
begin
  lse_init_value(@data);
  try
    runner := anylist^.engine.MainRunner;
    runner.Stack.PushObject(anylist^.list, anylist^.clss);
    runner.Goon(anylist^.count, 1, @data);
    Result := __AsInt64(@data);
  finally
    lse_clear_value(@data);
  end;
end;

function cvgr_anylist_rewind(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_anylist;
begin
  cvgr := PLiVG_anylist(vrec^.vg_data); 
  with cvgr^ do
  begin
    index := 0;
    Result := Ord(index < cvgr_anylist_count(cvgr));
  end;
end;

function cvgr_anylist_hasNext(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_anylist;
begin
  cvgr := PLiVG_anylist(vrec^.vg_data); 
  with cvgr^ do
    Result := Ord(index < cvgr_anylist_count(cvgr));
end;

function cvgr_anylist_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  cvgr: PLiVG_anylist;
  runner: KLiRunner;
begin
  cvgr := PLiVG_anylist(vrec^.vg_data);
  with cvgr^ do
    if index < cvgr_anylist_count(cvgr) then
    begin
      runner := engine.MainRunner;
      runner.Stack.PushObject(list, clss);
      runner.Stack.PushInt64(index);
      runner.Goon(getiv, 2, Value);
      Inc(index);
      Result := 1;
    end
    else Result := 0;
end;

function cvgr_anylist_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_anylist;
begin
  cvgr := PLiVG_anylist(vrec^.vg_data); 
  with cvgr^ do
  begin
    clss.ClassRec^.incRefcount(list);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_anylist));
  end;
end;

function cvgr_anylist_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_anylist;
begin
  cvgr := PLiVG_anylist(vrec^.vg_data); 
  with cvgr^ do
  begin
    clss.ClassRec^.decRefcount(list);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_anylist));
  end;
end;

function cvgr_anylist(obj: pointer; crec: PLseClassRec; eng: KLiEngine): PLseVargen;cdecl;
var
  list: KLiVarList;
  clss: KLiClass;
  cvgr: PLiVG_anylist;
  count, getiv: KLiFunc;
begin
  Result := nil;
  list := KLiVarList(obj);
  if list <> nil then
  begin
    clss := KLiClass(crec^.lysee_class);
    count := clss.SingleMethod(cmCount);
    if count <> nil then
    begin
      getiv := clss.SingleMethod(cmGetAt);
      if getiv <> nil then
      begin
        cvgr := lse_mem_alloc_zero(sizeof(RLiVG_anylist));
        cvgr^.vgrec.vg_data := cvgr;
        cvgr^.vgrec.vg_engine := eng;
        cvgr^.vgrec.vg_rewind := @cvgr_anylist_rewind;
        cvgr^.vgrec.vg_has_next := @cvgr_anylist_hasNext;
        cvgr^.vgrec.vg_get_next := @cvgr_anylist_getNext;
        cvgr^.vgrec.vg_addref := @cvgr_anylist_addref;
        cvgr^.vgrec.vg_release := @cvgr_anylist_release;
        cvgr^.vgref := 0;
        cvgr^.list := list;
        cvgr^.clss := clss;
        cvgr^.index := 0;
        cvgr^.engine := eng;
        cvgr^.count := count;
        cvgr^.getiv := getiv;
        Result := @(cvgr^.vgrec);
      end;
    end;
  end;
end;

type
  RLiVG_func = packed record
    vgrec: RLseVargen;
    vgref: integer;
    func: KLiFunc;
  end;
  PLiVG_func = ^RLiVG_func;

function cvgr_func_rewind(vrec: PLseVargen): integer;cdecl;
begin
  Result := 1;
end;

function cvgr_func_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  Result := cvgr_func_rewind(vrec);
end;

function cvgr_func_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  func: KLiFunc;
begin
  if cvgr_func_hasNext(vrec) <> 0 then
  begin
    func := PLiVG_func(vrec^.vg_data)^.func;
    KLiEngine(vrec^.vg_engine).MainRunner.Goon(func, 0, value);
    Result := 1;
  end
  else Result := 0;
end;

function cvgr_func_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_func;
begin
  cvgr := PLiVG_func(vrec^.vg_data); 
  with cvgr^ do
  begin
    func.IncRefcount;
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_func));
  end;
end;

function cvgr_func_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_func;
begin
  cvgr := PLiVG_func(vrec^.vg_data); 
  with cvgr^ do
  begin
    func.DecRefcount;
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_func));
  end;
end;

function cvgr_func(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  func: KLiFunc;
  cvgr: PLiVG_func;
begin
  Result := nil;
  func := KLiFunc(obj);
  if func <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_func));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_func_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_func_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_func_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_func_addref;
    cvgr^.vgrec.vg_release := @cvgr_func_release;
    cvgr^.vgref := 0;
    cvgr^.func := func;
    Result := @(cvgr^.vgrec);
  end;
end;

type
  RLiVG_stream = packed record
    vgrec: RLseVargen;
    vgref: integer;
    stream: PLseStream;
  end;
  PLiVG_stream = ^RLiVG_stream;

function cvgr_stream_rewind(vrec: PLseVargen): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  lse_stream_seek(stream, 0);
  Result := Ord(not lse_stream_eof(stream));
end;

function cvgr_stream_hasNext(vrec: PLseVargen): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  Result := Ord(not lse_stream_eof(stream));
end;

function cvgr_stream_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  stream: PLseStream;
  ch: char;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  if not lse_stream_eof(stream) then
  begin
    Result := lse_stream_read(stream, @ch, sizeof(char));
    if Result > 0 then
      lse_set_char(Value, ch);
  end
  else Result := 0;
end;

function cvgr_stream_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_stream;
begin
  cvgr := PLiVG_stream(vrec^.vg_data); 
  with cvgr^ do
  begin
    stream^.addref(stream);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_stream));
  end;
end;

function cvgr_stream_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_stream;
begin
  cvgr := PLiVG_stream(vrec^.vg_data); 
  with cvgr^ do
  begin
    stream^.release(stream);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_stream));
  end;
end;

function cvgr_stream(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  stream: PLseStream;
  cvgr: PLiVG_stream;
begin
  stream := PLseStream(obj);
  if stream <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_stream));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_stream_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_stream_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_stream_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_stream_addref;
    cvgr^.vgrec.vg_release := @cvgr_stream_release;
    cvgr^.vgref := 0;
    cvgr^.stream := stream;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

function cvgr_stream_getLine(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  stream: PLseStream;
begin
  stream := PLiVG_stream(vrec^.vg_data)^.stream;
  if not lse_stream_eof(stream) then
  begin
    lse_set_string(Value, lse_stream_readln(stream));
    Result := 1;
  end
  else Result := 0;
end;

function cvgr_stream_lines(obj, kernel_engine: pointer): PLseVargen;cdecl;
begin
  Result := cvgr_stream(obj, kernel_engine);
  Result^.vg_get_next := @cvgr_stream_getLine;
end;

type
  RLiVG_string = packed record
    vgrec: RLseVargen;
    vgref: integer;
    srec: PLseString;
    slen: integer;
    index: integer;
  end;
  PLiVG_string = ^RLiVG_string;

function cvgr_string_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
  begin
    index := 0;
    Result := Ord(index < slen);
  end;
end;

function cvgr_string_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
    Result := Ord(index < slen);
end;

function cvgr_string_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_string(vrec^.vg_data)^ do
    if index < slen then
    begin
      lse_set_char(Value, lse_strec_data(srec)[index]);
      Inc(index);
      Result := 1;
    end
    else Result := 0;
end;

function cvgr_string_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_string;
begin
  cvgr := PLiVG_string(vrec^.vg_data); 
  with cvgr^ do
  begin
    lse_strec_inclife(srec);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_string));
  end;
end;

function cvgr_string_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_string;
begin
  cvgr := PLiVG_string(vrec^.vg_data);
  with cvgr^ do
  begin
    lse_strec_declife(srec);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_string));
  end;
end;

function cvgr_string_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  Result := Ord(__contains(PLiVG_string(vrec^.vg_data)^.srec, Value));
end;

function cvgr_string(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  srec: PLseString;
  slen: integer;
  cvgr: PLiVG_string;
begin
  srec := PLseString(obj);
  slen := lse_strec_length(srec);
  if slen > 0 then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_string));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_string_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_string_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_string_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_string_addref;
    cvgr^.vgrec.vg_release := @cvgr_string_release;
    cvgr^.vgrec.vg_contains := @cvgr_string_contains;
    cvgr^.vgref := 0;
    cvgr^.srec := srec;
    cvgr^.slen := slen;
    cvgr^.index := 0;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_dataset = packed record
    vgrec: RLseVargen;
    vgref: integer;
    dsrec: PLseDS;
    nextf: boolean;
  end;
  PLiVG_dataset = ^RLiVG_dataset;

function cvgr_dataset_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    nextf := false;
    lse_ds_first(dsrec);
    Result := Ord(not lse_ds_eof(dsrec));
  end;
end;

function cvgr_dataset_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    if nextf then
    begin
      nextf := false;
      lse_ds_next(dsrec);
    end;
    Result := Ord(not lse_ds_eof(dsrec));
  end;
end;

function cvgr_dataset_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_dataset(vrec^.vg_data)^ do
  begin
    if nextf then
    begin
      nextf := false;
      lse_ds_next(dsrec);
    end;
    if not lse_ds_eof(dsrec) then
    begin
      lse_set_object(Value, KR_DS, dsrec);
      nextf := true;
      Result := 1;
    end
    else Result := 0;
  end;
end;

function cvgr_dataset_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_dataset;
begin
  cvgr := PLiVG_dataset(vrec^.vg_data); 
  with cvgr^ do
  begin
    lse_ds_addref(dsrec);
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_dataset));
  end;
end;

function cvgr_dataset_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_dataset;
begin
  cvgr := PLiVG_dataset(vrec^.vg_data); 
  with cvgr^ do
  begin
    lse_ds_release(dsrec);
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_dataset));
  end;
end;

function cvgr_dataset(obj, kernel_engine: pointer): PLseVargen;cdecl;
var
  dsrec: PLseDS;
  cvgr: PLiVG_dataset;
begin
  dsrec := PLseDS(obj);
  if dsrec <> nil then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_dataset));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_dataset_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_dataset_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_dataset_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_dataset_addref;
    cvgr^.vgrec.vg_release := @cvgr_dataset_release;
    cvgr^.vgref := 0;
    cvgr^.dsrec := dsrec;
    cvgr^.nextf := false;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_range = packed record
    vgrec: RLseVargen;
    vgref: integer;
    begv, endv, step, curr: int64;
  end;
  PLiVG_range = ^RLiVG_range;

function cvgr_upto_rewind(vrec: PLseVargen): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
  begin
    curr := begv;
    Result := Ord(curr <= endv);
  end;
end;

function cvgr_upto_hasNext(vrec: PLseVargen): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
    Result := Ord(curr <= endv);
end;

function cvgr_upto_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  range: PLiVG_range;
begin
  range := PLiVG_range(vrec^.vg_data);
  with range^ do
    if curr <= endv then
    begin
      lse_set_int64(Value, curr);
      Inc(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function cvgr_range_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_range;
begin
  cvgr := PLiVG_range(vrec^.vg_data); 
  with cvgr^ do
  begin
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_range));
  end;
end;

function cvgr_range_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_range;
begin
  cvgr := PLiVG_range(vrec^.vg_data); 
  with cvgr^ do
  begin
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      lse_mem_free(cvgr, sizeof(RLiVG_range));
  end;
end;

function cvgr_range_contains(vrec: PLseVargen; Value: PLseValue): integer;cdecl;

  function in_range(V: int64): boolean;
  var
    cvgr: PLiVG_range;
    BV, EV: int64;
  begin
    cvgr := PLiVG_range(vrec^.vg_data);
    BV := cvgr^.begv;
    EV := cvgr^.endv;
    if BV <= EV then
      Result := (V >= BV) and (V <= EV) else
      Result := (V >= EV) and (V <= BV);
  end;

begin
  case lse_vtype(Value) of
    LSV_INT : Result := Ord(in_range(Value^.VInteger));
    LSV_CHAR: Result := Ord(in_range(Ord(Value^.VChar)));
    LSV_BOOL: Result := Ord(in_range(Ord(Value^.VBool)));
         else Result := 0;
  end;
end;

function cvgr_upto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
var
  cvgr: PLiVG_range;
begin
  if (begv <= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_upto_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_upto_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_upto_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_range_addref;
    cvgr^.vgrec.vg_release := @cvgr_range_release;
    cvgr^.vgrec.vg_contains := @cvgr_range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

function cvgr_downto_rewind(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
  begin
    curr := begv;
    Result := Ord(curr >= endv);
  end;
end;

function cvgr_downto_hasNext(vrec: PLseVargen): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    Result := Ord(curr >= endv);
end;

function cvgr_downto_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
begin
  with PLiVG_range(vrec^.vg_data)^ do
    if curr >= endv then
    begin
      lse_set_int64(Value, curr);
      Dec(curr, step);
      Result := 1;
    end
    else Result := 0;
end;

function cvgr_downto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
var
  cvgr: PLiVG_range;
begin
  if (begv >= endv) and (step > 0) then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_range));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := kernel_engine;
    cvgr^.vgrec.vg_rewind := @cvgr_downto_rewind; 
    cvgr^.vgrec.vg_has_next := @cvgr_downto_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_downto_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_range_addref;
    cvgr^.vgrec.vg_release := @cvgr_range_release;
    cvgr^.vgrec.vg_contains := @cvgr_range_contains;
    cvgr^.vgref := 0;
    cvgr^.begv := begv;
    cvgr^.endv := endv;
    cvgr^.step := step;
    cvgr^.curr := begv;
    Result := @(cvgr^.vgrec);
  end
  else Result := nil;
end;

type
  RLiVG_combine = packed record
    vgrec: RLseVargen;
    vgref: integer;
    list: TList;
    curr: integer;
  end;
  PLiVG_combine = ^RLiVG_combine;

function cvgr_combine_rewind(vrec: PLseVargen): integer;cdecl;
var
  index: integer;
  vg: PLseVargen;
begin
  Result := 0;
  with PLiVG_combine(vrec^.vg_data)^ do
  begin
    curr := 0;
    if list.Count > 0 then
    begin
      vg := PLseVargen(list[0]);
      Result := vg^.vg_rewind(vg);
      for index := 1 to list.Count - 1 do
      begin
        vg := PLseVargen(list[index]);
        vg^.vg_rewind(vg);
      end;
    end;
  end;
end;

function cvgr_combine_hasNext(vrec: PLseVargen): integer;cdecl;
var
  vg: PLseVargen;
begin
  Result := 0;
  with PLiVG_combine(vrec^.vg_data)^ do
    if curr < list.Count then
    begin
      vg := PLseVargen(list[curr]);
      Result := vg^.vg_has_next(vg);
    end;
end;

function cvgr_combine_getNext(vrec: PLseVargen; Value: PLseValue): integer;cdecl;
var
  vg: PLseVargen;
begin
  Result := 0;
  with PLiVG_combine(vrec^.vg_data)^ do
    if curr < list.Count then
    begin
      vg := PLseVargen(list[curr]);
      Result := vg^.vg_get_next(vg, Value);
      if Result <> 0 then
        curr := (curr + 1) mod list.Count;
    end;
end;

procedure cvgr_combine_free(cvgr: PLiVG_combine);
var
  index: integer;
  vg: PLseVargen;
begin
  for index := 0 to cvgr^.list.Count - 1 do
  begin
    vg := PLseVargen(cvgr^.list[index]);
    vg^.vg_release(vg);
  end;
  cvgr^.list.Free;
  lse_mem_free(cvgr, sizeof(RLiVG_combine));
end;

function cvgr_combine_addref(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_combine;
begin
  cvgr := PLiVG_combine(vrec^.vg_data);
  with cvgr^ do
  begin
    Inc(vgref);
    Result := vgref;
    if Result = 0 then
      cvgr_combine_free(cvgr);
  end;
end;

function cvgr_combine_release(vrec: PLseVargen): integer;cdecl;
var
  cvgr: PLiVG_combine;
begin
  cvgr := PLiVG_combine(vrec^.vg_data); 
  with cvgr^ do
  begin
    Dec(vgref);
    Result := vgref;
    if Result = 0 then
      cvgr_combine_free(cvgr);
  end;
end;

function cvgr_combine(List: KLiVarList; Count: integer): PLseVargen;
var
  cvgr: PLiVG_combine;
  base, index: integer;
  varg: PLseVargen;
  data: PLseValue;
begin
  if Count > 1 then
  begin
    cvgr := lse_mem_alloc_zero(sizeof(RLiVG_combine));
    cvgr^.vgrec.vg_data := cvgr;
    cvgr^.vgrec.vg_engine := List.Engine;
    cvgr^.vgrec.vg_rewind := @cvgr_combine_rewind;
    cvgr^.vgrec.vg_has_next := @cvgr_combine_hasNext;
    cvgr^.vgrec.vg_get_next := @cvgr_combine_getNext;
    cvgr^.vgrec.vg_addref := @cvgr_combine_addref;
    cvgr^.vgrec.vg_release := @cvgr_combine_release;
    cvgr^.vgref := 0;
    cvgr^.list := TList.Create;
    cvgr^.curr := 0;
    base := List.Count - Count;
    for index := 0 to Count - 1 do
    begin
      data := List[base + index];
      varg := __AsVargen(List.Engine, data);
      if varg = nil then
        varg := lse_vargen_none;
      varg^.vg_addref(varg);
      cvgr^.list.Add(varg);
    end;
    Result := @(cvgr^.vgrec);
  end
  else
  if Count = 1 then
  begin
    Result := __AsVargen(List.Engine, List.Last);
    if Result = nil then
      Result := lse_vargen_none;
  end
  else Result := lse_vargen_none;
end;

{ ADDI}

function addi_strlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
begin
  Result := KLiStrList(obj).Add(__AsString(Value));
end;

function addi_varlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
begin
  Result := KLiVarList(obj).Count;
  KLiVarList(obj).Push(Value);
end;

function addi_stream(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  S: PLseStream;
begin
  S := PLseStream(obj);
  if lse_vtype(Value) = LSV_STRING then
    Result := lse_stream_write(S, Value^.VString) else
    Result := lse_stream_write(S, __AsString(Value));
end;

function addi_function(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  F: KLiFunc;
  R: KLiRunner;
begin
  F := KLiFunc(obj);
  R := KLiEngine(Engine).MainRunner;
  R.Stack.Push(Value);
  Result := Ord(R.Goon(F, 1, nil));
end;

{ sqldb }

function dbv_index(const Vendor: string): integer;
var
  index: integer;
begin
  Result := -1;
  lse_kernel.lock_kernel;
  try
    for index := 0 to Length(db_vendor_list) - 1 do
      if AnsiSameText(Vendor, db_vendor_list[index].dv_name) then
      begin
        Result := index;
        Break;
      end;
  finally
    lse_kernel.unlock_kernel;
  end;
end;

function dbv_register(dbv: PLseDBVendor): boolean;
var
  index: integer;
begin
  Result := lse_dbv_check(dbv);
  if Result then
  begin
    lse_kernel.lock_kernel;
    try
      Result := (dbv_index(dbv^.dv_name) < 0);
      if Result then
      begin
        index := Length(db_vendor_list);
        SetLength(db_vendor_list, index + 1);
        db_vendor_list[index] := dbv^;
      end;
    finally
      lse_kernel.unlock_kernel;
    end;
  end;
end;

function dbv_provide(const Vendor: string): PLseDB;
var
  index: integer;
begin
  Result := nil;
  lse_kernel.lock_kernel;
  try
    for index := 0 to Length(db_vendor_list) - 1 do
      if AnsiSameText(Vendor, db_vendor_list[index].dv_name) then
      begin
        Result := lse_dbv_provide(@db_vendor_list[index]);
        Break;
      end;
  finally
    lse_kernel.unlock_kernel;
  end;
  if Result = nil then
    lse_error('DB vendor "%s" was not registered', [Vendor]);
end;

{ sys-curry }

function curry_func(func: KLiFunc; params: KLiVarList; module: KLiModule): KLiFunc;
var
  this, curry: KLiFunc_curry;
  base, index: integer;
  varb: KLiVarb;
begin
  Result := func;
  
  if (func.ParamCount = 0) or (params = nil) or (Params.Count = 0) then Exit;

  if func.IsCurryFunc then
  begin
    this := KLiFunc_curry(func);
    func := this.CurryFunc;
  end
  else this := nil;

  curry := KLiFunc_curry.Create(module, '', func);
  
  base := 0;
  if this <> nil then
    while base < this.CurryCount do
    begin
      curry.AddCurry(this.CurryData[base]);
      Inc(base);
    end;

  for index := base to func.ParamCount - 1 do
    if (index - base) >= Params.Count then
    begin
      varb := func.Params[index];
      curry.AddParam(varb.Name, varb.ValueType);
    end
    else curry.AddCurry(Params[index - base]);

  Result := curry;
end;

function curry_one(func: KLiFunc; Value: PLseValue; module: KLiModule): KLiFunc;
var
  this, curry: KLiFunc_curry;
  base, index: integer;
  varb: KLiVarb;
begin
  Result := func;

  if func.ParamCount = 0 then Exit;

  if func.IsCurryFunc then
  begin
    this := KLiFunc_curry(func);
    func := this.CurryFunc;
  end
  else this := nil;

  curry := KLiFunc_curry.Create(module, '', func);
  
  base := 0;
  if this <> nil then
    while base < this.CurryCount do
    begin
      curry.AddCurry(this.CurryData[base]);
      Inc(base);
    end;

  curry.AddCurry(Value);

  for index := base + 1 to func.ParamCount - 1 do
  begin
    varb := func.Params[index];
    curry.AddParam(varb.Name, varb.ValueType);
  end;

  Result := curry;
end;

{ sys }

procedure pp_system_dir(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, GetCurrentDir);
end;

procedure pp_system_cd(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.result, SetCurrentDir(__AsFileName(Param^.param[0])));
end;

procedure pp_system_mkdir(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.result, ForceDirectories(__AsFileName(Param^.param[0])));
end;

procedure pp_system_rmdir(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.result, RemoveDir(__AsFileName(Param^.param[0])));
end;

procedure pp_system_cp(const Param: PLseParam);cdecl;
var
  src, dst: string;
begin
  src := __AsFileName(Param^.param[0]);
  dst := __AsFileName(Param^.param[1]);
  lse_set_bool(Param^.result, __copyFile(src, dst));
end;

procedure pp_system_rm(const Param: PLseParam);cdecl;
var
  src: string;
begin
  src := __AsFileName(Param^.param[0]);
  lse_set_bool(Param^.result, SysUtils.DeleteFile(src));
end;

procedure pp_system_mv(const Param: PLseParam);cdecl;
var
  src, dst: string;
begin
  src := __AsFileName(Param^.param[0]);
  dst := __AsFileName(Param^.param[1]);
  lse_set_bool(Param^.result, SysUtils.RenameFile(src, dst));
end;

procedure pp_system_isdir(const Param: PLseParam);cdecl;
var
  dir: string;
begin
  dir := __AsFileName(Param^.param[0]);
  lse_set_bool(Param^.result, DirectoryExists(dir));
end;

procedure pp_system_isfile(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := __AsFileName(Param^.param[0]);
  lse_set_bool(Param^.result, __IsFile(fname));
end;

procedure pp_system_modules(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := __AsEngine(Param);
  __SetVarlist(Param^.result, eng.Modules.ToVarlist(eng));
end;

procedure pp_system_exit(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := __AsEngine(Param);
  lock_engine(eng);
  try
    eng.SetResult(Param^.param[0]);
    eng.Exited := true;
    eng.Error.Clear;
    eng.Terminate;
  finally
    unlock_engine(eng);
  end;
end;

procedure pp_system_print(const Param: PLseParam);cdecl;
begin
  lse_stream_write(__AsEngine(Param).StdoutStream, Param^.param[0]^.VString);
end;

// void sys::printf(string fileName)
procedure pp_system_printf(const Param: PLseParam);cdecl;
var
  inf: TFileStream;
  buf: array[0..1023] of char;
  len: integer;
  eng: KLiEngine;
begin
  if Param^.count > 0 then
  begin
    inf := TFileStream.Create(__AsFileName(Param^.param[0]), fmShareDenyWrite);
    try
      eng := __AsEngine(Param);
      len := inf.Read(buf, sizeof(buf));
      while len > 0 do
      begin
        lse_stream_write(eng.StdoutStream, buf, len);
        len := inf.Read(buf, sizeof(buf));
      end;
    finally
      inf.Free;
    end;
  end;
end;

procedure pp_system_println(const Param: PLseParam);cdecl;
var
  stdout: PLseStream;
begin
  stdout := __AsEngine(Param).StdoutStream;
  lse_stream_write(stdout, Param^.param[0]^.VString);
  lse_stream_writeln(stdout);
end;

procedure pp_system_readln(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    lse_stream_readln(__AsEngine(Param).StdinStream));
end;

var
  random_inited: boolean = false;

procedure pp_system_random(const Param: PLseParam);cdecl;
var
  F, T: integer;
begin
  if not random_inited then
  begin
    Random(MaxInt);
    random_inited := true;
  end;
  F := __AsInt64(Param^.param[0]);
  T := __AsInt64(Param^.param[1]);
  if F <> T then
    F := Random(Abs(F - T)) + Min(F, T) else
  if F = 0 then
    F := Random(MaxInt);
  lse_set_int64(Param^.result, F);
end;

procedure pp_system_sleep(const Param: PLseParam);cdecl;
var
  timeout: integer;
begin
  timeout := __AsInt64(Param^.param[0]);
  if timeout > 0 then
    Sleep(timeout);
end;

procedure pp_system_getenv(const Param: PLseParam);cdecl;
var
  ID: string;
begin
  ID := __AsString(Param^.param[0]);
  if ID <> '' then
    lse_set_string(Param^.result, lse_getenv(ID)) else
    lse_set_string(Param^.result, '', 0);
end;

procedure pp_system_dumpc(const Param: PLseParam);cdecl;
var
  stream: TStringStream;
  vtype: KLiClass;
  func: KLiFunc;
  list: TStrings;
  module: KLiModule;
begin
  if Param^.count = 0 then
  begin
    stream := TStringStream.Create('');
    try
      __AsEngine(Param).DumpCodeToStream(stream, '');
      lse_set_string(Param^.result, stream.DataString);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    vtype := __AsClass(Param^.param[0]);
    if vtype = KT_FUNC then
      func := KLiFunc(__AsObject(Param^.param[0])) else
      func := nil;
    if func <> nil then
    begin
      list := TStringList.Create;
      try
        func.DumpCode(list, '');
        lse_set_string(Param^.result, list.Text);
      finally
        list.Free;
      end;
    end
    else
    begin
      stream := TStringStream.Create('');
      try
        if vtype = KT_CLASS then
        begin
          vtype := KLiClass(__AsObject(Param^.param[0]));
          if vtype = nil then
            vtype := KT_CLASS;
        end
        else
        if vtype = KT_MODULE then
        begin
          module := KLiModule(__AsObject(Param^.param[0]));
          if module <> nil then
            vtype := module.ModuleClass else
            vtype := KT_MODULE;
        end;
        vtype.DumpCodeToStream(stream, '');
        lse_set_string(Param^.result, stream.DataString);
      finally
        stream.Free;
      end;
    end;
  end;
end;

procedure pp_system_length(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: KLiClass;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  data := Param^.param[0];
  clss := __AsClass(data);
  func := clss.SingleMethod(cmCount);
  if func <> nil then
  begin
    if func.IsScript then
    begin
      rnnr := __AsRunner(Param);
      rnnr.Stack.Push(Param^.param[0]);
      rnnr.Goon(func, 1, Param^.result);
    end
    else func.Execute(Param);
  end
  else __SetError(Param, 'method %s.get_length not found)', [clss.Name]);
end;

// variant sys::getiv(variant any, variant index)
procedure pp_system_getiv(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: KLiClass;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  data := Param^.param[0];
  clss := __AsClass(data);
  func := clss.SingleMethod(cmGetAt);
  if func <> nil then
  begin
    if func.IsScript then
    begin
      rnnr := __AsRunner(Param);
      rnnr.Stack.Push(Param^.param[0]);
      rnnr.Stack.Push(Param^.param[1]);
      rnnr.Goon(func, 2, Param^.result);
    end
    else func.Execute(Param);
    __SetClassValue(__AsEngine(Param), Param^.result, func.ResultType);
  end
  else __SetError(Param, 'method %s.getiv not found', [clss.Name]);
end;

procedure pp_system_setiv(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: KLiClass;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  data := Param^.param[0];
  clss := __AsClass(data);
  func := clss.SingleMethod(cmSetAt);
  if func <> nil then
  begin
    if func.IsScript then
    begin
      rnnr := __AsRunner(Param);
      rnnr.Stack.Push(Param^.param[0]);
      rnnr.Stack.Push(Param^.param[1]);
      rnnr.Stack.Push(Param^.param[2]);
      rnnr.Goon(func, 3, Param^.result);
    end
    else func.Execute(Param);
  end
  else __SetError(Param, 'method %s.setiv not found', [clss.Name]);
end;

// variant sys::getpv(variant data, string propertyName)
procedure pp_system_getpv(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: KLiClass;
  func, curry: KLiFunc;
  name: string;
  rnnr: KLiRunner;
begin
  rnnr := __AsRunner(Param);  
  data := Param^.param[0];
  clss := __AsClass(data);
  name := __AsString(Param^.param[1]);
  func := clss.FindMethod(cmMethod, name);
  if func <> nil then
  begin
    curry := curry_one(func, data, rnnr.CurrentFunc.Module);
    __SetFunc(Param^.result, curry);
    if curry <> func then
      curry.DecRefcount; // adjust refcount
  end
  else
  begin
    func := clss.FindGetMethod(name);
    if func = nil then
      func := clss.SingleMethod(cmGetPv);
    if func <> nil then
    begin
      if func.IsScript then
      begin
        rnnr.Stack.Push(Param^.param[0]);
        rnnr.Stack.Push(Param^.param[1]);
        rnnr.Goon(func, 2, Param^.result);
      end
      else func.Execute(Param);
      __SetClassValue(rnnr.Engine, Param^.result, func.ResultType);
    end
    else __SetError(Param, 'method %s.get_%s and %s.getpv not found',
                    [clss.Name, name, clss.Name]);
  end;
end;

// void sys::setpv(variant any, string propertyName, variant value)
procedure pp_system_setpv(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: KLiClass;
  func: KLiFunc;
  name: string;
  rnnr: KLiRunner;
begin
  data := Param^.param[0];
  clss := __AsClass(data);
  name := __AsString(Param^.param[1]);
  func := clss.FindSetMethod(name);
  if func = nil then
    func := clss.SingleMethod(cmSetPV);
  if func <> nil then
  begin
    if func.IsScript then
    begin
      rnnr := __AsRunner(Param);
      rnnr.Stack.Push(Param^.param[0]);
      rnnr.Stack.Push(Param^.param[1]);
      rnnr.Stack.Push(Param^.param[2]);
      rnnr.Goon(func, 3, Param^.result);
    end
    else
    begin
      if func.ParamCount = 2 then
        Param^.param[1] := Param^.param[2];
      func.Execute(Param);
    end;
  end
  else __SetError(Param, 'method %s.set_%s and %s.setpv not found',
                  [clss.Name, name, clss.Name]);
end;

procedure pp_system_genid(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, __genid);
end;

procedure pp_system_load(const Param: PLseParam);cdecl;
var
  engine: KLiEngine;
  runner: KLiRunner;
  module: KLiModule;
  f_name: string;
  m_name: string;
  is_lib, is_str, is_lsp: boolean;
  index, count: integer;

  procedure roll_back;
  var
    X: integer;
    A: KLiObject;
  begin
    try
      X := engine.CompiledObjects.Count - 1;
      while X >= 0 do
      begin
        A := KLiObject(engine.CompiledObjects[X]);
        engine.CompiledObjects.Delete(X);
        A.Free;
        X := engine.CompiledObjects.Count - 1;
      end;
    finally
      engine.CompiledObjects.Free;
      engine.CompiledObjects := nil;
    end;
  end;

begin
  runner := __AsRunner(Param);
  engine := runner.Engine;

  is_lib := false;
  is_lsp := false;

  m_name := __AsFileName(Param^.param[0]);
  is_str := not __inCharSet(pchar(m_name), IDChar);
  if is_str then
  begin
    f_name := __ExpandValue(m_name, engine);
    f_name := __fullFileName(f_name, ExtractFilePath(runner.CurrentModule.FileName));
    if not FileExists(f_name) then
      lse_error('module file "%s" not exists', [m_name]);
    m_name := ExtractFileExt(f_name);
    is_lib := AnsiSameText(m_name, LSE_DLLEXT);
    is_lsp := not is_lib and not AnsiSameText(m_name, '.ls');
    m_name := ChangeFileExt(ExtractFileName(f_name), '');
  end;

  module := engine.Modules.Find(m_name);
  if module = nil then
    module := KLiModule(__findNamed(sys_libraries, m_name));

  if module <> nil then
    if not is_str or __sameFileName(f_name, module.FileName) then
    begin
      lse_set_object(Param^.result, KR_MODULE, module);
      Exit;
    end
    else lse_error('reload module %s from another file', [m_name]);

  if not is_str then
  begin
    f_name := m_name;
    if not __searchModule(f_name, engine.GetSearchPath, is_lib) then
      lse_error('module %s not found', [m_name]);
    is_lsp := not is_lib and not AnsiSameText(ExtractFileExt(f_name), '.ls');
  end;

  lock_kernel;
  try
    if is_lib then
    begin
      module := __loadLibrary(m_name, f_name);
      if module = nil then
        lse_error('can not load file "%s"', [f_name]);
    end
    else
    begin
      count := engine.Modules.Count;
      engine.CompiledObjects := KLiList.Create;
      try
        module := KLiModule.Create(m_name, engine, moyScript);
        module.FileName := f_name;
        module.Parsing := true;
        KLiParser.Create(module).ParseAndFree(__fileText(f_name), is_lsp);
        for index := engine.Modules.Count - 1 downto count do
          engine.Modules[index].Satisfy;
        engine.CompiledObjects.Free;
        engine.CompiledObjects := nil;
      except
        roll_back;
        raise;
      end;
    end;
  finally
    unlock_kernel;
  end;

  lse_set_object(Param^.result, KR_MODULE, module);
end;

procedure pp_system_parse(const Param: PLseParam);cdecl;
var
  code: string;
  islsp: boolean;
  rnnr: KLiRunner;
begin
  if Param^.count > 0 then
  begin
    rnnr := __AsRunner(Param);
    try
      code := __AsString(Param^.param[0]);
      islsp := __AsBool(Param^.param[1]);
      __SetFunc(Param^.result, rnnr.Engine.DoCompile(code, islsp));
    except
      if rnnr.Engine.Error.errno <> 0 then
      begin
        code := rnnr.Engine.Error.ErrorText;
        rnnr.Excepted := false;
        lse_error(code);
      end;
      raise;
    end;
  end;
end;

procedure pp_system_eval(const Param: PLseParam);cdecl;
var
  code: string;
  islsp: boolean;
  rnnr: KLiRunner;
begin
  if Param^.count > 0 then
  begin
    code := TrimRight(__AsString(Param^.param[0]));
    if code <> '' then
    begin
      islsp := __AsBool(Param^.param[1]);
      if not islsp then
        code := code + LB + ';';
      rnnr := __AsRunner(Param);
      rnnr.Eval(code, Param^.result, islsp);
      if rnnr.Excepted then
      begin
        code := rnnr.Engine.Error.ErrorText;
        rnnr.Excepted := false;
        lse_error(code);
      end;
    end;
  end;
end;

procedure pp_system_format(const Param: PLseParam);cdecl;
var
  frmt: string;
  args: KLiVarList;
begin
  frmt := __AsString(Param^.param[0]);
  args := KLiVarList(__AsObject(Param^.param[1]));
  lse_set_string(Param^.result, __AsRunner(Param).FormatFor(frmt, args));
end;

procedure pp_system_now(const Param: PLseParam);cdecl;
begin
  lse_set_time(Param^.result, Now);
end;

procedure pp_system_max(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.count > 0 then
  begin
    v1 := Param^.param[0];
    v2 := Param^.param[1];
    if (Param^.count = 1) or (__compare(V1, V2) in [crEqual, crMore]) then
      lse_set_value(Param^.result, v1) else
      lse_set_value(Param^.result, v2);
  end;
end;

procedure pp_system_min(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.count > 0 then
  begin
    v1 := Param^.param[0];
    v2 := Param^.param[1];
    if (Param^.count = 1) or (__compare(V1, V2) in [crEqual, crLess]) then
      lse_set_value(Param^.result, v1) else
      lse_set_value(Param^.result, v2);
  end;
end;

procedure pp_system_leap(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.result, IsLeapYear(__AsInt64(Param^.param[0])));
end;

procedure pp_system_which(const Param: PLseParam);cdecl;
var
  v_name: string;
  rec: KLiFindRec;
begin
  v_name := Trim(__AsString(Param^.param[0]));
  if __AsRunner(Param).CurrentFunc.FindBy(v_name, @rec) then
    case rec.fo_type of
      foVarb : __SetVarb(Param^.result, rec.VVarb);
      foFunc : __SetFunc(Param^.result, rec.VFunc);
      foClass: if rec.VClass.IsModuleClass then
                 __SetModule(Param^.result, rec.VClass.Module) else
                 lse_set_class(Param^.result, rec.VClass.ClassRec);
    end;
end;

procedure pp_system_current_dbvs(const Param: PLseParam);cdecl;
var
  index: integer;
  list: KLiStrlist;
begin
  lse_kernel.lock_kernel;
  try
    list := KLiStrlist.Create;
    __SetStrlist(Param^.result, list);
    for index := 0 to Length(db_vendor_list) - 1 do
      list.Add(db_vendor_list[index].dv_name + '=' +
               db_vendor_list[index].dv_desc);
  finally
    lse_kernel.unlock_kernel;
  end;
end;

procedure pp_system_exportApi(const Param: PLseParam);cdecl;
begin
  __ExportAPI(__AsFileName(Param^.param[0]));
end;

procedure pp_system_curry(const Param: PLseParam);cdecl;
var
  func, curry: KLiFunc;
  list: KLiVarList;
begin
  func := __AsFunc(Param^.param[0]);
  if (func <> nil) and (func.ParamCount > 0) then
  begin
    list := __AsVarlist(Param^.param[1]);
    if (list <> nil) and (list.Count > 0) then
    begin
      curry := curry_func(func, list, __AsRunner(Param).CurrentModule);
      __SetFunc(Param^.result, curry);
      if curry <> func then
        curry.DecRefcount; // adjust refcount
    end
    else __SetFunc(Param^.result, func);
  end
  else __SetFunc(Param^.result, func);
end;

procedure pp_system_curryone(const Param: PLseParam);cdecl;
var
  func, curry: KLiFunc;
begin
  func := __AsFunc(Param^.param[0]);
  if (func <> nil) and (func.ParamCount > 0) and (Param^.count > 1) then
  begin
    curry := curry_one(func, Param^.param[1],
      __AsRunner(Param).CurrentModule); 
    __SetFunc(Param^.result, curry);
    if curry <> func then
      curry.DecRefcount; // adjust refcount
  end
  else __SetFunc(Param^.result, func);
end;

procedure pp_system_gc(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, __AsEngine(Param).GarbageCollect);
end;

procedure pp_system_apply(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
begin
  func := __AsFunc(Param^.param[0]);
  if func <> nil then
  begin
    rnnr := __AsRunner(Param);
    list := __AsVarlist(Param^.param[1]);
    if list <> nil then
      prms := Min(func.ParamCount, list.Count) else
      prms := 0;
    rnnr.Stack.PushValues(list, prms);
    rnnr.Goon(func, prms, Param^.result);
  end;
end;

// bool time.isValidDate(int year, int month, int day)
procedure pp_system_isValidDate(const Param: PLseParam);cdecl;
var
  Y, M, D: integer;
begin
  Y := __AsInt64(Param^.param[0]);
  M := __AsInt64(Param^.param[1]);
  D := __AsInt64(Param^.param[2]);
  lse_set_bool(Param^.result, IsValidDate(Y, M, D));
end;

// time time.encodeDateTime(int year, int month, int day, int hour, int minute,
//                          int second, int milliSecond, time DefValue)
procedure pp_system_encodeDateTime(const Param: PLseParam);cdecl;
var
  Y, M, D, H, N, S, L: integer;
begin
  Y := __AsInt64(Param^.param[0]);
  M := __AsInt64(Param^.param[1]);
  D := __AsInt64(Param^.param[2]);
  H := __AsInt64(Param^.param[3]);
  N := __AsInt64(Param^.param[4]);
  S := __AsInt64(Param^.param[5]);
  L := __AsInt64(Param^.param[6]);
  lse_set_time(Param^.result, EncodeDateTime(Y, M, D, H, N, S, L));
end;

procedure pp_system_tmpfname(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := sys_tmpath + __genid + __AsFileName(Param^.param[0]);
  lse_set_string(Param^.result, fname);
end;

procedure pp_system_encodeGMT(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, lse_encode_GMT(__AsTime(Param^.param[0])));
end;

procedure pp_system_decodeGMT(const Param: PLseParam);cdecl;
begin
  lse_set_time(Param^.result, lse_decode_GMT(__AsString(Param^.param[0])));
end;

procedure pp_system_encodeUTF8(const Param: PLseParam);cdecl;
begin
  if Param^.count > 0 then
    lse_set_string(Param^.result, __encodeUTF8(__AsString(Param^.param[0])));
end;

procedure pp_system_decodeUTF8(const Param: PLseParam);cdecl;
begin
  if Param^.count > 0 then
    lse_set_string(Param^.result, __decodeUTF8(__AsString(Param^.param[0])));
end;

procedure pp_system_encodeS(const Param: PLseParam);cdecl;
begin
  if Param^.count > 0 then
    lse_set_string(Param^.result, __encodeS(__AsString(Param^.param[0]),
      __AsBool(Param^.param[1])));
end;

procedure pp_system_decodeS(const Param: PLseParam);cdecl;
begin
  if Param^.count > 0 then
    lse_set_string(Param^.result, __decodeS(__AsString(Param^.param[0])));
end;

const
  EOPENMODE = 'Unknown file open mode "%s"';

procedure pp_system_openfs(const Param: PLseParam);cdecl;
var
  fname, fmode: string;
  open_mode: word;
  read, write: boolean;
  stream: PLseStream;
begin
  fname := __AsFileName(Param^.param[0]);
  fmode := __AsString(Param^.param[1]);
  if fmode = '' then fmode := 'r';
  if __strToFileMode(fmode, open_mode, read, write) then
  begin
    stream := lse_file_stream(fname, open_mode);
    lse_set_stream(Param^.result, stream);
  end
  else __SetError(Param, EOPENMODE, [fmode]);
end;

procedure pp_system_memory(const Param: PLseParam);cdecl;
var
  stream: PLseStream;
begin
  stream := lse_memory_stream;
  lse_set_stream(Param^.result, stream);
  if Param^.count > 1 then
    lse_stream_resize(stream, Max(0, Param^.param[0]^.VInteger));
end;

procedure pp_system_incPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_string(Param^.result, IncludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_excPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_string(Param^.result, ExcludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_veryPD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, lse_veryPD(__AsString(Param^.param[0])));
end;

procedure pp_system_veryUD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, lse_veryUD(__AsString(Param^.param[0])));
end;

procedure pp_system_system(const Param: PLseParam);cdecl;
var
  v_cmd, v_dir: string;
  status: integer;
begin
  v_cmd := Trim(__FormatParam(Param, 0));
  v_dir := Trim(__FormatParam(Param, 1));
  lse_set_string(Param^.result, spawn_shouts(v_cmd, v_dir, status));
  __AsRunner(Param).ShellExitCode := status;
end;

procedure pp_system_shexec(const Param: PLseParam);cdecl;
var
  v_cmd, v_dir: string;
  status: integer;
begin
  v_cmd := Trim(__FormatParam(Param, 0));
  v_dir := Trim(__FormatParam(Param, 1));
  lse_set_bool(Param^.result, spawn_shexec(v_cmd, v_dir,
    __AsBool(Param^.param[2]), status));
  __AsRunner(Param).ShellExitCode := status;
end;

procedure pp_system_msecs(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
  beg_time: TDateTime;
begin
  func := __AsFunc(Param^.param[0]);
  if func <> nil then
  begin
    beg_time := Now;
    rnnr := __AsRunner(Param);
    list := KLiVarList(__AsObject(Param^.param[1]));
    if list <> nil then
      prms := Min(func.ParamCount, list.Count) else
      prms := 0;
    rnnr.Stack.PushValues(list, prms);
    rnnr.Goon(func, prms, Param^.result);
    lse_set_int64(Param^.result, MilliSecondsBetween(Now, beg_time));
  end
  else lse_set_int64(Param^.result, 0);
end;

procedure pp_system_current_module(const Param: PLseParam);cdecl;
begin
  __SetModule(Param^.result, __AsRunner(Param).CurrentFunc.Module);
end;

procedure pp_system_current_func(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
begin
  rnnr := __AsRunner(Param);
  __SetFunc(Param^.result, rnnr.Current^.func);
end;

procedure pp_system_current_error(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.result, KR_ERROR, __AsEngine(Param).Error);
end;

procedure pp_system_current_args(const Param: PLseParam);cdecl;
begin
  __SetStrlist(Param^.result, __AsEngine(Param).Arguments);
end;

procedure pp_system_current_prmc(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, __AsRunner(Param).Current^.values.ActualParamCount);
end;

procedure pp_system_current_prms(const Param: PLseParam);cdecl;
var
  list: KLiVarList;
begin
  list := __NewVarlist(__AsEngine(Param));
  __SetVarlist(Param^.result, list);
  __AsRunner(Param).Current^.values.GetParamValues(list);
end;

procedure pp_system_current_xcode(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, __AsRunner(Param).ShellExitCode);
end;

procedure pp_system_current_line(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, int64(__AsRunner(Param).Exprrec^.Pos.row) + 1);
end;

procedure pp_system_current_envs(const Param: PLseParam);cdecl;
var
  index, count: integer;
  list: KLiStrlist;
begin
  list := KLiStrlist.Create;
  __SetStrlist(Param^.result, list);
  count := lse_getenv_count;
  for index := 0 to count - 1 do
    list.Add(lse_getenv_string(index));
end;

procedure pp_system_current_file(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, __AsRunner(Param).CurrentFunc.Module.FileName);
end;

procedure pp_system_current_ifile(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, __AsRunner(Param).IncludedFile);
end;

procedure pp_system_current_pd(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, LSE_PATH_DELIMITER);
end;

procedure pp_system_eol(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result, sys_LB);
end;

procedure pp_system_each(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  if Param^.count > 1 then
  begin
    func := KLiFunc(Param^.param[1]^.VObject);
    if func <> nil then
    begin
      rnnr := __AsRunner(Param);
      this := lse_vargen_this(Param);
      while rnnr.Stack.AddSend(this) do
        rnnr.Goon(func, 1, Param^.result);
    end;
  end;
end;

procedure pp_system_map(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := __AsRunner(Param);
  list := __NewVarlist(rnnr.Engine);
  lse_set_object(Param^.result, KR_VARLIST, list);
  if Param^.count > 0 then
  begin
    this := lse_vargen_this(Param);
    lse_init_value(@data);
    try
      func := __AsFunc(Param^.param[1]);
      if func = nil then list.AddSendAll(this) else
      while rnnr.Stack.AddSend(this) do
        if rnnr.Goon(func, 1, @data) then
          list.Push(@data) else
          Break;
    finally
      lse_set_nil(@data);
    end;
  end;
end;

procedure pp_system_reduce(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  if Param^.count > 1 then
  begin
    lse_set_value(Param^.result, Param^.param[1]);
    func := __AsFunc(Param^.param[2]);
    if func <> nil then
    begin
      this := lse_vargen_this(Param);
      rnnr := __AsRunner(Param);
      while true do
      begin
        rnnr.Stack.Push(Param^.result);
        if not rnnr.Stack.AddSend(this)
        or not rnnr.Goon(func, 2, Param^.result) then Break;
      end;
    end;
  end;
end;

procedure pp_system_filter(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  list: KLiVarList;
  test, data: RLseValue;
  rnnr: KLiRunner;
begin
  rnnr := __AsRunner(Param);
  list := __NewVarlist(rnnr.Engine);
  lse_set_object(Param^.result, KR_VARLIST, list);
  func := __AsFunc(Param^.param[1]);
  if func <> nil then
  begin
    lse_init_value(@test);
    lse_init_value(@data);
    try
      this := lse_vargen_this(Param);
      while lse_vargen_send(this, @data) do
      begin
        rnnr.Stack.Push(@data);
        if not rnnr.Goon(func, 1, @test) then Break else
        if __AsBool(@test) then
          list.Push(@data);
      end;
    finally
      lse_clear_value(@data);
      lse_clear_value(@test);
    end;
  end;
end;

procedure pp_system_sum(const Param: PLseParam);cdecl;
var
  G: PLseVargen;
  T: PLseValue;
begin
  if Param^.count > 0 then
  begin
    G := __AsVargen(__AsEngine(Param), Param^.param[0]);
    lse_set_object(Param^.param[0], KR_VARGEN, G);

    if Param^.count = 1 then
    begin
      lse_set_object(Param^.param[1], KR_FUNC, sys_oper_inc);
      Param^.count := 2;
    end;

    if Param^.count = 2 then
    begin
      if not lse_vargen_send(G, Param^.param[2]) then Exit;
      Param^.count := 3;
    end;

    T := Param^.param[1];
    Param^.param[1] := Param^.param[2];
    Param^.param[2] := T;
    
    pp_system_reduce(Param);  
  end;
end;

procedure pp_system_maxint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, high(int64));
end;

procedure pp_system_minint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, low(int64));
end;

procedure pp_system_gget(const Param: PLseParam);cdecl;
begin
  if Param^.count > 0 then
    __AsEngine(Param).GetValue(
      __AsString(Param^.param[0]), Param^.result);
end;

procedure pp_system_gput(const Param: PLseParam);cdecl;
begin
  __AsEngine(Param).MainValues.SetValueByChain(
    __AsString(Param^.param[0]), Param^.param[1]);
end;

procedure pp_system_get_stdin(const Param: PLseParam);cdecl;
begin
  lse_set_stream(Param^.result, __AsEngine(Param).StdinStream);
end;

procedure pp_system_set_stdin(const Param: PLseParam);cdecl;
begin
  __AsEngine(Param).StdinStream := PLseStream(Param^.param[0]^.VObject);
end;

procedure pp_system_get_stdout(const Param: PLseParam);cdecl;
begin
  lse_set_stream(Param^.result, __AsEngine(Param).StdoutStream);
end;

procedure pp_system_set_stdout(const Param: PLseParam);cdecl;
begin
  __AsEngine(Param).StdoutStream := PLseStream(Param^.param[0]^.VObject);
end;

procedure pp_system_get_stderr(const Param: PLseParam);cdecl;
begin
  lse_set_stream(Param^.result, __AsEngine(Param).StderrStream);
end;

procedure pp_system_set_stderr(const Param: PLseParam);cdecl;
begin
  __AsEngine(Param).StderrStream := PLseStream(Param^.param[0]^.VObject);
end;

procedure pp_system_log(const Param: PLseParam);cdecl;
var
  S: PLseString;
begin
  if Param^.count > 0 then
  begin
    S := __AsStrec(Param^.param[0]);
    __log(lse_strec_data(S), lse_strec_length(S));
  end;
end;

procedure pp_system_abs(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: PLseClassRec;
begin
  data := Param^.param[0];
  clss := lse_class(data);
  case clss^.vtype of
    LSV_INT  : lse_set_int64(Param^.result, Abs(data^.VInteger));
    LSV_FLOAT: lse_set_float(Param^.result, Abs(data^.VFloat));
    LSV_MONEY: lse_set_money(Param^.result, Abs(data^.VMoney));
          else lse_set_value(Param^.result, data);
  end;
end;

procedure pp_system_find(const Param: PLseParam);cdecl;
var
  runner: KLiRunner;
  mp: PLiMatchPatten;
  varlist: KLiVarList;
  source: pchar;
  index: integer;
begin
  if Param^.count > 1 then
  begin
    runner := __AsRunner(Param);
    mp := runner.MatchPatten;
    if init_patten(mp, Param^.param[1]) and exec_patten(mp, Param^.param[0]) then
    begin
      if __AsBool(Param^.param[2]) then // find all
      begin
        varlist := __NewVarlist(runner.Engine);
        __SetVarlist(Param^.result, varlist);
        varlist.PushObject(runner.ListMatchResult, KT_VARLIST);
        source := mp^.mp_source;
        while not mp^.mp_anchor do
        begin
          if mp^.mp_result.mr_len > 0 then
            index := (mp^.mp_result.mr_str - source) + mp^.mp_result.mr_len else
            index := (mp^.mp_result.mr_str - source) + 1;
          if exec_patten(mp, source + index, (mp^.mp_eos - source) - index) then
          begin
            mp^.mp_source := source;
            varlist.PushObject(runner.ListMatchResult, KT_VARLIST);
          end
          else Break;
        end;
      end
      else __SetVarlist(Param^.result, runner.ListMatchResult);
      Exit;
    end;
  end;
  __SetVarlist(Param^.result, nil);
end;

procedure pp_system_gsub(const Param: PLseParam);cdecl;
var
  match_list: array of RLiMatchRec;
  match_len: integer;
  mp: PLiMatchPatten;
  times, X: integer;
  src_str: pchar;      // source string
  src_len: integer;    // length of source string
  new_str: pchar;      // new string
  new_len: integer;    // length of new string;
  result_str: pchar;   // result string
  result_len: integer; // length of result string
  srec: PLseString;

  procedure save_match_result;
  var
    index: integer;
  begin
    index := Length(match_list);
    SetLength(match_list, index + 1);
    match_list[index] := mp^.mp_result;
    Inc(match_len, mp^.mp_result.mr_len);
  end;
  
begin
  mp := __AsRunner(Param).MatchPatten;

  if Param^.count > 3 then
    times := __AsInt64(Param^.param[3]) else
    times := MaxInt;

  if (times < 1)
    or not init_patten(mp, Param^.param[1])
      or not exec_patten(mp, Param^.param[0]) then
      begin
        lse_set_string(Param^.result, Param^.param[0]^.VString);
        Exit;
      end;

  src_str := mp^.mp_source;
  src_len := mp^.mp_eos - src_str;

  match_len := 0;
  save_match_result;
  Dec(times);
  while not mp^.mp_anchor and (times > 0) do
  begin
    if mp^.mp_result.mr_len > 0 then
      X := (mp^.mp_result.mr_str - src_str) + mp^.mp_result.mr_len else
      X := (mp^.mp_result.mr_str - src_str) + 1;
    if exec_patten(mp, src_str + X, (mp^.mp_eos - src_str) - X) then
    begin
      mp^.mp_source := src_str;
      save_match_result;
      Dec(times);
    end
    else Break;
  end;

  new_str := lse_strec_data(Param^.param[2]^.VString);
  new_len := lse_strec_length(Param^.param[2]^.VString);
  times := Length(match_list);
  
  result_len := src_len - match_len + (new_len * times);
  if result_len < 1 then Exit;

  srec := lse_strec_alloc(nil, result_len);
  lse_set_string(Param^.result, srec);
  result_str := lse_strec_data(srec);

  for X := 0 to times - 1 do
  begin
    src_len := match_list[X].mr_str - src_str;
     
    Move(src_str^, result_str^, src_len);
    Dec(result_len, src_len);
    Inc(result_str, src_len);
    
    Move(new_str^, result_str^, new_len);
    Dec(result_len, new_len);
    Inc(result_str, new_len);

    src_str := match_list[X].mr_str + match_list[X].mr_len;
  end;

  Move(src_str^, result_str^, result_len);
end;

procedure pp_system_split(const Param: PLseParam);cdecl;
var
  mp: PLiMatchPatten;
  len: integer;
  source: pchar;
  strlist: KLiStrlist;
  line: string;
begin
  strlist :=KLiStrlist.Create;
  __SetStrlist(Param^.result, strlist);
  
  mp := __AsRunner(Param).MatchPatten;
  if not init_patten(mp, Param^.param[1])
    or not exec_patten(mp, Param^.param[0]) then
    begin
      strlist.Add(__AsString(Param^.param[0]));
      Exit;
    end;

  if mp^.mp_source < mp^.mp_result.mr_str then
  begin
    SetString(line, mp^.mp_source, mp^.mp_result.mr_str - mp^.mp_source);
    strlist.Add(line);
  end;

  if mp^.mp_anchor then
  begin
    source := mp^.mp_result.mr_str + mp^.mp_result.mr_len;
    if source < mp^.mp_eos then
    begin
      SetString(line, source, mp^.mp_eos - source);
      strlist.Add(line);
    end;
    Exit;
  end;

  while true do
  begin
    len := mp^.mp_result.mr_len; 
    if len > 0 then
      source := mp^.mp_result.mr_str + len else
      source := mp^.mp_result.mr_str + 1;
    if exec_patten(mp, source, mp^.mp_eos - source) then
    begin
      SetString(line, mp^.mp_source, mp^.mp_result.mr_str - mp^.mp_source);
      strlist.Add(line);
    end
    else
    begin
      if len < 1 then Dec(source);
      if source < mp^.mp_eos then
      begin
        SetString(line, source, mp^.mp_eos - source);
        strlist.Add(line);
      end;
      Exit;
    end;
  end;
end;

procedure pp_system_getcs(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.result, KR_VARLIST,
    __AsRunner(Param).CallStack.GetCallSnap(__AsInt64(Param^.param[0])));
end;

procedure pp_system_throw(const Param: PLseParam);cdecl;
var
  eid, msg: string;
  rnnr: KLiRunner;
begin
  if Param^.count < 1 then
  begin
    rnnr := __AsRunner(Param);
    if rnnr.Engine.Error.errno = 0 then
      __SetError(Param, RuntimeError, 0, EsRuntimeError) else
      rnnr.Excepted := true;
  end
  else
  begin
     if Param^.count > 1 then
    begin
      eid := Trim(__AsString(Param^.param[1]));
      if eid = '' then
        eid := RuntimeError;
    end
    else eid := RuntimeError;

    msg := Trim(__AsString(Param^.param[0]));
    if msg = '' then
      msg := EsRuntimeError;

    __SetError(Param, eid, 0, msg);
  end;
end;

procedure pp_system_writeTo(const Param: PLseParam);cdecl;
var
  S: PLseStream;
  V: PLseValue;
  R: PLseClassRec;
  L: integer;
begin
  S := PLseStream(Param^.param[0]^.VObject);
  if S <> nil then
  begin
    L := 0;
    if Param^.count > 1 then
    begin
      V := Param^.param[1];
      R := lse_class(V);
      case R^.vtype of
        lSV_STRING: L := lse_stream_write(S, V^.VString);
        LSV_INT   : L := lse_stream_write(S, @V^.VInteger, sizeof(Int64));
        LSV_FLOAT : L := lse_stream_write(S, @V^.VFloat, sizeof(double));
        LSV_MONEY : L := lse_stream_write(S, @V^.VMoney, sizeof(currency));
        LSV_TIME  : L := lse_stream_write(S, @V^.VTime, sizeof(TDateTime));
        LSV_BOOL  : L := lse_stream_write(S, @V^.VBool, sizeof(boolean));
        LSV_CHAR  : L := lse_stream_write(S, @V^.VChar, sizeof(char));
        LSV_OBJECT: if Assigned(R^.writeTo) then
                      L := R^.writeTo(V^.VObject, S);
      end;
    end;
    lse_set_int64(Param^.result, L);
  end
  else __SetError(Param, 'Output stream not supplied.');
end;

{ error }

procedure pp_error_text(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.ErrorText);
end;

procedure pp_error_module(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.module);
end;

procedure pp_error_name(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Name);
end;

procedure pp_error_message(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.msg);
end;

procedure pp_error_row(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.row);
end;

procedure pp_error_col(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.col);
end;

procedure pp_error_errno(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.errno);
end;

{ function }

procedure pp_func_name(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Name);
end;

procedure pp_func_type(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_class(Param^.result, this.ResultType.ClassRec);
end;

procedure pp_func_prototype(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, otos_function(pointer(this)));
end;

procedure pp_func_module(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    __SetModule(Param^.result, this.Module);
end;

procedure pp_func_parent(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_class(Param^.result, this.OwnerClass.ClassRec);
end;

procedure pp_func_params(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    __SetVarlist(Param^.result,
      this.Params.ToVarlist(__AsEngine(Param)));
end;

procedure pp_func_locals(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
  list: KLiVarList;
begin
  if __GetThis(Param, this) then
  begin
    if this.Codes = nil then
      list := __NewVarlist(__AsEngine(Param)) else
      list := this.Codes.Locals.ToVarlist(__AsEngine(Param));
    __SetVarlist(Param^.result, list);
  end;
end;

{ hashed }

procedure pp_hashed_create(const Param: PLseParam);cdecl;
var
  buckets: integer;
  clss: KLiClass;
  hash: KLiHashed;
begin
  if Param^.count > 1 then
    buckets := Max(1, __AsInt64(Param^.param[1])) else
    buckets := 1;
  hash := KLiHashed.Create(__AsEngine(Param), buckets); 
  clss := __AsClass(Param^.param[0]);
  lse_set_object(Param^.result, clss.ClassRec, hash);
end;

procedure pp_hashed_length(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.ItemCount);
end;

procedure pp_hashed_exchange(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  L, R: PLiHashItem;
  data: pointer;
begin
  if __GetThis(Param, this) then
  begin
    key := __AsString(Param^.param[1]);
    L := this.Find(key);
    if L = nil then
    begin
      __SetError(Param, 'key "%s" not found', [key]);
      Exit;
    end;

    key := __AsString(Param^.param[2]);
    R := this.Find(key);
    if R = nil then
    begin
      __SetError(Param, 'key "%s" not found', [key]);
      Exit;
    end;

    if L <> R then
    begin
      data := L^.hi_data;
      L^.hi_data := R^.hi_data;
      R^.hi_data := data;
    end;
  end;
end;

procedure pp_hashed_keys(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: KLiStrlist;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiStrlist.Create();
    __SetStrlist(Param^.result, list);
    this.ListKey(list);
  end;
end;

procedure pp_hashed_listKV(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: KLiVarList;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiVarList.Create(__AsEngine(Param));
    __SetVarlist(Param^.result, list);
    this.ListKeyValue(list);
  end;
end;

procedure pp_hashed_values(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: TList;
  values: KLiVarList;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := TList.Create;
    try
      this.ListData(list);
      values := __NewVarlist(__AsEngine(Param));
      __SetVarlist(Param^.result, values);
      for index := 0 to list.Count - 1 do
        values.Push(PLseValue(list[index]));
    finally
      list.Free;
    end;
  end;
end;

procedure pp_hashed_remove(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
    this.Remove(__AsString(Param^.param[1]));
end;

procedure pp_hashed_clear(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) then
    this.Clear;
end;

procedure pp_hashed_getpv(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if __GetThis(Param, this) then
  begin
    key := __AsString(Param^.param[1]);
    data := this.FindValue(key);
    if data = nil then
      __SetError(Param, 'key "%s" not found', [key]) else
      lse_set_value(Param^.result, data);
  end;
end;

procedure pp_hashed_setpv(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) then
    this.SetValue(__AsString(Param^.param[1]), Param^.param[2]);
end;

procedure pp_hashed_gget(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
  begin
    key := __AsString(Param^.param[1]);
    data := this.FindValueByChain(key);
    if data = nil then
      __SetError(Param, 'key "%s" not found', [key]) else
      lse_set_value(Param^.result, data);
  end;
end;

procedure pp_hashed_gput(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
    this.SetValueByChain(__AsString(Param^.param[1]), Param^.param[2]);
end;

procedure pp_hashed_read(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if __GetThis(Param, this) then
  begin
    key := __AsString(Param^.param[1]);
    data := this.FindValue(key);
    if data = nil then
      lse_set_value(Param^.result, Param^.param[2]) else
      lse_set_value(Param^.result, data);
  end;
end;

procedure pp_hashed_isset(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
  begin
    key := __AsString(Param^.param[1]);
    lse_set_bool(Param^.result, this.IsSet(key));
  end;
end;

procedure pp_hashed_curry(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  func: KLiFunc;
  clss: KLiClass;
  name: string;
begin
  __SetFunc(Param^.result, nil);
  if __GetThis(Param, this) and (Param^.count > 2) then
  begin
    name := __AsString(Param^.param[1]);
    if name <> '' then
    begin
      func := __AsFunc(Param^.param[2]);
      if func <> nil then
      begin
        if func.ParamCount > 0 then
          clss := func.Params[0].ValueType else
          clss := nil;

        if (clss = KT_HASHED) or (clss = KT_VARIANT) then
        begin
          func := curry_one(func, Param^.param[0],
            __AsRunner(Param).CurrentModule);
          __SetFunc(Param^.result, func);
          func.DecRefcount;  // adjust refcount
        end
        else __SetFunc(Param^.result, func);

        this.SetObject(name, func, KR_FUNC);
      end;
    end;
  end;
end;

{ int }

procedure pp_int_hex(const Param: PLseParam);cdecl;
var
  this: int64;
  size, digits: integer;
  text: string;
begin
  this := __AsInt64(Param^.param[0]);
  size := Max(0, __AsInt64(Param^.param[1]));
  digits := Min(16, size);
  if digits > 1 then
    text := Format('%.' + IntToStr(digits) + 'x', [this]) else
    text := Format('%x', [this]);
  digits := Length(text);
  if digits < size then
    text := StringOfChar('0', size - digits) + text;
  lse_set_string(Param^.Result, text);
end;

procedure pp_int_bitlist(const Param: PLseParam);cdecl;
const
  bitf: array[0..1] of char = ('0', '1');
var
  list: array[0..64] of char;
  index, size: integer;
  this: int64;
  base: pchar;
begin
  this := __AsInt64(Param^.param[0]);
  for index := 0 to 63 do
    list[index] := bitf[(this shr (63 - index)) and 1];
  list[64] := #0;
  size := 64 - Min(64, Max(1, __AsInt64(Param^.param[1])));
  base := list;
  for index := 1 to size do
    if base^ = '0' then Inc(base) else break;
  lse_set_string(Param^.result, base);
end;

procedure pp_int_upto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.count > 1 then
  begin
    if Param^.count > 2 then
      step := Param^.param[2]^.VInteger else
      step := 1;
    varg := cvgr_upto(Param^.param[0]^.VInteger,
                      Param^.param[1]^.VInteger,
                      step, __AsEngine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.result, lse_vargen_ensure(varg));
end;

procedure pp_int_downto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.count > 1 then
  begin
    if Param^.count > 2 then
    begin
      step := Param^.param[2]^.VInteger;
      if step < 0 then
        step := - step;
    end
    else step := 1;
    varg := cvgr_downto(Param^.param[0]^.VInteger,
                        Param^.param[1]^.VInteger,
                        step, __AsEngine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.result, lse_vargen_ensure(varg));
end;

{ module }

procedure pp_module_name(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Name);
end;

procedure pp_module_file(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.FileName);
end;

procedure pp_module_modules(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  engine: KLiEngine;
begin
  if __GetThis(Param, this) then
  begin
    engine := __AsEngine(Param);
    __SetVarlist(Param^.result,
      this.Modules.ToVarlist(engine));
  end;
end;

procedure pp_module_funcs(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  list: KLiVarList;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := __NewVarlist(__AsEngine(Param));
    __SetVarlist(Param^.result, list);
    for index := 0 to this.FuncCount - 1 do
      list.PushObject(this.GetFunc(index), KT_FUNC);
  end;
end;

procedure pp_module_types(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  list: KLiVarList;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := __NewVarlist(__AsEngine(Param));
    __SetVarlist(Param^.result, list);
    for index := 0 to this.ClassCount - 1 do
      list.PushObject(this.GetClass(index), KT_CLASS);
  end;
end;

procedure pp_module_version(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Version);
end;

procedure pp_module_getpv(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  name: string;
  func: KLiFunc;
  clss: KLiClass;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);

    clss := this.FindClass(name);
    if clss <> nil then
    begin
      if clss.IsModuleClass then
        __SetModule(Param^.result, clss.Module) else
        lse_set_class(Param^.result, clss.ClassRec);
      Exit;
    end;

    func := this.FindFunc(name);
    if func <> nil then
    begin
      if func.IsNameCall then
        __AsRunner(Param).Goon(func, 0, Param^.result) else
        __SetFunc(Param^.result, func);
      Exit;
    end;

    __SetError(Param, '%s::%s does not exists.', [this.Name, name]);
  end;
end;

procedure pp_module_main(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    __SetFunc(Param^.result, this.MainFunc);
end;

procedure pp_module_imports(const Param: PLseParam);cdecl;
var
  this, curr: KLiModule;
begin
  if __GetThis(Param, this) then
  begin
    if not this.IsScript then
      lse_error('%s can not import other modules', [this.Name]);
    Param^.param[0] := Param^.param[1];
    pp_system_load(Param);
    curr := KLiModule(Param^.result^.VObject);
    if curr.IsScript then
    begin
      curr.AddImporter(this);
      this.Modules.Add(curr);
    end
    else this.Modules.Add(curr);
    curr.ImportNotification;
  end;
end;

{ database }

procedure pp_database_create(const Param: PLseParam);cdecl;
var
  vendor: string;
begin
  vendor := lse_strec_string(Param^.param[1]^.VString);
  lse_set_db(Param^.result, dbv_provide(vendor));
end;

procedure pp_database_execSQL(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  SQL: pchar;
begin
  if __GetThis(Param, this) then
  begin
    SQL := lse_strec_data(Param^.param[1]^.VString);
    lse_set_int64(Param^.result, lse_db_execSQL(this, SQL));
  end;
end;

procedure pp_database_openSQL(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_ds(Param^.result,
      lse_db_openSQL(this, lse_strec_data(Param^.param[1]^.VString)));
end;

procedure pp_database_tables(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  list: KLiStrlist;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiStrlist.Create;
    __SetStrlist(Param^.result, list);
    list.CommaText := lse_db_tables(this);
  end;
end;

procedure pp_database_procedures(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  list: KLiStrlist;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiStrlist.Create;
    __SetStrlist(Param^.result, list);
    list.CommaText := lse_db_storedprocs(this);
  end;
end;

procedure pp_database_connecTo(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_connecTo(this, __AsString(Param^.param[1]),  // target
                          __AsString(Param^.param[2]),  // user
                          __AsString(Param^.param[3]),  // password
                          __AsString(Param^.param[4]),  // source
                          __AsString(Param^.param[5])); // params
end;

procedure pp_database_disconnect(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_disconnect(this);
end;

procedure pp_database_reconnect(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_reconnect(this);
end;

procedure pp_database_transact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_transact(this);
end;

procedure pp_database_inTransaction(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, lse_db_transacting(this));
end;

procedure pp_database_commit(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_commit(this, false);
end;

procedure pp_database_commitRetaining(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_commit(this, true);
end;

procedure pp_database_commitAndTransact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
  begin
    lse_db_commit(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_rollback(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_rollback(this, false);
end;

procedure pp_database_rollbackRetaining(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_db_rollback(this, true);
end;

procedure pp_database_rollbackAndTransact(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
  begin
    lse_db_rollback(this, true);
    lse_db_transact(this);
  end;
end;

procedure pp_database_connected(const Param: PLseParam);cdecl;
var
  this: PLseDB;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, lse_db_connected(this));
end;

procedure pp_database_escape(const Param: PLseParam);cdecl;
var
  this: PLseDB;
  srec: PLseString;
begin
  if __GetThis(Param, this) then
  begin
    srec := lse_db_escape(this, Param^.param[1]^.VString);
    lse_set_string(Param^.result, srec);
  end;
end;

{ dataset }

procedure pp_dataset_close(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_close(this);
end;

procedure pp_dataset_open(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
  begin
    lse_ds_close(this);
    this^.ds_setSQL(this, lse_strec_data(Param^.param[1]^.VString));
    lse_ds_check(this);
    this^.ds_open(this);
    lse_ds_check(this);
    lse_set_ds(Param^.result, this);
  end;
end;

procedure pp_dataset_first(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_first(this);
end;

procedure pp_dataset_last(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_last(this);
end;

procedure pp_dataset_prior(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_prior(this);
end;

procedure pp_dataset_next(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_ds_next(this);
end;

procedure pp_dataset_eof(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, lse_ds_eof(this));
end;

procedure pp_dataset_bof(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, lse_ds_bof(this));
end;

procedure pp_dataset_count(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, lse_ds_count(this));
end;

procedure pp_dataset_length(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, lse_ds_field_count(this));
end;

procedure pp_dataset_indexOf(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result,
      lse_ds_indexof(this, lse_strec_data(Param^.param[1]^.VString)));
end;

procedure pp_dataset_field_name(const Param: PLseParam);cdecl;
var
  this: PLseDS;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result,
      lse_ds_fname(this, Param^.param[1]^.VInteger, true));
end;

procedure pp_dataset_field_type(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  ftype: PLseClassRec;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      ftype := lse_ds_field_class(this, index);
      lse_set_object(Param^.result, KR_CLASS, ftype^.lysee_class);
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
      begin
        ftype := lse_ds_field_class(this, index);
        lse_set_object(Param^.result, KR_CLASS, ftype^.lysee_class);
      end
      else __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_string(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      lse_set_string(Param^.result, lse_ds_getfs(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_string(Param^.result, lse_ds_getfs(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_int(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      lse_set_int64(Param^.result, lse_ds_getfi(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_int64(Param^.result, lse_ds_getfi(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_bool(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      lse_set_bool(Param^.result, lse_ds_getfb(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_bool(Param^.result, lse_ds_getfb(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_float(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      lse_set_float(Param^.result, lse_ds_getfd(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_float(Param^.result, lse_ds_getfd(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_money(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
  clss: KLiClass;
  name: pchar;
begin
  if __GetThis(Param, this) then
  begin
    clss := __AsClass(Param^.param[1]);
    if clss = KT_INT then
    begin
      index := lse_ds_vary_index(this, Param^.param[1]^.VInteger);
      lse_set_money(Param^.result, lse_ds_getfm(this, index));
    end
    else
    if clss = KT_STRING then
    begin
      name := lse_strec_data(Param^.param[1]^.VString); 
      index := lse_ds_indexof(this, name);
      if index >= 0 then
        lse_set_money(Param^.result, lse_ds_getfm(this, index)) else
        __SetError(Param, 'field "%s" not found', [name]);
    end
    else __SetError(Param, 'Invalid parametre type %s', [clss.FullName]);
  end;
end;

procedure pp_dataset_getiv(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index, count: integer;
begin
  if __GetThis(Param, this) then
  begin
    count := lse_ds_field_count(this);
    index := lse_vary_index(Param^.param[1]^.VInteger, count);
    lse_check_index(index, count);
    case lse_ds_field_type(this, index) of
      LSV_INT  : lse_set_int64(Param^.result, lse_ds_getfi(this, index));
      LSV_FLOAT: lse_set_float(Param^.result, lse_ds_getfd(this, index));
      LSV_MONEY: lse_set_money(Param^.result, lse_ds_getfm(this, index));
      LSV_BOOL : lse_set_bool(Param^.result, lse_ds_getfb(this, index));
     {lSV_STRING, LSV_CHAR}
            else lse_set_string(Param^.result, lse_ds_getfs(this, index));
    end;
  end;
end;

procedure pp_dataset_getpv(const Param: PLseParam);cdecl;
var
  this: PLseDS;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_ds_indexof(this, lse_strec_data(Param^.param[1]^.VString));
    if index < 0 then
      __SetError(Param, 'field "%s" not found',
        [lse_strec_string(Param^.param[1]^.VString)])
    else
    case lse_ds_field_type(this, index) of
      LSV_INT  : lse_set_int64(Param^.result, lse_ds_getfi(this, index));
      LSV_FLOAT: lse_set_float(Param^.result, lse_ds_getfd(this, index));
      LSV_MONEY: lse_set_money(Param^.result, lse_ds_getfm(this, index));
      LSV_BOOL : lse_set_bool(Param^.result, lse_ds_getfb(this, index));
     {lSV_STRING, LSV_CHAR}
            else lse_set_string(Param^.result, lse_ds_getfs(this, index));
    end;
  end;
end;

{ vargen }

procedure pp_vargen_create(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := __AsVargen(__AsEngine(Param), Param^.param[1]); 
  lse_set_vargen(Param^.result, this);
end;

procedure pp_vargen_eof(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.result, lse_vargen_eof(this));
end;

procedure pp_vargen_next(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_vargen_send(this, Param^.result);
end;

procedure pp_vargen_rewind(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.result, lse_vargen_rewind(this));
end;

procedure pp_vargen_send(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  varb: KLiVarb;
  data: PLseValue;
begin
  this := lse_vargen_this(Param);
  varb := KLiVarb(__AsObject(Param^.param[1]));
  if varb <> nil then
  begin
    data := __AsRunner(Param).GetValue(varb);
    if data <> nil then
    begin
      lse_set_bool(Param^.result, lse_vargen_send(this, data));
      __SetClassValue(__AsEngine(Param), data, varb.ValueType);
    end;
  end;
end;

{ stream }

procedure pp_stream_close(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    this^.close(this) else
    __SetErrorThis(Param);
end;

procedure pp_stream_eof(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    lse_set_bool(Param^.result, this^.eof(this) <> 0) else
    __SetErrorThis(Param);
end;

procedure pp_stream_get_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.result, this^.seek(this, 0, SSF_CURRENT)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_set_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    this^.seek(this, Param^.param[1]^.VInteger, SSF_BEGINNING) else
    __SetErrorThis(Param);
end;

procedure pp_stream_get_length(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.result, this^.get_size(this)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_set_length(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    this^.set_size(this, Param^.param[1]^.VInteger) else
    __SetErrorThis(Param);
end;

procedure pp_stream_read(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
  line: string;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
  begin
    if Assigned(this^.read) then
    begin
      size := Param^.param[1]^.VInteger;
      if size > 0 then
      begin
        SetLength(line, size);
        size := this^.read(this, pointer(line), size);
        if size > 0 then
        begin
          lse_set_string(Param^.result, pchar(line), size);
          Exit;
        end;
      end;
    end;
    lse_set_string(Param^.result, '');
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_readln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    lse_set_string(Param^.result, this^.readln(this)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_write(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
  begin
    size := lse_stream_write(this, Param^.param[1]^.VString);
    lse_set_int64(Param^.result, size);
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_writeln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.result,
      lse_stream_writeln(this, Param^.param[1]^.VString)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_writeTo(const Param: PLseParam);cdecl;
var
  this, desti: PLseStream;
  bytes: integer;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
  begin
    desti := PLseStream(Param^.param[1]^.VObject);
    if (desti <> nil) and (desti <> this) then
      bytes := lse_stream_fill(desti, this, Param^.param[2]^.VInteger) else
      bytes := 0;
    lse_set_int64(Param^.result, bytes);
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_flush(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
    this^.flush(this) else
    __SetErrorThis(Param);
end;

procedure pp_stream_lines(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  varg: PLseVargen;
begin
  this := PLseStream(Param^.param[0]^.VObject);
  if this <> nil then
  begin
    varg := cvgr_stream_lines(this, __AsEngine(Param));
    lse_set_object(Param^.result, KR_VARGEN, varg);
  end
  else __SetErrorThis(Param);
end;

{ char }

procedure pp_char_ord(const Param: PLseParam);cdecl;
var
  ch: char;
begin
  ch := __AsChar(Param^.param[0]);
  lse_set_int64(Param^.result, Ord(ch));
end;

procedure pp_char_inMBCS(const Param: PLseParam);cdecl;
var
  ch: char;
begin
  ch := __AsChar(Param^.param[0]);
  lse_set_bool(Param^.result, ch in LeadBytes);
end;

{ string }

procedure pp_string_length(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_int64(Param^.result, lse_strec_length(this));
end;

procedure pp_string_getiv(const Param: PLseParam);cdecl;
var
  this: PLseString;
  index, range: int64;
begin
  this := Param^.param[0]^.VString;
  range := lse_strec_length(this);
  index := lse_vary_index(__AsInt64(Param^.param[1]), range);
  lse_check_index(index, range);
  lse_set_char(Param^.result, lse_strec_data(this)[index]);
end;

procedure pp_string_setAt(const Param: PLseParam);cdecl;
var
  this: PLseString;
  index, range: int64;
begin
  this := Param^.param[0]^.VString;
  range := lse_strec_length(this);
  index := lse_vary_index(__AsInt64(Param^.param[1]), range);
  lse_check_index(index, range);
  this := lse_strec_alloc(lse_strec_data(this), range);
  lse_strec_data(this)[index] := __AsChar(Param^.param[2]);
  lse_set_string(Param^.result, this);
end;

procedure pp_string_name(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_string(Param^.result, __extractName(lse_strec_data(this)));
end;

procedure pp_string_value(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_string(Param^.result, __extractValue(lse_strec_data(this)));
end;

procedure pp_string_lower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := __smrLower(lse_strec_dup(Param^.param[0]^.VString));
  lse_set_string(Param^.result, this);
end;

procedure pp_string_upper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := __smrUpper(lse_strec_dup(Param^.param[0]^.VString));
  lse_set_string(Param^.result, this);
end;

procedure pp_string_compare(const Param: PLseParam);cdecl;
var
  this, value: PLseString;
  IgnoreCase: boolean;
begin
  this := Param^.param[0]^.VString;
  value := Param^.param[1]^.VString;
  IgnoreCase := __AsBool(Param^.param[2]);
  lse_set_int64(Param^.Result, __smrComp(this, Value, IgnoreCase));
end;

procedure pp_string_replace(const Param: PLseParam);cdecl;
var
  this, patten, newStr: pchar;
  flags: TReplaceFlags;
begin
  this := lse_strec_data(Param^.param[0]^.VString);
  patten := lse_strec_data(Param^.param[1]^.VString);
  newStr := lse_strec_data(Param^.param[2]^.VString);
  flags := [rfReplaceAll];
  if __AsBool(Param^.param[3]) then // IgnoreCase
    flags := flags + [rfIgnoreCase];
  if __AsBool(Param^.param[4]) then // FirstOnly
    flags := flags - [rfReplaceAll];
  lse_set_string(Param^.result, StringReplace(this, patten, newStr, flags));
end;

procedure pp_string_pos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.param[0]^.VString;
  patten := Param^.param[1]^.VString;
  IgnoreCase := __AsBool(Param^.param[2]);
  str := lse_strec_data(this);
  pos := __pos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.Result, pos - str) else
    lse_set_int64(Param^.Result, -1);
end;

procedure pp_string_lastPos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.param[0]^.VString;
  patten := Param^.param[1]^.VString;
  IgnoreCase := __AsBool(Param^.param[2]);
  str := lse_strec_data(this);
  pos := __lastPos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.Result, pos - str) else
    lse_set_int64(Param^.Result, -1);
end;

procedure pp_string_left(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
begin
  this := Param^.param[0]^.VString;
  slen := lse_strec_length(this);
  size := min(__AsInt64(Param^.param[1]), slen);
  if size = slen then
    lse_set_string(Param^.result, this) else
  if size > 0 then
    lse_set_string(Param^.result, lse_strec_alloc(lse_strec_data(this), size)) else
    lse_set_string(Param^.result, '', 0);
end;

procedure pp_string_right(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
  base: pchar;
begin
  this := Param^.param[0]^.VString;
  slen := lse_strec_length(this);
  size := min(__AsInt64(Param^.param[1]), slen);
  if size = slen then
    lse_set_string(Param^.result, this) else
  if size > 0 then
  begin
    base := lse_strec_data(this) + (slen - size);
    lse_set_string(Param^.result, lse_strec_alloc(base, size));
  end
  else lse_set_string(Param^.result, '', 0);
end;

procedure pp_string_trim(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.param[0]^.VString;
  if (__smrCountTab(this, L, M, R) > 0) and ((L + R) > 0) then
  begin
    M := lse_strec_length(this) - (L + R);
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.result, this);
end;

procedure pp_string_trimLeft(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.param[0]^.VString;
  if (__smrCountTab(this, L, M, R) > 0) and (L > 0) then
  begin
    M := lse_strec_length(this) - L;
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.result, this);
end;

procedure pp_string_trimRight(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
begin
  this := Param^.param[0]^.VString;
  if (__smrCountTab(this, L, M, R) > 0) and (R > 0) then
  begin
    M := lse_strec_length(this) - R;
    lse_set_string(Param^.result, lse_strec_alloc(lse_strec_data(this), M));
  end
  else lse_set_string(Param^.result, this);
end;

procedure pp_string_trimAll(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base, next: pchar;
begin
  this := Param^.param[0]^.VString;
  if __smrCountTab(this, L, M, R) > 0 then
  begin
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, lse_strec_length(this) - (L + M + R));
    if this <> nil then
    begin
      next := lse_strec_data(this);
      while base^ <> #0  do
      begin
        if not (base^ in SpaceChar) then
        begin
          next^ := base^;
          Inc(next);
        end;
        Inc(base);
      end;
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_copy(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, index, count: int64;
begin
  this := Param^.param[0]^.VString;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := __AsInt64(Param^.param[1]);
    count := __AsInt64(Param^.param[2]);
    index := lse_vary_range(index, slen, count);
    if count > 0 then
    begin
      if count < slen then
        this := lse_strec_alloc(lse_strec_data(this) + index, count);
      lse_set_string(Param^.result, this);
    end;
  end;
end;

procedure pp_string_delete(const Param: PLseParam);cdecl;
var
  this: PLseString;
  base, next: pchar;
  slen, index, count: int64;
begin
  this := Param^.param[0]^.VString;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := __AsInt64(Param^.param[1]);
    if Param^.count > 2 then
      count := __AsInt64(Param^.param[2]) else
      count := 1;
    index := lse_vary_range(index, slen, count);
    if count > 0 then
    begin
      Dec(slen, count);
      if slen > 0 then
      begin
        base := lse_strec_data(this);
        this := lse_strec_alloc(nil, slen);
        next := lse_strec_data(this);
        if index > 0 then
        begin
          Move(base^, next^, index);
          Dec(slen, index);
          Inc(next, index);
          Inc(base, index);
        end;
        Move((base + count)^, next^, slen);
      end
      else this := nil;
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_insert(const Param: PLseParam);cdecl;
var
  this, text: PLseString;
  base, next: pchar;
  index, slen, size: int64;
begin
  this := Param^.param[0]^.VString;
  size := lse_strec_length(this);
  text := Param^.param[1]^.VString;
  slen := lse_strec_length(text);
  if slen > 0 then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[2]), size);
    if (index >= 0) and (index <= size) then
    begin
      base := lse_strec_data(this);
      this := lse_strec_alloc(nil, size + slen);
      next := lse_strec_data(this);
      if index > 0 then
      begin
        Move(base^, next^, index);
        Dec(size, index);
        Inc(base, index);
        Inc(next, index);
      end;
      Move(lse_strec_data(text)^, next^, slen);
      Inc(next, slen);
      Move(base^, next^, size);
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_isAlpha(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), AlphaChar));
end;

procedure pp_string_isAlnum(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), AlnumChar));
end;

procedure pp_string_isCntrl(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), CntrlChar));
end;

procedure pp_string_isDigit(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), DigitChar));
end;

procedure pp_string_isSpace(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), SpaceChar));
end;

procedure pp_string_isHex(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this), HexChar));
end;

procedure pp_string_extractName(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.param[0]^.VString;
  if Param^.count > 1 then
    midc := __AsString(Param^.param[1]) else
    midc := '=';
  lse_set_string(Param^.result, __extractName(lse_strec_data(this), midc));
end;

procedure pp_string_extractValue(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.param[0]^.VString;
  if Param^.count > 1 then
    midc := __AsString(Param^.param[1]) else
    midc := '=';
  lse_set_string(Param^.result, __extractValue(lse_strec_data(this), midc));
end;

procedure pp_string_saveToFile(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(__AsFileName(Param^.param[1]), fmCreate) do
  try
    this := Param^.param[0]^.VString;
    WriteBuffer(lse_strec_data(this)^, lse_strec_length(this));
  finally
    Free;
  end;
end;

procedure pp_string_fileText(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(__AsFileName(Param^.param[0]), fmShareDenyWrite) do
  try
    this := lse_strec_alloc(nil, size);
    lse_set_string(Param^.result, this);
    if this <> nil then
      Read(lse_strec_data(this)^, size);
  finally
    Free;
  end;
end;

procedure pp_string_lformat(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.param[0]^.VString;
  size := lse_strec_length(this);
  width := max(0, __AsInt64(Param^.param[1]));
  if width > size then
  begin
    filler := __AsChar(Param^.param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    Move(base^, next^, size);
    Inc(next, size);
    for width := width - size downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_rformat(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.param[0]^.VString;
  size := lse_strec_length(this);
  width := __AsInt64(Param^.param[1]);
  if width > size then
  begin
    filler := __AsChar(Param^.param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    for width := width - size downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
    Move(base^, next^, size);
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_center(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width, A: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.param[0]^.VString;
  size := lse_strec_length(this);
  width := __AsInt64(Param^.param[1]);
  if width > size then
  begin
    filler := __AsChar(Param^.param[2]);
    if filler = #0 then
      filler := ' ';
    base := lse_strec_data(this);
    this := lse_strec_alloc(nil, width);
    next := lse_strec_data(this);
    Dec(width, size);
    for A := width div 2 downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
    Move(base^, next^, size);
    Inc(next, size);
    for A := width - (width div 2) downto 1 do
    begin
      next^ := filler;
      Inc(next);
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_randomOrder(const Param: PLseParam);cdecl;

  function __randomOrder(const S: string): string;
  var
    A, L, X: integer;
    C: char;
  begin
    Result := S;
    L := Length(Result);
    if L > 1 then
    repeat
      Randomize;
      for A := 1 to L do
      begin
        repeat
          X := System.Random(L) + 1;
        until X <> A;
        C := Result[A];
        Result[A] := Result[X];
        Result[X] := C;
      end;
    until Result <> S;
  end;

var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  if lse_strec_length(this) > 1 then
    lse_set_string(Param^.result, __randomOrder(lse_strec_data(this))) else
    lse_set_string(Param^.result, this);
end;

procedure pp_string_html(const Param: PLseParam);cdecl;
var
  smr: PLseString;
begin
  smr := Param^.param[0]^.VString;
  lse_set_string(Param^.result, __encodeHTML(lse_strec_data(smr), lse_strec_length(smr), false));
end;

// string string.reverse()
procedure pp_string_reverse(const Param: PLseParam);cdecl;
var
  this: PLseString;
  head, last: pchar;
  temp: char;
begin
  this := Param^.param[0]^.VString;
  if lse_strec_length(this) > 1 then
  begin
    this := lse_strec_dup(this);
    head := lse_strec_data(this);
    last := head + lse_strec_length(this) - 1;
    while head < last do
    begin
      temp := head^;
      head^ := last^;
      last^ := temp;
      Inc(head);
      Dec(last);
    end;
  end;
  lse_set_string(Param^.result, this);
end;

// bool string.isLower()
procedure pp_string_isLower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this),
    lse_strec_length(this), ['a'..'z']));
end;

// bool string.isUpper()
procedure pp_string_isUpper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  lse_set_bool(Param^.result, __inCharSet(lse_strec_data(this),
    lse_strec_length(this), ['A'..'Z']));
end;

// string string.translate(string OrdCharList, string newCharList)
procedure pp_string_translate(const Param: PLseParam);cdecl;
var
  this, old, new: PLseString;
  size, index: integer;
  map: array[char] of char;
  base, next: pchar;
begin
  this := Param^.param[0]^.VString;
  old  := Param^.param[1]^.VString;
  new  := Param^.param[2]^.VString;
  size  := min(lse_strec_length(old), lse_strec_length(new));
  if (size > 0) and (lse_strec_length(this) > 0) then
  begin
    base := lse_strec_data(old);
    next := lse_strec_data(new);
    FillChar(map, sizeof(map), 0);
    for index := 1 to size do
    begin
      map[base^] := next^;
      Inc(base);
      Inc(next);
    end;
    this := lse_strec_dup(this);
    base := lse_strec_data(this);
    size := lse_strec_length(this);
    for index := 1 to size do
    begin
      if map[base^] <> #0 then
        base^ := map[base^];
      Inc(base);
    end;
  end;
  lse_set_string(Param^.result, this);
end;

procedure pp_string_filePath(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    ExtractFilePath(__AsFileName(Param^.param[0])));
end;

procedure pp_string_fullFileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    lse_expand_fname(__AsFileName(Param^.param[0])));
end;

procedure pp_string_fileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    ExtractFileName(__AsFileName(Param^.param[0])));
end;

procedure pp_string_fileExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    ExtractFileExt(__AsFileName(Param^.param[0])));
end;

procedure pp_string_changeExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.result,
    ChangeFileExt(__AsFileName(Param^.param[0]),
                  __AsFileName(Param^.param[1])));
end;

procedure pp_string_md5sum(const Param: PLseParam);cdecl;
var
  this: PLseString;
  sum5: string;
begin
  if not __AsBool(Param^.param[1]) then
  begin
    this := Param^.param[0]^.VString;
    sum5 := __md5sumBuf(lse_strec_data(this), lse_strec_length(this));
  end
  else sum5 := __md5sumFile(__AsFileName(Param^.param[0]));
  lse_set_string(Param^.result, sum5);
end;

procedure pp_string_hexToInt(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.param[0]^.VString;
  if __inCharSet(lse_strec_data(this), lse_strec_length(this), HexChar) then
    lse_set_int64(Param^.Result, StrToInt64('$' + lse_strec_data(this))) else
    lse_set_int64(Param^.result, __AsInt64(Param^.param[1]));
end;

procedure pp_string_hash(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.result, __hashof(__AsString(Param^.param[0])));
end;

{ strlist }

procedure pp_strlist_create(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  this := KLiStrlist.Create;
  __SetStrlist(Param^.result, this);
  if Param^.count > 1 then
    this.Text := __AsString(Param^.param[1]);
end;

procedure pp_strlist_length(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.Count);
end;

procedure pp_strlist_getiv(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    lse_set_string(Param^.result, this.Strings[index]);
  end;
end;

procedure pp_strlist_setiv(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    sorted := this.Sorted;
    this.Sorted := false;
    this.Strings[index] := __AsString(Param^.param[2]);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_getText(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, __getText(this, LB));
end;

procedure pp_strlist_setText(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    sorted := this.Sorted;
    this.Sorted := false;
    __SetText(this, __AsString(Param^.param[1]), LB, false);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_getCommaText(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.CommaText);
end;

procedure pp_strlist_setCommaText(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    sorted := this.Sorted;
    this.Sorted := false;
    this.CommaText := __AsString(Param^.param[1]);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_add(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  text: string;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    text := __AsString(Param^.param[1]);
    if Param^.param[2]^.VBool then
    begin
      index := this.IndexOf(text);
      if index < 0 then
        index := this.Add(text);
    end
    else index := this.Add(text);
    lse_set_int64(Param^.result, index);
  end;
end;

procedure pp_strlist_addFrom(const Param: PLseParam);cdecl;
var
  this, list: KLiStrlist;
  index, count: integer;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    list := __AsStrlist(Param^.param[1]);
    if (list <> nil) and (list.Count > 0) then
    begin
      sorted := this.Sorted;
      this.Sorted := false;
      try
        count := list.Count;
        for index := 0 to count - 1 do
          this.Add(list[index]);
      finally
        this.Sorted := sorted;
      end;
    end;
  end;
end;

procedure pp_strlist_fill(const Param: PLseParam);cdecl;
var
  this: KLiStrList;
  varg: PLseVargen;
  data: RLseValue;
begin
  if __GetThis(Param, this) then
    if Param^.count > 1 then
    begin
      if Param^.param[2]^.VBool then
        this.Clear;
      varg := __AsVargen(__AsEngine(Param), Param^.param[1]);
      lse_init_value(@data);
      try
        while lse_vargen_send(varg, @data) do
          this.Add(__AsString(@data));
      finally
        lse_set_nil(@data);
      end;
    end;
end;

procedure pp_strlist_insert(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, int64(this.Count) + 1);
    sorted := this.Sorted;
    this.Sorted := false;
    this.Insert(index, __AsString(Param^.param[2]));
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_move(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  x1, x2: int64;
begin
  if __GetThis(Param, this) and not this.Sorted then
  begin
    x1 := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(x1, this.Count);
    x2 := lse_vary_index(__AsInt64(Param^.param[2]), this.Count);
    lse_check_index(x2, this.Count);
    this.Move(x1, x2);
  end;
end;

procedure pp_strlist_exchange(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  x1,x2: int64;
begin
  if __GetThis(Param, this) and not this.Sorted then
  begin
    x1 := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(x1, this.Count);
    x2 := lse_vary_index(__AsInt64(Param^.param[2]), this.Count);
    lse_check_index(x2, this.Count);
    this.Exchange(x1, x2);
  end;
end;

procedure pp_strlist_delete(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    this.Delete(index);
  end;
end;

procedure pp_strlist_clear(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    this.Clear;
end;

procedure pp_strlist_loadFromFile(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    sorted := this.Sorted;
    this.Sorted := false;
    this.LoadFromFile(__AsFileName(Param^.param[1]));
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_saveToFile(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    this.SaveToFile(__AsFileName(Param^.param[1]));
end;

procedure pp_strlist_indexOf(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.IndexOf(__AsString(Param^.param[1])));
end;

procedure pp_strlist_indexOfName(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.IndexOfName(__AsString(Param^.param[1])));
end;

procedure pp_strlist_setName(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    sorted := this.Sorted;
    this.Sorted := false;
    this[Index] := __AsString(Param^.param[2]) + '=' + __extractValue(this[Index]);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_setValue(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: int64;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    sorted := this.Sorted;
    this.Sorted := false;
    this[Index] := this.Names[Index] + '=' + __AsString(Param^.param[2]);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_read(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  name, defv: string;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
  begin
    name := __AsString(Param^.param[1]);
    defv := __AsString(Param^.param[2]);
    lse_set_string(Param^.result, this.ReadValue(name, defv));
  end;
end;

procedure pp_strlist_getpv(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  name: string;
  index: integer;
begin
  if __GetThis(Param, this) and (Param^.count > 1) then
  begin
    name := __AsString(Param^.param[1]);
    index := this.IndexOfName(name);
    if index < 0 then
      __SetError(Param, 'name "%s" not found', [name]) else
      lse_set_string(Param^.result, __extractValue(this[index]));
  end;
end;

procedure pp_strlist_setpv(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  sorted: boolean;
begin
  if __GetThis(Param, this) then
  begin
    sorted := this.Sorted;
    this.Sorted := false;
    this.Values[__AsString(Param^.param[1])] := __AsString(Param^.param[2]);
    this.Sorted := sorted;
  end;
end;

procedure pp_strlist_names(const Param: PLseParam);cdecl;
var
  this, list: KLiStrlist;
  index: integer;
  name: string;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiStrlist.Create;
    __SetStrlist(Param^.result, list);
    for index := 0 to this.Count - 1 do
    begin
      name := Trim(__extractName(this[index]));
      if name <> '' then
        list.Add(name);
    end;
  end;
end;

procedure pp_strlist_copy(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index, count: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := __AsInt64(Param^.param[1]);
    count := __AsInt64(Param^.param[2]);
    index := lse_vary_range(index, this.Count, count);
    __SetStrlist(Param^.result, this.Copy(index, count));
  end;
end;

procedure pp_strlist_sort(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    this.Sort;
end;

procedure pp_strlist_getSorted(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, this.Sorted);
end;

procedure pp_strlist_setSorted(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    this.Sorted := __AsBool(Param^.param[1]);
end;

procedure pp_strlist_reverse(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  L, H: integer;
begin
  if __GetThis(Param, this) then
  begin
    this.Sorted := false;
    L := 0;
    H := this.Count - 1;
    while L < H do
    begin
      this.Exchange(L, H);
      Inc(L);
      Dec(H);
    end;
  end;
end;

procedure pp_strlist_unique(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index, X: integer;
  line: string;
  found: boolean;
begin
  if __GetThis(Param, this) then
  begin
    index := 1;
    while index < this.Count do
    begin
      line := this[index];
      found := false;
      for X := index - 1 downto 0 do
      begin
        found := (this.CompStr(line, this[X]) = 0);
        if found then Break;
      end;
      if found then
        this.Delete(index) else
        Inc(index);
    end;
  end;
end;

procedure pp_strlist_getCaseSensitive(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, this.CaseSensitive);
end;

procedure pp_strlist_setCaseSensitive(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    this.CaseSensitive := __AsBool(Param^.param[1]);
end;

procedure pp_strlist_filter(const Param: PLseParam);cdecl;
var
  this, list: KLiStrlist;
  func: KLiFunc;
  cond: RLseValue;
  rnnr: KLiRunner;
  line: string;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiStrList.Create();
    lse_set_object(Param^.result, KR_STRLIST, list);
    func := __AsFunc(Param^.param[1]);
    if (func <> nil) and (func.ResultType <> KT_VOID) then
    begin
      rnnr := __AsRunner(Param);
      lse_init_value(@cond);
      try
        index := 0;
        while index < this.Count do
        begin
          line := this[index];
          Inc(index);
          rnnr.Stack.PushString(line);
          if not rnnr.Goon(func, 1, @cond) then Break else
          if __AsBool(@cond) then
            list.Add(line);
        end;
      finally
        lse_clear_value(@cond);
      end;
    end;
  end;
end;

procedure pp_strlist_min(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) and (this.Count > 0) then
    lse_set_string(Param^.result, this.MinStr);
end;

procedure pp_strlist_max(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) and (this.Count > 0) then
    lse_set_string(Param^.result, this.MaxStr);
end;

procedure pp_strlist_first(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.First);
end;

procedure pp_strlist_last(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Last);
end;

procedure pp_strlist_shift(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
begin
  if __GetThis(Param, this) then
  begin
    lse_set_string(Param^.result, this[0]);
    this.Delete(0);
  end;
end;

procedure pp_strlist_pop(const Param: PLseParam);cdecl;
var
  this: KLiStrlist;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    index := this.Count - 1;
    lse_set_string(Param^.result, this[index]);
    this.Delete(index);
  end;
end;

{ time }

procedure pp_time_yearOf(const Param: PLseParam);cdecl;
var
  this: TDateTime;
begin
  this := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, YearOf(this));
end;

procedure pp_time_yearsBetween(const Param: PLseParam);cdecl;
var
  this, time: TDateTime;
begin
  this := __AsTime(Param^.param[0]);
  time := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, yearsBetween(this, time));
end;

procedure pp_time_incYear(const Param: PLseParam);cdecl;
var
  this: TDateTime;
  years: integer;
begin
  this := __AsTime(Param^.param[0]);
  years := __AsInt64(Param^.param[1]);
  lse_set_time(Param^.result, incYear(this, years));
end;

procedure pp_time_monthOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, MonthOf(V));
end;

procedure pp_time_monthsBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, monthsBetween(V1, V2));
end;

procedure pp_time_incMonth(const Param: PLseParam);cdecl;
var
  V: TDateTime;
  N: integer;
begin
  V := __AsTime(Param^.param[0]);
  N := __AsInt64(Param^.param[1]);
  lse_set_time(Param^.result, incMonth(V, N));
end;

procedure pp_time_dayOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, DayOf(V));
end;

procedure pp_time_dayOfWeek(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, DayOfTheWeek(V));
end;

procedure pp_time_dayOfYear(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, DayOfTheYear(V));
end;

procedure pp_time_daysBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, DaysBetween(V1, V2));
end;

procedure pp_time_incDay(const Param: PLseParam);cdecl;
var
  V: TDateTime;
  N: integer;
begin
  V := __AsTime(Param^.param[0]);
  N := __AsInt64(Param^.param[1]);
  lse_set_time(Param^.result, incDay(V, N));
end;

procedure pp_time_format(const Param: PLseParam);cdecl;
var
  D: TDateTime;
  F: string;
begin
  D := __AsTime(Param^.param[0]);
  F := __AsString(Param^.param[1]);
  if F = '' then
    F :=  'yyyy/mm/dd hh:nn:ss zzz';
  lse_set_string(Param^.Result, FormatDateTime(F, D));
end;

procedure pp_time_weekOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, WeekOf(V));
end;

procedure pp_time_weekOfMonth(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, WeekOfTheMonth(V));
end;

procedure pp_time_weeksBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, WeeksBetween(V1, V2));
end;

procedure pp_time_hourOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, HourOf(V));
end;

procedure pp_time_hoursBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, HoursBetween(V1, V2));
end;

procedure pp_time_minuteOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, MinuteOf(V));
end;

procedure pp_time_minutesBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, MinutesBetween(V1, V2));
end;

procedure pp_time_secondOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, SecondOf(V));
end;

procedure pp_time_secondsBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, SecondsBetween(V1, V2));
end;

procedure pp_time_milliSecondOf(const Param: PLseParam);cdecl;
var
  V: TDateTime;
begin
  V := __AsTime(Param^.param[0]);
  lse_set_int64(Param^.Result, MilliSecondOf(V));
end;

procedure pp_time_milliSecondsBetween(const Param: PLseParam);cdecl;
var
  V1, V2: TDateTime;
begin
  V1 := __AsTime(Param^.param[0]);
  V2 := __AsTime(Param^.param[1]);
  lse_set_int64(Param^.Result, MilliSecondsBetween(V1, V2));
end;

{ type }

procedure pp_type_name(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Name);
end;

procedure pp_type_description(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Description);
end;

procedure pp_type_simple(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, this.IsSimpleType);
end;

procedure pp_type_builtin(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, this.Builtin);
end;

procedure pp_type_info(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Infomation);
end;

procedure pp_type_module(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    __SetModule(Param^.result, this.Module);
end;

procedure pp_type_methods(const Param: PLseParam);cdecl;
var
  this: KLiClass;
  list: KLiVarList;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    list := __NewVarlist(__AsEngine(Param));
    __SetVarlist(Param^.result, list);
    for index := 0 to this.MethodList.Count - 1 do
      list.PushObject(KLiFunc(this.MethodList.Objects[index]), KT_FUNC);
  end;
end;

procedure pp_type_getpv(const Param: PLseParam);cdecl;
var
  this: KLiClass;
  name: string;
  func: KLiFunc;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    func := this.FindMethod(cmMethod, name);
    if func = nil then
      __SetError(Param, 'method %s.%s() not found.', [this.FullName, name]) else
      lse_set_object(Param^.result, KR_FUNC, func);
  end;
end;

procedure pp_type_isUDC(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.result, this.IsHashed and (this <> KT_HASHED));
end;

procedure pp_type_addr(const Param: PLseParam);cdecl;
var
  this: KLiClass;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.UID);
end;

{ variable }

procedure pp_varb_name(const Param: PLseParam);cdecl;
var
  this: KLiVarb;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.result, this.Name);
end;

procedure pp_varb_type(const Param: PLseParam);cdecl;
var
  this: KLiVarb;
begin
  if __GetThis(Param, this) then
    lse_set_class(Param^.result, this.ValueType.ClassRec);
end;

procedure pp_varb_func(const Param: PLseParam);cdecl;
var
  this: KLiVarb;
begin
  if __GetThis(Param, this) then
    __SetFunc(Param^.result, this.Func);
end;

{ varlist }

procedure pp_varlist_create(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  this := __NewVarlist(__AsEngine(Param)); 
  __SetVarlist(Param^.result, this);
  if Param^.count > 1 then
    this.Count := Max(0, Param^.param[1]^.VInteger);
end;

procedure pp_varlist_getpv(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  snap: KLiVarSnap;
  name: string;
  data: PLseValue;
  varb: KLiVarb;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    if this.IsSnap then
    begin
      snap := this as KLiVarSnap;
      data := snap.GetByName(name, varb);
      if data = nil then
        __SetError(Param, 'variable %s not found', [name]) else
        lse_set_value(Param^.result, data);
    end
    else
    begin
      data := this.GetNamed(name);
      if data <> nil then
        lse_set_value(Param^.result, data) else
        __setError(Param, 'value "%s" not found', [name]);
    end;
  end;
end;

procedure pp_varlist_setpv(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  snap: KLiVarSnap;
  name: string;
  data: PLseValue;
  varb: KLiVarb;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    if this.IsSnap then
    begin
      snap := this as KLiVarSnap;
      data := snap.GetByName(name, varb);
      if data = nil then
         __SetError(Param, 'variable %s not found', [name]) else
        lse_set_value(data, varb.ValueType.Cast(Param, 2));
    end
    else
    begin
      data := this.GetNamed(name);
      if data = nil then
        data := this.AddNamed(name);
      lse_set_value(data, Param^.param[2]);
    end;
  end;
end;

procedure pp_varlist_isset(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  snap: KLiVarSnap;
  name: string;
begin
  if __GetThis(Param, this) then
  begin
    name := lse_strec_data(Param^.param[1]^.VString);
    if this.IsSnap then
    begin
      snap := this as KLiVarSnap;
      lse_set_bool(Param^.result, snap.FindVarb(name) <> nil);
    end
    else lse_set_bool(Param^.result, this.ValueIndex(name) >= 0);
  end;
end;

procedure pp_varlist_read(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  name: string;
  data: PLseValue;
  varb: KLiVarb;
begin
  if __GetThis(Param, this) then
  begin
    name := __AsString(Param^.param[1]);
    if this.IsSnap then
      data := (this as KLiVarSnap).GetByName(name, varb) else
      data := this.GetNamed(name);
    if data <> nil then
      lse_set_value(Param^.result, data);
  end;
end;

procedure pp_varlist_remove(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  name: string;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      name := lse_strec_data(Param^.param[1]^.VString);
      lse_set_bool(Param^.result, this.RemoveNamed(name));
    end;
end;

procedure pp_varlist_keys(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  snap: KLiVarSnap;
  list: KLiStrList;
  index: integer;
begin
  if __GetThis(Param, this) then
    if this.IsSnap then
    begin
      snap := this as KLiVarSnap;
      list := KLiStrList.Create;
      __SetStrlist(Param^.result, list);
      for index := 0 to snap.Count - 1 do
        list.Add(snap.Varbs[index].Name);
    end
    else lse_set_object(Param^.result, KR_STRLIST,
      this.GetNameList(__AsBool(Param^.param[1])));
end;

procedure pp_varlist_get_name(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: integer;
  name: string;
begin
  if __GetThis(Param, this) then
  begin
    index := __AsInt64(Param^.param[1]); 
    if this.IsSnap then
      name := (this as KLiVarSnap).Varbs[index].Name else
      name := this.Names[index];
    lse_set_string(Param^.result, name);
  end;
end;

procedure pp_varlist_get_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.result, this.Count);
end;

procedure pp_varlist_set_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
      this.Count := Max(0, Param^.param[1]^.VInteger);
end;

procedure pp_varlist_getiv(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    lse_set_value(Param^.result, this[index]);
  end;
end;

procedure pp_varlist_setiv(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  snap: KLiVarSnap;
  index: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
    lse_check_index(index, this.Count);
    if this.IsSnap then
    begin
      snap := this as KLiVarSnap;
      __SetClassValue(__AsEngine(Param),
        lse_set_value(snap[index], Param^.param[2]),
          snap.Varbs[index].ValueType);
    end
    else lse_set_value(this[index], Param^.param[2]);
  end;
end;

procedure pp_varlist_exchange(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  X1, X2: int64;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      X1 := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
      X2 := lse_vary_index(__AsInt64(Param^.param[2]), this.Count);
      if X1 <> X2 then
        this.Exchange(X1, X2);
    end;
end;

procedure pp_varlist_move(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  X1, X2: int64;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      X1 := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
      X2 := lse_vary_index(__AsInt64(Param^.param[2]), this.Count);
      if X1 <> X2 then
        this.Move(X1, X2);
    end;
end;

procedure pp_varlist_add(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      lse_set_int64(Param^.result, this.Count);
      this.Push(Param^.param[1]);
    end;
end;

procedure pp_varlist_addFrom(const Param: PLseParam);cdecl;
var
  this, list: KLiVarList;
  index, count: integer;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      list := KLiVarList(Param^.param[1]^.VObject);
      if (list <> nil) and (list.Count > 0) then
      begin
        count := list.Count;
        for index := 0 to count - 1 do
          this.Push(list[index]);
      end;
    end;
end;

procedure pp_varlist_fill(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  varg: PLseVargen;
  data: RLseValue;
begin
  if __GetThis(Param, this) then
    if (Param^.count > 1) and not this.IsSnap then
    begin
      if Param^.param[2]^.VBool then
        this.Clear;
      varg := __AsVargen(__AsEngine(Param), Param^.param[1]);
      lse_init_value(@data);
      try
        while lse_vargen_send(varg, @data) do
          this.Push(@data);
      finally
        lse_set_nil(@data);
      end;
    end;
end;

procedure pp_varlist_insert(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
      lse_check_index(index, int64(this.Count) + 1);
      lse_set_value(this.Insert(index), Param^.param[2]);
    end;
end;

procedure pp_varlist_delete(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if __GetThis(Param, this) then
    if not this.IsSnap then
    begin
      index := lse_vary_index(__AsInt64(Param^.param[1]), this.Count);
      lse_check_index(index, this.Count);
      this.Delete(index);
    end;
end;

procedure pp_varlist_clear(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if this.IsSnap then
      (this as KLiVarSnap).ClearValues else
      this.Clear;
end;

procedure pp_varlist_copy(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index, count: int64;
begin
  if __GetThis(Param, this) then
  begin
    index := __AsInt64(Param^.param[1]);
    count := __AsInt64(Param^.param[2]);
    index := lse_vary_range(index, this.Count, count);
    __SetVarlist(Param^.result, this.Copy(index, count));
  end;
end;

procedure pp_varlist_left(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if __GetThis(Param, this) then
  begin
    count := __AsInt64(Param^.param[1]);
    __SetVarlist(Param^.result, this.Left(count));
  end;
end;

procedure pp_varlist_right(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if __GetThis(Param, this) then
  begin
    count := __AsInt64(Param^.param[1]);
    __SetVarlist(Param^.result, this.Right(count));
  end;
end;

procedure pp_varlist_filter(const Param: PLseParam);cdecl;
var
  this, list: KLiVarList;
  func: KLiFunc;
  cond: RLseValue;
  rnnr: KLiRunner;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    rnnr := __AsRunner(Param);
    list := __NewVarlist(rnnr.Engine);
    lse_set_object(Param^.result, KR_VARLIST, list);
    func := __AsFunc(Param^.param[1]);
    if (func <> nil) and (func.ResultType <> KT_VOID) then
    begin
      lse_init_value(@cond);
      try
        index := 0;
        while index < this.Count do
        begin
          rnnr.Stack.Push(this[index]);
          if not rnnr.Goon(func, 1, @cond) then Break else
          if __AsBool(@cond) then
            list.Push(this[index]);
          Inc(index);
        end;
      finally
        lse_clear_value(@cond);
      end;
    end;
  end;
end;

procedure pp_varlist_min(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) and (this.Count > 0) then
    lse_set_value(Param^.result, this.MinValue);
end;

procedure pp_varlist_max(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) and (this.Count > 0) then
    lse_set_value(Param^.result, this.MaxValue);
end;

procedure pp_varlist_first(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_value(Param^.result, this.First);
end;

procedure pp_varlist_last(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_value(Param^.result, this.Last);
end;

procedure pp_varlist_shift(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
  begin
    lse_set_value(Param^.result, this[0]);
    if not this.IsSnap then
      this.Delete(0);
  end;
end;

procedure pp_varlist_pop(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: integer;
begin
  if __GetThis(Param, this) then
  begin
    index := this.Count - 1;
    lse_set_value(Param^.result, this[index]);
    if not this.IsSnap then
      this.Delete(index);
  end;
end;

end.
