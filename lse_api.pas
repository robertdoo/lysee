{==============================================================================}
{        UNIT: lse_api                                                         }
{ DESCRIPTION: APIs builtin in lysee kernel                                    }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/02/26                                                      }
{    MODIFIED: 2011/07/09                                                      }
{==============================================================================}
{ Copyright (c) 2003-2011, Li Yun Jie                                          }
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

const
  KTN_ERROR    = 'error';
  KTD_ERROR    = 'error';
  KTN_FLOAT    = 'float';
  KTD_FLOAT    = 'float data type';
  KTN_FUNC     = 'function';
  KTD_FUNC     = 'function';
  KTN_HASHED   = 'hashed';
  KTD_HASHED   = 'hashed key-value list';
  KTN_INT      = 'int';
  KTD_INT      = 'integer value';
  KTN_MODULE   = 'module';
  KTD_MODULE   = 'module';
  KTN_VARGEN   = 'vargen';
  KTD_VARGEN   = 'variant generator';
  KTN_STREAM   = 'stream';
  KTD_STREAM   = 'stream';
  KTN_STRING   = 'string';
  KTD_STRING   = 'string';
  KTN_TYPE     = 'type';
  KTD_TYPE     = 'type';
  KTN_VARIANT  = 'variant';
  KTD_VARIANT  = 'variant data type';
  KTN_VARIABLE = 'variable';
  KTD_VARIABLE = 'variable';
  KTN_VARLIST  = 'varlist';
  KTD_VARLIST  = 'variant list';
  KTN_VARSNAP  = 'varsnap';
  KTD_VARSNAP  = 'variant snap';
  KTN_VOID     = 'void';
  KTD_VOID     = 'void';

{ sys }

procedure pp_system_dir(const Param: PLseParam);cdecl;
procedure pp_system_isdir(const Param: PLseParam);cdecl;
procedure pp_system_isfile(const Param: PLseParam);cdecl;
procedure pp_system_print(const Param: PLseParam);cdecl;
procedure pp_system_printf(const Param: PLseParam);cdecl;
procedure pp_system_println(const Param: PLseParam);cdecl;
procedure pp_system_readln(const Param: PLseParam);cdecl;
procedure pp_system_modules(const Param: PLseParam);cdecl;
procedure pp_system_libs(const Param: PLseParam);cdecl;
procedure pp_system_exit(const Param: PLseParam);cdecl;
procedure pp_system_random(const Param: PLseParam);cdecl;
procedure pp_system_sleep(const Param: PLseParam);cdecl;
procedure pp_system_getenv(const Param: PLseParam);cdecl;
procedure pp_system_dumpc(const Param: PLseParam);cdecl;
procedure pp_system_length(const Param: PLseParam);cdecl;
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
procedure pp_system_curry(const Param: PLseParam);cdecl;
procedure pp_system_curryone(const Param: PLseParam);cdecl;
procedure pp_system_gc(const Param: PLseParam);cdecl;
procedure pp_system_apply(const Param: PLseParam);cdecl;
procedure pp_system_tmpfname(const Param: PLseParam);cdecl;
procedure pp_system_encodeGMT(const Param: PLseParam);cdecl;
procedure pp_system_decodeGMT(const Param: PLseParam);cdecl;
procedure pp_system_encodeUTF8(const Param: PLseParam);cdecl;
procedure pp_system_decodeUTF8(const Param: PLseParam);cdecl;
procedure pp_system_openfs(const Param: PLseParam);cdecl;
procedure pp_system_memory(const Param: PLseParam);cdecl;
procedure pp_system_incPD(const Param: PLseParam);cdecl;
procedure pp_system_excPD(const Param: PLseParam);cdecl;
procedure pp_system_veryPD(const Param: PLseParam);cdecl;
procedure pp_system_veryUD(const Param: PLseParam);cdecl;
procedure pp_system_msecs(const Param: PLseParam);cdecl;
procedure pp_system_current_module(const Param: PLseParam);cdecl;
procedure pp_system_current_func(const Param: PLseParam);cdecl;
procedure pp_system_current_error(const Param: PLseParam);cdecl;
procedure pp_system_current_args(const Param: PLseParam);cdecl;
procedure pp_system_current_prmc(const Param: PLseParam);cdecl;
procedure pp_system_current_prms(const Param: PLseParam);cdecl;
procedure pp_system_current_line(const Param: PLseParam);cdecl;
procedure pp_system_current_envs(const Param: PLseParam);cdecl;
procedure pp_system_current_file(const Param: PLseParam);cdecl;
procedure pp_system_current_pd(const Param: PLseParam);cdecl;
procedure pp_system_eol(const Param: PLseParam);cdecl;
procedure pp_system_each(const Param: PLseParam);cdecl;
procedure pp_system_map(const Param: PLseParam);cdecl;
procedure pp_system_reduce(const Param: PLseParam);cdecl;
procedure pp_system_filter(const Param: PLseParam);cdecl;
procedure pp_system_sum(const Param: PLseParam);cdecl;
procedure pp_system_maxint(const Param: PLseParam);cdecl;
procedure pp_system_minint(const Param: PLseParam);cdecl;
procedure pp_system_abs(const Param: PLseParam);cdecl;
procedure pp_system_find(const Param: PLseParam);cdecl;
procedure pp_system_gsub(const Param: PLseParam);cdecl;
procedure pp_system_split(const Param: PLseParam);cdecl;
procedure pp_system_getcs(const Param: PLseParam);cdecl;
procedure pp_system_throw(const Param: PLseParam);cdecl;
procedure pp_system_hex(const Param: PLseParam);cdecl;
procedure pp_system_bitlist(const Param: PLseParam);cdecl;
procedure pp_system_upto(const Param: PLseParam);cdecl;
procedure pp_system_downto(const Param: PLseParam);cdecl;
procedure pp_system_typeof(const Param: PLseParam);cdecl;

{ error }

procedure pp_error_text(const Param: PLseParam);cdecl;
procedure pp_error_module(const Param: PLseParam);cdecl;
procedure pp_error_name(const Param: PLseParam);cdecl;
procedure pp_error_message(const Param: PLseParam);cdecl;
procedure pp_error_row(const Param: PLseParam);cdecl;
procedure pp_error_col(const Param: PLseParam);cdecl;
procedure pp_error_errno(const Param: PLseParam);cdecl;

{ function }

procedure pp_func_name(const Param: PLseParam);cdecl;
procedure pp_func_desc(const Param: PLseParam);cdecl;
procedure pp_func_type(const Param: PLseParam);cdecl;
procedure pp_func_prototype(const Param: PLseParam);cdecl;
procedure pp_func_module(const Param: PLseParam);cdecl;
procedure pp_func_params(const Param: PLseParam);cdecl;
procedure pp_func_locals(const Param: PLseParam);cdecl;

{ hashed }

procedure pp_hashed_create(const Param: PLseParam);cdecl;
procedure pp_hashed_read(const Param: PLseParam);cdecl;
procedure pp_hashed_remove(const Param: PLseParam);cdecl;
procedure pp_hashed_clear(const Param: PLseParam);cdecl;
procedure pp_hashed_isset(const Param: PLseParam);cdecl;
procedure pp_hashed_keys(const Param: PLseParam);cdecl;
procedure pp_hashed_values(const Param: PLseParam);cdecl;

{ module }

procedure pp_module_name(const Param: PLseParam);cdecl;
procedure pp_module_desc(const Param: PLseParam);cdecl;
procedure pp_module_file(const Param: PLseParam);cdecl;
procedure pp_module_modules(const Param: PLseParam);cdecl;
procedure pp_module_funcs(const Param: PLseParam);cdecl;
procedure pp_module_types(const Param: PLseParam);cdecl;
procedure pp_module_version(const Param: PLseParam);cdecl;
procedure pp_module_main(const Param: PLseParam);cdecl;
procedure pp_module_imports(const Param: PLseParam);cdecl;

{ vargen }

procedure pp_vargen_create(const Param: PLseParam);cdecl;
procedure pp_vargen_eof(const Param: PLseParam);cdecl;
procedure pp_vargen_next(const Param: PLseParam);cdecl;
procedure pp_vargen_rewind(const Param: PLseParam);cdecl;

{ stream }

procedure pp_stream_close(const Param: PLseParam);cdecl;
procedure pp_stream_eof(const Param: PLseParam);cdecl;
procedure pp_stream_get_position(const Param: PLseParam);cdecl;
procedure pp_stream_set_position(const Param: PLseParam);cdecl;
procedure pp_stream_set_length(const Param: PLseParam);cdecl;
procedure pp_stream_read(const Param: PLseParam);cdecl;
procedure pp_stream_readln(const Param: PLseParam);cdecl;
procedure pp_stream_write(const Param: PLseParam);cdecl;
procedure pp_stream_writeln(const Param: PLseParam);cdecl;
procedure pp_stream_writeTo(const Param: PLseParam);cdecl;
procedure pp_stream_flush(const Param: PLseParam);cdecl;
procedure pp_stream_lines(const Param: PLseParam);cdecl;

{ string }

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

{ type }

procedure pp_type_name(const Param: PLseParam);cdecl;
procedure pp_type_description(const Param: PLseParam);cdecl;
procedure pp_type_simple(const Param: PLseParam);cdecl;
procedure pp_type_module(const Param: PLseParam);cdecl;

{ variable }

procedure pp_varb_name(const Param: PLseParam);cdecl;
procedure pp_varb_type(const Param: PLseParam);cdecl;

{ varlist }

procedure pp_varlist_create(const Param: PLseParam);cdecl;
procedure pp_varlist_get_length(const Param: PLseParam);cdecl;
procedure pp_varlist_set_length(const Param: PLseParam);cdecl;
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
procedure pp_varlist_first(const Param: PLseParam);cdecl;
procedure pp_varlist_last(const Param: PLseParam);cdecl;
procedure pp_varlist_shift(const Param: PLseParam);cdecl;
procedure pp_varlist_pop(const Param: PLseParam);cdecl;

const
  sys_func_count = 184;
  sys_func_array: array[0..sys_func_count - 1] of RLseFunc = (
    (fr_prot:'dir:string ||';
     fr_addr:@pp_system_dir;
     fr_desc:'get current directory';
    ),
    (fr_prot:'isdir:int |directory:string|';
     fr_addr:@pp_system_isdir;
     fr_desc:'test if directory exists';
    ),
    (fr_prot:'isfile:int |fileName:string|';
     fr_addr:@pp_system_isfile;
     fr_desc:'test if file exists';
    ),
    (fr_prot:'modules:varlist ||';
     fr_addr:@pp_system_modules;
     fr_desc:'return imported module list';
    ),
    (fr_prot:'libs:varlist ||';
     fr_addr:@pp_system_libs;
     fr_desc:'return loaded libraries';
    ),
    (fr_prot:'print:void |text:string|';
     fr_addr:@pp_system_print;
     fr_desc:'print text into standard output';
    ),
    (fr_prot:'println:void |text:string|';
     fr_addr:@pp_system_println;
     fr_desc:'print text and a line break into standard output';
    ),
    (fr_prot:'printf:void |fileName:string|';
     fr_addr:@pp_system_printf;
     fr_desc:'print file content into standard output';
    ),
    (fr_prot:'readln:string ||';
     fr_addr:@pp_system_readln;
     fr_desc:'read a line from standard input';
    ),
    (fr_prot:'exit:void |status:int|';
     fr_addr:@pp_system_exit;
     fr_desc:'exit with status code';
    ),
    (fr_prot:'random:int |low:int, high:int|';
     fr_addr:@pp_system_random;
     fr_desc:'generate random number';
    ),
    (fr_prot:'sleep:void |milliSeconds:int|';
     fr_addr:@pp_system_sleep;
     fr_desc:'sleep a number of milliseconds';
    ),
    (fr_prot:'getenv:string |name:string|';
     fr_addr:@pp_system_getenv;
     fr_desc:'get environment value';
    ),
    (fr_prot:'dumpc:string |any|';
     fr_addr:@pp_system_dumpc;
     fr_desc:'dump object p-codes to standard output';
    ),
    (fr_prot:'length:int |any|';
     fr_addr:@pp_system_length;
     fr_desc:'get string length or item count';
    ),
    (fr_prot:'genid:string ||';
     fr_addr:@pp_system_genid;
     fr_desc:'generate a global unique string';
    ),
    (fr_prot:'load:module |fileName:string|';
     fr_addr:@pp_system_load;
     fr_desc:'load module';
    ),
    (fr_prot:'parse:function |script:string|';
     fr_addr:@pp_system_parse;
     fr_desc:'parse and compile script';
    ),
    (fr_prot:'eval |script:string|';
     fr_addr:@pp_system_eval;
     fr_desc:'evaluate a block of lysee script';
    ),
    (fr_prot:'format:string |fmt:string, args:varlist|';
     fr_addr:@pp_system_format;
     fr_desc:'format string';
    ),
    (fr_prot:'max |v1, v2|';
     fr_addr:@pp_system_max;
     fr_desc:'get max value';
    ),
    (fr_prot:'min |v1, v2|';
     fr_addr:@pp_system_min;
     fr_desc:'get min value';
    ),
    (fr_prot:'leap:int |year:int|';
     fr_addr:@pp_system_leap;
     fr_desc:'test leap year';
    ),
    (fr_prot:'which |name:string|';
     fr_addr:@pp_system_which;
     fr_desc:'find variable, function, module or anything else by name';
    ),
    (fr_prot:'curry:function |func:function, paramList:varlist|';
     fr_addr:@pp_system_curry;
     fr_desc:'curry function with parametre list';
    ),
    (fr_prot:'curryOne:function |func:function, value|';
     fr_addr:@pp_system_curryone;
     fr_desc:'curry function with one parametre';
    ),
    (fr_prot:'gc:int ||';
     fr_addr:@pp_system_gc;
     fr_desc:'execute garbage collection immediately';
    ),
    (fr_prot:'apply |func:function, params:varlist|';
     fr_addr:@pp_system_apply;
     fr_desc:'call function with supplied parametres';
    ),
    (fr_prot:'tempFileName:string |fileExt:string|';
     fr_addr:@pp_system_tmpfname;
     fr_desc:'generate temp file name at temp path';
    ),
    (fr_prot:'encodeUTF8:string |ANSI:string|';
     fr_addr:@pp_system_encodeUTF8;
     fr_desc:'encode ANSI string to UTF8 format';
    ),
    (fr_prot:'decodeUTF8:string |UTF8:string|';
     fr_addr:@pp_system_decodeUTF8;
     fr_desc:'decode UTF8 string to ANSI format';
    ),
    (fr_prot:'openfs:stream |fileName:string, mode:string|';
     fr_addr:@pp_system_openfs;
     fr_desc:'open file stream by specified mode: CRWE (default R)';
    ),
    (fr_prot:'memory:stream |size:int|';
     fr_addr:@pp_system_memory;
     fr_desc:'create memory stream';
    ),
    (fr_prot:'incPD:string |dir:string|';
     fr_addr:@pp_system_incPD;
     fr_desc:'include trailing path delimiter';
    ),
    (fr_prot:'excPD:string |path:string|';
     fr_addr:@pp_system_excPD;
     fr_desc:'exclude trailing path delimiter';
    ),
    (fr_prot:'veryPD:string |path:string|';
     fr_addr:@pp_system_veryPD;
     fr_desc:'correct path delimiter';
    ),
    (fr_prot:'veryUD:string |RUL:string|';
     fr_addr:@pp_system_veryUD;
     fr_desc:'correct URL delimiter';
    ),
    (fr_prot:'msecs:int |func:function, params:varlist|';
     fr_addr:@pp_system_msecs;
     fr_desc:'count milliseconds used to call a function';
    ),
    (fr_prot:'__module__:module ||';
     fr_addr:@pp_system_current_module;
     fr_desc:'get current module';
    ),
    (fr_prot:'__func__:function ||';
     fr_addr:@pp_system_current_func;
     fr_desc:'get current function';
    ),
    (fr_prot:'__error__:error ||';
     fr_addr:@pp_system_current_error;
     fr_desc:'get current error';
    ),
    (fr_prot:'__args__:varlist ||';
     fr_addr:@pp_system_current_args;
     fr_desc:'get argument list';
    ),
    (fr_prot:'__prmc__:int ||';
     fr_addr:@pp_system_current_prmc;
     fr_desc:'get actual parametre count';
    ),
    (fr_prot:'__prms__:varlist ||';
     fr_addr:@pp_system_current_prms;
     fr_desc:'get parametre value list';
    ),
    (fr_prot:'__line__:int ||';
     fr_addr:@pp_system_current_line;
     fr_desc:'get current line number';
    ),
    (fr_prot:'__envs__:varlist ||';
     fr_addr:@pp_system_current_envs;
     fr_desc:'get environment value list';
    ),
    (fr_prot:'__file__:string ||';
     fr_addr:@pp_system_current_file;
     fr_desc:'get current file';
    ),
    (fr_prot:'__pd__:string ||';
     fr_addr:@pp_system_current_pd;
     fr_desc:'get system path delimiter';
    ),
    (fr_prot:'eol:string ||';
     fr_addr:@pp_system_eol;
     fr_desc:'get eol(line break)';
    ),
    (fr_prot:'each:void |any:vargen, proc:function|';
     fr_addr:@pp_system_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'map:varlist |any:vargen, proc:function|';
     fr_addr:@pp_system_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'reduce |any:vargen, initValue, proc:function|';
     fr_addr:@pp_system_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'filter:varlist |any:vargen, proc:function|';
     fr_addr:@pp_system_filter;
     fr_desc:'filter item: |item| ... end';
    ),
    (fr_prot:'sum |any:vargen, proc:function, initValue|';
     fr_addr:@pp_system_sum;
     fr_desc:'sum all: |result, item| ... end';
    ),
    (fr_prot:'__maxint__:int ||';
     fr_addr:@pp_system_maxint;
     fr_desc:'get max integer value';
    ),
    (fr_prot:'__minint__:int ||';
     fr_addr:@pp_system_minint;
     fr_desc:'get min integer value';
    ),
    (fr_prot:'abs |value|';
     fr_addr:@pp_system_abs;
     fr_desc:'get absolute value';
    ),
    (fr_prot:'find:varlist |S:string, patten:string, findAll|';
     fr_addr:@pp_system_find;
     fr_desc:'find patten matches';
    ),
    (fr_prot:'gsub:string |S:string, patten:string, newStr:string, count:int|';
     fr_addr:@pp_system_gsub;
     fr_desc:'replace patten with new string';
    ),
    (fr_prot:'split:varlist |S:string, patten:string|';
     fr_addr:@pp_system_split;
     fr_desc:'split string to varlist';
    ),
    (fr_prot:'getcs:varlist |index:int|';
     fr_addr:@pp_system_getcs;
     fr_desc:'get call stack item';
    ),
    (fr_prot:'throw:void |exceptionMsg:string, exceptionID:string|';
     fr_addr:@pp_system_throw;
     fr_desc:'throw exception';
    ),
    (fr_prot:'hex:string |value:int, size:int|';
     fr_addr:@pp_system_hex;
     fr_desc:'convert to hex string';
    ),
    (fr_prot:'bitlist:string |value:int, size:int|';
     fr_addr:@pp_system_bitlist;
     fr_desc:'convert to bit list string';
    ),
    (fr_prot:'upto:vargen |from:int, to:int, step:int|';
     fr_addr:@pp_system_upto;
     fr_desc:'create a upto range';
    ),
    (fr_prot:'downto:vargen |from:int, to:int, step:int|';
     fr_addr:@pp_system_downto;
     fr_desc:'create a downto range';
    ),
    (fr_prot:'typeof:type |any|';
     fr_addr:@pp_system_typeof;
     fr_desc:'get value type';
    ),

    { error }
    
    (fr_prot:'error_get_text:string |e:error|';
     fr_addr:@pp_error_text;
     fr_desc:'error text';
    ),
    (fr_prot:'error_get_module:string |e:error|';
     fr_addr:@pp_error_module;
     fr_desc:'error module';
    ),
    (fr_prot:'error_get_name:string |e:error|';
     fr_addr:@pp_error_name;
     fr_desc:'error name';
    ),
    (fr_prot:'error_get_message:string |e:error|';
     fr_addr:@pp_error_message;
     fr_desc:'error message';
    ),
    (fr_prot:'error_get_row:int |e:error|';
     fr_addr:@pp_error_row;
     fr_desc:'error row';
    ),
    (fr_prot:'error_get_col:int |e:error|';
     fr_addr:@pp_error_col;
     fr_desc:'error column';
    ),
    (fr_prot:'error_get_errno:int |e:error|';
     fr_addr:@pp_error_errno;
     fr_desc:'error code';
    ),

    { function }

    (fr_prot:'function_get_name:string |f:function|';
     fr_addr:@pp_func_name;
     fr_desc:'function name';
    ),
    (fr_prot:'function_get_description:string |f:function|';
     fr_addr:@pp_func_desc;
     fr_desc:'function description';
    ),
    (fr_prot:'function_get_type:string |f:function|';
     fr_addr:@pp_func_type;
     fr_desc:'get function result type';
    ),
    (fr_prot:'function_get_prototype:string |f:function|';
     fr_addr:@pp_func_prototype;
     fr_desc:'function prototype';
    ),
    (fr_prot:'function_get_module:module |f:function|';
     fr_addr:@pp_func_module;
     fr_desc:'function module';
    ),
    (fr_prot:'function_get_params:varlist |f:function|';
     fr_addr:@pp_func_params;
     fr_desc:'get parametre list';
    ),
    (fr_prot:'function_get_locals:varlist |f:function|';
     fr_addr:@pp_func_locals;
     fr_desc:'get local varible list';
    ),

    { hashed }

    (fr_prot:'hashed_create:hashed |buckets:int|';
     fr_addr:@pp_hashed_create;
     fr_desc:'create a hashed key-value list';
    ),
    (fr_prot:'hashed_clear:void |h:hashed|';
     fr_addr:@pp_hashed_clear;
     fr_desc:'clear list item';
    ),
    (fr_prot:'hashed_remove:void |h:hashed, key:string|';
     fr_addr:@pp_hashed_remove;
     fr_desc:'remove specifed key value';
    ),
    (fr_prot:'hashed_isset:int |h:hashed, key:string|';
     fr_addr:@pp_hashed_isset;
     fr_desc:'check if the key exists';
    ),
    (fr_prot:'hashed_read |h:hashed, key:string, defaultValue|';
     fr_addr:@pp_hashed_read;
     fr_desc:'read key value';
    ),
    (fr_prot:'hashed_get_keys:varlist |h:hashed|';
     fr_addr:@pp_hashed_keys;
     fr_desc:'get hashed key list';
    ),
    (fr_prot:'hashed_get_values:varlist |h:hashed|';
     fr_addr:@pp_hashed_values;
     fr_desc:'get hashed value list';
    ),

    { module }

    (fr_prot:'module_get_name:string |m:module|';
     fr_addr:@pp_module_name;
     fr_desc:'module name';
    ),
    (fr_prot:'module_get_description:string |m:module|';
     fr_addr:@pp_module_desc;
     fr_desc:'module description';
    ),
    (fr_prot:'module_get_file:string |m:module|';
     fr_addr:@pp_module_file;
     fr_desc:'module file';
    ),
    (fr_prot:'module_get_modules:varlist |m:module|';
     fr_addr:@pp_module_modules;
     fr_desc:'imported module list';
    ),
    (fr_prot:'module_get_funcs:varlist |m:module|';
     fr_addr:@pp_module_funcs;
     fr_desc:'global function list';
    ),
    (fr_prot:'module_get_classes:varlist |m:module|';
     fr_addr:@pp_module_types;
     fr_desc:'get type list';
    ),
    (fr_prot:'module_get_version:string |m:module|';
     fr_addr:@pp_module_version;
     fr_desc:'type version';
    ),
    (fr_prot:'module_imports:module |m:module, name:string|';
     fr_addr:@pp_module_imports;
     fr_desc:'import module by name';
    ),

    { vargen }

    (fr_prot:'vargen_create:vargen |any|';
     fr_addr:@pp_vargen_create;
     fr_desc:'create variant generator';
    ),
    (fr_prot:'vargen_get_eof:int |v:vargen|';
     fr_addr:@pp_vargen_eof;
     fr_desc:'test if finished';
    ),
    (fr_prot:'vargen_next |v:vargen|';
     fr_addr:@pp_vargen_next;
     fr_desc:'generate next value';
    ),
    (fr_prot:'vargen_rewind:int |v:vargen|';
     fr_addr:@pp_vargen_rewind;
     fr_desc:'restart from first element';
    ),
    (fr_prot:'vargen_each:void |v:vargen, proc:function|';
     fr_addr:@pp_system_each;
     fr_desc:'process each item: |item| ... end';
    ),
    (fr_prot:'vargen_map:varlist |v:vargen, proc:function|';
     fr_addr:@pp_system_map;
     fr_desc:'map each item: |item| ... end';
    ),
    (fr_prot:'vargen_reduce |v:vargen, initValue, proc:function|';
     fr_addr:@pp_system_reduce;
     fr_desc:'reduce each item: |result, item| ... end';
    ),
    (fr_prot:'vargen_filter:varlist |v:vargen, proc:function|';
     fr_addr:@pp_system_filter;
     fr_desc:'filter item: |item| ... end';
    ),

    { stream }

    (fr_prot:'stream_close:void |s:stream|';
     fr_addr:@pp_stream_close;
     fr_desc:'close stream';
    ),
    (fr_prot:'stream_resize:void |s:stream, length:int|';
     fr_addr:@pp_stream_set_length;
     fr_desc:'set stream size';
    ),
    (fr_prot:'stream_get_eof:int |s:stream|';
     fr_addr:@pp_stream_eof;
     fr_desc:'test if is at end of the stream';
    ),
    (fr_prot:'stream_position:string |s:stream|';
     fr_addr:@pp_stream_get_position;
     fr_desc:'get current position';
    ),
    (fr_prot:'stream_seekTo:void |s:stream, newPosition:int|';
     fr_addr:@pp_stream_set_position;
     fr_desc:'set current position';
    ),
    (fr_prot:'stream_read:string |s:stream, count:int|';
     fr_addr:@pp_stream_read;
     fr_desc:'read value';
    ),
    (fr_prot:'stream_readln:string |s:stream|';
     fr_addr:@pp_stream_readln;
     fr_desc:'read a line';
    ),
    (fr_prot:'stream_write:int |s:stream, text:string|';
     fr_addr:@pp_stream_write;
     fr_desc:'write into stream';
    ),
    (fr_prot:'stream_writeln:int |s:stream, text:string|';
     fr_addr:@pp_stream_writeln;
     fr_desc:'write text and a line break into stream';
    ),
    (fr_prot:'stream_writeTo:int |s:stream, stream:stream, count:int|';
     fr_addr:@pp_stream_writeTo;
     fr_desc:'write part data into another stream';
    ),
    (fr_prot:'stream_flush:void |s:stream|';
     fr_addr:@pp_stream_flush;
     fr_desc:'flush stream';
    ),
    (fr_prot:'stream_get_lines:vargen |s:stream|';
     fr_addr:@pp_stream_lines;
     fr_desc:'wrap stram as a line generator';
    ),

    { string }

    (fr_prot:'string_setAt:string |s:string, index:int, value:string|';
     fr_addr:@pp_string_setAt;
     fr_desc:'set char by index';
    ),
    (fr_prot:'string_get_name:string |s:string|';
     fr_addr:@pp_string_name;
     fr_desc:'extract name';
    ),
    (fr_prot:'string_get_value:string |s:string|';
     fr_addr:@pp_string_value;
     fr_desc:'extract value';
    ),
    (fr_prot:'string_compare:int |s:string, S2:string, ignoreCase|';
     fr_addr:@pp_string_compare;
     fr_desc:'compare two string';
     ),
    (fr_prot:'string_replace:string |s:string, patten:string, newStr:string, ignoreCase, replaceFirstOnly|';
     fr_addr:@pp_string_replace;
     fr_desc:'replace patten to new string';
    ),
    (fr_prot:'string_pos:int |s:string, SubStr:string, IgnoreCase|';
     fr_addr:@pp_string_pos;
     fr_desc:'get first sub-string position';
    ),
    (fr_prot:'string_lastPos:int |s:string, SubStr:string, IgnoreCase|';
     fr_addr:@pp_string_lastPos;
     fr_desc:'get last sub-string position';
    ),
    (fr_prot:'string_left:string |s:string, count:int|';
     fr_addr:@pp_string_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'string_right:string |s:string, count:int|';
     fr_addr:@pp_string_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'string_trim:string |s:string|';
     fr_addr:@pp_string_trim;
     fr_desc:'trim left and right';
    ),
    (fr_prot:'string_trimLeft:string |s:string|';
     fr_addr:@pp_string_trimLeft;
     fr_desc:'trim left';
    ),
    (fr_prot:'string_trimRight:string |s:string|';
     fr_addr:@pp_string_trimRight;
     fr_desc:'trim right';
    ),
    (fr_prot:'string_trimAll:string |s:string|';
     fr_addr:@pp_string_trimAll;
     fr_desc:'trim all spaces';
    ),
    (fr_prot:'string_copy:string |s:string, index:int, count:int|';
     fr_addr:@pp_string_copy;
     fr_desc:'copy sub-string';
    ),
    (fr_prot:'string_delete:string |s:string, index:int, count:int|';
     fr_addr:@pp_string_delete;
     fr_desc:'delete by range';
    ),
    (fr_prot:'string_insert:string |s:string, substr:string, index:int|';
     fr_addr:@pp_string_insert;
     fr_desc:'insert sub-string';
    ),
    (fr_prot:'string_extractName:string |s:string, separator:string|';
     fr_addr:@pp_string_extractName;
     fr_desc:'extract name with specified separator';
    ),
    (fr_prot:'string_extractValue:string |s:string, separator:string|';
     fr_addr:@pp_string_extractValue;
     fr_desc:'extract value with specified separator';
    ),
    (fr_prot:'string_lformat:string |s:string, width:int, filler:int|';
     fr_addr:@pp_string_lformat;
     fr_desc:'format to left';
    ),
    (fr_prot:'string_rformat:string |s:string, width:int, filler:int|';
     fr_addr:@pp_string_rformat;
     fr_desc:'format to right';
    ),
    (fr_prot:'string_center:string |s:string, width:int, filler:int|';
     fr_addr:@pp_string_center;
     fr_desc:'format to center';
    ),
    (fr_prot:'string_html:string |s:string, translateMBC|';
     fr_addr:@pp_string_html;
     fr_desc:'encode to HTML code';
    ),
    (fr_prot:'string_random:string |s:string|';
     fr_addr:@pp_string_randomOrder;
     fr_desc:'randomize string charactors';
    ),
    (fr_prot:'string_lower:string |s:string|';
     fr_addr:@pp_string_lower;
     fr_desc:'convert to lower case string';
    ),
    (fr_prot:'string_upper:string |s:string|';
     fr_addr:@pp_string_upper;
     fr_desc:'convert to upper case string';
    ),
    (fr_prot:'string_isAlpha:int |s:string|';
     fr_addr:@pp_string_isAlpha;
     fr_desc:'test if the string contains only alpha charactors';
    ),
    (fr_prot:'string_isAlnum:int |s:string|';
     fr_addr:@pp_string_isAlnum;
     fr_desc:'test if the string contains only alpha and digit charactors';
    ),
    (fr_prot:'string_isCntrl:int |s:string|';
     fr_addr:@pp_string_isCntrl;
     fr_desc:'test if the string contains only control charactors';
    ),
    (fr_prot:'string_isSpace:int |s:string|';
     fr_addr:@pp_string_isSpace;
     fr_desc:'test if the string contains only space charactors';
    ),
    (fr_prot:'string_isDigit:int |s:string|';
     fr_addr:@pp_string_isDigit;
     fr_desc:'test if the string contains only digit charactors';
    ),
    (fr_prot:'string_isHex:int |s:string|';
     fr_addr:@pp_string_isHex;
     fr_desc:'test if the string contains only HEX charactors';
    ),
    (fr_prot:'string_fileText:string |s:string|';
     fr_addr:@pp_string_fileText;
     fr_desc:'get file text';
    ),
    (fr_prot:'string_saveToFile:void |s:string, fileName:string|';
     fr_addr:@pp_string_saveToFile;
     fr_desc:'save string to file';
    ),
    (fr_prot:'string_fullFileName:string |s:string|';
     fr_addr:@pp_string_fullFileName;
     fr_desc:'expand to full file name';
    ),
    (fr_prot:'string_filePath:string |s:string|';
     fr_addr:@pp_string_filePath;
     fr_desc:'extract file path';
    ),
    (fr_prot:'string_fileName:string |s:string|';
     fr_addr:@pp_string_fileName;
     fr_desc:'extract file name';
    ),
    (fr_prot:'string_fileExt:string |s:string|';
     fr_addr:@pp_string_fileExt;
     fr_desc:'extract file extension';
    ),
    (fr_prot:'string_changeExt:string |s:string, newFileExt:string|';
     fr_addr:@pp_string_changeExt;
     fr_desc:'change file extension';
    ),
    (fr_prot:'string_md5sum:string |s:string, sumFile|';
     fr_addr:@pp_string_md5sum;
     fr_desc:'calculate MD5 sum';
    ),
    (fr_prot:'string_hexToInt:int |s:string, defaultValue:int|';
     fr_addr:@pp_string_hexToInt;
     fr_desc:'convert HEX string to integer value';
    ),
    (fr_prot:'string_reverse:string |s:string|';
     fr_addr:@pp_string_reverse;
     fr_desc:'reverse string charactors';
    ),
    (fr_prot:'string_isLower:int |s:string|';
     fr_addr:@pp_string_isLower;
     fr_desc:'test if is lower case string';
    ),
    (fr_prot:'string_isUpper:int |s:string|';
     fr_addr:@pp_string_isUpper;
     fr_desc:'test if is upper case string';
    ),
    (fr_prot:'string_translate:string |s:string, OrdCharList:string, newCharList:string|';
     fr_addr:@pp_string_translate;
     fr_desc:'translate original char to new char';
    ),

    { type }

    (fr_prot:'type_get_name:string |t:type|';
     fr_addr:@pp_type_name;
     fr_desc:'type name';
    ),
    (fr_prot:'type_get_module:module |t:type|';
     fr_addr:@pp_type_module;
     fr_desc:'owner module';
    ),
    (fr_prot:'type_get_description:string |t:type|';
     fr_addr:@pp_type_description;
     fr_desc:'type description';
    ),

    { variable }

    (fr_prot:'variable_get_name:string |v:variable|';
     fr_addr:@pp_varb_name;
     fr_desc:'variable name';
    ),
    (fr_prot:'variable_get_type:type |v:variable|';
     fr_addr:@pp_varb_type;
     fr_desc:'variable type';
    ),

    { varlist }

    (fr_prot:'varlist_create:varlist |count:int|';
     fr_addr:@pp_varlist_create;
     fr_desc:'create variant list';
    ),
    (fr_prot:'varlist_get_length:int |l:varlist|';
     fr_addr:@pp_varlist_get_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_set_length:void |l:varlist, count:int|';
     fr_addr:@pp_varlist_set_length;
     fr_desc:'set list size';
    ),
    (fr_prot:'varlist_exchange:void |l:varlist, X1:int, X2:int|';
     fr_addr:@pp_varlist_exchange;
     fr_desc:'exchange by index';
    ),
    (fr_prot:'varlist_move:void |l:varlist, curIndex:int, newIndex:int|';
     fr_addr:@pp_varlist_move;
     fr_desc:'move variant to new position';
    ),
    (fr_prot:'varlist_add:int |l:varlist, value|';
     fr_addr:@pp_varlist_add;
     fr_desc:'add variant';
    ),
    (fr_prot:'varlist_addFrom:int |l:varlist, variants:varlist|';
     fr_addr:@pp_varlist_addFrom;
     fr_desc:'add variants from';
    ),
    (fr_prot:'varlist_fill:void |l:varlist, source:vargen, clearBeforeFill|';
     fr_addr:@pp_varlist_fill;
     fr_desc:'fill generated value';
    ),
    (fr_prot:'varlist_insert:void |l:varlist, index:int, value|';
     fr_addr:@pp_varlist_insert;
     fr_desc:'insert variant at specified position';
    ),
    (fr_prot:'varlist_delete:void |l:varlist, index:int|';
     fr_addr:@pp_varlist_delete;
     fr_desc:'delete variant by index';
    ),
    (fr_prot:'varlist_clear:void |l:varlist|';
     fr_addr:@pp_varlist_clear;
     fr_desc:'clear variant list';
    ),
    (fr_prot:'varlist_copy:varlist |l:varlist, index:int, count:int|';
     fr_addr:@pp_varlist_copy;
     fr_desc:'copy to another variant list';
    ),
    (fr_prot:'varlist_left:varlist |l:varlist, count:int|';
     fr_addr:@pp_varlist_left;
     fr_desc:'copy left';
    ),
    (fr_prot:'varlist_right:varlist |l:varlist, count:int|';
     fr_addr:@pp_varlist_right;
     fr_desc:'copy right';
    ),
    (fr_prot:'varlist_filter:varlist |l:varlist, filterFunc:function|';
     fr_addr:@pp_varlist_filter;
     fr_desc:'filter variant list';
    ),
    (fr_prot:'varlist_get_first |l:varlist|';
     fr_addr:@pp_varlist_first;
     fr_desc:'get first variant item';
    ),
    (fr_prot:'varlist_get_last |l:varlist|';
     fr_addr:@pp_varlist_last;
     fr_desc:'get last variant item';
    ),
    (fr_prot:'varlist_shift |l:varlist|';
     fr_addr:@pp_varlist_shift;
     fr_desc:'get and delete first item';
    ),
    (fr_prot:'varlist_pop |l:varlist|';
     fr_addr:@pp_varlist_pop;
     fr_desc:'get and delete last item';
    )
  );

  sys_module_funcs: RLseFuncListRec = (
    fl_count: sys_func_count;
    fl_entry:@sys_func_array;
  );

{ OTOS: object to string }

function otos_error(inst: pointer): PLseString;cdecl;
function otos_function(inst: pointer): PLseString;cdecl;
function otos_module(inst: pointer): PLseString;cdecl;
function otos_type(inst: pointer): PLseString;cdecl;
function otos_variable(inst: pointer): PLseString;cdecl;
function otos_varlist(inst: pointer): PLseString;cdecl;

{ STOO: string to object}

function stoo_module(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_type(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_variable(S: PLseString; KernelEngine: pointer): pointer;cdecl;
function stoo_varlist(S: PLseString; KernelEngine: pointer): pointer;cdecl;

{ WTOS: write to stream }

function wtos_stream(inst: pointer; stream: PLseStream): integer;cdecl;
                             
{ CVGR: create RLseVargen record}

function cvgr_varlist(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_func(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_stream(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_stream_lines(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_string(obj, kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_upto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;
function cvgr_downto(begv, endv, step: int64; kernel_engine: pointer): PLseVargen;cdecl;

{ ADDI: add item }

function addi_varlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function addi_stream(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
function addi_function(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;

{ GETIV: get item value }

function getiv_string(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function getiv_varlist(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function getiv_varsnap(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;

{ SETIV: set item value }

function setiv_varlist(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
function setiv_varsnap(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;

{ GETPV: get property value }

function getpv_hashed(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function getpv_module(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function getpv_varsnap(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;

{ SETPV: set property value }

function setpv_hashed(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function setpv_module(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
function setpv_varsnap(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;

{ LENGTH: length object }

function length_string(obj: pointer): integer;cdecl;
function length_hashed(obj: pointer): integer;cdecl;
function length_stream(obj: pointer): integer;cdecl;
function length_varlist(obj: pointer): integer;cdecl;
function length_varsnap(obj: pointer): integer;cdecl;

{ sys-curry }

function curry_func(func: KLiFunc; params: KLiVarList; module: KLiModule): KLiFunc;
function curry_one(func: KLiFunc; Value: PLseValue; module: KLiModule): KLiFunc;

implementation

uses
  Math, StrUtils, DateUtils, lse_symbol, lse_patten;

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

function otos_type(inst: pointer): PLseString;cdecl;
var
  clss: KLiType;
begin
  clss := KLiType(inst);
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

function stoo_type(S: PLseString; KernelEngine: pointer): pointer;cdecl;
var
  o_name, m_name, T: string;
  module: KLiModule;
begin
  module := KLiEngine(KernelEngine).MainRunner.CurrentFunc.Module;
  T := lse_strec_string(S);
  o_name := __decodeTypeName(T, m_name);
  if (m_name <> '') or (System.Pos('::', T) > 0) then
  begin
    if m_name <> '' then
      module := module.FindModule(m_name, false); 
    Result := module.FindType(o_name);
  end
  else Result := module.FindTypeBy(o_name, '');
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
      list.Add(strs[index]);
  finally
    strs.Free;
  end;
  Result := list;
end;

{ WTOS: write to stream }

function wtos_stream(inst: pointer; stream: PLseStream): integer;cdecl;
begin
  if (inst <> nil) and (stream <> nil) and (inst <> stream) then
    Result := lse_stream_fill(stream, PLseStream(inst)) else
    Result := 0;
end;

{ CVGR: create RLseVargen record}

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
    stream^.s_addref(stream);
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
    stream^.s_release(stream);
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
      lse_set_string(value, lse_strec_data(srec)[index]);
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

{ ADDI}

function addi_varlist(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
begin
  Result := KLiVarList(obj).Count;
  KLiVarList(obj).Add(Value);
end;

function addi_stream(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  S: PLseStream;
begin
  S := PLseStream(obj);
  if lse_vtype(Value) = LSV_STRING then
    Result := lse_stream_write(S, Value^.VObject) else
    Result := lse_stream_write(S, __AsString(Value));
end;

function addi_function(obj: pointer; Value: PLseValue; Engine: pointer): integer;cdecl;
var
  F: KLiFunc;
  R: KLiRunner;
begin
  F := KLiFunc(obj);
  R := KLiEngine(Engine).MainRunner;
  R.Stack.Add(Value);
  Result := Ord(R.Goon(F, 1, nil));
end;

{ GETIV: get item value }

function getiv_string(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  S: PLseString;
  L, X: int64;
begin
  S := PLseString(obj);
  L := lse_strec_length(S);
  X := lse_vary_index(index, L);
  if (X >= 0) and (X < L) then
  begin
    lse_set_string(value, lse_strec_data(S)[X]);
    Result := 1;
  end
  else Result := 0;
end;

function getiv_varlist(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  L: KLiVarList;
begin
  Result := 0;
  if obj <> nil then
  begin
    L := KLiVarList(obj);
    index := lse_vary_index(index, L.Count);
    if (index >= 0) and (index < L.Count) then
    begin
      lse_set_value(value, L[index]);
      Result := 1;
    end;
  end;
end;

function getiv_varsnap(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  snap: KLiVarSnap;
  varb: KLiVarb;
  data: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    snap := KLiVarSnap(obj);
    data := snap.GetByIndex(index, varb);
    if data <> nil then
    begin
      lse_set_value(value, data);
      Result := 1;
    end;
  end;
end;

{ SETIV: set item value }

function setiv_varlist(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  L: KLiVarList;
begin
  Result := 0;
  if obj <> nil then
  begin
    L := KLiVarList(obj);
    index := lse_vary_index(index, L.Count);
    if (index >= 0) and (index < L.Count) then
    begin
      lse_set_value(L[index], value);
      Result := 1;
    end;
  end;
end;

function setiv_varsnap(obj: pointer; index: integer; value: PLseValue; engine: pointer): integer;cdecl;
var
  snap: KLiVarSnap;
  varb: KLiVarb;
  data: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    snap := KLiVarSnap(obj);
    data := snap.GetByIndex(index, varb);
    if data <> nil then
    begin
      lse_set_value(data, value);
      __SetTypeValue(KLiEngine(engine), data, varb.ValueType);
      Result := 1;
    end;
  end;
end;

{ GETPV: get property value }

function getpv_hashed(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  V: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    V := KLiHashed(obj).FindValue(name);
    if V <> nil then
    begin
      lse_set_value(value, V);
      Result := 1;
    end;
  end;
end;

function getpv_module(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  this: KLiModule;
  func: KLiFunc;
  clss: KLiType;
  engi: KLiEngine;
begin
  Result := 0;
  if obj <> nil then
  begin
    this := KLiModule(obj);
    engi := KLiEngine(engine);

    if this = sys_module then
    begin
      Result := 1;
      if name = 'input' then
        lse_set_stream(value, engi.Input) else
      if name = 'output' then
        lse_set_stream(value, engi.Output) else
      if name = 'errput' then
        lse_set_stream(value, engi.Errput) else       
      if name = 'current' then
        lse_set_object(value, KR_VARSNAP, engi.MainRunner.Current^.values) else
        Result := 0;
      if Result > 0 then Exit;
    end;

    clss := this.FindType(name);
    if clss <> nil then
    begin
      Result := 1;
      clss.SaveTo(value);
      Exit;
    end;

    func := this.FindFunc(name);
    if func <> nil then
    begin
      Result := 1;
      if func.IsNameCall then
        engi.MainRunner.Goon(func, 0, value) else
        func.SaveTo(value);
      Exit;
    end;
  end;
end;

function getpv_varsnap(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  snap: KLiVarSnap;
  varb: KLiVarb;
  data: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    snap := KLiVarSnap(obj);
    data := snap.GetByName(name, varb);
    if data <> nil then
    begin
      lse_set_value(value, data);
      Result := 1;
    end;
  end;
end;

{ SETPV: set property value }

function setpv_hashed(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
begin
  Result := 0;
  if obj <> nil then
  begin
    KLiHashed(obj).SetValue(name, value);
    Result := 1;
  end;
end;

function setpv_module(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  this: KLiModule;
  engi: KLiEngine;

  function get_stream: PLseStream;
  begin
    if __AsType(value) = KT_STREAM then
      Result := PLseStream(value^.VObject) else
      Result := nil;
  end;
  
begin
  Result := 0;
  if obj <> nil then
  begin
    this := KLiModule(obj);
    if this = sys_module then
    begin
      Result := 1;
      engi := KLiEngine(engine);
      if name = 'input' then
        engi.Input := get_stream else
      if name = 'output' then
        engi.Output := get_stream else
      if name = 'errput' then
        engi.Errput := get_stream else
        Result := 0;
    end;
  end;
end;

function setpv_varsnap(obj: pointer; const name: pchar; value: PLseValue; engine: pointer): integer;cdecl;
var
  snap: KLiVarSnap;
  varb: KLiVarb;
  data: PLseValue;
begin
  Result := 0;
  if obj <> nil then
  begin
    snap := KLiVarSnap(obj);
    data := snap.GetByName(name, varb);
    if data <> nil then
    begin
      lse_set_value(data, value);
      __SetTypeValue(snap.Engine, data, varb.ValueType);
      Result := 1;
    end;
  end;
end;

{ LENO: length object }

function length_string(obj: pointer): integer;cdecl;
begin
  Result := lse_strec_length(PLseString(obj));
end;

function length_hashed(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiHashed(obj).ItemCount else
    Result := 0;
end;

function length_stream(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := PLseStream(obj)^.s_get_size(obj) else
    Result := 0;
end;

function length_varlist(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiVarList(obj).Count else
    Result := 0;
end;

function length_varsnap(obj: pointer): integer;cdecl;
begin
  if obj <> nil then
    Result := KLiVarSnap(obj).Count else
    Result := 0;
end;

{ sys-curry }

function curry_func(func: KLiFunc; params: KLiVarList; module: KLiModule): KLiFunc;
var
  this, curry: KLiFunc_curry;
  base, index: integer;
  varb: KLiVarb;
begin
  Result := func;
  
  if (func.Params.Count = 0) or func.IsEmptyFunc or
     (params = nil) or
     (Params.Count = 0) then Exit;

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

  for index := base to func.Params.Count - 1 do
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

  if (func.Params.Count = 0) or func.IsEmptyFunc then Exit;

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

  for index := base + 1 to func.Params.Count - 1 do
  begin
    varb := func.Params[index];
    curry.AddParam(varb.Name, varb.ValueType);
  end;

  Result := curry;
end;

{ sys }

procedure pp_system_dir(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, GetCurrentDir);
end;

procedure pp_system_isdir(const Param: PLseParam);cdecl;
var
  dir: string;
begin
  dir := __AsFileName(Param^.p_param[0]);
  lse_set_bool(Param^.p_result, DirectoryExists(dir));
end;

procedure pp_system_isfile(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := __AsFileName(Param^.p_param[0]);
  lse_set_bool(Param^.p_result, __IsFile(fname));
end;

procedure pp_system_modules(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := __AsEngine(Param);
  eng.Modules.ToVarlist(eng).SaveTo(Param^.p_result);
end;

procedure pp_system_libs(const Param: PLseParam);cdecl;
var
  list: KLiVarList;
  index: integer;
begin
  list := __NewVarlist(__AsEngine(Param));
  list.SaveTo(Param^.p_result);
  for index := 0 to sys_libraries.Count - 1 do
    list.Add(sys_libraries.Objects[index], KR_MODULE);
end;

procedure pp_system_exit(const Param: PLseParam);cdecl;
var
  eng: KLiEngine;
begin
  eng := __AsEngine(Param);
  lock_engine(eng);
  try
    eng.SetResult(Param^.p_param[0]);
    eng.Exited := true;
    eng.Error.Clear;
    eng.Terminate;
  finally
    unlock_engine(eng);
  end;
end;

procedure pp_system_print(const Param: PLseParam);cdecl;
begin
  lse_stream_write(__AsEngine(Param).Output, Param^.p_param[0]^.VObject);
end;

// void sys::printf(string fileName)
procedure pp_system_printf(const Param: PLseParam);cdecl;
var
  inf: TFileStream;
  buf: array[0..1023] of char;
  len: integer;
  eng: KLiEngine;
begin
  if Param^.p_count > 0 then
  begin
    inf := TFileStream.Create(__AsFileName(Param^.p_param[0]), fmShareDenyWrite);
    try
      eng := __AsEngine(Param);
      len := inf.Read(buf, sizeof(buf));
      while len > 0 do
      begin
        lse_stream_write(eng.Output, buf, len);
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
  stdout := __AsEngine(Param).Output;
  lse_stream_write(stdout, Param^.p_param[0]^.VObject);
  lse_stream_writeln(stdout);
end;

procedure pp_system_readln(const Param: PLseParam);cdecl;
var
  E: KLiEngine;
  S: PLseString;
begin
  E := __AsEngine(Param); 
  S := lse_stream_readln(E.Input);
  lse_set_string(Param^.p_result, S);
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
  F := __AsInt64(Param^.p_param[0]);
  T := __AsInt64(Param^.p_param[1]);
  if F <> T then
    F := Random(Abs(F - T)) + Min(F, T) else
  if F = 0 then
    F := Random(MaxInt);
  lse_set_int64(Param^.p_result, F);
end;

procedure pp_system_sleep(const Param: PLseParam);cdecl;
var
  timeout: integer;
begin
  timeout := __AsInt64(Param^.p_param[0]);
  if timeout > 0 then
    Sleep(timeout);
end;

procedure pp_system_getenv(const Param: PLseParam);cdecl;
var
  ID: string;
begin
  ID := __AsString(Param^.p_param[0]);
  if ID <> '' then
    lse_set_string(Param^.p_result, lse_getenv(ID)) else
    lse_set_string(Param^.p_result, '', 0);
end;

procedure pp_system_dumpc(const Param: PLseParam);cdecl;
var
  stream: TStringStream;
  clss: KLiType;
  func: KLiFunc;
  list: TStrings;
  module: KLiModule;
begin
  if Param^.p_count = 0 then
  begin
    stream := TStringStream.Create('');
    try
      __AsEngine(Param).DumpCodeToStream(stream, '');
      lse_set_string(Param^.p_result, stream.DataString);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    clss := __AsType(Param^.p_param[0]);
    if clss = KT_FUNC then
    begin
      func := KLiFunc(__AsObject(Param^.p_param[0]));
      if func <> nil then
      begin
        list := TStringList.Create;
        try
          func.DumpCode(list, '');
          lse_set_string(Param^.p_result, list.Text);
        finally
          list.Free;
        end;
      end
    end
    else
    if clss = KT_MODULE then
    begin
      module := KLiModule(__AsObject(Param^.p_param[0]));
      if module <> nil then
      begin
        stream := TStringStream.Create('');
        try
          module.DumpCodeToStream(stream, '');
          lse_set_string(Param^.p_result, stream.DataString);
        finally
          stream.Free;
        end;
      end;
    end;
  end;
end;

procedure pp_system_length(const Param: PLseParam);cdecl;
begin
  lse_set_integer(Param^.p_result, lse_length(Param^.p_param[0]));
end;

procedure pp_system_genid(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, __genid);
end;

procedure pp_system_load(const Param: PLseParam);cdecl;
var
  engine: KLiEngine;
  runner: KLiRunner;
  module: KLiModule;
  f_name: string;
  m_name: string;
  is_lib, is_str: boolean;
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

  m_name := __AsFileName(Param^.p_param[0]);
  is_str := not __inCharSet(pchar(m_name), IDChar);
  if is_str then
  begin
    f_name := __ExpandValue(m_name, engine);
    f_name := __fullFileName(f_name, ExtractFilePath(runner.CurrentModule.FileName));
    if not FileExists(f_name) then
      lse_error('module file "%s" not exists', [m_name]);
    m_name := ExtractFileExt(f_name);
    is_lib := AnsiSameText(m_name, LSE_DLLEXT);
    m_name := ChangeFileExt(ExtractFileName(f_name), '');
  end;

  module := engine.Modules.Find(m_name);
  if module = nil then
    module := KLiModule(__findNamed(sys_libraries, m_name));

  if module <> nil then
    if not is_str or __sameFileName(f_name, module.FileName) then
    begin
      lse_set_object(Param^.p_result, KR_MODULE, module);
      Exit;
    end
    else lse_error('reload module %s from another file', [m_name]);

  if not is_str then
  begin
    f_name := m_name;
    if not __searchModule(f_name, engine.GetSearchPath, is_lib) then
      lse_error('module %s not found', [m_name]);
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
        KLiParser.Create(module).ParseAndFree(__fileText(f_name));
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

  if module.MainFunc <> nil then
    runner.Goon(module.MainFunc, 0, nil);
  lse_set_object(Param^.p_result, KR_MODULE, module);
end;

procedure pp_system_parse(const Param: PLseParam);cdecl;
var
  code: string;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 0 then
  begin
    rnnr := __AsRunner(Param);
    try
      code := __AsString(Param^.p_param[0]);
      rnnr.Engine.DoCompile(code).SaveTo(Param^.p_result);
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
  rnnr: KLiRunner;
begin
  if Param^.p_count > 0 then
  begin
    code := TrimRight(__AsString(Param^.p_param[0]));
    if code <> '' then
    begin
      code := code + LB + ';';
      rnnr := __AsRunner(Param);
      rnnr.Eval(code, Param^.p_result);
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
  frmt := __AsString(Param^.p_param[0]);
  args := KLiVarList(__AsObject(Param^.p_param[1]));
  lse_set_string(Param^.p_result, __AsRunner(Param).FormatFor(frmt, args));
end;

procedure pp_system_now(const Param: PLseParam);cdecl;
begin
//lse_set_time(Param^.result, Now);
end;

procedure pp_system_max(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or (__compare(V1, V2) in [crEqual, crMore]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure pp_system_min(const Param: PLseParam);cdecl;
var
  v1, v2: PLseValue;
begin
  if Param^.p_count > 0 then
  begin
    v1 := Param^.p_param[0];
    v2 := Param^.p_param[1];
    if (Param^.p_count = 1) or (__compare(V1, V2) in [crEqual, crLess]) then
      lse_set_value(Param^.p_result, v1) else
      lse_set_value(Param^.p_result, v2);
  end;
end;

procedure pp_system_leap(const Param: PLseParam);cdecl;
begin
  lse_set_bool(Param^.p_result, IsLeapYear(__AsInt64(Param^.p_param[0])));
end;

procedure pp_system_which(const Param: PLseParam);cdecl;
var
  v_name: string;
  rec: KLiFindRec;
begin
  v_name := Trim(__AsString(Param^.p_param[0]));
  if __AsRunner(Param).CurrentFunc.FindBy(v_name, @rec) then
    case rec.fo_type of
      foVarb  : rec.VVarb.SaveTo(Param^.p_result);
      foFunc  : rec.VFunc.SaveTo(Param^.p_result);
      foType  : rec.VType.SaveTo(Param^.p_result);
      foModule: rec.VModule.SaveTo(Param^.p_result)
    end;
end;

procedure pp_system_curry(const Param: PLseParam);cdecl;
var
  func, curr: KLiFunc;
  list: KLiVarList;
begin
  func := __AsFunc(Param^.p_param[0]);
  if (func <> nil) and (func.Params.Count > 0) then
  begin
    list := __AsVarlist(Param^.p_param[1]);
    if (list <> nil) and (list.Count > 0) then
    begin
      curr := curry_func(func, list, __AsRunner(Param).CurrentModule);
      curr.SaveTo(Param^.p_result);
      if curr <> func then
        curr.DecRefcount; // adjust refcount
    end
    else func.SaveTo(Param^.p_result);
  end
  else func.SaveTo(Param^.p_result);
end;

procedure pp_system_curryone(const Param: PLseParam);cdecl;
var
  func, curr: KLiFunc;
begin
  func := __AsFunc(Param^.p_param[0]);
  if (func <> nil) and (func.Params.Count > 0) and (Param^.p_count > 1) then
  begin
    curr := curry_one(func, Param^.p_param[1], __AsRunner(Param).CurrentModule); 
    curr.SaveTo(Param^.p_result);
    if curr <> func then
      curr.DecRefcount; // adjust refcount
  end
  else func.SaveTo(Param^.p_result);
end;

procedure pp_system_gc(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, __AsEngine(Param).GarbageCollect);
end;

procedure pp_system_apply(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
begin
  func := __AsFunc(Param^.p_param[0]);
  if func <> nil then
  begin
    rnnr := __AsRunner(Param);
    list := __AsVarlist(Param^.p_param[1]);
    if list <> nil then
      prms := Min(func.Params.Count, list.Count) else
      prms := 0;
    rnnr.Stack.AddFrom(list, prms);
    rnnr.Goon(func, prms, Param^.p_result);
  end;
end;

procedure pp_system_tmpfname(const Param: PLseParam);cdecl;
var
  fname: string;
begin
  fname := sys_tmpath + __genid + __AsFileName(Param^.p_param[0]);
  lse_set_string(Param^.p_result, fname);
end;

procedure pp_system_encodeGMT(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_encode_GMT(__AsTime(Param^.p_param[0])));
end;

procedure pp_system_decodeGMT(const Param: PLseParam);cdecl;
begin
//lse_set_time(Param^.result, lse_decode_GMT(__AsString(Param^.param[0])));
end;

procedure pp_system_encodeUTF8(const Param: PLseParam);cdecl;
begin
//if Param^.count > 0 then
//  lse_set_string(Param^.result, __encodeUTF8(__AsString(Param^.param[0])));
end;

procedure pp_system_decodeUTF8(const Param: PLseParam);cdecl;
begin
  if Param^.p_count > 0 then
    lse_set_string(Param^.p_result, __decodeUTF8(__AsString(Param^.p_param[0])));
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
  fname := __AsFileName(Param^.p_param[0]);
  fmode := __AsString(Param^.p_param[1]);
  if fmode = '' then fmode := 'r';
  if __strToFileMode(fmode, open_mode, read, write) then
  begin
    stream := lse_file_stream(fname, open_mode);
    lse_set_stream(Param^.p_result, stream);
  end
  else __SetError(Param, EOPENMODE, [fmode]);
end;

procedure pp_system_memory(const Param: PLseParam);cdecl;
var
  stream: PLseStream;
begin
  stream := lse_memory_stream;
  lse_set_stream(Param^.p_result, stream);
  if Param^.p_count > 1 then
    lse_stream_resize(stream, Max(0, Param^.p_param[0]^.VInteger));
end;

procedure pp_system_incPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, IncludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_excPD(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, ExcludeTrailingPathDelimiter(Trim(lse_strec_data(this))));
end;

procedure pp_system_veryPD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryPD(__AsString(Param^.p_param[0])));
end;

procedure pp_system_veryUD(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, lse_veryUD(__AsString(Param^.p_param[0])));
end;

procedure pp_system_msecs(const Param: PLseParam);cdecl;
var
  func: KLiFunc;
  list: KLiVarList;
  rnnr: KLiRunner;
  prms: integer;
  beg_time: TDateTime;
begin
  func := __AsFunc(Param^.p_param[0]);
  if func <> nil then
  begin
    beg_time := Now;
    rnnr := __AsRunner(Param);
    list := KLiVarList(__AsObject(Param^.p_param[1]));
    if list <> nil then
      prms := Min(func.Params.Count, list.Count) else
      prms := 0;
    rnnr.Stack.AddFrom(list, prms);
    rnnr.Goon(func, prms, Param^.p_result);
    lse_set_int64(Param^.p_result, MilliSecondsBetween(Now, beg_time));
  end
  else lse_set_int64(Param^.p_result, 0);
end;

procedure pp_system_current_module(const Param: PLseParam);cdecl;
begin
  __AsRunner(Param).CurrentFunc.Module.SaveTo(Param^.p_result);
end;

procedure pp_system_current_func(const Param: PLseParam);cdecl;
var
  rnnr: KLiRunner;
begin
  rnnr := __AsRunner(Param);
  rnnr.Current^.func.SaveTo(Param^.p_result);
end;

procedure pp_system_current_error(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.p_result, KR_ERROR, __AsEngine(Param).Error);
end;

procedure pp_system_current_args(const Param: PLseParam);cdecl;
var
  L: KLiVarList;
  E: KLiEngine;
begin
  E := __AsEngine(Param);
  L := KLiVarList.Create(E);
  L.SaveTo(Param^.p_result);
  L.AddStrings(E.Arguments);
end;

procedure pp_system_current_prmc(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, __AsRunner(Param).Current^.values.ActualParamCount);
end;

procedure pp_system_current_prms(const Param: PLseParam);cdecl;
var
  list: KLiVarList;
begin
  list := __NewVarlist(__AsEngine(Param));
  list.SaveTo(Param^.p_result);
  __AsRunner(Param).Current^.values.GetParamValues(list);
end;

procedure pp_system_current_line(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, int64(__AsRunner(Param).Exprrec^.Pos.row) + 1);
end;

procedure pp_system_current_envs(const Param: PLseParam);cdecl;
var
  index, count: integer;
  list: KLiVarList;
begin
  list := KLiVarList.Create(__AsEngine(Param));
  list.SaveTo(Param^.p_result);
  count := lse_getenv_count;
  for index := 0 to count - 1 do
    list.Add(lse_getenv_string(index));
end;

procedure pp_system_current_file(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, __AsRunner(Param).CurrentFunc.Module.FileName);
end;

procedure pp_system_current_pd(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, LSE_PATH_DELIMITER);
end;

procedure pp_system_eol(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result, sys_LB);
end;

procedure pp_system_each(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
  func: KLiFunc;
  rnnr: KLiRunner;
begin
  if Param^.p_count > 1 then
  begin
    func := KLiFunc(Param^.p_param[1]^.VObject);
    if func <> nil then
    begin
      rnnr := __AsRunner(Param);
      this := lse_vargen_this(Param);
      while rnnr.Stack.AddSend(this) do
        rnnr.Goon(func, 1, Param^.p_result);
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
  lse_set_object(Param^.p_result, KR_VARLIST, list);
  if Param^.p_count > 0 then
  begin
    this := lse_vargen_this(Param);
    lse_init_value(@data);
    try
      func := __AsFunc(Param^.p_param[1]);
      if func = nil then list.AddAll(this) else
      while rnnr.Stack.AddSend(this) do
        if rnnr.Goon(func, 1, @data) then
          list.Add(PLseValue(@data)) else
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
  if Param^.p_count > 1 then
  begin
    lse_set_value(Param^.p_result, Param^.p_param[1]);
    func := __AsFunc(Param^.p_param[2]);
    if func <> nil then
    begin
      this := lse_vargen_this(Param);
      rnnr := __AsRunner(Param);
      while true do
      begin
        rnnr.Stack.Add(Param^.p_result);
        if not rnnr.Stack.AddSend(this)
        or not rnnr.Goon(func, 2, Param^.p_result) then Break;
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
  lse_set_object(Param^.p_result, KR_VARLIST, list);
  func := __AsFunc(Param^.p_param[1]);
  if func <> nil then
  begin
    lse_init_value(@test);
    lse_init_value(@data);
    try
      this := lse_vargen_this(Param);
      while lse_vargen_send(this, @data) do
      begin
        rnnr.Stack.Add(PLseValue(@data));
        if not rnnr.Goon(func, 1, @test) then Break else
        if __AsBool(@test) then
          list.Add(PLseValue(@data));
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
  if Param^.p_count > 0 then
  begin
    G := __AsVargen(__AsEngine(Param), Param^.p_param[0]);
    lse_set_object(Param^.p_param[0], KR_VARGEN, G);

    if Param^.p_count = 1 then
    begin
      lse_set_object(Param^.p_param[1], KR_FUNC, sys_oper_inc);
      Param^.p_count := 2;
    end;

    if Param^.p_count = 2 then
    begin
      if not lse_vargen_send(G, Param^.p_param[2]) then Exit;
      Param^.p_count := 3;
    end;

    T := Param^.p_param[1];
    Param^.p_param[1] := Param^.p_param[2];
    Param^.p_param[2] := T;
    
    pp_system_reduce(Param);  
  end;
end;

procedure pp_system_maxint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, high(int64));
end;

procedure pp_system_minint(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, low(int64));
end;

procedure pp_system_abs(const Param: PLseParam);cdecl;
var
  data: PLseValue;
  clss: PLseType;
begin
  data := Param^.p_param[0];
  clss := lse_type(data);
  case clss^.cr_type of
    LSV_INT  : lse_set_int64(Param^.p_result, Abs(data^.VInteger));
    LSV_FLOAT: lse_set_float(Param^.p_result, Abs(data^.VFloat));
          else lse_set_value(Param^.p_result, data);
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
  if Param^.p_count > 1 then
  begin
    runner := __AsRunner(Param);
    mp := runner.MatchPatten;
    if init_patten(mp, Param^.p_param[1]) and exec_patten(mp, Param^.p_param[0]) then
    begin
      if __AsBool(Param^.p_param[2]) then // find all
      begin
        varlist := __NewVarlist(runner.Engine);
        varlist.SaveTo(Param^.p_result);
        varlist.Add(runner.ListMatchResult, KR_VARLIST);
        source := mp^.mp_source;
        while not mp^.mp_anchor do
        begin
          if mp^.mp_result.mr_len > 0 then
            index := (mp^.mp_result.mr_str - source) + mp^.mp_result.mr_len else
            index := (mp^.mp_result.mr_str - source) + 1;
          if exec_patten(mp, source + index, (mp^.mp_eos - source) - index) then
          begin
            mp^.mp_source := source;
            varlist.Add(runner.ListMatchResult, KR_VARLIST);
          end
          else Break;
        end;
      end
      else runner.ListMatchResult.SaveTo(Param^.p_result);
      Exit;
    end;
  end;
  lse_set_object(Param^.p_result, KR_VARLIST, nil);
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

  if Param^.p_count > 3 then
    times := __AsInt64(Param^.p_param[3]) else
    times := MaxInt;

  if (times < 1)
    or not init_patten(mp, Param^.p_param[1])
      or not exec_patten(mp, Param^.p_param[0]) then
      begin
        lse_set_string(Param^.p_result, PLseString(Param^.p_param[0]^.VObject));
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

  new_str := lse_strec_data(Param^.p_param[2]^.VObject);
  new_len := lse_strec_length(Param^.p_param[2]^.VObject);
  times := Length(match_list);
  
  result_len := src_len - match_len + (new_len * times);
  if result_len < 1 then Exit;

  srec := lse_strec_alloc(nil, result_len);
  lse_set_string(Param^.p_result, srec);
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
  varlist: KLiVarList;
  line: string;
begin
  varlist :=KLiVarList.Create(__AsEngine(Param));
  varlist.SaveTo(Param^.p_result);
  
  mp := __AsRunner(Param).MatchPatten;
  if not init_patten(mp, Param^.p_param[1])
    or not exec_patten(mp, Param^.p_param[0]) then
    begin
      varlist.Add(__AsString(Param^.p_param[0]));
      Exit;
    end;

  if mp^.mp_source < mp^.mp_result.mr_str then
  begin
    SetString(line, mp^.mp_source, mp^.mp_result.mr_str - mp^.mp_source);
    varlist.Add(line);
  end;

  if mp^.mp_anchor then
  begin
    source := mp^.mp_result.mr_str + mp^.mp_result.mr_len;
    if source < mp^.mp_eos then
    begin
      SetString(line, source, mp^.mp_eos - source);
      varlist.Add(line);
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
      varlist.Add(line);
    end
    else
    begin
      if len < 1 then Dec(source);
      if source < mp^.mp_eos then
      begin
        SetString(line, source, mp^.mp_eos - source);
        varlist.Add(line);
      end;
      Exit;
    end;
  end;
end;

procedure pp_system_getcs(const Param: PLseParam);cdecl;
begin
  lse_set_object(Param^.p_result, KR_VARLIST,
    __AsRunner(Param).CallStack.GetCallSnap(__AsInt64(Param^.p_param[0])));
end;

procedure pp_system_throw(const Param: PLseParam);cdecl;
var
  eid, msg: string;
  rnnr: KLiRunner;
begin
  if Param^.p_count < 1 then
  begin
    rnnr := __AsRunner(Param);
    if rnnr.Engine.Error.errno = 0 then
      __SetError(Param, RuntimeError, 0, EsRuntimeError) else
      rnnr.Excepted := true;
  end
  else
  begin
     if Param^.p_count > 1 then
    begin
      eid := Trim(__AsString(Param^.p_param[1]));
      if eid = '' then
        eid := RuntimeError;
    end
    else eid := RuntimeError;

    msg := Trim(__AsString(Param^.p_param[0]));
    if msg = '' then
      msg := EsRuntimeError;

    __SetError(Param, eid, 0, msg);
  end;
end;

procedure pp_system_hex(const Param: PLseParam);cdecl;
var
  this: int64;
  size, digits: integer;
  text: string;
begin
  this := __AsInt64(Param^.p_param[0]);
  size := Max(0, __AsInt64(Param^.p_param[1]));
  digits := Min(16, size);
  if digits > 1 then
    text := Format('%.' + IntToStr(digits) + 'x', [this]) else
    text := Format('%x', [this]);
  digits := Length(text);
  if digits < size then
    text := StringOfChar('0', size - digits) + text;
  lse_set_string(Param^.p_result, text);
end;

procedure pp_system_bitlist(const Param: PLseParam);cdecl;
const
  bitf: array[0..1] of char = ('0', '1');
var
  list: array[0..64] of char;
  index, size: integer;
  this: int64;
  base: pchar;
begin
  this := __AsInt64(Param^.p_param[0]);
  for index := 0 to 63 do
    list[index] := bitf[(this shr (63 - index)) and 1];
  list[64] := #0;
  size := 64 - Min(64, Max(1, __AsInt64(Param^.p_param[1])));
  base := list;
  for index := 1 to size do
    if base^ = '0' then Inc(base) else break;
  lse_set_string(Param^.p_result, base);
end;

procedure pp_system_upto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.p_count > 1 then
  begin
    if Param^.p_count > 2 then
      step := Param^.p_param[2]^.VInteger else
      step := 1;
    varg := cvgr_upto(Param^.p_param[0]^.VInteger,
                      Param^.p_param[1]^.VInteger,
                      step, __AsEngine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.p_result, lse_vargen_ensure(varg));
end;

procedure pp_system_downto(const Param: PLseParam);cdecl;
var
  step: int64;
  varg: PLseVargen;
begin
  if Param^.p_count > 1 then
  begin
    if Param^.p_count > 2 then
    begin
      step := Param^.p_param[2]^.VInteger;
      if step < 0 then
        step := - step;
    end
    else step := 1;
    varg := cvgr_downto(Param^.p_param[0]^.VInteger,
                        Param^.p_param[1]^.VInteger,
                        step, __AsEngine(Param));
  end
  else varg := nil;
  lse_set_vargen(Param^.p_result, lse_vargen_ensure(varg));
end;

procedure pp_system_typeof(const Param: PLseParam);cdecl;
var
  T: KLiType;
begin
  T := __AsType(Param^.p_param[0]);
  if T = KT_CLASS then
    T := KLiType(Param^.p_param[0]^.VObject);
  T.SaveTo(Param^.p_result);
end;

{ error }

procedure pp_error_text(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.ErrorText);
end;

procedure pp_error_module(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.module);
end;

procedure pp_error_name(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_error_message(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.msg);
end;

procedure pp_error_row(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result, this.row);
end;

procedure pp_error_col(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result, this.col);
end;

procedure pp_error_errno(const Param: PLseParam);cdecl;
var
  this: KLiError;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result, this.errno);
end;

{ function }

procedure pp_func_name(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_func_desc(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure pp_func_type(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    this.ResultType.SaveTo(Param^.p_result);
end;

procedure pp_func_prototype(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, otos_function(pointer(this)));
end;

procedure pp_func_module(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    this.Module.SaveTo(Param^.p_result);
end;

procedure pp_func_params(const Param: PLseParam);cdecl;
var
  this: KLiFunc;
begin
  if __GetThis(Param, this) then
    this.Params.ToVarlist(__AsEngine(Param)).SaveTo(Param^.p_result);
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
    list.SaveTo(Param^.p_result);
  end;
end;

{ hashed }

procedure pp_hashed_create(const Param: PLseParam);cdecl;
var
  buckets: integer;
  hash: KLiHashed;
begin
  if Param^.p_count > 0 then
    buckets := Max(1, __AsInt64(Param^.p_param[0])) else
    buckets := 1;
  hash := KLiHashed.Create(__AsEngine(Param), buckets); 
  lse_set_object(Param^.p_result, KR_HASHED, hash);
end;

procedure pp_hashed_keys(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  list: KLiVarList;
begin
  if __GetThis(Param, this) then
  begin
    list := KLiVarList.Create(__AsEngine(Param));
    list.SaveTo(Param^.p_result);
    this.ListKeys(list);
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
      values.SaveTo(Param^.p_result);
      for index := 0 to list.Count - 1 do
        values.Add(PLseValue(list[index]));
    finally
      list.Free;
    end;
  end;
end;

procedure pp_hashed_remove(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) and (Param^.p_count > 1) then
    this.Remove(__AsString(Param^.p_param[1]));
end;

procedure pp_hashed_clear(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
begin
  if __GetThis(Param, this) then
    this.Clear;
end;

procedure pp_hashed_read(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
  data: PLseValue;
begin
  if __GetThis(Param, this) then
  begin
    key := __AsString(Param^.p_param[1]);
    data := this.FindValue(key);
    if data = nil then
      lse_set_value(Param^.p_result, Param^.p_param[2]) else
      lse_set_value(Param^.p_result, data);
  end;
end;

procedure pp_hashed_isset(const Param: PLseParam);cdecl;
var
  this: KLiHashed;
  key: string;
begin
  if __GetThis(Param, this) and (Param^.p_count > 1) then
  begin
    key := __AsString(Param^.p_param[1]);
    lse_set_bool(Param^.p_result, this.IsSet(key));
  end;
end;

{ module }

procedure pp_module_name(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_module_desc(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure pp_module_file(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.FileName);
end;

procedure pp_module_modules(const Param: PLseParam);cdecl;
var
  this: KLiModule;
  engine: KLiEngine;
begin
  if __GetThis(Param, this) then
  begin
    engine := __AsEngine(Param);
    this.Modules.ToVarlist(engine).SaveTo(Param^.p_result);
  end;
end;

procedure pp_module_funcs(const Param: PLseParam);cdecl;
var
  M: KLiModule;
  L: KLiVarList;
  F: KLiFunc;
begin
  if __GetThis(Param, M) then
  begin
    L := __NewVarlist(__AsEngine(Param));
    L.SaveTo(Param^.p_result);
    F := M.FirstFunc;
    while F <> nil do
    begin
      L.Add(F, KR_FUNC);
      F := F.Next;
    end;
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
    list.SaveTo(Param^.p_result);
    for index := 0 to this.TypeCount - 1 do
      list.Add(this.GetType(index), KR_CLASS);
  end;
end;

procedure pp_module_version(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Version);
end;

procedure pp_module_main(const Param: PLseParam);cdecl;
var
  this: KLiModule;
begin
  if __GetThis(Param, this) then
    this.MainFunc.SaveTo(Param^.p_result);
end;

procedure pp_module_imports(const Param: PLseParam);cdecl;
var
  this, curr: KLiModule;
begin
  if __GetThis(Param, this) then
  begin
    if this.ModuleType <> moyScript then
      lse_error('%s can not import other modules', [this.Name]);
    Param^.p_param[0] := Param^.p_param[1];
    pp_system_load(Param);
    curr := KLiModule(Param^.p_result^.VObject);
    if curr.ModuleType = moyScript then
    begin
      curr.AddImporter(this);
      this.Modules.Add(curr);
    end
    else this.Modules.Add(curr);
  end;
end;

{ vargen }

procedure pp_vargen_create(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := __AsVargen(__AsEngine(Param), Param^.p_param[0]);
  lse_set_vargen(Param^.p_result, this);
end;

procedure pp_vargen_eof(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.p_result, lse_vargen_eof(this));
end;

procedure pp_vargen_next(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_vargen_send(this, Param^.p_result);
end;

procedure pp_vargen_rewind(const Param: PLseParam);cdecl;
var
  this: PLseVargen;
begin
  this := lse_vargen_this(Param);
  lse_set_bool(Param^.p_result, lse_vargen_rewind(this));
end;

{ stream }

procedure pp_stream_close(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_close(this) else
    __SetErrorThis(Param);
end;

procedure pp_stream_eof(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_bool(Param^.p_result, this^.s_eof(this) <> 0) else
    __SetErrorThis(Param);
end;

procedure pp_stream_get_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.p_result, this^.s_seek(this, 0, SSF_CURRENT)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_set_position(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_seek(this, Param^.p_param[1]^.VInteger, SSF_BEGINNING) else
    __SetErrorThis(Param);
end;

procedure pp_stream_set_length(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_set_size(this, Param^.p_param[1]^.VInteger) else
    __SetErrorThis(Param);
end;

procedure pp_stream_read(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
  line: string;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    if Assigned(this^.s_read) then
    begin
      size := Param^.p_param[1]^.VInteger;
      if size > 0 then
      begin
        SetLength(line, size);
        size := this^.s_read(this, pointer(line), size);
        if size > 0 then
        begin
          lse_set_string(Param^.p_result, pchar(line), size);
          Exit;
        end;
      end;
    end;
    lse_set_string(Param^.p_result, '');
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_readln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_string(Param^.p_result, this^.s_readln(this)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_write(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  size: integer;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    size := lse_stream_write(this, Param^.p_param[1]^.VObject);
    lse_set_int64(Param^.p_result, size);
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_writeln(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    lse_set_int64(Param^.p_result,
      lse_stream_writeln(this, Param^.p_param[1]^.VObject)) else
    __SetErrorThis(Param);
end;

procedure pp_stream_writeTo(const Param: PLseParam);cdecl;
var
  this, desti: PLseStream;
  bytes: integer;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    desti := PLseStream(Param^.p_param[1]^.VObject);
    if (desti <> nil) and (desti <> this) then
      bytes := lse_stream_fill(desti, this, Param^.p_param[2]^.VInteger) else
      bytes := 0;
    lse_set_int64(Param^.p_result, bytes);
  end
  else __SetErrorThis(Param);
end;

procedure pp_stream_flush(const Param: PLseParam);cdecl;
var
  this: PLseStream;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
    this^.s_flush(this) else
    __SetErrorThis(Param);
end;

procedure pp_stream_lines(const Param: PLseParam);cdecl;
var
  this: PLseStream;
  varg: PLseVargen;
begin
  this := PLseStream(Param^.p_param[0]^.VObject);
  if this <> nil then
  begin
    varg := cvgr_stream_lines(this, __AsEngine(Param));
    lse_set_object(Param^.p_result, KR_VARGEN, varg);
  end
  else __SetErrorThis(Param);
end;

{ string }

procedure pp_string_setAt(const Param: PLseParam);cdecl;
var
  this: PLseString;
  index, range: int64;
begin
  this := Param^.p_param[0]^.VObject;
  range := lse_strec_length(this);
  index := lse_vary_index(__AsInt64(Param^.p_param[1]), range);
  lse_check_index(index, range);
  this := lse_strec_alloc(lse_strec_data(this), range);
  lse_strec_data(this)[index] := __AsChar(Param^.p_param[2]);
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_name(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, __extractName(lse_strec_data(this)));
end;

procedure pp_string_value(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, __extractValue(lse_strec_data(this)));
end;

procedure pp_string_lower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := __smrLower(lse_strec_dup(Param^.p_param[0]^.VObject));
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_upper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := __smrUpper(lse_strec_dup(Param^.p_param[0]^.VObject));
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_compare(const Param: PLseParam);cdecl;
var
  this, value: PLseString;
  IgnoreCase: boolean;
begin
  this := Param^.p_param[0]^.VObject;
  value := Param^.p_param[1]^.VObject;
  IgnoreCase := __AsBool(Param^.p_param[2]);
  lse_set_int64(Param^.p_result, __smrComp(this, Value, IgnoreCase));
end;

procedure pp_string_replace(const Param: PLseParam);cdecl;
var
  this, patten, newStr: pchar;
  flags: TReplaceFlags;
begin
  this := lse_strec_data(Param^.p_param[0]^.VObject);
  patten := lse_strec_data(Param^.p_param[1]^.VObject);
  newStr := lse_strec_data(Param^.p_param[2]^.VObject);
  flags := [rfReplaceAll];
  if __AsBool(Param^.p_param[3]) then // IgnoreCase
    flags := flags + [rfIgnoreCase];
  if __AsBool(Param^.p_param[4]) then // FirstOnly
    flags := flags - [rfReplaceAll];
  lse_set_string(Param^.p_result, StringReplace(this, patten, newStr, flags));
end;

procedure pp_string_pos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  patten := Param^.p_param[1]^.VObject;
  IgnoreCase := __AsBool(Param^.p_param[2]);
  str := lse_strec_data(this);
  pos := __pos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.p_result, pos - str) else
    lse_set_int64(Param^.p_result, -1);
end;

procedure pp_string_lastPos(const Param: PLseParam);cdecl;
var
  this, patten: PLseString;
  IgnoreCase: boolean;
  str, pos: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  patten := Param^.p_param[1]^.VObject;
  IgnoreCase := __AsBool(Param^.p_param[2]);
  str := lse_strec_data(this);
  pos := __lastPos(str, lse_strec_length(this), lse_strec_data(patten),
    lse_strec_length(patten), IgnoreCase);
  if pos <> nil then
    lse_set_int64(Param^.p_result, pos - str) else
    lse_set_int64(Param^.p_result, -1);
end;

procedure pp_string_left(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  size := min(__AsInt64(Param^.p_param[1]), slen);
  if size = slen then
    lse_set_string(Param^.p_result, this) else
  if size > 0 then
    lse_set_string(Param^.p_result, lse_strec_alloc(lse_strec_data(this), size)) else
    lse_set_string(Param^.p_result, '', 0);
end;

procedure pp_string_right(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, size: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  size := min(__AsInt64(Param^.p_param[1]), slen);
  if size = slen then
    lse_set_string(Param^.p_result, this) else
  if size > 0 then
  begin
    base := lse_strec_data(this) + (slen - size);
    lse_set_string(Param^.p_result, lse_strec_alloc(base, size));
  end
  else lse_set_string(Param^.p_result, '', 0);
end;

procedure pp_string_trim(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  if (__smrCountTab(this, L, M, R) > 0) and ((L + R) > 0) then
  begin
    M := lse_strec_length(this) - (L + R);
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.p_result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure pp_string_trimLeft(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base: pchar;
begin
  this := Param^.p_param[0]^.VObject;
  if (__smrCountTab(this, L, M, R) > 0) and (L > 0) then
  begin
    M := lse_strec_length(this) - L;
    base := lse_strec_data(this) + L;
    lse_set_string(Param^.p_result, lse_strec_alloc(base, M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure pp_string_trimRight(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
begin
  this := Param^.p_param[0]^.VObject;
  if (__smrCountTab(this, L, M, R) > 0) and (R > 0) then
  begin
    M := lse_strec_length(this) - R;
    lse_set_string(Param^.p_result, lse_strec_alloc(lse_strec_data(this), M));
  end
  else lse_set_string(Param^.p_result, this);
end;

procedure pp_string_trimAll(const Param: PLseParam);cdecl;
var
  this: PLseString;
  L, M, R: integer;
  base, next: pchar;
begin
  this := Param^.p_param[0]^.VObject;
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_copy(const Param: PLseParam);cdecl;
var
  this: PLseString;
  slen, index, count: int64;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := __AsInt64(Param^.p_param[1]);
    count := __AsInt64(Param^.p_param[2]);
    index := lse_vary_range(index, slen, count);
    if count > 0 then
    begin
      if count < slen then
        this := lse_strec_alloc(lse_strec_data(this) + index, count);
      lse_set_string(Param^.p_result, this);
    end;
  end;
end;

procedure pp_string_delete(const Param: PLseParam);cdecl;
var
  this: PLseString;
  base, next: pchar;
  slen, index, count: int64;
begin
  this := Param^.p_param[0]^.VObject;
  slen := lse_strec_length(this);
  if slen > 0 then
  begin
    index := __AsInt64(Param^.p_param[1]);
    if Param^.p_count > 2 then
      count := __AsInt64(Param^.p_param[2]) else
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_insert(const Param: PLseParam);cdecl;
var
  this, text: PLseString;
  base, next: pchar;
  index, slen, size: int64;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  text := Param^.p_param[1]^.VObject;
  slen := lse_strec_length(text);
  if slen > 0 then
  begin
    index := lse_vary_index(__AsInt64(Param^.p_param[2]), size);
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_isAlpha(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), AlphaChar));
end;

procedure pp_string_isAlnum(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), AlnumChar));
end;

procedure pp_string_isCntrl(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), CntrlChar));
end;

procedure pp_string_isDigit(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), DigitChar));
end;

procedure pp_string_isSpace(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), SpaceChar));
end;

procedure pp_string_isHex(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this), HexChar));
end;

procedure pp_string_extractName(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.p_param[0]^.VObject;
  if Param^.p_count > 1 then
    midc := __AsString(Param^.p_param[1]) else
    midc := '=';
  lse_set_string(Param^.p_result, __extractName(lse_strec_data(this), midc));
end;

procedure pp_string_extractValue(const Param: PLseParam);cdecl;
var
  this: PLseString;
  midc: string;
begin
  this := Param^.p_param[0]^.VObject;
  if Param^.p_count > 1 then
    midc := __AsString(Param^.p_param[1]) else
    midc := '=';
  lse_set_string(Param^.p_result, __extractValue(lse_strec_data(this), midc));
end;

procedure pp_string_saveToFile(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(__AsFileName(Param^.p_param[1]), fmCreate) do
  try
    this := Param^.p_param[0]^.VObject;
    WriteBuffer(lse_strec_data(this)^, lse_strec_length(this));
  finally
    Free;
  end;
end;

procedure pp_string_fileText(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  with TFileStream.Create(__AsFileName(Param^.p_param[0]), fmShareDenyWrite) do
  try
    this := lse_strec_alloc(nil, size);
    lse_set_string(Param^.p_result, this);
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
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := max(0, __AsInt64(Param^.p_param[1]));
  if width > size then
  begin
    filler := __AsChar(Param^.p_param[2]);
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_rformat(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := __AsInt64(Param^.p_param[1]);
  if width > size then
  begin
    filler := __AsChar(Param^.p_param[2]);
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_center(const Param: PLseParam);cdecl;
var
  this: PLseString;
  size, width, A: integer;
  base, next: pchar;
  filler: char;
begin
  this := Param^.p_param[0]^.VObject;
  size := lse_strec_length(this);
  width := __AsInt64(Param^.p_param[1]);
  if width > size then
  begin
    filler := __AsChar(Param^.p_param[2]);
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
  lse_set_string(Param^.p_result, this);
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
  this := Param^.p_param[0]^.VObject;
  if lse_strec_length(this) > 1 then
    lse_set_string(Param^.p_result, __randomOrder(lse_strec_data(this))) else
    lse_set_string(Param^.p_result, this);
end;

procedure pp_string_html(const Param: PLseParam);cdecl;
var
  smr: PLseString;
begin
  smr := Param^.p_param[0]^.VObject;
  lse_set_string(Param^.p_result, __encodeHTML(lse_strec_data(smr), lse_strec_length(smr), false));
end;

// string string.reverse()
procedure pp_string_reverse(const Param: PLseParam);cdecl;
var
  this: PLseString;
  head, last: pchar;
  temp: char;
begin
  this := Param^.p_param[0]^.VObject;
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
  lse_set_string(Param^.p_result, this);
end;

// bool string.isLower()
procedure pp_string_isLower(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this),
    lse_strec_length(this), ['a'..'z']));
end;

// bool string.isUpper()
procedure pp_string_isUpper(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  lse_set_bool(Param^.p_result, __inCharSet(lse_strec_data(this),
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
  this := Param^.p_param[0]^.VObject;
  old  := Param^.p_param[1]^.VObject;
  new  := Param^.p_param[2]^.VObject;
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
  lse_set_string(Param^.p_result, this);
end;

procedure pp_string_filePath(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFilePath(__AsFileName(Param^.p_param[0])));
end;

procedure pp_string_fullFileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    lse_expand_fname(__AsFileName(Param^.p_param[0])));
end;

procedure pp_string_fileName(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileName(__AsFileName(Param^.p_param[0])));
end;

procedure pp_string_fileExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ExtractFileExt(__AsFileName(Param^.p_param[0])));
end;

procedure pp_string_changeExt(const Param: PLseParam);cdecl;
begin
  lse_set_string(Param^.p_result,
    ChangeFileExt(__AsFileName(Param^.p_param[0]),
                  __AsFileName(Param^.p_param[1])));
end;

procedure pp_string_md5sum(const Param: PLseParam);cdecl;
var
  this: PLseString;
  sum5: string;
begin
  if not __AsBool(Param^.p_param[1]) then
  begin
    this := Param^.p_param[0]^.VObject;
    sum5 := __md5sumBuf(lse_strec_data(this), lse_strec_length(this));
  end
  else sum5 := __md5sumFile(__AsFileName(Param^.p_param[0]));
  lse_set_string(Param^.p_result, sum5);
end;

procedure pp_string_hexToInt(const Param: PLseParam);cdecl;
var
  this: PLseString;
begin
  this := Param^.p_param[0]^.VObject;
  if __inCharSet(lse_strec_data(this), lse_strec_length(this), HexChar) then
    lse_set_int64(Param^.p_result, StrToInt64('$' + lse_strec_data(this))) else
    lse_set_int64(Param^.p_result, __AsInt64(Param^.p_param[1]));
end;

procedure pp_string_hash(const Param: PLseParam);cdecl;
begin
  lse_set_int64(Param^.p_result, __hashof(__AsString(Param^.p_param[0])));
end;

{ type }

procedure pp_type_name(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_type_description(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Description);
end;

procedure pp_type_simple(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if __GetThis(Param, this) then
    lse_set_bool(Param^.p_result, this.IsSimpleType);
end;

procedure pp_type_module(const Param: PLseParam);cdecl;
var
  this: KLiType;
begin
  if __GetThis(Param, this) then
    this.Module.SaveTo(Param^.p_result);
end;

{ variable }

procedure pp_varb_name(const Param: PLseParam);cdecl;
var
  this: KLiVarb;
begin
  if __GetThis(Param, this) then
    lse_set_string(Param^.p_result, this.Name);
end;

procedure pp_varb_type(const Param: PLseParam);cdecl;
var
  this: KLiVarb;
begin
  if __GetThis(Param, this) then
    this.ValueType.SaveTo(Param^.p_result);
end;

{ varlist }

procedure pp_varlist_create(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  this := __NewVarlist(__AsEngine(Param)); 
  this.SaveTo(Param^.p_result);
  if Param^.p_count > 0 then
    this.Count := Max(0, Param^.p_param[0]^.VInteger);
end;

procedure pp_varlist_get_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_int64(Param^.p_result, this.Count);
end;

procedure pp_varlist_set_length(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
      this.Count := Max(0, Param^.p_param[1]^.VInteger);
end;

procedure pp_varlist_exchange(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  X1, X2: int64;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      X1 := lse_vary_index(__AsInt64(Param^.p_param[1]), this.Count);
      X2 := lse_vary_index(__AsInt64(Param^.p_param[2]), this.Count);
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
    if not (this is KLiVarSnap) then
    begin
      X1 := lse_vary_index(__AsInt64(Param^.p_param[1]), this.Count);
      X2 := lse_vary_index(__AsInt64(Param^.p_param[2]), this.Count);
      if X1 <> X2 then
        this.Move(X1, X2);
    end;
end;

procedure pp_varlist_add(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      lse_set_int64(Param^.p_result, this.Count);
      this.Add(Param^.p_param[1]);
    end;
end;

procedure pp_varlist_addFrom(const Param: PLseParam);cdecl;
var
  this, list: KLiVarList;
  index, count: integer;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      list := KLiVarList(Param^.p_param[1]^.VObject);
      if (list <> nil) and (list.Count > 0) then
      begin
        count := list.Count;
        for index := 0 to count - 1 do
          this.Add(list[index]);
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
    if (Param^.p_count > 1) and not (this is KLiVarSnap) then
    begin
      if __AsBool(Param^.p_param[2]) then
        this.Clear;
      varg := __AsVargen(__AsEngine(Param), Param^.p_param[1]);
      lse_init_value(@data);
      try
        while lse_vargen_send(varg, @data) do
          this.Add(PLseValue(@data));
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
    if not (this is KLiVarSnap) then
    begin
      index := lse_vary_index(__AsInt64(Param^.p_param[1]), this.Count);
      lse_check_index(index, int64(this.Count) + 1);
      lse_set_value(this.Insert(index), Param^.p_param[2]);
    end;
end;

procedure pp_varlist_delete(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  index: int64;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
    begin
      index := lse_vary_index(__AsInt64(Param^.p_param[1]), this.Count);
      lse_check_index(index, this.Count);
      this.Delete(index);
    end;
end;

procedure pp_varlist_clear(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    if not (this is KLiVarSnap) then
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
    index := __AsInt64(Param^.p_param[1]);
    count := __AsInt64(Param^.p_param[2]);
    index := lse_vary_range(index, this.Count, count);
    this.Copy(index, count).SaveTo(Param^.p_result);
  end;
end;

procedure pp_varlist_left(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if __GetThis(Param, this) then
  begin
    count := __AsInt64(Param^.p_param[1]);
    this.Left(count).SaveTo(Param^.p_result);
  end;
end;

procedure pp_varlist_right(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
  count: int64;
begin
  if __GetThis(Param, this) then
  begin
    count := __AsInt64(Param^.p_param[1]);
    this.Right(count).SaveTo(Param^.p_result);
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
    lse_set_object(Param^.p_result, KR_VARLIST, list);
    func := __AsFunc(Param^.p_param[1]);
    if (func <> nil) and (func.ResultType <> KT_VOID) then
    begin
      lse_init_value(@cond);
      try
        index := 0;
        while index < this.Count do
        begin
          rnnr.Stack.Add(this[index]);
          if not rnnr.Goon(func, 1, @cond) then Break else
          if __AsBool(@cond) then
            list.Add(this[index]);
          Inc(index);
        end;
      finally
        lse_clear_value(@cond);
      end;
    end;
  end;
end;

procedure pp_varlist_first(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_value(Param^.p_result, this[0]);
end;

procedure pp_varlist_last(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
    lse_set_value(Param^.p_result, this[-1]);
end;

procedure pp_varlist_shift(const Param: PLseParam);cdecl;
var
  this: KLiVarList;
begin
  if __GetThis(Param, this) then
  begin
    lse_set_value(Param^.p_result, this[0]);
    if not (this is KLiVarSnap) then
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
    lse_set_value(Param^.p_result, this[index]);
    if not (this is KLiVarSnap) then
      this.Delete(index);
  end;
end;

end.
