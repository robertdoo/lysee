{==============================================================================}
{     PROJECT: lysee_sh_funcs                                                  }
{ DESCRIPTION: shell functions of current OS (FPC)                             }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2010/09/01                                                      }
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
unit lysee_sh_funcs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lseu
  {$IFDEF WINDOWS},Windows{$ENDIF};

type
  TLiSearch = class(TLseObject)
  private
    FSrec: TSearchRec;
    FPath: string;
    FActive: boolean;
    function GetName: string;
    function GetFullName: string;
    function GetIsFile: boolean;
    function GetIsDir: boolean;
  public
    constructor Create(const Path: string);
    destructor Destroy;override;
    function IsDD(const S: string): boolean;
    function FindFirst(const Path: string): boolean;
    function FindNext: boolean;
    function FindClose: boolean;
    function DeleteAll(Files, Directories, DirTree: boolean): integer;
    property Active: boolean read FActive;
    property SearchPath: string read FPath;
    property Name: string read GetName;
    property FullName: string read GetFullName;
    property IsFile: boolean read GetIsFile;
    property IsDir: boolean read GetIsDir;
  end;

procedure searcher_create(const Param: pointer);cdecl;
procedure searcher_findFirst(const Param: pointer);cdecl;
procedure searcher_findNext(const Param: pointer);cdecl;
procedure searcher_findClose(const Param: pointer);cdecl;
procedure searcher_getName(const Param: pointer);cdecl;
procedure searcher_eof(const Param: pointer);cdecl;
procedure searcher_getSize(const Param: pointer);cdecl;
procedure searcher_getTime(const Param: pointer);cdecl;
procedure searcher_getPath(const Param: pointer);cdecl;
procedure searcher_getFullName(const Param: pointer);cdecl;
procedure searcher_getIsDir(const Param: pointer);cdecl;
procedure searcher_getIsFile(const Param: pointer);cdecl;

const
  KTE_SEARCHREC = 'searcherError';

  searchrec_func_count = 12;
  searchrec_func_array: array[0..searchrec_func_count - 1] of RLseFuncRec = (
    (fr_prot:'searcher searcher(string fileNameMask)';
     fr_addr:@searcher_create;
     fr_desc:'create file search object'
    ),
    (fr_prot:'string get_path()';
     fr_addr:@searcher_getPath;
     fr_desc:'get search path'
    ),
    (fr_prot:'string get_name()';
     fr_addr:@searcher_getName;
     fr_desc:'get file name without search path'
    ),
    (fr_prot:'bool get_eof()';
     fr_addr:@searcher_eof;
     fr_desc:'return true when no more file can be found'
    ),
    (fr_prot:'string fullName()';
     fr_addr:@searcher_getFullName;
     fr_desc:'get full file name'
    ),
    (fr_prot:'int get_size()';
     fr_addr:@searcher_getSize;
     fr_desc:'get file size'
    ),
    (fr_prot:'time get_age()';
     fr_addr:@searcher_getTime;
     fr_desc:'get file age'
    ),
    (fr_prot:'bool isdir()';
     fr_addr:@searcher_getisdir;
     fr_desc:'is it a directory?'
    ),
    (fr_prot:'bool isfile()';
     fr_addr:@searcher_getisfile;
     fr_desc:'is it an ordinary file?'
    ),
    (fr_prot:'bool find(string fileNameMask)';
     fr_addr:@searcher_findFirst;
     fr_desc:'find first file'
    ),
    (fr_prot:'bool next()';
     fr_addr:@searcher_findNext;
     fr_desc:'find next file'
    ),
    (fr_prot:'void close()';
     fr_addr:@searcher_findClose;
     fr_desc:'close searcher'
    )
  );

  searcher_class: RLseClassRec = (
    vtype      : LSV_OBJECT;
    name       : 'searcher';
    desc       : 'file searcher';
    incRefcount:@lse_incRefcount;
    decRefcount:@lse_decRefcount;
    funcs      : (count:searchrec_func_count; entry:@searchrec_func_array);
    writeTo    : nil;
    toVargen   : nil;
    toString   : nil;
    stringTo   : nil;
    addItem    : nil;
    lysee_class: nil
  );

procedure sh_find(const Param: pointer);cdecl;
procedure sh_isfile(const Param: pointer);cdecl;
procedure sh_isdir(const Param: pointer);cdecl;
procedure sh_dir(const Param: pointer);cdecl;
procedure sh_change_dir(const Param: pointer);cdecl;
procedure sh_copy(const Param: pointer);cdecl;
procedure sh_delete(const Param: pointer);cdecl;
procedure sh_list(const Param: pointer);cdecl;
procedure sh_rmdir(const Param: pointer);cdecl;
procedure sh_mkdir(const Param: pointer);cdecl;
procedure sh_system(const Param: pointer);cdecl;
procedure sh_shexec(const Param: pointer);cdecl;

const
  KTN_FS = 'fs';
  KTD_FS = 'file system management module for lysee';
  KTE_FS = 'fsError';

  sh_func_count = 12;
  sh_func_array: array[0..sh_func_count - 1] of RLseFuncRec = (
    (fr_prot:'searcher find(string fileNameMask)';
     fr_addr:@sh_find;
     fr_desc:'find first file'
    ),
    (fr_prot:'bool isfile(string fileName)';
     fr_addr:@sh_isfile;
     fr_desc:'is it an ordinary file?'
    ),
    (fr_prot:'bool isdir(string directory)';
     fr_addr:@sh_isdir;
     fr_desc:'is it an directory?'
    ),
    (fr_prot:'string dir()';
     fr_addr:@sh_dir;
     fr_desc:'get current directory'
    ),
    (fr_prot:'bool cd(string newDirectory)';
     fr_addr:@sh_change_dir;
     fr_desc:'change current directory'
    ),
    (fr_prot:'bool copy(string sourceFile, string destiFile, bool failIfFileExists)';
     fr_addr:@sh_copy;
     fr_desc:'copy file'
    ),
    (fr_prot:'int del(string fileMask)';
     fr_addr:@sh_delete;
     fr_desc:'delete specified file'
    ),
    (fr_prot:'strlist ls(string directoryOrfileMask)';
     fr_addr:@sh_list;
     fr_desc:'list file'
    ),
    (fr_prot:'bool rmdir(string dir, bool deltree)';
     fr_addr:@sh_rmdir;
     fr_desc:'remove specified directory'
    ),
    (fr_prot:'bool mkdir(string dir)';
     fr_addr:@sh_mkdir;
     fr_desc:'create directory'
    ),
    (fr_prot:'string system(string commandline, string dir)';
     fr_addr:@sh_system;
     fr_desc:'execute command line and get its output'
    ),
    (fr_prot:'bool shexec(string commandline, string dir, bool wait)';
     fr_addr:@sh_shexec;
     fr_desc:'execute command line'
    )
  );
  
implementation

uses
  lse_spawn;
  
function copy_file(const Source, Desti: string; FailIfExists: boolean): boolean;
{$IFNDEF WINDOWS}
var
  fi, fo: TFileStream;
{$ENDIF}
begin
  Result := false;
  try
    {$IFDEF WINDOWS}
    Result := Windows.CopyFile(pchar(Source), pchar(Desti), FailIfExists);
    {$ELSE}
    if not FailIfExists or not FileExists(Desti) then
    begin
      fi := TFileStream.Create(Source, fmShareDenyWrite);
      try
        fo := TFileStream.Create(Desti, fmCreate);
        try
          fo.CopyFrom(fi, 0);
        finally
          fo.Free;
        end;
      finally
        fi.Free;
      end;
      Result := true;
    end;
    {$ENDIF}
  except
    { do nothing }
  end;
end;

function remove_tree(const dir: string): boolean;
var
  sr: TLiSearch;
begin
  Result := SysUtils.RemoveDir(dir);
  if not Result then
  begin
    sr := TLiSearch.Create(dir + '\*.*');
    try
      sr.DeleteAll(true, true, true);
    finally
      sr.Free;
    end;
    Result := SysUtils.RemoveDir(dir);
  end;
end;
  
procedure searcher_create(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    this := TLiSearch.Create(invoker.paramStr(1));
    invoker.returnObject(searcher_class.lysee_class, this);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_findFirst(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnBool(this.FindFirst(invoker.paramStr(1)));
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_findNext(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnBool(this.FindNext);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_findClose(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      this.FindClose;
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getName(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnStr(this.Name);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_eof(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnBool(not this.Active);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getSize(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnInt64(this.FSrec.size);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getTime(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnFloat(FileDateToDateTime(this.FSrec.Time));
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getPath(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnStr(this.SearchPath);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getFullName(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnStr(this.FullName);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getIsDir(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnBool(this.IsDir);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure searcher_getIsFile(const Param: pointer);cdecl;
var
  this: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    if invoker.GetThis(this) then
      invoker.returnBool(this.IsFile);
  except
    invoker.returnError(KTE_SEARCHREC, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_find(const Param: pointer);cdecl;
var
  sr: TLiSearch;
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    sr := TLiSearch.Create(invoker.paramStr(0));
    if sr.Active then
      invoker.returnObject(searcher_class.lysee_class, sr) else
      sr.Free;
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_isfile(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnBool(FileExists(invoker.paramStr(0)));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_isdir(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnBool(DirectoryExists(invoker.paramStr(0)));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_dir(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnStr(GetCurrentDir);
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_change_dir(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnBool(SetCurrentDir(invoker.paramStr(0)));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_copy(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  f_src, f_dst: string;
begin
  invoker := TLseInvoke.Create(Param);
  try
    f_src := invoker.paramStr(0);
    f_dst := invoker.paramStr(1);
    invoker.returnBool(copy_file(f_src, f_dst, invoker.paramBool(2)));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_delete(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  fmask: string;
  count: integer;
  sr: TLiSearch;
begin
  invoker := TLseInvoke.Create(Param);
  try
    count := 0;
    fmask := invoker.paramStr(0);
    if (Pos('*', fmask) > 0) or (Pos('?', fmask) > 0) then
    begin
      sr := TLiSearch.Create(fmask);
      try
        count := sr.DeleteAll(true, false, false);
      finally
        sr.Free;
      end;
    end;
    if SysUtils.DeleteFile(fmask) then Inc(count);
    invoker.returnInt(count);
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_list(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  list: string;
  sr: TLiSearch;
begin
  invoker := TLseInvoke.Create(Param);
  try
    list := Trim(invoker.paramStr(0));
    if list = '' then list := '.\*.*' else
    if DirectoryExists(list) then
      list := IncludeTrailingPathDelimiter(list) + '*.*';
    sr := TLiSearch.Create(list);
    try
      if sr.Active then
      begin
        list := sr.FullName;
        while sr.FindNext do
          list := list + sLineBreak + sr.FullName;
        invoker.returnStr(list);
      end;
    finally
      sr.Free;
    end;
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_rmdir(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  dir: string;
begin
  invoker := TLseInvoke.Create(Param);
  try
    dir := ExpandFileName(Trim(invoker.paramStr(0)));
    dir := ExcludeTrailingPathDelimiter(dir);
    if DirectoryExists(dir) then
    begin
      if SysUtils.RemoveDir(dir) then
        invoker.returnBool(true) else
      if invoker.paramBool(1) then
        invoker.returnBool(remove_tree(dir));
    end;
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_mkdir(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
begin
  invoker := TLseInvoke.Create(Param);
  try
    invoker.returnBool(ForceDirectories(Trim(invoker.paramStr(0))));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_system(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  v_cmd, v_dir: string;
  status: integer;
begin
  invoker := TLseInvoke.Create(Param);
  try
    v_cmd := Trim(invoker.paramFmt(0));
    v_dir := Trim(invoker.paramFmt(1));
    invoker.returnStr(spawn_shouts(v_cmd, v_dir, status));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

procedure sh_shexec(const Param: pointer);cdecl;
var
  invoker: TLseInvoke;
  v_cmd, v_dir: string;
  status: integer;
begin
  invoker := TLseInvoke.Create(Param);
  try
    v_cmd := Trim(invoker.paramFmt(0));
    v_dir := Trim(invoker.paramFmt(1));
    invoker.returnBool(spawn_shexec(v_cmd, v_dir,
      invoker.paramBool(2), status));
  except
    invoker.returnError(KTE_FS, 0, lse_exception_str);
  end;
  invoker.Free;
end;

{ TLiSearch }

constructor TLiSearch.Create(const Path: string);
begin
  FindFirst(Path);
end;

function TLiSearch.DeleteAll(Files, Directories, DirTree: boolean): integer;
begin
  Result := 0;
  if Files or Directories then
    while Active do
      if Files and IsFile then
      begin
        if SysUtils.DeleteFile(FullName) then Inc(Result);
        FindNext;
      end
      else
      if Directories and IsDir then
      begin
        if SysUtils.RemoveDir(FullName)
          or (DirTree and remove_tree(FullName)) then
            Inc(Result);
        FindNext;
      end
      else FindNext;
end;

destructor TLiSearch.Destroy;
begin
  FindClose;
  inherited;
end;

function TLiSearch.FindClose: boolean;
begin
  Result := FActive;
  if Result then
  begin
    FActive := false;
    SysUtils.FindClose(FSrec);
  end;
end;

function TLiSearch.FindFirst(const Path: string): boolean;
var
  fmask: string;
begin
  FindClose;
  fmask := ExpandFileName(Trim(Path));
  FPath := ExtractFilePath(fmask);
  FActive := (SysUtils.FindFirst(fmask, faAnyFile, FSrec) = 0);
  if FActive and IsDD(FSrec.Name) then FindNext;
  Result := FActive;
end;

function TLiSearch.FindNext: boolean;
begin
  Result := FActive;
  if Result then
  begin
    Result := (SysUtils.FindNext(FSrec) = 0);
    if not Result then
      FindClose else if IsDD(FSrec.Name) then
      Result := FindNext;
  end;
end;

function TLiSearch.GetFullName: string;
begin
  if FActive then
    Result := FPath + FSrec.Name else
    Result := '';
end;

function TLiSearch.GetIsDir: boolean;
begin
  Result := FActive and ((FSrec.Attr and faDirectory) <> 0);
end;

function TLiSearch.GetIsFile: boolean;
begin
  Result := FActive and ((FSrec.Attr and faDirectory) = 0);
end;

function TLiSearch.GetName: string;
begin
  if FActive then
    Result := FSrec.Name else
    Result := '';
end;

function TLiSearch.IsDD(const S: string): boolean;
begin
  Result := (S = '.') or (S = '..');
end;

end.
