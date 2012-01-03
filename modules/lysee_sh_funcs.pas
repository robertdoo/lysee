{==============================================================================}
{     PROJECT: lysee_sh_funcs                                                  }
{ DESCRIPTION: shell functions of current OS (FPC)                             }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2011/08/07                                                      }
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

procedure searcher_create(const invoker: TLseInvoke);cdecl;
procedure searcher_findFirst(const invoker: TLseInvoke);cdecl;
procedure searcher_findNext(const invoker: TLseInvoke);cdecl;
procedure searcher_findClose(const invoker: TLseInvoke);cdecl;
procedure searcher_getName(const invoker: TLseInvoke);cdecl;
procedure searcher_eof(const invoker: TLseInvoke);cdecl;
procedure searcher_getSize(const invoker: TLseInvoke);cdecl;
procedure searcher_getPath(const invoker: TLseInvoke);cdecl;
procedure searcher_getFullName(const invoker: TLseInvoke);cdecl;
procedure searcher_getIsDir(const invoker: TLseInvoke);cdecl;
procedure searcher_getIsFile(const invoker: TLseInvoke);cdecl;

procedure sh_isfile(const invoker: TLseInvoke);cdecl;
procedure sh_isdir(const invoker: TLseInvoke);cdecl;
procedure sh_dir(const invoker: TLseInvoke);cdecl;
procedure sh_change_dir(const invoker: TLseInvoke);cdecl;
procedure sh_copy(const invoker: TLseInvoke);cdecl;
procedure sh_delete(const invoker: TLseInvoke);cdecl;
procedure sh_list(const invoker: TLseInvoke);cdecl;
procedure sh_rmdir(const invoker: TLseInvoke);cdecl;
procedure sh_mkdir(const invoker: TLseInvoke);cdecl;
procedure sh_system(const invoker: TLseInvoke);cdecl;
procedure sh_shexec(const invoker: TLseInvoke);cdecl;

const
  func_count = 22;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'isfile:int |fname:string|';
     fr_addr:@sh_isfile;
     fr_desc:'is it an ordinary file?'
    ),
    (fr_prot:'isdir:int |dir:string|';
     fr_addr:@sh_isdir;
     fr_desc:'is it an directory?'
    ),
    (fr_prot:'dir:string ||';
     fr_addr:@sh_dir;
     fr_desc:'get current directory'
    ),
    (fr_prot:'cd:int |dir:string|';
     fr_addr:@sh_change_dir;
     fr_desc:'change current directory'
    ),
    (fr_prot:'cp:int |srcFile:string, dstFile:string, failIfFileExists:int|';
     fr_addr:@sh_copy;
     fr_desc:'copy file'
    ),
    (fr_prot:'rm:int |fname:string|';
     fr_addr:@sh_delete;
     fr_desc:'remove file'
    ),
    (fr_prot:'ls:string |mask:string|';
     fr_addr:@sh_list;
     fr_desc:'list files'
    ),
    (fr_prot:'rmdir:int |dir:string, deltree:int|';
     fr_addr:@sh_rmdir;
     fr_desc:'remove specified directory'
    ),
    (fr_prot:'mkdir:int |dir:string|';
     fr_addr:@sh_mkdir;
     fr_desc:'create directory'
    ),
    (fr_prot:'system:string |cmdline:string, dir:string|';
     fr_addr:@sh_system;
     fr_desc:'execute command line and get its output'
    ),
    (fr_prot:'shexec:int |cmdline:string, dir:string, wait:int|';
     fr_addr:@sh_shexec;
     fr_desc:'execute command line'
    ),

    { searcher }

    (fr_prot:'searcher_create:searcher |mask:string|';
     fr_addr:@searcher_create;
     fr_desc:'create file search object'
    ),
    (fr_prot:'searcher_path:string |sr:searcher|';
     fr_addr:@searcher_getPath;
     fr_desc:'get search path'
    ),
    (fr_prot:'searcher_name:string |sr:searcher|';
     fr_addr:@searcher_getName;
     fr_desc:'get file name without search path'
    ),
    (fr_prot:'searcher_eof:int |sr:searcher|';
     fr_addr:@searcher_eof;
     fr_desc:'return true when no more file can be found'
    ),
    (fr_prot:'searcher_fullName:string |sr:searcher|';
     fr_addr:@searcher_getFullName;
     fr_desc:'get full file name'
    ),
    (fr_prot:'searcher_size:int |sr:searcher|';
     fr_addr:@searcher_getSize;
     fr_desc:'get file size'
    ),
    (fr_prot:'searcher_isdir:int |sr:searcher|';
     fr_addr:@searcher_getisdir;
     fr_desc:'is it a directory?'
    ),
    (fr_prot:'searcher_isfile:int |sr:searcher|';
     fr_addr:@searcher_getisfile;
     fr_desc:'is it an ordinary file?'
    ),
    (fr_prot:'searcher_find:int |sr:searcher, mask:string|';
     fr_addr:@searcher_findFirst;
     fr_desc:'find first file'
    ),
    (fr_prot:'searcher_next:int |sr:searcher|';
     fr_addr:@searcher_findNext;
     fr_desc:'find next file'
    ),
    (fr_prot:'searcher_close:void |sr:searcher|';
     fr_addr:@searcher_findClose;
     fr_desc:'close searcher'
    )
  );
  
var
  searcher_type: RLseType = (
    cr_type    : LSV_OBJECT;
    cr_name    : 'searcher';
    cr_desc    : 'file searcher';
    cr_module  : nil;
    cr_addref  :@lse_addref_obj;
    cr_release :@lse_release_obj
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
  
procedure searcher_create(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  this := TLiSearch.Create(invoker.paramStr(0));
  invoker.returnObject(@searcher_type, this);
end;

procedure searcher_findFirst(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.FindFirst(invoker.paramStr(1)));
end;

procedure searcher_findNext(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.FindNext);
end;

procedure searcher_findClose(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    this.FindClose;
end;

procedure searcher_getName(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.Name);
end;

procedure searcher_eof(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(not this.Active);
end;

procedure searcher_getSize(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnInt64(this.FSrec.size);
end;

procedure searcher_getPath(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.SearchPath);
end;

procedure searcher_getFullName(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.FullName);
end;

procedure searcher_getIsDir(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.IsDir);
end;

procedure searcher_getIsFile(const invoker: TLseInvoke);cdecl;
var
  this: TLiSearch;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.IsFile);
end;

procedure sh_isfile(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnBool(FileExists(invoker.paramStr(0)));
end;

procedure sh_isdir(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnBool(DirectoryExists(invoker.paramStr(0)));
end;

procedure sh_dir(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnStr(GetCurrentDir);
end;

procedure sh_change_dir(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnBool(SetCurrentDir(invoker.paramStr(0)));
end;

procedure sh_copy(const invoker: TLseInvoke);cdecl;
var
  f_src, f_dst: string;
begin
  f_src := invoker.paramStr(0);
  f_dst := invoker.paramStr(1);
  invoker.returnBool(copy_file(f_src, f_dst, invoker.paramBool(2)));
end;

procedure sh_delete(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnBool(SysUtils.DeleteFile(invoker.paramStr(0)));
end;

procedure sh_list(const invoker: TLseInvoke);cdecl;
var
  list: string;
  sr: TLiSearch;
begin
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
end;

procedure sh_rmdir(const invoker: TLseInvoke);cdecl;
var
  dir: string;
begin
  dir := ExpandFileName(Trim(invoker.paramStr(0)));
  dir := ExcludeTrailingPathDelimiter(dir);
  if DirectoryExists(dir) then
  begin
    if SysUtils.RemoveDir(dir) then
      invoker.returnBool(true) else
    if invoker.paramBool(1) then
      invoker.returnBool(remove_tree(dir));
  end;
end;

procedure sh_mkdir(const invoker: TLseInvoke);cdecl;
begin
  invoker.returnBool(ForceDirectories(Trim(invoker.paramStr(0))));
end;

procedure sh_system(const invoker: TLseInvoke);cdecl;
var
  v_cmd, v_dir: string;
  status: integer;
begin
  v_cmd := Trim(invoker.paramFmt(0));
  v_dir := Trim(invoker.paramFmt(1));
  invoker.returnStr(spawn_shouts(v_cmd, v_dir, status));
end;

procedure sh_shexec(const invoker: TLseInvoke);cdecl;
var
  v_cmd, v_dir: string;
  status: integer;
begin
  v_cmd := Trim(invoker.paramFmt(0));
  v_dir := Trim(invoker.paramFmt(1));
  invoker.returnBool(spawn_shexec(v_cmd, v_dir,
    invoker.paramBool(2), status));
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
