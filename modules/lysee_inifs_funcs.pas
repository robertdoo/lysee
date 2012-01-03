{==============================================================================}
{     PROJECT: lysee_inifs_funcs                                               }
{ DESCRIPTION: functions of init file                                          }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2011/08/07                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_inifs_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lseu, inifiles;

type

  { TLiIniFile}

  TLiIniFile = class(TLseObject)
  private
    FIniFile: TIniFile;
    FFileName: string;
  public
    constructor Create(const FileName: string);
    destructor Destroy;override;
    procedure Open(const FileName: string);
    procedure Close;
    function Read(const Section, Key, DefValue: string): string;
    procedure Write(const Section, Key, Value: string);
    property fileName: string read FFileName write Open;
    property IniFile: TIniFile read FIniFile;
  end;

{ inifile }

procedure pp_inifile_create(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_fname(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_open(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_close(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_read(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_write(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_sections(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_keys(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_values(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_sectionExists(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_eraseSection(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_keyExists(const invoker: TLseInvoke);cdecl;
procedure pp_inifile_deleteKey(const invoker: TLseInvoke);cdecl;

const

  func_count = 13;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'inifile_create:inifile |fileName:string|';
     fr_addr:@pp_inifile_create;
     fr_desc:'open ini file'
    ),
    (fr_prot:'inifile_fname:string |inif:inifile|';
     fr_addr:@pp_inifile_fname;
     fr_desc:'get ini file name';
    ),
    (fr_prot:'inifile_open:void |inif:inifile, fileName:string|';
     fr_addr:@pp_inifile_open;
     fr_desc:'open another ini file'
    ),
    (fr_prot:'inifile_close:void |inif:inifile|';
     fr_addr:@pp_inifile_close;
     fr_desc:'close current file'
    ),
    (fr_prot:'inifile_read:string |inif:inifile, section:string, key:string, defValue:string|';
     fr_addr:@pp_inifile_read;
     fr_desc:'read value'
    ),
    (fr_prot:'inifile_write:void |inif:inifile, section:string, key:string, value:string|';
     fr_addr:@pp_inifile_write;
     fr_desc:'write value'
    ),
    (fr_prot:'inifile_sections:string |inif:inifile|';
     fr_addr:@pp_inifile_sections;
     fr_desc:'get section list'
    ),
    (fr_prot:'inifile_keys:string |inif:inifile, section:string|';
     fr_addr:@pp_inifile_keys;
     fr_desc:'get key list of specified section'
    ),
    (fr_prot:'inifile_values:string |inif:inifile, section:string|';
     fr_addr:@pp_inifile_values;
     fr_desc:'get value list of specified section'
    ),
    (fr_prot:'inifile_sectionExists:int |inif:inifile, section:string|';
     fr_addr:@pp_inifile_sectionExists;
     fr_desc:'check if a named section eixsts'
    ),
    (fr_prot:'inifile_eraseSection:void |inif:inifile, section:string|';
     fr_addr:@pp_inifile_eraseSection;
     fr_desc:'erase a section'
    ),
    (fr_prot:'inifile_keyExists:int |inif:inifile, section:string, key:string|';
     fr_addr:@pp_inifile_keyExists;
     fr_desc:'check if a key exists'
    ),
    (fr_prot:'inifile_deleteKey:void |inif:inifile, section:string, key:string|';
     fr_addr:@pp_inifile_deleteKey;
     fr_desc:'delete a key'
    )
  );

var
  inifile_class: RLseType = (
    cr_type    : LSV_OBJECT;
    cr_name    : 'inifile';
    cr_desc    : 'ini file read/write class';
    cr_module  : nil;
    cr_addref  :@lse_addref_obj;
    cr_release :@lse_release_obj
  );

implementation

procedure pp_inifile_create(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  this := TLiIniFile.Create(invoker.paramStr(0));
  invoker.returnObj(@inifile_class, this);
end;

procedure pp_inifile_fname(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.FFileName);
end;

procedure pp_inifile_open(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    this.Open(invoker.paramStr(1));
end;

procedure pp_inifile_close(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    this.Close;
end;

procedure pp_inifile_read(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    invoker.returnStr(this.Read(
      invoker.paramStr(1),
      invoker.paramStr(2),
      invoker.paramStr(3)));
end;

procedure pp_inifile_write(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    this.Write(invoker.paramStr(1),
               invoker.paramStr(2),
               invoker.paramStr(3));
end;

procedure pp_inifile_sections(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
  list: TStringList;
begin
  if invoker.GetThis(this) then
  begin
    list := TStringList.Create;
    try
      this.FIniFile.ReadSections(list);
      invoker.returnStr(list.Text);
    finally
      list.Free;
    end;
  end;
end;

procedure pp_inifile_keys(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
  list: TStringList;
begin
  if invoker.GetThis(this) then
  begin
    list := TStringList.Create;
    try
      this.FIniFile.ReadSection(invoker.paramStr(1), list);
      invoker.returnStr(list.Text);
    finally
      list.Free;
    end;
  end;
end;

procedure pp_inifile_values(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
  list: TStringList;
begin
  if invoker.GetThis(this) then
  begin
    list := TStringList.Create;
    try
      this.FIniFile.ReadSectionValues(invoker.paramStr(1), list);
      invoker.returnStr(list.Text);
    finally
      list.Free;
    end;
  end;
end;

procedure pp_inifile_sectionExists(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.FIniFile.SectionExists(invoker.paramStr(1)));
end;

procedure pp_inifile_eraseSection(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    this.FIniFile.EraseSection(invoker.paramStr(1));
end;

procedure pp_inifile_keyExists(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    invoker.returnBool(this.FIniFile.ValueExists(
      invoker.paramStr(1),
      invoker.paramStr(2)));
end;

procedure pp_inifile_deleteKey(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  if invoker.GetThis(this) then
    this.FIniFile.DeleteKey(invoker.paramStr(1),
                            invoker.paramStr(2));
end;

{ TLiIniFile }

procedure TLiIniFile.Close;
begin
  FFileName := '';
  FreeAndNil(FIniFile);
end;

constructor TLiIniFile.Create(const FileName: string);
begin
  Open(FileName);
end;

destructor TLiIniFile.Destroy;
begin
  Close;
  inherited;
end;

procedure TLiIniFile.Open(const FileName: string);
begin
  try
    Close;
    FFileName := ExpandFileName(Trim(FileName));
    if FFileName <> '' then
      FIniFile := TIniFile.Create(FFileName);
  except
    FFileName := '';
    raise;
  end;
end;

function TLiIniFile.Read(const Section, Key, DefValue: string): string;
begin
  if FIniFile <> nil then
    Result := FIniFile.ReadString(Section, Key, DefValue) else
    Result := DefValue;
end;

procedure TLiIniFile.Write(const Section, Key, Value: string);
begin
  if FIniFile <> nil then
    FIniFile.WriteString(Section, Key, Value);
end;

end.

