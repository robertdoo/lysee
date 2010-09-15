{==============================================================================}
{     PROJECT: lysee_inifs_funcs                                               }
{ DESCRIPTION: functions of init file                                          }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2010/09/01                                                      }
{==============================================================================}
{ Copyright (c) 2008-2010, Li Yun Jie                                          }
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
{ Portions created by Li Yun Jie are Copyright (C) 2007-2010.                  }
{ All Rights Reserved.                                                         }
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

  inifile_func_count = 13;
  inifile_func_array: array[0..inifile_func_count - 1] of RLseFuncRec = (
    (fr_prot:'inifile inifile(string fileName)';
     fr_addr:@pp_inifile_create;
     fr_desc:'open ini file'
    ),
    (fr_prot:'string get_fileName()';
     fr_addr:@pp_inifile_fname;
     fr_desc:'get ini file name';
    ),
    (fr_prot:'inifile open(string fileName)';
     fr_addr:@pp_inifile_open;
     fr_desc:'open another ini file'
    ),
    (fr_prot:'inifile close()';
     fr_addr:@pp_inifile_close;
     fr_desc:'close current file'
    ),
    (fr_prot:'string read(string section, string key, string defValue)';
     fr_addr:@pp_inifile_read;
     fr_desc:'read value'
    ),
    (fr_prot:'inifile write(string section, string key, string value)';
     fr_addr:@pp_inifile_write;
     fr_desc:'write value'
    ),
    (fr_prot:'strlist get_sections()';
     fr_addr:@pp_inifile_sections;
     fr_desc:'get section list'
    ),
    (fr_prot:'strlist keys(string section)';
     fr_addr:@pp_inifile_keys;
     fr_desc:'get key list of specified section'
    ),
    (fr_prot:'strlist values(string section)';
     fr_addr:@pp_inifile_values;
     fr_desc:'get value list of specified section'
    ),
    (fr_prot:'bool sectionExists(string section)';
     fr_addr:@pp_inifile_sectionExists;
     fr_desc:'check if a named section eixsts'
    ),
    (fr_prot:'inifile eraseSection(string section)';
     fr_addr:@pp_inifile_eraseSection;
     fr_desc:'erase a section'
    ),
    (fr_prot:'bool keyExists(string section, string key)';
     fr_addr:@pp_inifile_keyExists;
     fr_desc:'check if a key exists'
    ),
    (fr_prot:'inifile deleteKey(string section, string key)';
     fr_addr:@pp_inifile_deleteKey;
     fr_desc:'delete a key'
    )
  );

  inifile_class: RLseClassRec = (
    vtype      : LSV_OBJECT;
    name       : 'inifile';
    desc       : 'ini file read/write class';
    incRefcount:@lse_incRefcount;
    decRefcount:@lse_decRefcount;
    funcs      : (count:inifile_func_count; entry:@inifile_func_array)
  );

implementation

procedure pp_inifile_create(const invoker: TLseInvoke);cdecl;
var
  this: TLiIniFile;
begin
  this := TLiIniFile.Create(invoker.paramStr(1));
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

