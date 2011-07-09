{==============================================================================}
{        UNIT: lysee_zipper_funcs                                              }
{ DESCRIPTION: zip/unzip functions (FPC)                                       }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2010/08/31                                                      }
{==============================================================================}
{ Copyright (c) 2007-2010, Li Yun Jie                                          }
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
unit lysee_zipper_funcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, lseu;

type

  { TMyUnZipper }

  TMyUnZipper = class(TUnZipper)
  private
    FFileList: TStrings;
  public
    procedure UnZipOneFile(Item: TFullZipFileEntry);override;
  end;

  { TMyZipper }

  TMyZipper = TZipper;

procedure zipper_list(const invoker: TLseInvoke);cdecl;
procedure zipper_unzip(const invoker: TLseInvoke);cdecl;
procedure zipper_unzip_file(const invoker: TLseInvoke);cdecl;
procedure zipper_zip(const invoker: TLseInvoke);cdecl;

const
  func_count = 4;
  func_array: array[0..func_count - 1] of RLseFunc = (
    (fr_prot:'list:string |zipFile:string|';
     fr_addr:@zipper_list;
     fr_desc:'list sub-files in source zip file'
    ),
    (fr_prot:'unzip:void |zipFile:string, outputPath:string|';
     fr_addr:@zipper_unzip;
     fr_desc:'unzip all files to specified path'
    ),
    (fr_prot:'unzipf:void |zipFile:string, subFile:string, outputPath:string|';
     fr_addr:@zipper_unzip_file;
     fr_desc:'unzip specified sub file'
    ),
    (fr_prot:'zip:int |zipFile:string, sourceFile:string|';
     fr_addr:@zipper_zip;
     fr_desc:'create zip file with passed source file or LB delimited list'
    )
  );

implementation

procedure zipper_list(const invoker: TLseInvoke);cdecl;
var
  Z: TMyUnZipper;
begin
  Z := TMyUnZipper.Create;
  try
    Z.FFileList := TStringList.Create;
    try
      Z.UnZipAllFiles(invoker.paramStr(0));
      invoker.returnStr(Z.FFileList.Text);
    finally
      FreeAndNil(Z.FFileList);
    end;
  finally
    Z.Free;
  end;
end;

procedure zipper_unzip(const invoker: TLseInvoke);cdecl;
var
  Z: TMyUnZipper;
begin
  Z := TMyUnZipper.Create;
  try
    Z.OutputPath := Trim(invoker.paramStr(1));
    Z.UnZipAllFiles(Trim(invoker.paramStr(0)));
  finally
    Z.Free;
  end;
end;

procedure zipper_unzip_file(const invoker: TLseInvoke);cdecl;
var
  Z: TMyUnZipper;
  L: TStrings;
  F: string;
begin
  F := Trim(invoker.paramStr(1));
  if F <> '' then
  begin
    Z := TMyUnZipper.Create;
    try
      L := TStringList.Create;
      try
        Z.OutputPath := Trim(invoker.paramStr(2));
        L.Add(F);
        Z.UnZipFiles(Trim(invoker.paramStr(0)), L);
      finally
        L.Free;
      end;
    finally
      Z.Free;
    end;
  end;
end;

procedure zipper_zip(const invoker: TLseInvoke);cdecl;
var
  Z: TZipper;
  L: TStrings;
  X: integer;
begin
  L := TStringList.Create;
  try
    L.Text := Trim(invoker.paramStr(1));
    for X := L.Count - 1 downto 0 do
    begin
      L[X] := Trim(L[X]);
      if not FileExists(L[X]) then
        L.Delete(X);
    end;

    if L.Count > 0 then
    begin
      Z := TMyZipper.Create;
      try
        Z.ZipFiles(Trim(invoker.paramStr(0)), L);
      finally
        Z.Free;
      end;
    end;

    invoker.returnInt(L.Count);
  finally
    L.Free;
  end;
end;

{ TMyUnZipper }

procedure TMyUnZipper.UnZipOneFile(Item: TFullZipFileEntry);
var
  ofile: string;
begin
  ofile := Item.ArchiveFileName;

  if FFileList <> nil then
  begin
    FFileList.Add(ofile);
    Exit;
  end;

  ofile := lse_veryPD(ofile);
  Item.DiskFileName := ofile;

  if ofile[Length(ofile)] = LSE_PATH_DELIMITER then
  begin
    if OutputPath <> '' then
      ForceDirectories(lse_veryPD(OutputPath+ LSE_PATH_DELIMITER + ofile)) else
      ForceDirectories(ofile);
    Exit;
  end;

  inherited UnZipOneFile(Item);
end;

end.

