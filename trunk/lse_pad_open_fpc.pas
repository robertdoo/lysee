{==============================================================================}
{        UNIT: lysee_pad_open_fpc                                              }
{ DESCRIPTION: open file dialog or lysee_pad_fpc (FPC)                         }
{     CREATED: 2008/08/23                                                      }
{    MODIFIED: 2010/08/31                                                      }
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
{ Portions created by Li Yun Jie are Copyright (C) 2003-2010.                  }
{ All Rights Reserved.                                                         }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lse_pad_open_fpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TOpenForm }

  TOpenForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnOpen: TButton;
    dlgOpen: TOpenDialog;
    edtFile: TLabeledEdit;
    procedure btnOpenClick(Sender: TObject);
    procedure edtFileChange(Sender: TObject);
  private
    FFileName: string;
  end; 

var
  OpenForm: TOpenForm;

function GetOpenFile(var FileName: string): boolean;

implementation

function GetOpenFile(var FileName: string): boolean;
begin
  if OpenForm = nil then
    OpenForm := TOpenForm.Create(Application);
  {$IFDEF WINDOWS}
  Result := OpenForm.dlgOpen.Execute;
  if Result then
    FileName := OpenForm.dlgOpen.FileName else
    FileName := '';
  {$ELSE}
  Result := (OpenForm.ShowModal = mrOK);
  if Result then
  begin
    FileName := OpenForm.FFileName;
    OpenForm.dlgOpen.FileName := FileName;
  end
  else FileName := '';
  {$ENDIF}
end;

{ TOpenForm }

procedure TOpenForm.edtFileChange(Sender: TObject);
begin
  FFileName := Trim(edtFile.Text);
  if FFileName <> '' then
  begin
    FFileName := ExpandFileName(FFileName);
    btnOK.Enabled := FileExists(FFileName) and not DirectoryExists(FFileName);
  end
  else btnOK.Enabled := false;
end;

procedure TOpenForm.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtFile.Text := dlgOpen.FileName;
end;

initialization
  {$I lse_pad_open_fpc.lrs}

end.

