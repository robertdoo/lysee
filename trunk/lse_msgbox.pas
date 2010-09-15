{==============================================================================}
{        UNIT: lse_msgbox                                                      }
{ DESCRIPTION: show message box                                                }
{     CREATED: 2008/04/05                                                      }
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
unit lse_msgbox;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ELSE}
{$IFNDEF WINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, Forms, Dialogs, Controls
  {$IFNDEF FPC}
  , Windows, Messages
  {$ENDIF};

function AnsYes(const Question: string): boolean;
function AnsYesFmt(const Fmt: string; const Args: array of const): boolean;
function MsgOK(const Msg, Title: string): integer;
function MsgSys(const Msg: string): integer;
function MsgSysFmt(const Fmt: string; const Args: array of const): integer;
function MsgErr(const Msg: string): integer;
function MsgErrFmt(const Fmt: string; const Args: array of const): integer;

function ModalShow(AFormClass: TFormClass): integer;
procedure CenterScreen(AForm: TForm);

procedure lock_cursor;
procedure unlock_cursor;

{$IFNDEF FPC}
procedure SetTopmost(Form: TCustomForm; Topmost: boolean);
procedure TabNext(AForm: TForm);
{$ENDIF}

var
  title_confirm: string = '';
  title_ok: string = '';
  title_sys: string = '';
  title_error: string = '';

implementation

function AnsTitle: string;
begin
  if title_confirm = '' then
    Result := 'Confirmation' else
    Result := title_confirm;
end;

function OkTitle: string;
begin
  if title_ok = '' then
    Result := 'Information' else
    Result := title_ok;
end;

function SysTitle: string;
begin
  if title_sys = '' then
    Result := OkTitle else
    Result := title_sys;
end;

function ErrorTitle: string;
begin
  if title_error = '' then
    Result := 'Error' else
    Result := title_error;
end;

{$IFNDEF FPC}
function MsgBox(const Msg, Title: string; Flags: Integer): integer;
begin
  Result := MessageBox(Application.Handle, pchar(Msg), pchar(Title), Flags);
end;
{$ENDIF}

function AnsYes(const Question: string): boolean;
begin
  {$IFDEF FPC}
  Result := (mrYes = MessageDlg(AnsTitle, Question, mtConfirmation, [mbYes, mbNo], 0));
  {$ELSE}
  Result := MsgBox(Question, AnsTitle, MB_YESNO + MB_ICONQUESTION) = IDYES;
  {$ENDIF}
end;

function AnsYesFmt(const Fmt: string; const Args: array of const): boolean;
begin
  Result := AnsYes(Format(Fmt, Args));
end;

function MsgOK(const Msg, Title: string): integer;
var
  T: string;
begin
  if Title = '' then
    T := OkTitle else
    T := Title;
  {$IFDEF FPC}
  Result := MessageDlg(T, Msg, mtInformation, [mbYes], 0);
  {$ELSE}
  Result := MsgBox(Msg, T, MB_OK + MB_ICONINFORMATION);
  {$ENDIF}
end;

function MsgSys(const Msg: string): integer;
begin
  Result := MsgOK(Msg, SysTitle);
end;

function MsgSysFmt(const Fmt: string; const Args: array of const): integer;
begin
  Result := MsgSys(Format(Fmt, Args));
end;

function MsgErr(const Msg: string): integer;
begin
  {$IFDEF FPC}
  Result := MessageDlg(ErrorTitle, Msg, mtError, [mbYes], 0);
  {$ELSE}
  Result := MsgBox(Msg, ErrorTitle, MB_ICONERROR + MB_OK);
  {$ENDIF}
end;

function MsgErrFmt(const Fmt: string; const Args: array of const): integer;
begin
  Result := MsgErr(Format(Fmt, Args));
end;

function ModalShow(AFormClass: TFormClass): integer;
begin
  with AFormClass.Create(Application) do
  try
    Result := ShowModal;
  finally
    Release;
  end;
end;

procedure CenterScreen(AForm: TForm);
var
  x, y, w, h: integer;
begin
  w := AForm.Width;
  x := (Screen.Width - w) div 2;
  h := AForm.Height;
  y := (Screen.Height - h) div 2;
  AForm.SetBounds(x, y, w, h);
end;

var
  lock_count: integer;

procedure lock_cursor;
begin
  Screen.Cursor := crHourglass;
  inc(lock_count);
end;

procedure unlock_cursor;
begin
  if lock_count > 0 then
    dec(lock_count);
  if lock_count = 0 then
    Screen.Cursor := crDefault;
end;

{$IFNDEF FPC}
procedure SetTopmost(Form: TCustomForm; Topmost: boolean);
const
  M: array[boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Form.Handle, M[Topmost], 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);
end;

procedure TabNext(AForm: TForm);
begin
  SendMessage(AForm.Handle, WM_NEXTDLGCTL, 0, 0);
end;
{$ENDIF}

end.
