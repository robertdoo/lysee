{==============================================================================}
{        UNIT: lse_console                                                     }
{ DESCRIPTION: console implementation of lysee script interpreter              }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/07/09                                                      }
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
unit lse_console;

{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, lseu
  {$IFDEF WINDOWS},Windows{$ENDIF};

type
  KLiCommander = class(TLseObject)
  private
    FEngine: TLseEngine;
  public
    constructor Create;
    destructor Destroy;override;
    function ReadLine: string;
    procedure Print(const Text: string);overload;
    procedure Print(const Text: string; const Args: array of const);overload;
    procedure PrintLn(const Text: string);overload;
    procedure PrintLn(const Text: string; const Args: array of const);overload;
    procedure PrintHelp;
    procedure PrintVersion;
    procedure Command;
  end;

procedure Execute;

implementation

uses
  DateUtils, Math;

procedure Execute;
begin
  lse_startup;
  try
    with KLiCommander.Create do
    try
      Command;
    finally
      Free;
    end;
  finally
    lse_cleanup;
  end;
end;

procedure WaitForSeconds(Seconds: integer);
begin
  while Seconds > 0 do
  begin
    Sleep(1000);
    Dec(Seconds);
  end;
end;

{ KLiCommander }

procedure KLiCommander.Command;
var
  code, line, temp, spath, args: string;
  test, index, seconds, times: integer;
  exec_pause: boolean;

  function masked(mask: integer): boolean;
  begin
    Result := ((mask and test) = mask);
  end;

  procedure restart;
  begin
    code := '';
    FEngine.Clear;
    FEngine.WriteLine('');
    FEngine.WriteLine('************************* RESTARTED *************************');
    FEngine.WriteLine('');
  end;

  procedure Cancel;
  begin
    if code <> '' then
      FEngine.WriteLine('*************** CANCELED ***************');
    code := '';
  end;

  procedure Pause;
  begin
    FEngine.WriteLine('');
    FEngine.WriteText('Press ENTER to continue: ');
    ReadLine;
  end;
  
begin
  seconds := 0;
  spath := '';
  index := 1;
  times := 1;
  exec_pause := false;
  args := ParamStr(0);
  
  while index <= ParamCount do
  begin
    line := Trim(ParamStr(index));
    
    if (line = '-h') or (line = '--help') then
    begin
      PrintHelp;
      Exit;
    end;

    if (line = '-v') or (line = '--version') then
    begin
      PrintVersion;
      Exit;
    end;
    
    if (line = '-s') or (line = '--search') then
    begin
      if index = ParamCount then
      begin
        PrintLn('Error: search path is not specified.');
        Exit;
      end;
      Inc(index);
      spath := spath + ';' + Trim(ParamStr(index));
      Inc(index);
    end
    else
    if (line = '-w') or (line = '--wait') then
    begin
      if index = ParamCount then
      begin
        PrintLn('Error: seconds to wait not supplied.');
        Exit;
      end;
      Inc(index);
      seconds := StrToIntDef(ParamStr(index), 0);
      Inc(index);
    end
    else
    if (line = '-t') or (line = '--times') then
    begin
      if index = ParamCount then
      begin
        PrintLn('Error: execute times not supplied.');
        Exit;
      end;
      Inc(index);
      times := Max(1, StrToIntDef(ParamStr(index), 1));
      Inc(index);
    end
    else
    if (line = '-p') or (line = '--pause') then
    begin
      exec_pause := true;
      Inc(index);
    end
    else Break;
  end;

  FEngine.SearchPath := spath;
  FEngine.Arguments := args;

  if index <= ParamCount then
  begin
    while times > 0 do
    begin
      Dec(times);
      WaitForSeconds(seconds);
      FEngine.ExecCommandLine(index);
      FEngine.Clear;
    end;
    if exec_pause then Pause;
  end
  else
  begin
    line := lse_kernel_production(true);
    FEngine.WriteLine(line);
    FEngine.WriteLine('');
    temp := ':: /C=CANCEL /Q=QUIT /R=RESTART ::';
    FEngine.WriteLine(StringOfChar(' ', (Length(line) - Length(temp)) div 2) + temp);
    FEngine.WriteLine('');

    code := '';
    repeat
      try
        if code = '' then
          FEngine.WriteText('>>> ') else
          FEngine.WriteText('  > ');
        line := ReadLine;
        temp := Trim(line);
        if AnsiSameText('/q', temp) then Break else
        if AnsiSameText('/r', temp) then Restart else
        if AnsiSameText('/c', temp) then Cancel else
        if temp <> '' then
        begin
          if code <> '' then
            code := code + sLineBreak + line else
            code := line;
          test := lse_simple_test(code);
          if masked(SCT_ERROR) then
          begin
            FEngine.WriteLine('*************** INCORRECT ***************');
            code := '';
          end
          else
          if masked(SCT_OK + SCT_RBLOCK) then
          begin
            if FEngine.ExecuteCode(code) then
              FEngine.WriteLine(FEngine.ResultText) else
              FEngine.WriteLine(FEngine.Error);
            code := '';
          end;
        end
        else
        if code <> '' then
          code := code + sLineBreak;
      except
        FEngine.WriteLine('*** ' + lse_exception_str);
      end;
    until FEngine.Exited;
  end;
end;

constructor KLiCommander.Create;
begin
  FEngine := TLseEngine.Create(nil);
end;

destructor KLiCommander.Destroy;
begin
  FreeAndNil(FEngine);
  inherited;
end;

procedure KLiCommander.Print(const Text: string);
begin
  FEngine.WriteText(Text);
end;

procedure KLiCommander.Print(const Text: string; const Args: array of const);
begin
  Print(Format(Text, Args));
end;

procedure KLiCommander.PrintHelp;
begin
  PrintLn('Lysee %s - interactive LYSEE script interpreter', [lse_kernel_version]);
  PrintLn('');
  PrintLn('Usage: lysee [OPTION]... [FILE [ARGS]...]');
  PrintLn('');
  PrintLn('Option:');
  PrintLn('  -v, --version           display the version of lysee and exit.');
  PrintLn('  -h, --help              print this help and exit.');
  PrintLn('  -s, --search=PATH       set module search path.');
  PrintLn('  -w, --wait=SECONDS      wait for some seconds before going on.');
  PrintLn('  -t, --times=COUNT       execute specified times.');
  PrintLn('  -p, --pause             pause after execute script file.');
  PrintLn('');
  PrintLn('File:');
  PrintLn('  .ls                     execute this file directly.');
  PrintLn('  .lsp, .html, .htm, .*   execute as a LSP file (Lysee Script Page).');
  PrintLn('');
  PrintLn('Args:');
  PrintLn('  *                       arguments for file execution.');
end;

procedure KLiCommander.PrintLn(const Text: string);
begin
  FEngine.WriteLine(Text);
end;

procedure KLiCommander.PrintLn(const Text: string; const Args: array of const);
begin
  PrintLn(Format(Text, Args));
end;

procedure KLiCommander.PrintVersion;
begin
  PrintLn(lse_kernel_production(true));
  PrintLn('');
  PrintLn('This program is distributed in the hope that it will be useful,');
  PrintLn('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  PrintLn('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  PrintLn('');
  PrintLn('Enjoy it! I am libudi [li yunjie, zhengzhou, henan, china].');
end;

function KLiCommander.ReadLine: string;
begin
  System.Readln(Result);
end;

end.
