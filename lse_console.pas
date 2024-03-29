{==============================================================================}
{        UNIT: lse_console                                                     }
{ DESCRIPTION: console implementation of lysee script interpreter              }
{   COPYRIGHT: Copyright (c) 2003-2011, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/10/10                                                      }
{    MODIFIED: 2011/12/05                                                      }
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
  test, index: integer;
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
  spath := '';
  index := 1;
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
    FEngine.ExecCommandLine(index);
    if exec_pause then Pause;
    Exit;
  end;

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
        if masked(SCT_OK {+ SCT_RBLOCK}) then
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
  PrintLn('  -p, --pause             inteprete after execute script file.');
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
