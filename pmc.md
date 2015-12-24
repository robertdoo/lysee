# Pascal Module Compiler (PMC) #

What PMC does is to compile directly a free-pascal unit file to a lysee module.

By the time, PMC is packed in _**lysee\_pmc**_, a standalone console program.

## 1. PMC Usage ##

| **usage** | **description** |
|:----------|:----------------|
| `lysee_pmc` sample.pas | wrap sample.pas to samplew.lmp |
| `lysee_pmc` -FPC-OPTIONS... sample.pas | wrap sample.pas to samplew.lmp and compile samplew.lmp with fpc |

here, .**lmp** means lysee module project.

## 2. File Content ##

### 2.1. sample.pas ###
```pas

{@pmc-description sample pascal module}
{@pmc-version 0.0.1}
unit sample;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils;

function MaxStr(const S1, S2: string): string;

implementation

function MaxStr(const S1, S2: string): string;
begin
if S1 >= S2 then
Result := S1 else
Result := S2;
end;

end.
```
### 2.2. samplew.lmp ###
```pas

library samplew;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}
{$IFNDEF WINDOWS}
{$IFDEF MSWINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

uses
classes, sysutils, sample, lseu;

procedure pp_maxStr(const Param: PLseParam);cdecl;
var
s1: string;
s2: string;
fres: string;
begin
s1 := lse_get_str(Param^.p_param[0]);
s2 := lse_get_str(Param^.p_param[1]);
fres := maxStr(s1, s2);
lse_set_string(Param^.p_result, fres);
end;

const
func_array: array[0..0] of RLseFunc = (
(fr_prot:'maxStr:string(s1:string, s2:string)';
fr_addr:@pp_maxStr;
fr_desc:''
)
);

procedure InitExchange(const MR: PLseModule; const ER: PLseEntry);cdecl;
begin
lse_prepare(ER);
MR^.iw_version := '0.0.1';
MR^.iw_desc := 'sample pascal module';
MR^.iw_call := @lse_call_gate;
MR^.iw_funcs.fl_count := 1;
MR^.iw_funcs.fl_entry := @func_array;
end;

exports
InitExchange;

begin

end.
```