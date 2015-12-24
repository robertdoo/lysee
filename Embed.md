# Embed Lysee Step By Step #

This page will lead you create a expression evaluating application called _**calc.lpi**_ to explain how to extend a program's capability by embeding lyseee.

## 1. Create Project _calc.lpi_ ##

  1. Open lazarus and create a _**Application**_ project.
  1. Save this project as _**calc.lpi**_.
  1. Name main form as _**CalcForm**_ and save as _**frmcalc.pas**_.
  1. Place a label editor on _**CalcForm**_ and rename it as _**eExpr**_.
  1. Place a label editor on _**CalcForm**_ and rename is as _**eResult**_.
  1. Place a button on _**CalcForm**_ and rename it as _**bCalc**_.


---

## 2. Add Lysee Kernel To Project ##

Add _**lseu.pas**_ and _**lse\_kernel.pas**_ into this project.

```
program calc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  frmcalc,
  lseu in '../../lseu.pas',
  lse_kernel in '../../lse_kernel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCalcForm, CalcForm);
  Application.Run;
end.
```


---

## 3. Use _lseu.pas_ And _lse\_kernel.pas_ in _frmcalc_ ##

Add _**lseu**_, _**lse\_kernel**_ into _**frmcalc**_'s uses clause:

```
unit frmcalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, lseu, lse_kernel;
```


---

## 4. Startup Lysee Kernel ##

Create _**`OnCreate`**_ event handler and call _**lse\_startup**_ to initialize lysee kernel:

```
procedure TCalcForm.FormCreate(Sender: TObject);
begin
  lse_startup;
end;
```


---

## 5. Cleanup Lysee Kernel ##

Create _**`OnDestroy`**_ event handler and call _**lse\_cleanup**_ to finalize lysee kernel.

```
procedure TCalcForm.FormDestroy(Sender: TObject);
begin
  lse_cleanup;
end;
```


---

## 6. Create TLseEngine Instance ##

6.1. Define _**FEngine: TLseEngine**_ in TCalcForm:

```
type

  { TCalcForm }

  TCalcForm = class(TForm)
    bCalc: TButton;
    eExpr: TLabeledEdit;
    eResult: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FEngine: TLseEngine;
  end; 
```

6.2. Create FEngine:

```
procedure TCalcForm.FormCreate(Sender: TObject);
begin
  lse_startup;
  FEngine := TLseEngine.Create(nil);
end;
```

6.3. Destroy FEngine:

```
procedure TCalcForm.FormDestroy(Sender: TObject);
begin
  FEngine.Free;
  lse_cleanup;
end;
```


---

## 7. Evaluate Expression ##

Create _**bCalc**_'s _**`OnClick`**_ event handler and evaluate expression in _**eExpr**_:

```
procedure TCalcForm.bCalcClick(Sender: TObject);
begin
  eResult.Text := '';
  if FEngine.ExecuteCode(eExpr.Text) then
    eResult.Text := FEngine.ResultText else
    ShowMessage(FEngine.ErrorMsg);
end;
```

Press F9 to run this program and type a expression(for example: `6 * 9`) to evaluate.


---

## 8. Create '_calc_' Module ##

8.1. Define _**FCalcModule: KLiModule**_ in TCalcForm:

```
type

  { TCalcForm }

  TCalcForm = class(TForm)
    bCalc: TButton;
    eExpr: TLabeledEdit;
    eResult: TLabeledEdit;
    procedure bCalcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FEngine: TLseEngine;
    FCalcModule: KLiModule;
  end; 
```

8.2. Create And Load _**calc**_ Module:

```
procedure TCalcForm.FormCreate(Sender: TObject);
begin
  lse_startup;
  FEngine := TLseEngine.Create(nil);
  // create calc module
  FCalcModule := KLiModule.Create('calc', nil, mtBuiltin);
  // load calc module
  FEngine.Load('calc');
end;
```


---

## 9. Register Lysee Functions ##

9.1. Define lysee function _**triple\_mul**_ to mul a value 3 times:

```
// triple_mul:float(value:float)
procedure triple_mul(const Param: PLseParam);cdecl;
var
  V: double;
begin
  with Param^ do
  begin
    V := lse_get_float(p_param[0]);
    lse_set_float(p_result, V * V * V);
  end;
end;
```

9.2. Register function _**triple\_mul**_ into _**calc**_ module:

```
procedure TCalcForm.FormCreate(Sender: TObject);
begin
  lse_startup;
  FEngine := TLseEngine.Create(nil);
  // create calc module
  FCalcModule := KLiModule.Create('calc', nil, mtBuiltin);
  // register function triple_mul
  FCalcModule.SetupFunc(@triple_mul, 'triple_mul:float(value:float)', 'mul float value 3 times');
  // load calc module
  FEngine.ExecuteCode('load("calc")');
end;
```

Press F9 to run this program again and enter '_**triple\_mul(10)**_' to evaluate, you should got 1000.

From now on, you can repeat step 8..9 to register any amount of lysee modules and functions to extend _**calc**_'s evaluating capability.


---

**NOTE:**

If you like, you can directly register _**triple\_mul**_ and other functions into _**sys**_ module without creating _**calc**_ module.

```
procedure TCalcForm.FormCreate(Sender: TObject);
begin
  lse_startup;
  FEngine := TLseEngine.Create(nil);
  // register function triple_mul
  sys_module.SetupFunc(@triple_mul, 'triple_mul:float(value:float)', 'mul float value 3 times');
end;
```