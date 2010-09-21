unit frmTestlp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lysee_laz, SynEdit, lseu;

type

  { TTestlpForm }

  TTestlpForm = class(TForm)
    btnRun: TButton;
    Lysee: TLyseeEngine;
    lsfAlert: TLyseeFunc;
    lsmHii: TLyseeModule;
    lsfInput: TLyseeFunc;
    lscHello: TLyseeClass;
    lsmSay: TLyseeMethod;
    lsmHello: TLyseeMethod;
    Outputs: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Inputs: TSynEdit;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lscHelloDestroyObject(Sender: TObject; Obj: TLyseeObject);
    procedure lsfAlertInvoke(Sender: TObject; Invoker: TLseInvoke);
    procedure lsfInputInvoke(Sender: TObject; Invoker: TLseInvoke);
    procedure lsmSayExecute(Sender: TObject; Param: PLseParam);
    procedure lsmHelloInvoke(Sender: TObject; Invoker: TLseInvoke);
    procedure LyseeRead(Sender: TObject; const Buf: pchar; var Count: integer);
    procedure LyseeReadln(Sender: TObject; var S: string);
    procedure LyseeWrite(Sender: TObject; const Buf: pchar; var Count: integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TestlpForm: TTestlpForm;

implementation

uses
  Math, lse_kernel;

procedure hii_clear(const Param: PLseParam);cdecl;
begin
  TestlpForm.Outputs.Lines.Clear;
  TestlpForm.Outputs.Lines.Add('cleared');
end;

{ TTestlpForm }

procedure TTestlpForm.lsfAlertInvoke(Sender: TObject; Invoker: TLseInvoke);
begin
  ShowMessage(Invoker.ParamStr(0));
end;

procedure TTestlpForm.lsfInputInvoke(Sender: TObject; Invoker: TLseInvoke);
var
  S: string;
begin
  S := InputBox('Input', Invoker.paramStr(0), '');
  Invoker.returnStr(S);
end;

procedure TTestlpForm.lsmSayExecute(Sender: TObject; Param: PLseParam);
var
  obj: TLyseeObject;
begin
  if __getThis(Param, obj) then
    ShowMessage(string(obj.Data));
end;

procedure TTestlpForm.lsmHelloInvoke(Sender: TObject; Invoker: TLseInvoke);
var
  obj: TLyseeObject;
begin
  if Invoker.GetThis(obj) then
    string(obj.Data) := Invoker.paramStr(1);
end;

procedure TTestlpForm.LyseeRead(Sender: TObject; const Buf: pchar; var Count: integer);
var
  S: string;
  L: integer;
begin
  S := InputBox('Input', 'Input a string:', '');
  L := Max(0, Min(Count, Length(S)));
  if L > 0 then
    Move(pchar(S)^, Buf^, L);
  Count := L;
end;

procedure TTestlpForm.LyseeReadln(Sender: TObject; var S: string);
begin
  S := InputBox('Input', 'Input a string:', '');
end;

procedure TTestlpForm.LyseeWrite(Sender: TObject; const Buf: pchar; var Count: integer);
var
  S: string;
  L: TStrings;
  X: integer;
begin
  SetString(S, Buf, Count);
  L := TStringList.Create;
  try
    L.Text := S;
    for X := 0 to L.Count - 1 do
      Outputs.Lines.Add(L[X]);
  finally
    L.Free;
  end;
end;

procedure TTestlpForm.btnRunClick(Sender: TObject);
begin
  Outputs.Lines.Clear;
  Lysee.Codes.Assign(Inputs.Lines);
  if not Lysee.Execute then
    Outputs.Lines.Text := Lysee.Engine.Error;
end;

procedure TTestlpForm.FormCreate(Sender: TObject);
begin
  InitLysee([lsmHii]);
  lsmHii.AddFunc('clear||', @hii_clear);
end;

procedure TTestlpForm.FormDestroy(Sender: TObject);
begin
  ExitLysee;
end;

procedure TTestlpForm.lscHelloDestroyObject(Sender: TObject; Obj: TLyseeObject);
begin
  string(Obj.Data) := '';
end;

initialization
  {$I frmtestlp.lrs}

end.

