unit frmTestlp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, lse_components, lse_kernel, SynEdit, lseu;

type

  THelloObject = class(TLyseeObject)
  private
    Text: string;
  end;

  { TTestlpForm }

  TTestlpForm = class(TForm)
    btnRun: TButton;
    Lysee: TLyseeEngine;
    hii: TLyseeModule;
    hello: TLyseeClass;
    hello_say: TLyseeMethod;
    hello_xxx: TLyseeMethod;
    hello_hello: TLyseeMethod;
    hii_alert: TLyseeFunc;
    hii_input: TLyseeFunc;
    god: TLyseeClass;
    LyseeFunc1: TLyseeFunc;
    LyseeMethod1: TLyseeMethod;
    LyseeMethod2: TLyseeMethod;
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
    procedure hello_helloExecute(Lobj: THelloObject; Invoker: TLseInvoke);
    procedure hello_sayExecute(Lobj: THelloObject; Invoker: TLseInvoke);
    procedure hello_xxxExecute(Lobj: THelloObject; Invoker: TLseInvoke);
    procedure hii_alertExecute(Invoker: TLseInvoke);
    procedure hii_inputExecute(Invoker: TLseInvoke);
    procedure helloCreateObject(Sender: TObject; var Lobj: THelloObject);
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
  Math;

procedure hii_clear(const Param: PLseParam);cdecl;
begin
  TestlpForm.Outputs.Lines.Clear;
  TestlpForm.Outputs.Lines.Add('cleared');
end;

{ TTestlpForm }

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
  StartLysee;
  hii.AddFunc('clear||', @hii_clear);
  Caption := ParamStr(0);
end;

procedure TTestlpForm.FormDestroy(Sender: TObject);
begin
  CloseLysee;
end;

procedure TTestlpForm.hello_helloExecute(Lobj: THelloObject; Invoker: TLseInvoke);
begin
  Lobj.Text := Invoker.paramStr(1);
end;

procedure TTestlpForm.hello_sayExecute(Lobj: THelloObject; Invoker: TLseInvoke);
begin
  ShowMessage(Lobj.Text);
end;

procedure TTestlpForm.hello_xxxExecute(Lobj: THelloObject; Invoker: TLseInvoke);
begin
  Lobj.Text := Lobj.Text + Invoker.paramStr(1);
  ShowMessage(Lobj.Text);
end;

procedure TTestlpForm.hii_alertExecute(Invoker: TLseInvoke);
begin
  ShowMessage(Invoker.ParamStr(0));
end;

procedure TTestlpForm.hii_inputExecute(Invoker: TLseInvoke);
var
  S: string;
begin
  S := InputBox('Input', Invoker.paramStr(0), '');
  Invoker.returnStr(S);
end;

procedure TTestlpForm.helloCreateObject(Sender: TObject; var Lobj: THelloObject);
begin
  Lobj := THelloObject.Create(hello);
end;

initialization
  {$I frmtestlp.lrs}

end.

